import re
import os
import json
import subprocess
from pathlib import Path
import frontmatter
from langchain_core.messages import HumanMessage, SystemMessage, AIMessage, messages_to_dict, messages_from_dict
from dotenv import load_dotenv
from rich.console import Console
from rich.prompt import Prompt
from rich.live import Live
from platformdirs import user_config_dir

from .providers import PROVIDERS

class AIsh:
    def __init__(self, model_name: str = None, provider_name: str = None, agent_name: str = None, history_file: str = None):
        self.console = Console(force_terminal=True)

        self.config_path = self._get_config_path()
        self.config = self._load_config()
        
        load_dotenv(self.config_path / ".env")

        self.agent_name = agent_name or os.getenv("AISH_AGENT") or self.config["default_agent"]
        self.agent_prompt, self.agent_metadata = self._get_agent_prompt()

        self.default_connection = self.config["connections"][self.agent_metadata.get("connection")] or next(iter(self.config["connections"].values()))
        self.model_name = model_name or self.agent_metadata.get("model") or os.getenv("AISH_MODEL") or self.default_connection['model']
        self.provider_name = provider_name or self.agent_metadata.get("provider") or os.getenv("AISH_PROVIDER") or self.default_connection['provider']
        
        self._setup_api_key()
        
        self.llm = PROVIDERS[self.provider_name](model=self.model_name)

        self.session_id = self._get_session_id()
        self.session_file = self._get_session_file()
        self.history_file = Path(history_file).expanduser() if history_file else None
        
        if self.history_file and self.history_file.exists():
            self.history = self._load_history_from_file(self.history_file)
        else:
            self.history = self._load_session_history()

    def _setup_api_key(self):
        api_key = self.default_connection.get("api_key")
        
        if api_key:
            os.environ["OPENAI_API_KEY"] = api_key
        elif not os.getenv("OPENAI_API_KEY"):
            raise ValueError(
                "No API key found! Either:\n"
                f"  1. Add 'api_key' to {self.config_path / 'aish.json'}\n"
                "  2. Set OPENAI_API_KEY environment variable\n"
                f"  3. Create {self.config_path / '.env'} with OPENAI_API_KEY"
            )

    def _load_instruction(self, filename: str) -> str:
        return (self.config_path / "instructions" / filename).read_text()

    def _get_config_path(self) -> Path:
        # Development: Check for local config directory
        local_config = Path.cwd() / "config"
        if local_config.is_dir() and (local_config / "aish.json").exists():
            return local_config
        
        # Production: XDG-compliant user config directory
        user_config = Path(user_config_dir("aish"))
        user_config.mkdir(parents=True, exist_ok=True)
        return user_config
       
    def _load_config(self) -> dict:
        with open(self.config_path / "aish.json", "r", encoding="utf-8") as config_file:
            return json.load(config_file)


    def _get_agent_prompt(self) -> str:
        prompt_path = self.config_path / "agents" / f"{self.agent_name}.md"
        begin = "\n\n===========  BEGIN AGENT PROMPT ===========\n\n"
        end = "\n\n===========  END AGENT PROMPT ===========\n\n"
        
        with open(prompt_path, "r", encoding="utf-8") as file:
            post = frontmatter.load(file)
            post.content = begin + post.content + end
            return post.content, post.metadata

    def _run_context_module(self, module: str) -> str:
        module_path = self.config_path / "context_modules" / module

        result = subprocess.run([module_path], capture_output=True, text=True, check=True)
        return result.stdout


    def _build_context(self) -> str:
        context = "\n\n===========  BEGIN CONTEXT MODULES ===========\n\n"

        for module in self.agent_metadata["context_modules"].split(","):
            module = module.strip()
            context += f"\n\n--------- Context Module: {module} ---------\n"
            context += self._run_context_module(module)
            context += "\n\n-------------------------------------------\n"

        context += "\n\n===========  END CONTEXT MODULES ===========\n\n"

        return context

    def _build_request_body(self, history: list = []):
        system_messages = [
            SystemMessage(content=self._load_instruction("identity_primer.md")),
            SystemMessage(content=self._load_instruction("behavior_instructions.md")),
            SystemMessage(content=self._load_instruction("examples.md")),
            SystemMessage(content=self.agent_prompt),
            SystemMessage(content=self._build_context()),
        ]

        divider = [SystemMessage(content="\n\n=========== Conversation ===========\n\n")]

        return system_messages + divider + history

    def _request(self, history: list):
        conversation = self._build_request_body(history)
        response_stream = self.llm.stream(conversation)
        
        return response_stream

    def _execute_commands(self, response: str):
        commands = re.findall(r"<execute>(.*?)</execute>", response)

        if not commands:
            return None

        for command in commands:
            prompt = f"[bold on red] EXECUTE [/][bold on green] {command} [/]"
            user_consent = Prompt.ask(prompt, choices=["y", "n"], default="y")
            if user_consent == "n":
                self.history.append(HumanMessage(content=f"User did not consent to execute command: {command}"))
                continue
            if user_consent == "y":
                self.history.append(HumanMessage(content=f"User consented to execute command: {command}"))
                result = subprocess.run(
                    command,
                    shell=True,
                    capture_output=True,
                    text=True,
                    encoding='utf-8',
                    errors='replace'
                )
                output = result.stdout.strip()
                if output:
                    self.history.append(SystemMessage(content=f"<output>{output}</output>"))


    def _print_and_stream_response(self, response_stream):
        full_response = ''
        
        with Live('', refresh_per_second=15, console=self.console) as live:
            for chunk in response_stream:
                full_response += chunk.content
                
                display_text = full_response
                display_text = re.sub(r"<execute>(.*?)</execute>", "", display_text, flags=re.DOTALL)
                display_text = re.sub(r"<think>(.*?)</think>", "", display_text, flags=re.DOTALL)
                display_text = re.sub(r"<done>", "", display_text)
                display_text = re.sub(r"<end>", "", display_text)
                
                live.update(display_text.strip())
        
        return full_response

    def _get_session_id(self) -> str:
        return str(os.getppid())

    def _get_session_file(self) -> Path:
        runtime_dir = os.getenv("XDG_RUNTIME_DIR")
        if runtime_dir and Path(runtime_dir).exists():
            return Path(runtime_dir) / f"aish-{self.session_id}.json"
        return Path("/tmp") / f"aish-{self.session_id}.json"

    def _load_session_history(self) -> list:
        if self.session_file.exists():
            try:
                with open(self.session_file) as f:
                    return messages_from_dict(json.load(f))
            except (json.JSONDecodeError, FileNotFoundError):
                return []
        return []

    def _load_history_from_file(self, filepath: Path) -> list:
        try:
            with open(filepath) as f:
                return messages_from_dict(json.load(f))
        except (json.JSONDecodeError, FileNotFoundError) as e:
            self.console.print(f"[yellow]Warning: Could not load history from {filepath}: {e}[/yellow]")
            return []

    def _save_session_history(self):
        serialized = messages_to_dict(self.history)
        
        try:
            with open(self.session_file, "w") as f:
                json.dump(serialized, f)
        except Exception as e:
            self.console.print(f"[yellow]Warning: Could not save session history to tmpfs: {e}[/yellow]")
        
        if self.history_file:
            try:
                self.history_file.parent.mkdir(parents=True, exist_ok=True)
                with open(self.history_file, "w") as f:
                    json.dump(serialized, f, indent=2)
            except Exception as e:
                self.console.print(f"[yellow]Warning: Could not save history to {self.history_file}: {e}[/yellow]")

    def save_history(self, filepath: str):
        filepath = Path(filepath).expanduser()
        filepath.parent.mkdir(parents=True, exist_ok=True)
        with open(filepath, "w") as f:
            json.dump(messages_to_dict(self.history), f, indent=2)
        self.console.print(f"[green]History saved to {filepath}[/green]")

    def load_history(self, filepath: str) -> list:
        filepath = Path(filepath).expanduser()
        with open(filepath) as f:
            history = messages_from_dict(json.load(f))
        self.console.print(f"[green]History loaded from {filepath}[/green]")
        return history

    def process_user_message(self, message: str):
        self.history.append(HumanMessage(content=message))

        while True:
            with self.console.status('Thinking'):
                response_stream = self._request(self.history)
            
            try:
                full_response = self._print_and_stream_response(response_stream)
                
                self.history.append(AIMessage(content=full_response))
                self._save_session_history()
                
                self._execute_commands(full_response)
            except Exception as e:
                self.history.append(SystemMessage(content=f"System Error: {e}"))
                self._save_session_history()
                continue
        
            if "<done>" in full_response:
                return