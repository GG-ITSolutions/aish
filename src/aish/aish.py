import re
import os
import json
import subprocess
from pathlib import Path
import frontmatter
from langchain_core.messages import HumanMessage, SystemMessage, AIMessage
from dotenv import load_dotenv
from rich.console import Console
from rich.prompt import Prompt
from platformdirs import user_config_dir

from .utils import script_capture
from .providers import PROVIDERS

load_dotenv()

class AIsh:
    def __init__(self, model_name: str = None, provider_name: str = None, agent_name: str = None):
        self.console = Console(force_terminal=True)
        self.history = []

        self.config_path = self._get_config_path()
        self.config = self._load_config()

        self.system_overview = self._get_system_overview()

        self.agent_name = agent_name or os.getenv("AISH_AGENT") or self.config["default_agent"]
        self.agent_prompt, self.agent_metadata = self._get_agent_prompt()

        self.default_connection = self.config["connections"][self.agent_metadata.get("connection")] or next(iter(self.config["connections"].values()))
        self.model_name = model_name or self.agent_metadata.get("model") or os.getenv("AISH_MODEL") or self.default_connection['model']
        self.provider_name = provider_name or self.agent_metadata.get("provider") or os.getenv("AISH_PROVIDER") or self.default_connection['provider']

        self.llm = PROVIDERS[self.provider_name](model=self.model_name)

    def _get_system_overview(self) -> str:
        with open(self.config_path / "system_overview.md", "r", encoding="utf-8") as file:
            return file.read()

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
            SystemMessage(content=self._build_context()),
            SystemMessage(content=self.agent_prompt),
            SystemMessage(content=self.system_overview),
        ]

        divider = [SystemMessage(content="\n\n=========== Here starts the conversation ===========\n\n")]

        return system_messages + divider + history

    def _request(self, history: list):
        conversation = self._build_request_body(history)
        response = self.llm.invoke(conversation)

        return response.content

    def _execute_commands(self, response: str):
        commands = re.findall(r"<execute>(.*?)</execute>", response)

        if not commands:
            return None

        for command in commands:
            prompt = f"[bold on red] EXECUTE [/][bold on green] {command} [/]"
            user_consent = Prompt.ask(prompt, choices=["y", "n"], default="y")
            if user_consent == "n":
                self.history.append(HumanMessage(content=f"User did not consent to execute command: {command}   "))
                continue
            if user_consent == "y":
                self.history.append(HumanMessage(content=f"User consented to execute command: {command}"))
                with script_capture(command) as output:
                    output = output.strip()
                    if output:
                        self.history.append(SystemMessage(content=f"<output>{output}</output>"));


    def _print_response(self, response: str):
        response = re.sub(r"<execute>(.*?)</execute>", "", response, flags=re.DOTALL)
        response = re.sub(r"<think>(.*?)</think>", "", response, flags=re.DOTALL)
        response = re.sub(r"<done>", "", response)
        response = re.sub(r"<end>", "", response)
        self.console.print(response.strip())

    def process_user_message(self, message: str):
        self.history.append(HumanMessage(content=message))

        while True:
            with self.console.status('Thinking'):
                response = self._request(self.history)
                self.history.append(AIMessage(content=response))

            try:
                self._print_response(response)
                self._execute_commands(response)
            except Exception as e:
                self.history.append(SystemMessage(content=f"System Error: {e}"))
                continue
        
            if "<done>" in response:
                return