from abc import ABC, abstractmethod
from typing import Optional
import subprocess
from langchain_core.messages import HumanMessage, SystemMessage
from rich.prompt import Prompt
from rich.markup import escape

from .stream_processor import StreamContext


class ToolExecutedException(Exception):
    pass


class TagHandler(ABC):
    def __init__(self, tag_name: str):
        self.tag_name = tag_name
    
    @abstractmethod
    def handle(self, content: str, context: StreamContext) -> Optional[str]:
        pass
    
    def should_display_content(self) -> bool:
        return False


class ExecuteTagHandler(TagHandler):
    def __init__(self):
        super().__init__('execute')
    
    def handle(self, content: str, context: StreamContext) -> Optional[str]:
        command = content.strip()
        
        context.pause_display()
        prompt = f"[bold on red] EXECUTE [/][bold on green] {command} [/]"
        user_consent = Prompt.ask(prompt, choices=["y", "n"], default="y", console=context.console)
        
        if user_consent == "n":
            context.add_to_history(HumanMessage(content=f"User did not consent to execute command: {command}"))
            raise ToolExecutedException()
        
        context.add_to_history(HumanMessage(content=f"User consented to execute command: {command}"))
        
        result = subprocess.run(
            command,
            shell=True,
            capture_output=True,
            text=True,
            encoding='utf-8',
            errors='replace'
        )
        
        stdout = result.stdout.strip()
        stderr = result.stderr.strip()
        
        if stdout:
            context.add_to_history(SystemMessage(content=f"<output>{stdout}</output>"))
            escaped_output = escape(stdout)
            context.console.print(escaped_output)
        
        if stderr:
            context.add_to_history(SystemMessage(content=f"<error>{stderr}</error>"))
            escaped_error = escape(stderr)
            context.console.print(f"[red]{escaped_error}[/red]")
        
        raise ToolExecutedException()


class ThinkTagHandler(TagHandler):
    def __init__(self):
        super().__init__('think')
    
    def handle(self, content: str, context: StreamContext) -> Optional[str]:
        return None
    
    def should_display_content(self) -> bool:
        return False


class DoneTagHandler(TagHandler):
    def __init__(self):
        super().__init__('done')
    
    def handle(self, content: str, context: StreamContext) -> Optional[str]:
        return None


class EndTagHandler(TagHandler):
    def __init__(self):
        super().__init__('end')
    
    def handle(self, content: str, context: StreamContext) -> Optional[str]:
        return None
