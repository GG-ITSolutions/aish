import os
import socket
import time
from .utils import script_capture
from xonsh.main import setup
from prompt_toolkit import PromptSession
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.formatted_text import FormattedText
from langchain_core.messages import SystemMessage, HumanMessage

class CLI:
    def __init__(self, aish):
        self.aish = aish
        self.mode = "terminal"
        self.session = PromptSession()
        self.bindings = self._setup_bindings()

        setup(shell_type="prompt_toolkit")
        
    def _get_prompt_text(self):
        blocks = [('ansired', time.strftime("%H:%M:%S") + ' ')]
        blocks += [('ansiblue', os.getenv("USER", "user"))]
        blocks += [('ansipurple', '@')]
        
        if self.mode == "terminal":
            blocks += [('ansiyellow', socket.gethostname())]
            blocks += [('ansiwhite', ' ' + os.getcwd())]
            blocks += [('ansipurple', ' # ')]
        else:
            blocks += [('ansiyellow', self.aish.agent_name)]
            blocks += [('ansiwhite', ' ' + self.aish.model_name)]
            blocks += [('ansipurple', ' ? ')]
            

        return FormattedText(blocks)

    def _setup_bindings(self):
        self.bindings = KeyBindings()

        @self.bindings.add('c-space')
        def _(event):
            self._switch_mode()
            event.app.invalidate()

        return self.bindings
    
    def _switch_mode(self):
        self.mode = "terminal" if self.mode == "prompt" else "prompt"
        if self.aish.history and self.aish.history[-1].content.startswith("Mode switched to "):
            self.aish.history.pop()
        self.aish.history.append(SystemMessage(content=f"Mode switched to {self.mode}"))
    
    def _run_command(self, command):
        self.aish.history.append(HumanMessage(content=f"<execute>{command}</execute>"))
        try:
            with script_capture(command) as captured_output:
                if captured_output.strip():
                    self.aish.history.append(SystemMessage(content=f"<output>{captured_output.strip()}</output>"))
        except Exception as e:
            error_msg = str(e).split('\n')[0]
            print(f"Error: {error_msg}")
            self.aish.history.append(SystemMessage(content=f"Error executing command: {error_msg}"))
    
    
    def _handle_input(self, user_input):
        if self.mode == "prompt":
            self.aish.process_user_message(user_input)
        elif self.mode == "terminal":
            self._run_command(user_input)
    
    def run(self, message=None):
        self.aish.console.print("-> Welcome to AIsh. Use Ctrl+Space to switch between modes.")
        
        if message:
            self._handle_input(message)
            return
        
        self._run_interactive_session()
    
    def _run_interactive_session(self):
        while True:
            try:
                user_input = self.session.prompt(self._get_prompt_text, key_bindings=self.bindings)
                
                if user_input.lower() in ["exit", "quit"]:
                    break
                    
                self._handle_input(user_input)
                
            except KeyboardInterrupt:
                continue
            except EOFError:
                break