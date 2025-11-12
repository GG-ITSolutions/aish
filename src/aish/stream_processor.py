from typing import Dict, Optional
from rich.console import Console
from rich.live import Live


class StreamContext:
    def __init__(self, console: Console, history: list, live: Live):
        self.console = console
        self.history = history
        self.live = live
        self.current_buffer = ''
    
    def add_to_history(self, message):
        self.history.append(message)
    
    def update_display(self, content: str):
        self.current_buffer = content
        self.live.update(content.strip())
    
    def pause_display(self):
        text_to_preserve = self.current_buffer.strip()
        self.current_buffer = ''
        self.live.update('')
        self.live.stop()
        if text_to_preserve:
            self.console.print(text_to_preserve)
    
    def resume_display(self):
        self.live.start()


class StreamProcessor:
    def __init__(self):
        self.handlers: Dict[str, 'TagHandler'] = {}
    
    def register_handler(self, handler: 'TagHandler'):
        self.handlers[handler.tag_name] = handler
    
    def process_stream(self, response_stream, context: StreamContext):
        full_response = ''
        tag_buffer = ''
        current_tag = None
        tag_content_buffer = ''
        
        STATE_NORMAL = 'normal'
        STATE_TAG_OPEN = 'tag_open'
        STATE_TAG_CONTENT = 'tag_content'
        STATE_TAG_CLOSE = 'tag_close'
        state = STATE_NORMAL
        
        for chunk in response_stream:
            content = chunk.content
            full_response += content
            
            for char in content:
                if state == STATE_NORMAL:
                    if char == '<':
                        state = STATE_TAG_OPEN
                        tag_buffer = ''
                    else:
                        context.current_buffer += char
                        
                elif state == STATE_TAG_OPEN:
                    if char == '>':
                        tag_name = tag_buffer
                        if tag_name in self.handlers:
                            current_tag = tag_name
                            tag_content_buffer = ''
                            state = STATE_TAG_CONTENT
                        else:
                            context.current_buffer += f'<{tag_buffer}>'
                            state = STATE_NORMAL
                        tag_buffer = ''
                    elif char.isalnum() or char in '_-':
                        tag_buffer += char
                    else:
                        context.current_buffer += '<' + tag_buffer + char
                        state = STATE_NORMAL
                        tag_buffer = ''
                        
                elif state == STATE_TAG_CONTENT:
                    if char == '<':
                        state = STATE_TAG_CLOSE
                        tag_buffer = ''
                    else:
                        tag_content_buffer += char
                        handler = self.handlers[current_tag]
                        if handler.should_display_content():
                            context.current_buffer += char
                            
                elif state == STATE_TAG_CLOSE:
                    tag_buffer += char
                    if tag_buffer == f'/{current_tag}>':
                        handler = self.handlers[current_tag]
                        output = handler.handle(tag_content_buffer, context)
                        if output:
                            context.current_buffer += output
                        
                        state = STATE_NORMAL
                        current_tag = None
                        tag_content_buffer = ''
                        tag_buffer = ''
                    elif not (tag_buffer.startswith('/') or char.isalnum() or char in '_->'):
                        tag_content_buffer += '<' + tag_buffer
                        if handler.should_display_content():
                            context.current_buffer += '<' + tag_buffer
                        state = STATE_TAG_CONTENT
                        tag_buffer = ''
                
                context.update_display(context.current_buffer)
        
        return full_response
