import os
from langchain_openai import ChatOpenAI
from typing import Optional
from pydantic import SecretStr, Field

class ChatOpenRouter(ChatOpenAI):
    def __init__(self, **kwargs):
        base_url = kwargs.pop('base_url', "https://openrouter.ai/api/v1")
        
        super().__init__(
            base_url=base_url,
            default_headers={
                'X-Title': 'aish',
                'HTTP-Referer': 'https://github.com/GG-ITSolutions/aish'
            },
            **kwargs
        )

PROVIDERS = {
    "openrouter": ChatOpenRouter
}