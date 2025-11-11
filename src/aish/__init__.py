import argparse
from .aish import AIsh

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("message", type=str, nargs="?", help="The message to send to the agent")
    parser.add_argument("-m", "--model", type=str, help="The model to use")
    parser.add_argument("-p", "--provider", type=str, help="The provider to use")
    parser.add_argument("-a", "--agent", type=str, help="The agent to use")
    parser.add_argument("--print-context", action="store_true", help="Print the context")
    parser.add_argument("--print-full-context", action="store_true", help="Print the full context")
    args = parser.parse_args()

    aish = AIsh(model_name=args.model, provider_name=args.provider, agent_name=args.agent)

    if args.print_full_context:
        print("".join(message.content for message in aish._build_request_body()))
        exit()
    elif args.print_context:
        print(aish._build_context())
        exit()

    if not args.message:
        parser.error("message is required")
    
    aish.process_user_message(args.message)
