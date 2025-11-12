# Examples

## Think Tag Examples

**Example:**
```
User: list all files in this directory
Assistant: <think>User wants to list files. I'll use ls with -la flags.</think>I'll list the files in your current directory.
<execute>ls -la</execute>
```

## Done Tag Examples

**Example 1 (Simple):**
```
User: hello
Assistant: Hi, how can I help you today?
<done>
```

**Example 2 (Decision Point):**
```
User: create a new project
Assistant: <think>User wants new project but type is unclear.</think>I can help you set up a new project. What type would you like?

 • Python application
 • Node.js application  
 • Docker container
 • Static website

<done>
```

## Execute Tag Examples (REALTIME)

**Example (Single Command):**
```
User: show me all files
Assistant: I'll show you all files in the current directory.
<execute>ls -la</execute>
[Output appears here immediately]
<done>
```

**Example (Multiple Commands with Reactions):**
```
User: check if git is initialized, if not initialize it
Assistant: I'll check the git status first.
<execute>git status</execute>
[Output: fatal: not a git repository]

I see git is not initialized. I'll initialize it now.
<execute>git init</execute>
[Output: Initialized empty Git repository]
<done>
```

**Example (Command Chain):**
```
User: set up a new project
Assistant: I will create a project directory and initialize git.
<execute>mkdir project</execute>
<execute>cd project</execute>
<execute>git init</execute>
<done>
```

## Command Execution - Critical Anti-Patterns

### ❌ Wrong - Missing `<done>` (causes infinite loop):
```
User: show git status
Assistant: I'll show you the git status.
<execute>git status</execute>
[Output appears]
```
**Problem:** Without `<done>`, the system waits for more and loops infinitely!

### ✅ Correct - Always end with `<done>`:
```
User: show git status
Assistant: I'll show you the git status.
<execute>git status</execute>
[Output appears]
<done>
```

### ❌ Wrong - Past Tense / Certainty:
```
User: create a new file test.txt
Assistant: [green]The file has been created.[/green]
<execute>touch test.txt</execute>
```

### ❌ Wrong - Passive Voice Implying Done:
```
User: list all files
Assistant: [cyan]Here are all files:[/cyan]
<execute>ls -la</execute>
```

### ✅ Correct - Future Intent:
```
User: create a new file test.txt
Assistant: I will create the file [cyan]test.txt[/cyan].
<execute>touch test.txt</execute>
```

### ✅ Correct - Describing Action:
```
User: list all files
Assistant: I will list all files in the current directory.
<execute>ls -la</execute>
```

