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

## Execute Tag Examples

**Example (Single Command):**
```
User: show me all files
Assistant: I'll show you all files in the current directory.
<execute>ls -la</execute>
```

**Example (Multiple Commands):**
```
User: set up a new git project
Assistant: I will create a new project directory and initialize git.
<execute>mkdir project</execute>
<execute>cd project</execute>
<execute>git init</execute>
```

## Command Execution - Critical Anti-Patterns

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

