# CODEBUDDY.md

This file provides guidance to CodeBuddy Code when working with code in this repository.

## Common Development Commands

### Byte Compilation
```bash
emacs -batch -f batch-byte-compile *.el
```

### Running Tests
```bash
# Run a specific test file
emacs -batch -l ert -l test_ai-code-change.el -f ert-run-tests-batch-and-exit

# Run all tests
emacs -batch -l ert -l test_ai-code-change.el -l test_ai-code-prompt-mode.el -l test_ai-code-input.el -l test_ai-code-git.el -l test_ai-code-notifications.el -l test_ai-code-kiro-cli.el -f ert-run-tests-batch-and-exit
```

### CI Testing
The project uses melpazoid for CI checks. The workflow is defined in `.github/workflows/melpazoid.yml`.

## High-Level Architecture

### Core Design Philosophy
This is a unified interface package for AI-assisted software development that abstracts over multiple AI coding CLI backends (Claude Code, Gemini CLI, OpenAI Codex, GitHub Copilot CLI, Opencode, Grok CLI, Cursor CLI, CodeBuddy Code CLI, Kiro CLI). The package provides a consistent user experience across different AI tools while maintaining context-aware code actions and agile development workflows.

### Backend System Architecture

The backend system is pluggable and defined in `ai-code-backends.el`. Each backend is registered as a property list in `ai-code-backends`:

```elisp
(ai-code-backends
  '((claude-code
     :label "Claude Code"
     :require ai-code-claude-code
     :start   ai-code-claude-code
     :switch  ai-code-claude-code-switch-to-buffer
     :send    ai-code-claude-code-send-command
     :resume  ai-code-claude-code-resume
     :config  "~/.claude.json"
     :upgrade "npm install -g @anthropic-ai/claude-code@latest"
     :cli     "claude")
    ;; ... other backends
    ))
```

Backend switching is handled via `ai-code-set-backend`, which:
1. Loads the backend's requirement (e.g., `ai-code-claude-code`)
2. Sets up function aliases for common operations
3. Updates the UI to reflect the current backend

Backend implementations fall into two categories:
1. **Native backends** (in this repo): Implemented in files like `ai-code-codex-cli.el`, `ai-code-github-copilot-cli.el`, `ai-code-gemini-cli.el`, `ai-code-codebuddy-cli.el`, etc.
2. **External backends**: Packages like `claude-code-ide.el` and `claude-code.el` that provide backend functions

### Terminal Infrastructure

`ai-code-backends-infra.el` provides a unified abstraction over terminal emulators:
- **Default**: `vterm` (libvterm-based)
- **Alternative**: `eat` (Embraced as Terminal)
- Configuration via `ai-code-backends-infra-terminal-backend`

Key infrastructure features:
- Session management (multiple concurrent AI sessions)
- Window management (side windows, focus control)
- Performance optimizations (anti-flicker, reflow glitch prevention)
- Input handling (@-completion, clipboard integration)

### Module Organization

The codebase is organized into focused modules:

- **`ai-code.el`**: Main entry point, defines the transient menu (`C-c a`) and top-level commands
- **`ai-code-backends.el`**: Backend registration and selection system
- **`ai-code-backends-infra.el`**: Terminal abstraction (vterm/eat) and session management
- **`ai-code-change.el`**: Code manipulation operations (change code, implement TODOs, explain code)
- **`ai-code-discussion.el`**: Ask questions, explain code functionality
- **`ai-code-agile.el`**: Refactoring techniques catalog and TDD cycle workflow
- **`ai-code-git.el`**: Git integration (diff review, recent files, branch operations)
- **`ai-code-file.el`**: File operations (copy paths, recent files, sed-based prompt application)
- **`ai-code-prompt-mode.el`**: Prompt file management (`.ai.code.prompt.org`), @-completion, yasnippet integration
- **`ai-code-input.el`**: User input handling, context gathering, completion utilities
- **`ai-code-notifications.el`**: Desktop notifications for AI session completion
- **Backend implementations**: `ai-code-codex-cli.el`, `ai-code-github-copilot-cli.el`, `ai-code-gemini-cli.el`, `ai-code-codebuddy-cli.el`, `ai-code-opencode.el`, `ai-code-grok-cli.el`, `ai-code-cursor-cli.el`, `ai-code-kiro-cli.el`, `ai-code-claude-code.el`

### Transient Menu System

The main entry point is `ai-code-menu` (bound to `C-c a`), which uses the `transient` package to create an interactive menu with four sections:

1. **AI CLI Session**: Start, resume, switch, select backend, upgrade, open config, apply prompt on file
2. **AI Code Actions**: Code change, implement TODO, ask question, explain, send command, add context, create task file
3. **AI Agile Development**: Refactor, TDD cycle, pull/review diff, run file, build project, open prompt file, insert function
4. **Other Tools**: Init project, debug exception, fix Flycheck errors, copy file name, toggle dedicated, open recent file, debug MCP, take notes, toggle notifications

All context-aware actions automatically include:
- Current file path
- Visible buffers (context files)
- Function or region at point
- Stored repo context (`ai-code--repo-context-info`)
- Optional clipboard context (with `C-u` prefix)

### Context Engineering System

Context engineering is a core concept - the package provides tools to automatically assemble precise context blocks:

1. **Automatic file context**: `ai-code--get-context-files-string` includes visible buffers
2. **Function/region scoping**: Most actions capture `which-function` result or active region
3. **Manual context curation**: `ai-code-context-action` (C-c a @) stores file paths, function anchors, or region ranges in `ai-code--repo-context-info`
4. **@-completion**: Type `@` to open file completion list, inserts relative paths with `@` prefix
5. **Prompt suffix**: `ai-code-prompt-suffix` appends persistent constraints to all prompts

### Refactoring Techniques Catalog

`ai-code-agile.el` contains a comprehensive catalog of refactoring techniques following Martin Fowler's book. Each technique has:
- `:name` - Display name
- `:scopes` - Where it applies (region/global)
- `:description` - The prompt template sent to AI
- `:parameters` (optional) - Interactive parameters with placeholders, prompts, and default value functions

### TDD Workflow

The Test Driven Development cycle (`ai-code-tdd-cycle`) integrates with test frameworks:
- Python: `python-pytest`
- JavaScript/TypeScript: `jest`

The workflow guides through writing tests, running them, implementing code, and refactoring.

## Code Style and Conventions

### File Structure
- All files use `lexical-binding: t`
- Header includes Author, Version (for main file), Package-Requires, SPDX-License-Identifier: Apache-2.0
- Commentary section describes the module's purpose
- Code section follows

### Naming Conventions
- Public functions: `ai-code-function-name`
- Private functions: `ai-code--function-name` (double dash)
- Custom variables: `ai-code-variable-name`
- Backends: Use kebab-case symbols like `claude-code`, `github-copilot-cli`

### Function Definitions
- Use `defcustom` for user-configurable variables with `:group 'ai-code`
- Use `;;;###autoload` cookies for interactive commands that should be available when the package is loaded
- Use `declare-function` for forward declarations of functions that will be defined after loading (e.g., backend functions)
- Use `cl-labels` for local helper functions
- Prefix arguments (`C-u`) are used to modify behavior (e.g., append clipboard context, force full paths)

### Backend Integration Pattern
When adding a new backend:
1. Create `ai-code-<backend-name>.el` following the pattern of existing backends
2. Implement required functions: `ai-code-<backend>-start`, `ai-code-<backend>-switch-to-buffer`, `ai-code-<backend>-send-command`, `ai-code-<backend>-resume` (optional)
3. Add backend definition to `ai-code-backends` in `ai-code-backends.el`
4. Update README.org with backend setup instructions

### Testing Conventions
- Test files: `test_ai-code-<module>.el`
- Use `ert-deftest` for test definitions
- Test names: `test-module--function-name-scenario`
- Use `with-temp-buffer` for buffer-related tests
- Mock functions with `cl-letf` when needed
- Mock `which-function` for function context testing

## File Organization Context

- **`.ai.code.files/`**: Created automatically in project root, contains `.ai.code.prompt.org` for prompt history
- **`snippets/ai-code-prompt-mode/`**: Yasnippet templates for common prompts
- **`examples/`**: Example projects demonstrating usage (Battleship, Connect4)
- **`test/`**: ERT test files

## Integration Points

### Magit Integration
- `ai-code-magit-setup-transients` adds AI commands to Magit popups
- Functions use `magit-toplevel` to find git root
- Diff review integrates with Magit's diff generation

### External Package Integrations
- `gptel`: Optional, used for AI-generated headlines in prompt file
- `helm`: Optional, for enhanced completion
- `yasnippet`: Optional, for prompt templates
- `flycheck`: Optional, for `ai-code-flycheck-fix-errors-in-scope`
- `projectile`: Optional, for project root detection in `ai-code-init-project`

### External AI CLI Requirements
Each backend requires the corresponding CLI to be installed and available on PATH:
- Claude Code: `claude`
- Gemini CLI: `gemini`
- OpenAI Codex: `codex`
- GitHub Copilot CLI: `copilot`
- Opencode: `opencode`
- Grok CLI: `grok`
- Cursor CLI: `cursor`
- Kiro CLI: `kiro`
- CodeBuddy Code CLI: `codebuddy`
