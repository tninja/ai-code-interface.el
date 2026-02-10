# GitHub Copilot Instructions for ai-code-interface.el

## Project Overview

This is **ai-code-interface.el**, an Emacs package that provides a uniform interface for AI-assisted software development across different AI backends (Claude Code, Gemini CLI, OpenAI Codex, GitHub Copilot CLI).

## Language and Technology

- **Language**: Emacs Lisp
- **Package System**: Emacs package with `use-package` support
- **Version Control**: Git with Magit integration
- **Testing Framework**: ERT (Emacs Lisp Regression Testing)

## Code Style and Conventions

### File Naming
- Main module: `ai-code.el`
- Sub-modules: `ai-code-*.el` (e.g., `ai-code-backends.el`, `ai-code-agile.el`)
- Tests: `test_*.el` (e.g., `test_ai-code-change.el`)

### Emacs Lisp Conventions
- Use `lexical-binding: t` in all files
- Follow standard Emacs Lisp naming conventions:
  - Public functions: `ai-code-function-name`
  - Private functions: `ai-code--function-name` (double dash prefix)
  - Custom variables: `ai-code-variable-name`
- Include proper header comments with:
  - Author
  - Version (when applicable)
  - Package-Requires
  - SPDX-License-Identifier: Apache-2.0
  - Commentary section
- Use `declare-function` for forward declarations
- Prefer `defcustom` for user-configurable variables
- Use `;;;###autoload` cookies for interactive commands

### Code Organization
- Group related functionality in separate modules
- Use `require` statements at the beginning of files
- Maintain clear separation between backend implementations and interface code

## Testing

### Running Tests
- Tests use ERT (Emacs Lisp Regression Testing)
- Test files are named with `test_` prefix
- Run tests with: `emacs -batch -l ert -l <test-file>.el -f ert-run-tests-batch-and-exit`

### Test Conventions
- Use `ert-deftest` for defining tests
- Test names should be descriptive: `test-module--function-name-scenario`
- Use `with-temp-buffer` for testing buffer-related functionality
- Mock external dependencies with `cl-letf` when needed

## Dependencies

### Required
- Emacs 26.1 or later
- `org` (Org-mode support)
- `magit` (Git integration)
- `transient` (Menu system)
- One of the backend packages (e.g., `claude-code.el`)

### Optional
- `helm` (Enhanced auto-completion)
- `yasnippet` (Snippet support)
- `gptel` (AI-generated headlines)
- `flycheck` (Error fixing features)
- `projectile` (Project management)
- `helm-gtags` (Tag navigation)

## Backend Architecture

The package uses a pluggable backend system:
- Backends are registered in `ai-code-backends.el`
- Each backend provides functions for: start, resume, switch-to-buffer, send-command
- Backend selection is dynamic via `ai-code-set-backend`
- Supported backends: Claude Code, Claude Code IDE, Gemini CLI, Codex CLI, GitHub Copilot CLI

## Key Features to Preserve

1. **Interactive AI Chat**: Dedicated Emacs buffer for AI sessions
2. **Context-Aware Prompting**: Automatic inclusion of file paths, regions, and function context
3. **Code Manipulation**: Change code, implement TODOs, explain code, fix errors
4. **Agile Development**: TDD cycle support, refactoring assistance
5. **Git Integration**: Diff review, git blame/log analysis, Magit transient menus
6. **Prompt Management**: `.ai.code.prompt.org` file for reusable prompts
7. **Transient Menu**: Main menu via `C-c a`

## Common Tasks

### Adding a New Backend
- Reference PR #2 for GitHub Copilot CLI integration as an example
- Add backend definition to `ai-code-backends.el`
- Create backend-specific file if needed (e.g., `ai-code-github-copilot-cli.el`)
- Update README.org with new backend information

### Adding New Features
- Place in appropriate module (`ai-code-change.el`, `ai-code-agile.el`, etc.)
- Add interactive commands with `;;;###autoload`
- Update transient menu in `ai-code.el` if needed
- Add tests to corresponding test file

### Fixing Bugs
- Write a failing test first (TDD approach)
- Fix the issue with minimal changes
- Ensure existing tests still pass
- Update documentation if behavior changes

## Files to Avoid Modifying

- `.gitignore`: Contains project-specific ignore patterns
- `LICENSE`: Apache-2.0 license file
- `HISTORY.org`: Historical changelog
- Generated files: `GTAGS`, `GRTAGS`, `GPATH`, `.projectile`

## Build and Verification

### Byte Compilation
```bash
emacs -batch -f batch-byte-compile *.el
```

### Running All Tests
```bash
emacs -batch -l ert -l test_ai-code-change.el -l test_ai-code-prompt-mode.el -f ert-run-tests-batch-and-exit
```

## Documentation

- Main documentation: `README.org` (Org-mode format)
- Update README.org when adding features, backends, or changing workflows
- Include code examples in Emacs Lisp format
- Use Org-mode formatting for links, code blocks, and sections

## Special Considerations

- This package integrates with multiple external AI CLI tools
- Changes should maintain compatibility across different backends
- The user interface should remain consistent regardless of backend
- Transient menus are central to the UX - preserve the menu structure
- The package emphasizes agile development workflows - maintain TDD and refactoring features
