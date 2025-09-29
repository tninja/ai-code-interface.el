;;; ai-code-ai.el --- AI-specific helper utilities for AI Code Interface -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Helper functions that support AI workflows such as launching the MCP Inspector.

;;; Code:

(require 'compile)

(require 'ai-code-input)

(declare-function ai-code-read-string "ai-code-input")

;;;###autoload
(defun ai-code-debug-mcp ()
  "Debug MCP by choosing to run mcp or inspector.
If current buffer is a python file, ask user to choose either 'Run mcp' or 'Run inspector',
and call either ai-code-mcp-run or ai-code-mcp-inspector-run accordingly."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if (and current-file (string= (file-name-extension current-file) "py"))
        (let ((choice (completing-read "Choose MCP action: " 
                                       '("Run mcp" "Run inspector") 
                                       nil t)))
          (cond
           ((string= choice "Run mcp")
            (ai-code-mcp-run))
           ((string= choice "Run inspector")
            (ai-code-mcp-inspector-run))))
      (message "Current buffer is not a python file"))))

;;;###autoload
(defun ai-code-mcp-run ()
  "Run python mcp with uv command.
Run command: uv --directory <project-root-base-dir> run <relative-path-to-current-buffer file>.
Execute in compilation buffer named *ai-code-mcp-run:<full-path-of-python-file>*.
If current buffer is not a python file, message user and quit."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if (and current-file (string= (file-name-extension current-file) "py"))
        (let* ((project-root-base-dir (ai-code-mcp-inspector--find-project-root current-file))
               (relative-path (when project-root-base-dir
                                (file-relative-name current-file project-root-base-dir))))
          (if project-root-base-dir
              (let* ((default-command (format "uv --directory %s run %s"
                                              (shell-quote-argument project-root-base-dir)
                                              (shell-quote-argument relative-path)))
                     (command (ai-code-read-string "MCP run command: " default-command))
                     (buffer-name (format "*ai-code-mcp-run:%s*" current-file))
                     (default-directory project-root-base-dir))
                (when (and command (not (string= command "")))
                  (compilation-start
                   command
                   nil
                   (lambda (_mode) buffer-name))))
            (message "Could not find project root with pyproject.toml")))
      (message "Current buffer is not a python file"))))

;;;###autoload
(defun ai-code-mcp-inspector-run ()
  "Run MCP inspector for the current context.
For Python buffers, locate the project root via pyproject.toml and run the
inspector against the active file. For Dired buffers, prompt for an inspector
command, prefix the required ports, and execute it inside the listed
directory."
  (interactive)
  (let ((context (ai-code-mcp-inspector--build-context)))
    (when context
      (ai-code-mcp-inspector--start context))))

(defun ai-code-mcp-inspector--build-context ()
  "Gather execution context for `mcp-inspector-run'.
Returns a plist with metadata required to launch the inspector, or nil if the
current buffer is unsupported or user input is missing."
  (let* ((current-file (buffer-file-name))
         (is-dired (derived-mode-p 'dired-mode))
         base-dir relative-path display-entries)
    (cond
     (is-dired
      (setq base-dir (file-name-as-directory (expand-file-name default-directory)))
      (setq display-entries (list (cons "Working directory" base-dir))))
     ((and current-file (string= (file-name-extension current-file) "py"))
      (setq base-dir (ai-code-mcp-inspector--find-project-root current-file))
      (if base-dir
          (progn
            (setq base-dir (file-name-as-directory base-dir))
            (setq relative-path (file-relative-name current-file base-dir))
            (setq display-entries (list (cons "Server file" relative-path))))
        (message "Could not find project root with pyproject.toml")
        (setq base-dir nil)))
     (t
      (message "Current buffer must be a Python file or Dired buffer")
      (setq base-dir nil)))
    (when base-dir
      (let* ((base-dir-name (file-name-nondirectory (directory-file-name base-dir)))
             (hash-offset (mod (sxhash base-dir-name) 101))
             (server-port (+ 9001 hash-offset))
             (client-port (+ 8081 hash-offset))
             (buffer-name (format "*%s:client=%d:server=%s*" base-dir-name client-port server-port))
             (command (ai-code-mcp-inspector--build-command is-dired base-dir base-dir-name
                                                            client-port server-port relative-path)))
        (when command
          (list :base-dir base-dir
                :base-dir-name base-dir-name
                :buffer-name buffer-name
                :client-port client-port
                :server-port server-port
                :command command
                :display-entries (cons (cons "Base directory" base-dir)
                                       display-entries)))))))

(defun ai-code-mcp-inspector--build-command (is-dired base-dir base-dir-name client-port server-port relative-path)
  "Construct the inspector command string for the current context.
IS-DIRED selects interactive input, BASE-DIR and BASE-DIR-NAME describe the project,
CLIENT-PORT and SERVER-PORT configure networking, and RELATIVE-PATH targets a file."
  (if is-dired
      (let ((user-command (ai-code-read-string (format "Inspector command for %s: " base-dir-name))))
        (if (string-match-p "\\`[ \t\n\r]*\\'" user-command)
            (progn
              (message "Inspector command is required")
              nil)
          (format "CLIENT_PORT=%d SERVER_PORT=%d %s"
                  client-port server-port user-command)))
    (when relative-path
      (format
       "CLIENT_PORT=%d SERVER_PORT=%d npx @modelcontextprotocol/inspector -e VERIFY_SSL=true -e FASTMCP_LOG_LEVEL=INFO uv --directory %s run %s %s stdio"
       client-port server-port base-dir relative-path base-dir-name))))

(defun ai-code-mcp-inspector--start (context)
  "Launch the inspector using CONTEXT plist produced by `ai-code-mcp-inspector--build-context'."
  (let* ((base-dir (plist-get context :base-dir))
         (base-dir-name (plist-get context :base-dir-name))
         (buffer-name (plist-get context :buffer-name))
         (client-port (plist-get context :client-port))
         (server-port (plist-get context :server-port))
         (default-command (plist-get context :command))
         (command (ai-code-read-string "MCP inspector command: " default-command))
         (display-entries (plist-get context :display-entries))
         (compilation-buffer-name-function (lambda (_mode) buffer-name))
         (default-directory base-dir)
         (previous-setup compilation-process-setup-function)
         (compilation-process-setup-function
          (lambda ()
            (setq default-directory base-dir)
            (when previous-setup
              (funcall previous-setup))))
         (buffer (when (and command (not (string= command "")))
                   (compilation-start command nil))))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert (format "Running MCP Inspector for %s\n" base-dir-name))
          (dolist (entry display-entries)
            (insert (format "%s: %s\n" (car entry) (cdr entry))))
          (insert (format "Server port: %d\n" server-port))
          (insert (format "Client port: %d\n" client-port))
          (insert (format "Command: %s\n\n" command))
          (insert "Starting inspector...\n\n")))
      (display-buffer buffer))
    (when (and command (not (string= command "")))
      (message "MCP Inspector started, output in %s" buffer-name))))

(defun ai-code-mcp-inspector--find-project-root (file-path)
  "Find project root by looking for pyproject.toml in parent directories starting from FILE-PATH."
  (let ((dir (file-name-directory file-path)))
    (while (and dir
                (not (string= dir "/"))
                (not (file-exists-p (expand-file-name "pyproject.toml" dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (if (and dir (file-exists-p (expand-file-name "pyproject.toml" dir)))
        dir
      nil)))

(provide 'ai-code-ai)

;;; ai-code-ai.el ends here
