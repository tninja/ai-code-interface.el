;;; ai-code-ai.el --- AI-specific helper utilities for AI Code Interface -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Helper functions that support AI workflows such as launching the MCP Inspector.

;;; Code:

(require 'compile)
(require 'eshell)
(require 'json)

(require 'ai-code-input)

(declare-function ai-code-read-string "ai-code-input")
(declare-function eshell-send-input "esh-mode")
(defvar ai-code-selected-backend)

;;;###autoload
(defun ai-code-debug-mcp ()
  "Debug MCP by choosing to run mcp, inspector, or generate a config.
If current buffer is a python file, ask user to choose either
\\='Run mcp\\=', \\='Run inspector\\=', \\='Open inspector.sh\\=',
or \\='Generate mcp config\\=', and call the matching helper."
  (interactive)
  (let ((choice (completing-read "Choose MCP action: "
                                 '("Run mcp" "Run inspector" "Open inspector.sh" "Generate mcp config")
                                 nil t)))
    (cond
     ((string= choice "Run mcp")
      (ai-code-mcp-run))
     ((string= choice "Run inspector")
      (ai-code-mcp-inspector-run))
     ((string= choice "Open inspector.sh")
      (ai-code-mcp-open-inspector-script))
     ((string= choice "Generate mcp config")
      (ai-code-mcp-generate-config)))))

;;;###autoload
(defun ai-code-mcp-generate-config ()
  "Generate an MCP config snippet tailored for the active backend.
Claude-oriented backends receive JSON, while the Codex backend outputs toml.
The snippet is shown in *<base-dir-name>:mcp config*."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (project-root (ai-code-mcp-inspector--find-project-root current-file)))
      (if project-root
          (let* ((base-dir (file-name-as-directory project-root))
                 (base-dir-path (directory-file-name base-dir))
                 (base-dir-name (file-name-nondirectory base-dir-path))
                 (buffer-label (if (> (length base-dir-name) 0)
                                   base-dir-name
                                 base-dir-path))
                 (relative-path (file-relative-name current-file base-dir))
                 (buffer-name (format "*%s:mcp config*" buffer-label))
                 (use-codex-format (eq ai-code-selected-backend 'codex))
                 (config-string
                  (if use-codex-format
                      (ai-code--mcp-config-toml buffer-label base-dir-path relative-path)
                    (ai-code--mcp-config-json buffer-label base-dir-path relative-path))))
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert config-string)
                (when (or (null config-string)
                          (<= (length config-string) 0)
                          (not (eq (aref config-string (1- (length config-string))) ?\n)))
                  (insert "\n"))
                (cond
                 (use-codex-format
                  (when (fboundp 'conf-toml-mode)
                    (conf-toml-mode)))
                 ((fboundp 'json-mode)
                  (json-mode))))
              (goto-char (point-min))
              (display-buffer (current-buffer))
              (message "Generated MCP config in %s" buffer-name)))
        (message "Could not find project root with pyproject.toml"))))

(defun ai-code--mcp-config-json (server-label base-dir-path relative-path)
  "Return JSON MCP config string for SERVER-LABEL.
BASE-DIR-PATH and RELATIVE-PATH populate the uv command arguments."
  (let ((json-encoding-pretty-print t))
    (json-encode
     `(("mcpServers"
        . ((,server-label
            . (("command" . "uv")
               ("args" . ["--directory" ,base-dir-path "run" ,relative-path])))))))))

(defun ai-code--mcp-config-toml (server-label base-dir-path relative-path)
  "Return Codex TOML MCP config snippet for SERVER-LABEL.
BASE-DIR-PATH and RELATIVE-PATH populate the uv command arguments."
  (let ((quote (lambda (value) (json-encode value))))
    (mapconcat
     #'identity
     (list
      (format "[mcp_servers.%s]" server-label)
      (format "command = %s" (funcall quote "uv"))
      (format "args = [\n    %s,\n    %s,\n    %s,\n    %s\n]"
              (funcall quote "--directory")
              (funcall quote base-dir-path)
              (funcall quote "run")
              (funcall quote relative-path)))
     "\n")))

;;;###autoload
(defun ai-code-mcp-run ()
  "Run python mcp with uv command.
Run command: uv --directory PROJECT-ROOT-BASE-DIR run
RELATIVE-PATH-TO-CURRENT-BUFFER-FILE.
Execute in compilation buffer named
*ai-code-mcp-run:FULL-PATH-OF-PYTHON-FILE*.
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
(defun ai-code-mcp-open-inspector-script ()
  "Open inspector.sh file in other window.
First find project root with `ai-code-mcp-inspector--find-project-root',
then open inspector.sh in the project root.
If the file does not exist, still open it and show message in minibuffer."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
        (let ((project-root (ai-code-mcp-inspector--find-project-root current-file)))
          (if project-root
              (let ((inspector-path (expand-file-name "inspector.sh" project-root)))
                (find-file-other-window inspector-path)
                (if (file-exists-p inspector-path)
                    (message "Opened inspector.sh from project root: %s" project-root)
                  (message "Created new inspector.sh in project root: %s" project-root)))
            (message "Could not find project root with pyproject.toml")))
      (message "Current buffer is not visiting a file"))))

;;;###autoload
(defun ai-code-mcp-inspector-run ()
  "Run MCP inspector for the current context.
If inspector.sh is found in an ancestor directory, use that directory as
working directory and run it with bash.
Otherwise, for Python buffers, locate the project root via pyproject.toml and
run the inspector against the active file. For Dired buffers, prompt for an
inspector command, prefix the required ports, and execute it inside the listed
directory."
  (interactive)
  (let ((inspector-dir (ai-code-mcp-inspector--find-inspector-script)))
    (if inspector-dir
        ;; Found inspector.sh, use it directly
        (ai-code-mcp-inspector--run-script inspector-dir)
      ;; Fall back to original logic
      (let ((context (ai-code-mcp-inspector--build-context)))
        (when context
          (ai-code-mcp-inspector--start context))))))

(defun ai-code-mcp-inspector--run-script (inspector-dir)
  "Run inspector.sh found in INSPECTOR-DIR."
  (let* ((base-dir (file-name-as-directory inspector-dir))
         (base-dir-name (file-name-nondirectory (directory-file-name base-dir)))
         (default-command "bash inspector.sh")
         (command (ai-code-read-string "MCP inspector command: " default-command))
         (buffer-name (format "*ai-code-mcp-inspector:%s*" base-dir-name))
         (default-directory base-dir))
    (when (and command (not (string= command "")))
      (compilation-start
       command
       nil
       (lambda (_mode) buffer-name)))))

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
             (command (ai-code-mcp-inspector--build-command
                       is-dired base-dir base-dir-name
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

(defun ai-code-mcp-inspector--build-command (is-dired base-dir base-dir-name
                                                     client-port svr-port
                                                     relative-path)
  "Construct the inspector command string for the current context.
IS-DIRED selects interactive input, BASE-DIR and BASE-DIR-NAME describe
the project, CLIENT-PORT and SVR-PORT configure networking, and
RELATIVE-PATH targets a file."
  (if is-dired
      (let ((user-command (ai-code-read-string (format "Inspector command for %s: " base-dir-name))))
        (if (string-match-p "\\`[ \t\n\r]*\\'" user-command)
            (progn
              (message "Inspector command is required")
              nil)
          (format "CLIENT_PORT=%d SERVER_PORT=%d %s"
                  client-port svr-port user-command)))
    (when relative-path
      (format
       "CLIENT_PORT=%d SERVER_PORT=%d npx @modelcontextprotocol/inspector -e VERIFY_SSL=true -e FASTMCP_LOG_LEVEL=INFO uv run --directory %s %s "
       client-port svr-port base-dir base-dir-name))))

(defun ai-code-mcp-inspector--start (context)
  "Launch the inspector using CONTEXT plist.
Produced by `ai-code-mcp-inspector--build-context'."
  (let* ((base-dir (plist-get context :base-dir))
         (base-dir-name (plist-get context :base-dir-name))
         (buffer-name (plist-get context :buffer-name))
         (client-port (plist-get context :client-port))
         (server-port (plist-get context :server-port))
         (default-command (plist-get context :command))
         (command (ai-code-read-string "MCP inspector command: " default-command))
         (display-entries (plist-get context :display-entries)))
    (when (and command (not (string= command "")))
      (let* ((default-directory base-dir)
             (buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (eshell-mode)
          (goto-char (point-max))
          (insert (format "# Running MCP Inspector for %s\n" base-dir-name))
          (dolist (entry display-entries)
            (insert (format "# %s: %s\n" (car entry) (cdr entry))))
          (insert (format "# Server port: %d\n" server-port))
          (insert (format "# Client port: %d\n" client-port))
          (insert (format "# Command: %s\n\n" command))
          (insert "# Starting inspector...\n\n")
          (insert command)
          (eshell-send-input))
        (display-buffer buffer)
        (message "MCP Inspector started, output in %s" buffer-name)))))

(defun ai-code-mcp-inspector--find-inspector-script ()
  "Find inspector.sh by looking for it in parent directories.
Starts from current buffer file or default directory."
  (let* ((current-file (buffer-file-name))
         (start-path (or current-file default-directory))
         (dir (if (file-directory-p start-path)
                  start-path
                (file-name-directory start-path))))
    (while (and dir
                (not (string= dir "/"))
                (not (file-exists-p (expand-file-name "inspector.sh" dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (if (and dir (file-exists-p (expand-file-name "inspector.sh" dir)))
        dir
      nil)))

(defun ai-code-mcp-inspector--find-project-root (file-path)
  "Find project root by looking for pyproject.toml in parent directories.
Starting from FILE-PATH."
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
