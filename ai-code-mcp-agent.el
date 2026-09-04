;;; ai-code-mcp-agent.el --- Agent adapters for ai-code MCP tools -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Backend-facing helpers that expose Emacs MCP tools to AI agents.

;;; Code:

(require 'json)

(require 'ai-code-mcp-http-server)
(require 'ai-code-mcp-server)

(defgroup ai-code-mcp-agent nil
  "Agent adapters for AI Code MCP tools."
  :group 'tools
  :prefix "ai-code-mcp-agent-")

(defcustom ai-code-mcp-agent-enabled-backends
  '(codex open-interpreter github-copilot-cli claude-code antigravity)
  "Backends that should receive automatic Emacs MCP integration."
  :type '(repeat symbol)
  :group 'ai-code-mcp-agent)

(defcustom ai-code-mcp-agent-token-lifetime-seconds (* 24 60 60)
  "Lifetime of a per-launch MCP bearer token in seconds."
  :type 'positive-integer
  :group 'ai-code-mcp-agent)

(defconst ai-code-mcp-agent--server-name "emacs_tools"
  "Server name used in backend MCP config overrides.")

(defconst ai-code-mcp-agent--token-environment-variable
  "AI_CODE_MCP_BEARER_TOKEN"
  "Child-process environment variable that carries the MCP bearer token.")

(defconst ai-code-mcp-agent--status-buffer-name "*AI Code MCP Status*"
  "Buffer name used to display MCP status to users.")

(defconst ai-code-mcp-agent--antigravity-config-relative-path
  ".agents/mcp_config.json"
  "Workspace-relative path used by Antigravity for MCP configuration.")

(defvar ai-code-mcp-agent--antigravity-config-states
  (make-hash-table :test 'equal)
  "Active Antigravity MCP config leases keyed by absolute config path.")

(defvar-local ai-code-mcp-agent--backend nil
  "Backend symbol attached to the current agent buffer.")

(defvar-local ai-code-mcp-agent--session-id nil
  "Session id attached to the current agent buffer.")

(defvar-local ai-code-mcp-agent--server-url nil
  "MCP server URL attached to the current agent buffer.")

(defun ai-code-mcp-agent-buffer-status (&optional buffer)
  "Return MCP status for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ai-code-mcp-agent--session-id
      (list :backend ai-code-mcp-agent--backend
            :session-id ai-code-mcp-agent--session-id
            :server-url ai-code-mcp-agent--server-url))))

;;;###autoload
(defun ai-code-mcp-agent-show-buffer-status (&optional buffer)
  "Display MCP status for BUFFER or the current buffer."
  (interactive)
  (let ((status (ai-code-mcp-agent-buffer-status buffer)))
    (if (not status)
        (message "No MCP session is attached to the current buffer.")
      (with-help-window ai-code-mcp-agent--status-buffer-name
        (princ (ai-code-mcp-agent--format-status status))))
    status))

(defun ai-code-mcp-agent--format-status (status)
  "Return a display string for MCP STATUS."
  (concat
   "AI Code MCP Status\n\n"
   (format "Backend: %s\n" (plist-get status :backend))
   (format "Session ID: %s\n" (plist-get status :session-id))
   (format "Server URL: %s\n" (plist-get status :server-url))))

(defun ai-code-mcp-agent-prepare-launch (backend working-dir argv)
  "Return MCP launch metadata for BACKEND, WORKING-DIR, and ARGV."
  (when (memq backend ai-code-mcp-agent-enabled-backends)
    (let* ((backend-state
            (ai-code-mcp-agent--prepare-backend-state backend working-dir))
           (server-was-live (ai-code-mcp-http-server-live-p))
           (session-id (ai-code-mcp-agent--make-session-id backend))
           (token (ai-code-mcp--random-secret))
           launch-metadata)
      (condition-case err
          (progn
            (ai-code-mcp-builtins-setup)
            (let* ((port (ai-code-mcp-http-server-ensure))
                   (url (ai-code-mcp-agent--make-server-url port)))
              (setq launch-metadata
                    (ai-code-mcp-agent--inject-argv
                     backend argv url
                     ai-code-mcp-agent--token-environment-variable
                     token backend-state session-id))
              (ai-code-mcp-register-session
               session-id working-dir (current-buffer)
               (list :backend backend
                     :state 'pending
                     :tool-profile ai-code-mcp-default-tool-profile
                     :token token
                     :modern-protocol-enabled
                     (not (eq backend 'antigravity))
                     :expires-at
                     (time-add (current-time)
                               (seconds-to-time
                                ai-code-mcp-agent-token-lifetime-seconds))))
              (list :argv (plist-get launch-metadata :argv)
                    :env-vars
                    (list (format "%s=%s"
                                  ai-code-mcp-agent--token-environment-variable
                                  token))
                    :mcp-session-id session-id
                    :mcp-server-url url
                    :cleanup-fn
                    (ai-code-mcp-agent--make-cleanup-function
                     session-id
                     (plist-get launch-metadata :runtime-files)
                     (plist-get launch-metadata :runtime-cleanup-functions))
                    :post-start-fn
                    (lambda (buffer _process _instance-name)
                      (ai-code-mcp-agent--record-buffer-session
                       buffer backend session-id url)))))
        (error
         (ai-code-mcp-agent--discard-failed-launch
          session-id launch-metadata server-was-live)
         (signal (car err) (cdr err)))))))

(defun ai-code-mcp-agent--make-cleanup-function
    (session-id runtime-files &optional runtime-cleanup-functions)
  "Return a repeat-safe cleanup function for SESSION-ID and runtime resources.
RUNTIME-FILES are removed and RUNTIME-CLEANUP-FUNCTIONS are called.  Failed
resource cleanup is retried; session unregistration occurs at most once."
  (let ((remaining-files (copy-sequence runtime-files))
        (remaining-cleanups (copy-sequence runtime-cleanup-functions))
        (unregistered nil))
    (lambda ()
      (let (failed-files failed-cleanups)
        (dolist (path remaining-files)
          (when (stringp path)
            (condition-case err
                (when (file-exists-p path)
                  (delete-file path))
              (error
               (push path failed-files)
               (display-warning
                'ai-code-mcp-agent
                (format "Failed to delete runtime file %s: %s"
                        path (error-message-string err)))))))
        (dolist (cleanup remaining-cleanups)
          (condition-case err
              (funcall cleanup)
            (error
             (push cleanup failed-cleanups)
             (display-warning
              'ai-code-mcp-agent
              (format "Failed to clean up an MCP runtime resource: %s"
                      (error-message-string err))))))
        (setq remaining-files (nreverse failed-files)
              remaining-cleanups (nreverse failed-cleanups)))
      (unless unregistered
        (ai-code-mcp-unregister-session session-id)
        (setq unregistered t)
        (when (zerop (ai-code-mcp-session-count))
          (ai-code-mcp-http-server-stop))))))

(defun ai-code-mcp-agent--make-session-id (backend)
  "Create a fresh session id for BACKEND."
  (format "%s-%s-%d"
          (symbol-name backend)
          (format-time-string "%Y%m%d%H%M%S")
          (random 1000000)))

(defun ai-code-mcp-agent--make-server-url (port)
  "Build the shared MCP server endpoint for PORT.
The bearer token selects the registered agent session."
  (format "http://127.0.0.1:%d/mcp" port))

(defun ai-code-mcp-agent--record-buffer-session (buffer backend session-id url)
  "Record BUFFER session for BACKEND, SESSION-ID, and URL."
  (ai-code-mcp-attach-agent-buffer session-id buffer)
  (with-current-buffer buffer
    (setq-local ai-code-mcp-agent--backend backend
                ai-code-mcp-agent--session-id session-id
                ai-code-mcp-agent--server-url url)))

(defun ai-code-mcp-agent-refresh-source-context (agent-buffer source-buffer)
  "Refresh AGENT-BUFFER's MCP prompt origin from SOURCE-BUFFER."
  (when (and (buffer-live-p agent-buffer)
             (buffer-live-p source-buffer))
    (let ((session-id
           (buffer-local-value 'ai-code-mcp-agent--session-id agent-buffer)))
      (when session-id
        (ai-code-mcp-update-source-context session-id source-buffer)))))

(defun ai-code-mcp-agent--inject-argv
    (backend argv url token-env-var &optional token backend-state session-id)
  "Return MCP launch metadata for BACKEND and ARGV.
URL identifies the server and TOKEN-ENV-VAR names its secret environment
variable.  TOKEN, BACKEND-STATE, and SESSION-ID support backends that require a
workspace config file instead of environment substitution."
  (pcase backend
    ((or 'codex 'open-interpreter)
     (let ((config-override
            (format "mcp_servers.%s={ url = %s, bearer_token_env_var = %s }"
                    ai-code-mcp-agent--server-name
                    (json-encode url)
                    (json-encode token-env-var))))
       (list :argv (append argv (list "-c" config-override)))))
    ('github-copilot-cli
     (let ((config-json (ai-code-mcp-agent--copilot-config-json url token-env-var)))
       (list :argv
             (append argv (list "--additional-mcp-config" config-json)))))
    ('claude-code
     (let ((config-file (ai-code-mcp-agent--claude-code-config-file url token-env-var)))
       (list :argv (append argv (list "--mcp-config" config-file))
             :runtime-files (list config-file))))
    ('antigravity
     (list
      :argv argv
      :runtime-cleanup-functions
      (list (ai-code-mcp-agent--install-antigravity-config
             backend-state session-id url token))))
    (_ (list :argv argv))))

(defun ai-code-mcp-agent--prepare-backend-state (backend working-dir)
  "Validate and return pre-launch state for BACKEND in WORKING-DIR."
  (when (eq backend 'antigravity)
    (ai-code-mcp-agent--prepare-antigravity-config working-dir)))

(defun ai-code-mcp-agent--prepare-antigravity-config (working-dir)
  "Return validated Antigravity config state for WORKING-DIR."
  (unless (and (stringp working-dir) (file-directory-p working-dir))
    (error "Antigravity MCP working directory does not exist: %s" working-dir))
  (let* ((project-directory (file-name-as-directory
                             (file-truename working-dir)))
         (path (expand-file-name
                ai-code-mcp-agent--antigravity-config-relative-path
                project-directory))
         (directory (file-name-directory path))
         (active-state
          (gethash path ai-code-mcp-agent--antigravity-config-states)))
    (when (file-symlink-p directory)
      (error "Refusing to write Antigravity MCP config through symlink: %s"
             directory))
    (when (and (file-exists-p directory)
               (not (file-directory-p directory)))
      (error "Antigravity MCP config parent is not a directory: %s" directory))
    (when (file-symlink-p path)
      (error "Refusing to replace symlinked Antigravity MCP config: %s" path))
    (if active-state
        (progn
          (unless (and (file-regular-p path)
                       (equal (ai-code-mcp-agent--file-contents path)
                              (plist-get active-state :written-content)))
            (error "Antigravity MCP config changed during an active session: %s"
                   path))
          active-state)
      (let* ((original-exists-p (file-exists-p path))
             (original-content
              (when original-exists-p
                (unless (file-regular-p path)
                  (error "Antigravity MCP config is not a regular file: %s" path))
                (ai-code-mcp-agent--file-contents path))))
        (when original-exists-p
          (ai-code-mcp-agent--parse-antigravity-config original-content path))
        (list :path path
              :directory directory
              :directory-created-p nil
              :original-exists-p original-exists-p
              :original-content original-content
              :original-mode (and original-exists-p (file-modes path))
              :leases nil
              :written-content nil)))))

(defun ai-code-mcp-agent--file-contents (path)
  "Return the literal contents of PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ai-code-mcp-agent--parse-antigravity-config (content path)
  "Parse Antigravity config CONTENT from PATH as a JSON object."
  (let* ((config
          (json-parse-string content
                             :object-type 'hash-table
                             :array-type 'array
                             :null-object :null
                             :false-object :json-false))
         (missing (make-symbol "missing"))
         (servers (and (hash-table-p config)
                       (gethash "mcpServers" config missing))))
    (unless (hash-table-p config)
      (error "Antigravity MCP config must contain a JSON object: %s" path))
    (unless (or (eq servers missing) (hash-table-p servers))
      (error "Antigravity mcpServers must contain a JSON object: %s" path))
    config))

(defun ai-code-mcp-agent--antigravity-config-content (state url token)
  "Return merged Antigravity config for STATE using URL and TOKEN."
  (let* ((path (plist-get state :path))
         (config
          (if (plist-get state :original-exists-p)
              (ai-code-mcp-agent--parse-antigravity-config
               (plist-get state :original-content) path)
            (make-hash-table :test 'equal)))
         (servers (gethash "mcpServers" config)))
    (setq servers (if (hash-table-p servers)
                      (copy-hash-table servers)
                    (make-hash-table :test 'equal)))
    (let ((server (make-hash-table :test 'equal))
          (headers (make-hash-table :test 'equal)))
      (puthash "Authorization" (concat "Bearer " token) headers)
      (puthash "serverUrl" url server)
      (puthash "headers" headers server)
      (puthash ai-code-mcp-agent--server-name server servers))
    (puthash "mcpServers" servers config)
    (concat (json-serialize config
                            :null-object :null
                            :false-object :json-false)
            "\n")))

(defun ai-code-mcp-agent--write-private-file (path content &optional mode)
  "Atomically write CONTENT to PATH with private permissions or MODE."
  (let ((temp-file
         (make-temp-file
          (expand-file-name ".ai-code-mcp-" (file-name-directory path)))))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert content))
          (set-file-modes temp-file (or mode #o600))
          (rename-file temp-file path t)
          (setq temp-file nil))
      (when (and temp-file (file-exists-p temp-file))
        (delete-file temp-file)))))

(defun ai-code-mcp-agent--install-antigravity-config
    (state session-id url token)
  "Install an Antigravity config lease in STATE for SESSION-ID, URL, and TOKEN."
  (let* ((path (plist-get state :path))
         (directory (plist-get state :directory))
         (created-directory (not (file-directory-p directory)))
         (content (ai-code-mcp-agent--antigravity-config-content state url token)))
    (condition-case err
        (progn
          (when created-directory
            (make-directory directory t))
          (ai-code-mcp-agent--write-private-file path content)
          (setq state
                (plist-put state :directory-created-p
                           (or created-directory
                               (plist-get state :directory-created-p))))
          (setq state
                (plist-put state :leases
                           (cons (cons session-id content)
                                 (plist-get state :leases))))
          (setq state (plist-put state :written-content content))
          (puthash path state ai-code-mcp-agent--antigravity-config-states)
          (lambda ()
            (ai-code-mcp-agent--release-antigravity-config path session-id)))
      (error
       (when (and created-directory
                  (file-directory-p directory)
                  (null (directory-files
                         directory nil directory-files-no-dot-files-regexp)))
         (delete-directory directory))
       (signal (car err) (cdr err))))))

(defun ai-code-mcp-agent--release-antigravity-config (path session-id)
  "Release SESSION-ID's Antigravity config lease at PATH."
  (when-let* ((state
               (gethash path ai-code-mcp-agent--antigravity-config-states))
              (lease (assoc session-id (plist-get state :leases))))
    (let* ((remaining (delq lease (copy-sequence (plist-get state :leases))))
           (owned-p
            (and (not (file-symlink-p path))
                 (file-regular-p path)
                 (equal (ai-code-mcp-agent--file-contents path)
                        (plist-get state :written-content)))))
      (if (not owned-p)
          (display-warning
           'ai-code-mcp-agent
           (format
            "Antigravity MCP config changed externally; leaving it untouched: %s"
            path))
        (if remaining
            (let ((content (cdar remaining)))
              (unless (equal content (plist-get state :written-content))
                (ai-code-mcp-agent--write-private-file path content))
              (setq state (plist-put state :written-content content)))
          (if (plist-get state :original-exists-p)
              (ai-code-mcp-agent--write-private-file
               path
               (plist-get state :original-content)
               (plist-get state :original-mode))
            (delete-file path))))
      (if remaining
          (progn
            (setq state (plist-put state :leases remaining))
            (puthash path state ai-code-mcp-agent--antigravity-config-states))
        (remhash path ai-code-mcp-agent--antigravity-config-states)
        (ai-code-mcp-agent--remove-created-config-directory state)))))

(defun ai-code-mcp-agent--remove-created-config-directory (state)
  "Remove STATE's package-created config directory when it is still empty."
  (let ((directory (plist-get state :directory)))
    (when (and (plist-get state :directory-created-p)
               (file-directory-p directory)
               (not (file-symlink-p directory))
               (null (directory-files
                      directory nil directory-files-no-dot-files-regexp)))
      (delete-directory directory))))

(defun ai-code-mcp-agent--discard-failed-launch
    (session-id launch-metadata server-was-live)
  "Discard failed SESSION-ID resources described by LAUNCH-METADATA.
SERVER-WAS-LIVE records whether this launch started the shared HTTP server."
  (dolist (path (plist-get launch-metadata :runtime-files))
    (when (and (stringp path) (file-exists-p path))
      (condition-case err
          (delete-file path)
        (error
         (display-warning
          'ai-code-mcp-agent
          (format "Failed to discard MCP runtime file %s: %s"
                  path (error-message-string err)))))))
  (dolist (cleanup (plist-get launch-metadata :runtime-cleanup-functions))
    (condition-case err
        (funcall cleanup)
      (error
       (display-warning
        'ai-code-mcp-agent
        (format "Failed to discard an MCP runtime resource: %s"
                (error-message-string err))))))
  (when (ai-code-mcp-get-session-context session-id)
    (ai-code-mcp-unregister-session session-id))
  (when (and (not server-was-live)
             (zerop (ai-code-mcp-session-count)))
    (ai-code-mcp-http-server-stop)))

(defun ai-code-mcp-agent--authorization-template (token-env-var)
  "Return an Authorization header template for TOKEN-ENV-VAR."
  (format "Bearer ${%s}" token-env-var))

(defun ai-code-mcp-agent--copilot-config-json (url token-env-var)
  "Return a Copilot MCP config for URL using TOKEN-ENV-VAR."
  (json-encode
   `((mcpServers
      . ((,ai-code-mcp-agent--server-name
          . ((type . "http")
             (url . ,url)
             (headers
              . ((Authorization
                  . ,(ai-code-mcp-agent--authorization-template
                      token-env-var))))
             (tools . ["*"]))))))))

(defun ai-code-mcp-agent--claude-code-config-file (url token-env-var)
  "Write a Claude MCP config for URL using TOKEN-ENV-VAR and return its path."
  (let ((config-file (make-temp-file "ai-code-mcp-claude-code-" nil ".json")))
    (with-temp-file config-file
      (insert (json-encode
               `((mcpServers
                  . ((,ai-code-mcp-agent--server-name
                      . ((type . "http")
                         (url . ,url)
                         (headers
                          . ((Authorization
                              . ,(ai-code-mcp-agent--authorization-template
                                  token-env-var))))))))))))
    config-file))

(provide 'ai-code-mcp-agent)

;;; ai-code-mcp-agent.el ends here
