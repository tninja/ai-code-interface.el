;;; ai-code-mcp-agent.el --- Agent adapters for ai-code MCP tools -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Backend-facing helpers that expose Emacs MCP tools to AI agents.

;;; Code:

(require 'json)

(require 'ai-code-mcp-http-server)
(require 'ai-code-mcp-server)

(declare-function ai-code-backends-infra--terminal-navigation-mode-p
                  "ai-code-backends-infra" ())

(defgroup ai-code-mcp-agent nil
  "Agent adapters for AI Code MCP tools."
  :group 'tools
  :prefix "ai-code-mcp-agent-")

(defcustom ai-code-mcp-agent-enabled-backends
  '(codex open-interpreter github-copilot-cli claude-code antigravity pi)
  "Backends that should receive automatic Emacs MCP integration."
  :type '(repeat symbol)
  :group 'ai-code-mcp-agent)

(defcustom ai-code-mcp-agent-token-lifetime-seconds (* 24 60 60)
  "Idle lifetime of a per-launch MCP bearer token in seconds.
Accepted MCP requests and local session interactions renew the expiry using
the lifetime captured when the agent was launched.  After this much idle
time, remote requests are rejected until local session activity or a new
agent launch renews access."
  :type 'positive-integer
  :group 'ai-code-mcp-agent)

(defconst ai-code-mcp-agent--pi-valid-exposures
  '("codemode" "deferred" "direct" "hidden")
  "Exposure strings accepted by Pi's `registerMcpServer'.")

(defcustom ai-code-mcp-agent-pi-exposure "codemode"
  "Default tool exposure passed to `pi.registerMcpServer' for Emacs MCP tools.
\"codemode\" keeps most Emacs MCP schemas out of the direct tool list while
automatically enabling Pi's `codemode' script runner for parallel calls."
  :type '(choice (const :tag "Codemode (script-callable)" "codemode")
                 (const :tag "Deferred (tool_search)" "deferred")
                 (const :tag "Direct (all tools declared)" "direct")
                 (const :tag "Hidden" "hidden"))
  :group 'ai-code-mcp-agent)

(defcustom ai-code-mcp-agent-pi-tool-exposure
  '(("diagnostics_baseline" . "direct")
    ("get_diagnostics" . "direct")
    ("get_org_heading_context" . "direct"))
  "Per-tool exposure overrides passed as `toolExposure' for Pi MCP sessions.
Each entry is `(TOOL-OR-PATTERN . EXPOSURE)', where EXPOSURE is one of
\"codemode\", \"deferred\", \"direct\", or \"hidden\".  When nil, omit
`toolExposure' from the generated Pi extension."
  :type '(alist :key-type string
                :value-type (choice (const "codemode")
                                    (const "deferred")
                                    (const "direct")
                                    (const "hidden")))
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
  "Relative path inside an Antigravity `--add-dir' config directory.")

(defvar ai-code-mcp-agent--active-cleanups
  (make-hash-table :test 'equal)
  "Hash table mapping active MCP session ids to their cleanup functions.")

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
  "Return MCP launch metadata for BACKEND, WORKING-DIR, and ARGV.
Skip local MCP injection when BACKEND is disabled or WORKING-DIR is remote."
  (when (and (memq backend ai-code-mcp-agent-enabled-backends)
             (not (and (stringp working-dir) (file-remote-p working-dir))))
    (let* ((server-was-live (ai-code-mcp-http-server-live-p))
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
                     token))
              (ai-code-mcp-register-session
               session-id working-dir (current-buffer)
               (list :backend backend
                     :state 'pending
                     :tool-profile ai-code-mcp-default-tool-profile
                     :token token
                     :token-lifetime-seconds
                     ai-code-mcp-agent-token-lifetime-seconds
                     :expires-at
                     (time-add (current-time)
                               (seconds-to-time
                                ai-code-mcp-agent-token-lifetime-seconds))))
              (let ((cleanup-fn
                     (ai-code-mcp-agent--make-cleanup-function
                      session-id
                      (plist-get launch-metadata :runtime-files)
                      (plist-get launch-metadata :runtime-cleanup-functions))))
                (puthash session-id cleanup-fn ai-code-mcp-agent--active-cleanups)
                (list :argv (plist-get launch-metadata :argv)
                      :env-vars
                      (list (format "%s=%s"
                                    ai-code-mcp-agent--token-environment-variable
                                    token))
                      :mcp-session-id session-id
                      :mcp-server-url url
                      :cleanup-fn cleanup-fn
                      :post-start-fn
                      (lambda (buffer _process _instance-name)
                        (ai-code-mcp-agent--record-buffer-session
                         buffer backend session-id url))))))
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
          (ai-code-mcp-http-server-stop)))
      (when (and unregistered
                 (null remaining-files)
                 (null remaining-cleanups))
        (remhash session-id ai-code-mcp-agent--active-cleanups)))))

(defun ai-code-mcp-agent--cleanup-all-sessions ()
  "Clean up all active MCP sessions and their temporary runtime resources."
  (let (cleanups)
    (maphash (lambda (_session-id cleanup-fn)
               (push cleanup-fn cleanups))
             ai-code-mcp-agent--active-cleanups)
    (dolist (cleanup-fn cleanups)
      (condition-case err
          (funcall cleanup-fn)
        (error
         (display-warning
          'ai-code-mcp-agent
          (format "Failed to clean up MCP session on exit: %s"
                  (error-message-string err))))))))

(add-hook 'kill-emacs-hook #'ai-code-mcp-agent--cleanup-all-sessions)

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
                ai-code-mcp-agent--server-url url)
    (add-hook 'pre-command-hook
              #'ai-code-mcp-agent--renew-on-terminal-return nil t)))

(defun ai-code-mcp-agent-touch-session (agent-buffer)
  "Extend renewable MCP token expiry for AGENT-BUFFER without changing origin."
  (when (buffer-live-p agent-buffer)
    (when-let* ((session-id
                 (buffer-local-value 'ai-code-mcp-agent--session-id
                                     agent-buffer)))
      (ai-code-mcp-touch-session session-id))))

(defun ai-code-mcp-agent--renew-on-terminal-return ()
  "Renew the current agent buffer's MCP token on live interactive Return."
  (when (and ai-code-mcp-agent--session-id
             (not (and (fboundp 'ai-code-backends-infra--terminal-navigation-mode-p)
                       (ai-code-backends-infra--terminal-navigation-mode-p))))
    (let* ((keys (this-command-keys-vector))
           (key (and (> (length keys) 0)
                     (aref keys (1- (length keys))))))
      (when (memq key '(?\r ?\n return kp-enter))
        (ai-code-mcp-agent-touch-session (current-buffer))))))

(defun ai-code-mcp-agent-refresh-source-context (agent-buffer source-buffer)
  "Refresh AGENT-BUFFER's MCP prompt origin from SOURCE-BUFFER."
  (when (and (buffer-live-p agent-buffer)
             (buffer-live-p source-buffer))
    (let ((session-id
           (buffer-local-value 'ai-code-mcp-agent--session-id agent-buffer)))
      (when session-id
        (ai-code-mcp-update-source-context session-id source-buffer)))))

(defun ai-code-mcp-agent--inject-argv
    (backend argv url token-env-var &optional token)
  "Return MCP launch metadata for BACKEND and ARGV.
URL identifies the server and TOKEN-ENV-VAR names its secret environment
variable.  TOKEN supports backends that cannot expand environment variables in
MCP headers and instead receive a private per-session config directory."
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
    ('pi
     (let ((extension-file
            (ai-code-mcp-agent--pi-extension-file url token-env-var)))
       (list :argv (append argv (list "--extension" extension-file))
             :runtime-files (list extension-file))))
    ('antigravity
     (let ((config-dir (ai-code-mcp-agent--antigravity-config-dir url token)))
       (list
        :argv (append argv (list "--add-dir" config-dir))
        :runtime-cleanup-functions
        (list (lambda ()
                (when (file-directory-p config-dir)
                  (delete-directory config-dir t)))))))
    (_ (list :argv argv))))

(defun ai-code-mcp-agent--antigravity-config-json (url token)
  "Return an Antigravity MCP config JSON string for URL and TOKEN."
  (let ((config (make-hash-table :test 'equal))
        (servers (make-hash-table :test 'equal))
        (server (make-hash-table :test 'equal))
        (headers (make-hash-table :test 'equal)))
    (puthash "Authorization" (concat "Bearer " token) headers)
    (puthash "serverUrl" url server)
    (puthash "headers" headers server)
    (puthash ai-code-mcp-agent--server-name server servers)
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

(defun ai-code-mcp-agent--antigravity-config-dir (url token)
  "Create a private temporary Antigravity config directory for URL and TOKEN.
Return the directory path to pass via `--add-dir'.  Each launch gets its own
directory so concurrent Antigravity instances stay isolated and never touch the
workspace's `.agents/mcp_config.json'."
  (let* ((config-dir (make-temp-file "ai-code-mcp-antigravity-" t))
         (config-path (expand-file-name
                       ai-code-mcp-agent--antigravity-config-relative-path
                       config-dir))
         (agents-dir (file-name-directory config-path))
         (content (ai-code-mcp-agent--antigravity-config-json url token)))
    (condition-case err
        (progn
          (set-file-modes config-dir #o700)
          (make-directory agents-dir t)
          (set-file-modes agents-dir #o700)
          (ai-code-mcp-agent--write-private-file config-path content #o600)
          config-dir)
      (error
       (when (file-directory-p config-dir)
         (ignore-errors (delete-directory config-dir t)))
       (signal (car err) (cdr err))))))

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

(defun ai-code-mcp-agent--validate-pi-exposure (exposure)
  "Return EXPOSURE when Pi accepts it, or signal an error."
  (unless (and (stringp exposure)
               (member exposure ai-code-mcp-agent--pi-valid-exposures))
    (error "Unsupported Pi MCP exposure: %S" exposure))
  exposure)

(defun ai-code-mcp-agent--pi-tool-exposure-line (entries)
  "Return the `toolExposure' property line for ENTRIES, or empty string."
  (unless (listp entries)
    (error "Invalid Pi MCP tool exposure list: %S" entries))
  (if (null entries)
      ""
    (let ((pairs
           (mapcar
            (lambda (entry)
              (unless (and (consp entry)
                           (stringp (car entry))
                           (not (string-empty-p (car entry))))
                (error "Invalid Pi MCP tool exposure entry: %S" entry))
              (format "%s: %s"
                      (json-encode (car entry))
                      (json-encode
                       (ai-code-mcp-agent--validate-pi-exposure (cdr entry)))))
            entries)))
      (format "    toolExposure: { %s },\n"
              (mapconcat #'identity pairs ", ")))))

(defun ai-code-mcp-agent--pi-extension-source (url token-env-var)
  "Return Pi extension source registering the MCP server at URL.
The extension reads the bearer token from TOKEN-ENV-VAR at runtime, so the
secret never touches disk.  Each Pi process gets its own extension file and
token environment, which keeps concurrent Pi sessions independent."
  (let ((server-name (json-encode ai-code-mcp-agent--server-name))
        (url-json (json-encode url))
        (env-json (json-encode token-env-var))
        (exposure-json
         (json-encode
          (ai-code-mcp-agent--validate-pi-exposure
           ai-code-mcp-agent-pi-exposure)))
        (tool-exposure-line
         (ai-code-mcp-agent--pi-tool-exposure-line
          ai-code-mcp-agent-pi-tool-exposure)))
    (concat
     "// Generated by ai-code-mcp-agent.el for one Pi session.\n"
     "export default function (pi) {\n"
     "  if (typeof pi.registerMcpServer !== \"function\") {\n"
     "    pi.on(\"session_start\", async (_event, ctx) => {\n"
     "      ctx.ui.notify(\"AI Code: this Pi version cannot register MCP "
     "servers; Emacs MCP tools are unavailable.\", \"warning\");\n"
     "    });\n"
     "    return;\n"
     "  }\n"
     (format "  const token = process.env[%s] || \"\";\n" env-json)
     (format "  pi.registerMcpServer(%s, {\n" server-name)
     (format "    url: %s,\n" url-json)
     "    headers: { Authorization: `Bearer ${token}` },\n"
     (format "    exposure: %s,\n" exposure-json)
     tool-exposure-line
     "    description: \"Live Emacs editor, project, and debugging tools\"\n"
     "  });\n"
     "}\n")))

(defun ai-code-mcp-agent--pi-extension-file (url token-env-var)
  "Write a Pi MCP extension for URL using TOKEN-ENV-VAR and return its path."
  (let ((extension-file (make-temp-file "ai-code-mcp-pi-" nil ".js")))
    (with-temp-file extension-file
      (insert (ai-code-mcp-agent--pi-extension-source url token-env-var)))
    extension-file))

(provide 'ai-code-mcp-agent)

;;; ai-code-mcp-agent.el ends here
