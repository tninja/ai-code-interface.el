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

(defcustom ai-code-mcp-agent-enabled-backends '(codex open-interpreter github-copilot-cli claude-code)
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
    (ai-code-mcp-builtins-setup)
    (let* ((port (ai-code-mcp-http-server-ensure))
           (session-id (ai-code-mcp-agent--make-session-id backend))
           (token (ai-code-mcp--random-secret))
           (url (ai-code-mcp-agent--make-server-url port session-id))
           (launch-metadata
            (ai-code-mcp-agent--inject-argv backend argv url
                                            ai-code-mcp-agent--token-environment-variable))
           (runtime-files (plist-get launch-metadata :runtime-files)))
      (ai-code-mcp-register-session
       session-id working-dir (current-buffer)
       (list :backend backend
             :state 'pending
             :tool-profile ai-code-mcp-default-tool-profile
             :token token
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
             session-id runtime-files)
            :post-start-fn (lambda (buffer _process _instance-name)
                             (ai-code-mcp-agent--record-buffer-session
                              buffer backend session-id url))))))

(defun ai-code-mcp-agent--make-cleanup-function (session-id runtime-files)
  "Return a repeat-safe cleanup function for SESSION-ID and RUNTIME-FILES.
Failed file removals are retried; session unregistration occurs at most once."
  (let ((remaining-files (copy-sequence runtime-files))
        (unregistered nil))
    (lambda ()
      (let (failed-files)
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
        (setq remaining-files (nreverse failed-files)))
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

(defun ai-code-mcp-agent--make-server-url (port &optional session-id)
  "Build the MCP server URL for PORT, optionally scoped by SESSION-ID."
  (if session-id
      (format "http://127.0.0.1:%d/mcp/%s" port session-id)
    (format "http://127.0.0.1:%d/mcp" port)))

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

(defun ai-code-mcp-agent--inject-argv (backend argv url token-env-var)
  "Return launch metadata after injecting URL and TOKEN-ENV-VAR into ARGV for BACKEND."
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
    (_ (list :argv argv))))

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
