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

(defcustom ai-code-mcp-agent-enabled-backends '(codex github-copilot-cli claude-code)
  "Backends that should receive automatic Emacs MCP integration."
  :type '(repeat symbol)
  :group 'ai-code-mcp-agent)

(defconst ai-code-mcp-agent--server-name "emacs_tools"
  "Server name used in backend MCP config overrides.")

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

(defun ai-code-mcp-agent-prepare-launch (backend working-dir command)
  "Return MCP launch metadata for BACKEND, WORKING-DIR, and COMMAND."
  (when (memq backend ai-code-mcp-agent-enabled-backends)
    (ai-code-mcp-builtins-setup)
    (let* ((port (ai-code-mcp-http-server-ensure))
           (session-id (ai-code-mcp-agent--make-session-id backend))
           (url (ai-code-mcp-agent--make-server-url port session-id)))
      (list :command (ai-code-mcp-agent--inject-command backend command url)
            :cleanup-fn (lambda ()
                          (ai-code-mcp-unregister-session session-id))
            :post-start-fn (lambda (buffer _process _instance-name)
                             (ai-code-mcp-agent--record-buffer-session
                              buffer backend session-id working-dir url))))))

(defun ai-code-mcp-agent--make-session-id (backend)
  "Create a fresh session id for BACKEND."
  (format "%s-%s-%d"
          (symbol-name backend)
          (format-time-string "%Y%m%d%H%M%S")
          (random 1000000)))

(defun ai-code-mcp-agent--make-server-url (port session-id)
  "Build an MCP server URL from PORT and SESSION-ID."
  (format "http://127.0.0.1:%d/mcp/%s" port session-id))

(defun ai-code-mcp-agent--record-buffer-session (buffer backend session-id working-dir url)
  "Record session metadata for BUFFER."
  (ai-code-mcp-register-session session-id working-dir buffer)
  (with-current-buffer buffer
    (setq-local ai-code-mcp-agent--backend backend
                ai-code-mcp-agent--session-id session-id
                ai-code-mcp-agent--server-url url)))

(defun ai-code-mcp-agent--inject-command (backend command url)
  "Inject backend-specific MCP config into COMMAND for URL."
  (pcase backend
    ('codex
     (concat command
             " -c "
             (shell-quote-argument
              (format "mcp_servers.%s={ url = %s }"
                      ai-code-mcp-agent--server-name
                      (json-encode url)))))
    ('github-copilot-cli
     (concat command
             " --additional-mcp-config "
             (shell-quote-argument
              (ai-code-mcp-agent--copilot-config-json url))))
    ('claude-code
     (let ((config-file (ai-code-mcp-agent--claude-code-config-file url)))
       (concat command
               " --mcp-config "
               (shell-quote-argument config-file))))
    (_ command)))

(defun ai-code-mcp-agent--copilot-config-json (url)
  "Return a Copilot CLI MCP config JSON string for URL."
  (json-encode
   `((mcpServers
      . ((,ai-code-mcp-agent--server-name
          . ((type . "http")
             (url . ,url))))))))

(defun ai-code-mcp-agent--claude-code-config-file (url)
  "Write a Claude Code MCP config file for URL and return its path."
  (let ((config-file (make-temp-file "ai-code-mcp-claude-code-" nil ".json")))
    (with-temp-file config-file
      (insert (json-encode
               `((mcpServers
                  . ((,ai-code-mcp-agent--server-name
                      . ((type . "http")
                         (url . ,url)))))))))
    config-file))

(provide 'ai-code-mcp-agent)

;;; ai-code-mcp-agent.el ends here
