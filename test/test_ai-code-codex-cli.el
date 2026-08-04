;;; test_ai-code-codex-cli.el --- Tests for ai-code-codex-cli -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-codex-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-codex-cli)
(require 'ai-code-mcp-agent nil t)

(ert-deftest ai-code-test-codex-cli-start-injects-session-mcp-config ()
  "Starting Codex should inject an Emacs MCP server URL and lifecycle hooks."
  (should (fboundp 'ai-code-codex-cli))
  (let ((captured-command nil)
        (captured-cleanup-fn nil)
        (captured-post-start-fn nil)
        (captured-env-vars nil)
        (registered nil)
        (attached nil)
        (unregistered nil)
        (builtins-called nil)
        (ensure-called nil)
        (session-buffer (generate-new-buffer " *ai-code-codex-mcp*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
                   (lambda () "/tmp/test-codex"))
                  ((symbol-function 'ai-code-backends-infra--resolve-start-command)
                   (lambda (&rest _args)
                     (list :command "codex --full-auto"
                           :argv '("codex" "--full-auto"))))
                  ((symbol-function 'ai-code-mcp-builtins-setup)
                   (lambda () (setq builtins-called t)))
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda ()
                     (setq ensure-called t)
                     8765))
                  ((symbol-function 'ai-code-mcp--random-secret)
                   (lambda () "test-codex-token"))
                  ((symbol-function 'ai-code-mcp-register-session)
                   (lambda (session-id project-dir buffer metadata)
                     (setq registered
                           (list session-id project-dir buffer metadata))))
                  ((symbol-function 'ai-code-mcp-attach-agent-buffer)
                   (lambda (session-id buffer)
                     (setq attached (list session-id buffer))))
                  ((symbol-function 'ai-code-mcp-unregister-session)
                   (lambda (session-id)
                     (setq unregistered session-id)))
                  ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
                   (lambda (&rest args)
                     (cl-destructuring-bind
                         (_working-dir _buffer-name _process-table command
                                       &optional _escape-fn cleanup-fn
                                       _instance-name _prefix _force-prompt
                                       env-vars _multiline-input-sequence
                                       post-start-fn)
                         args
                       (setq captured-command command)
                       (setq captured-cleanup-fn cleanup-fn)
                       (setq captured-post-start-fn post-start-fn)
                       (setq captured-env-vars env-vars))
                     nil)))
          (ai-code-codex-cli)
          (should builtins-called)
          (should ensure-called)
          (let ((config-args (member "-c" captured-command)))
            (should config-args)
            (should
             (string-match-p
              "mcp_servers\\.emacs_tools"
              (cadr config-args)))
            (should
             (string-match-p
              "bearer_token_env_var"
              (cadr config-args))))
          (should-not (string-match-p "test-codex-token"
                                      (cadr (member "-c" captured-command))))
          (should (equal captured-env-vars
                         '("AI_CODE_MCP_BEARER_TOKEN=test-codex-token")))
          (should (functionp captured-cleanup-fn))
          (should (functionp captured-post-start-fn))
          (funcall captured-post-start-fn session-buffer nil "default")
          (should (equal "/tmp/test-codex" (nth 1 registered)))
          (should-not (eq session-buffer (nth 2 registered)))
          (should (equal "test-codex-token"
                         (plist-get (nth 3 registered) :token)))
          (should (equal (list (car registered) session-buffer) attached))
          (with-current-buffer session-buffer
            (should (fboundp 'ai-code-mcp-agent-buffer-status))
            (let ((status (ai-code-mcp-agent-buffer-status)))
              (should (eq 'codex (plist-get status :backend)))
              (should (equal "http://127.0.0.1:8765/mcp"
                             (plist-get status :server-url)))))
          (funcall captured-cleanup-fn)
          (should (equal (car registered) unregistered)))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(provide 'test_ai-code-codex-cli)

;;; test_ai-code-codex-cli.el ends here
