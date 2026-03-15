;;; test_ai-code-github-copilot-cli.el --- Tests for ai-code-github-copilot-cli -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-github-copilot-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-github-copilot-cli)
(require 'ai-code-mcp-agent nil t)

(ert-deftest ai-code-test-github-copilot-cli-start-passes-multiline-sequence ()
  "Starting Copilot should pass the configured multiline sequence to infra."
  (let ((captured-sequence :unset)
        (captured-env-vars :unset))
    (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test-copilot"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args)
                 (list :command "copilot")))
              ((symbol-function 'ai-code-mcp-agent-prepare-launch)
               (lambda (&rest _args) nil))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest args)
                 (cl-destructuring-bind
                     (_working-dir _buffer-name _process-table _command
                                   &optional _escape-fn _cleanup-fn
                                   _instance-name _prefix _force-prompt
                                   env-vars multiline-input-sequence
                                   _post-start-fn)
                     args
                   (setq captured-env-vars env-vars)
                   (setq captured-sequence multiline-input-sequence))
                 nil)))
      (let ((ai-code-github-copilot-cli-extra-env-vars '("TERM_PROGRAM=vscode"))
            (ai-code-github-copilot-cli-multiline-input-sequence "\\\r\n"))
        (ai-code-github-copilot-cli)
        (should (equal captured-env-vars '("TERM_PROGRAM=vscode")))
        (should (equal captured-sequence "\\\r\n"))))))

(ert-deftest ai-code-test-github-copilot-cli-defaults-to-mcp-enabled ()
  "Copilot should be enabled for automatic Emacs MCP wiring by default."
  (should (boundp 'ai-code-mcp-agent-enabled-backends))
  (should (memq 'github-copilot-cli ai-code-mcp-agent-enabled-backends)))

(ert-deftest ai-code-test-github-copilot-cli-start-injects-session-mcp-config ()
  "Starting Copilot should inject Emacs MCP config and session lifecycle hooks."
  (should (fboundp 'ai-code-github-copilot-cli))
  (let ((captured-command nil)
        (captured-cleanup-fn nil)
        (captured-post-start-fn nil)
        (captured-sequence :unset)
        (captured-env-vars :unset)
        (registered nil)
        (unregistered nil)
        (builtins-called nil)
        (ensure-called nil)
        (session-buffer (generate-new-buffer " *ai-code-copilot-mcp*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
                   (lambda () "/tmp/test-copilot"))
                  ((symbol-function 'ai-code-backends-infra--resolve-start-command)
                   (lambda (&rest _args)
                     (list :command "copilot --allow-all-tools")))
                  ((symbol-function 'ai-code-mcp-builtins-setup)
                   (lambda () (setq builtins-called t)))
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda ()
                     (setq ensure-called t)
                     8765))
                  ((symbol-function 'ai-code-mcp-register-session)
                   (lambda (session-id project-dir buffer)
                     (setq registered (list session-id project-dir buffer))))
                  ((symbol-function 'ai-code-mcp-unregister-session)
                   (lambda (session-id)
                     (setq unregistered session-id)))
                  ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
                   (lambda (&rest args)
                     (cl-destructuring-bind
                         (_working-dir _buffer-name _process-table command
                                       &optional _escape-fn cleanup-fn
                                       _instance-name _prefix _force-prompt
                                       env-vars multiline-input-sequence
                                       post-start-fn)
                         args
                       (setq captured-command command)
                       (setq captured-cleanup-fn cleanup-fn)
                       (setq captured-post-start-fn post-start-fn)
                       (setq captured-env-vars env-vars)
                       (setq captured-sequence multiline-input-sequence))
                     nil)))
          (let ((ai-code-github-copilot-cli-extra-env-vars '("TERM_PROGRAM=vscode"))
                (ai-code-github-copilot-cli-multiline-input-sequence "\\\r\n"))
            (ai-code-github-copilot-cli)
            (should builtins-called)
            (should ensure-called)
            (should (string-match-p "--additional-mcp-config" captured-command))
            (should (string-match-p "127\\.0\\.0\\.1" captured-command))
            (should (string-match-p "mcp/github-copilot-cli-" captured-command))
            (should (equal captured-env-vars '("TERM_PROGRAM=vscode")))
            (should (equal captured-sequence "\\\r\n"))
            (should (functionp captured-cleanup-fn))
            (should (functionp captured-post-start-fn))
            (funcall captured-post-start-fn session-buffer nil "default")
            (should (equal "/tmp/test-copilot" (nth 1 registered)))
            (should (eq session-buffer (nth 2 registered)))
            (with-current-buffer session-buffer
              (should (fboundp 'ai-code-mcp-agent-buffer-status))
              (let ((status (ai-code-mcp-agent-buffer-status)))
                (should (eq 'github-copilot-cli (plist-get status :backend)))
                (should (string-match-p
                         "^http://127\\.0\\.0\\.1:8765/mcp/"
                         (plist-get status :server-url)))))
            (funcall captured-cleanup-fn)
            (should (equal (car registered) unregistered))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(provide 'test_ai-code-github-copilot-cli)

;;; test_ai-code-github-copilot-cli.el ends here
