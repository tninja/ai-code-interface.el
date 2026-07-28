;;; test_ai-code-open-interpreter-cli.el --- Tests for Open Interpreter CLI -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-open-interpreter-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-open-interpreter-cli)
(require 'ai-code-mcp-agent nil t)

(ert-deftest ai-code-test-open-interpreter-cli-backend-contract ()
  "Open Interpreter should expose the expected backend and MCP defaults."
  (should (equal ai-code-open-interpreter-cli-program "interpreter"))
  (let ((spec (cdr (ai-code--backend-spec 'open-interpreter))))
    (should spec)
    (should (eq (plist-get spec :require) 'ai-code-open-interpreter-cli))
    (should (eq (plist-get spec :start) 'ai-code-open-interpreter-cli))
    (should (eq (plist-get spec :switch)
                'ai-code-open-interpreter-cli-switch-to-buffer))
    (should (eq (plist-get spec :send)
                'ai-code-open-interpreter-cli-send-command))
    (should (eq (plist-get spec :resume)
                'ai-code-open-interpreter-cli-resume))
    (should (equal (plist-get spec :config)
                   "~/.openinterpreter/config.toml"))
    (should (equal (plist-get spec :cli) "interpreter")))
  (should (memq 'open-interpreter ai-code-mcp-agent-enabled-backends)))

(ert-deftest ai-code-test-open-interpreter-cli-resume-appends-last ()
  "Resuming Open Interpreter should request the most recent session."
  (let ((captured-arg nil)
        (captured-switches nil)
        (ai-code-open-interpreter-cli-program-switches '("--model" "test")))
    (cl-letf (((symbol-function 'ai-code-open-interpreter-cli)
               (lambda (&optional arg)
                 (setq captured-arg arg
                       captured-switches
                       ai-code-open-interpreter-cli-program-switches))))
      (ai-code-open-interpreter-cli-resume 'prefix-arg))
    (should (eq captured-arg 'prefix-arg))
    (should (equal captured-switches
                   '("--model" "test" "resume" "--last")))))

(ert-deftest ai-code-test-open-interpreter-cli-start-injects-session-mcp-config ()
  "Starting Open Interpreter should inject MCP config and lifecycle hooks."
  (should (fboundp 'ai-code-open-interpreter-cli))
  (let ((captured-command nil)
        (captured-cleanup-fn nil)
        (captured-post-start-fn nil)
        (registered nil)
        (unregistered nil)
        (builtins-called nil)
        (ensure-called nil)
        (ai-code-mcp-agent-enabled-backends '(open-interpreter))
        (session-buffer
         (generate-new-buffer " *ai-code-open-interpreter-mcp*")))
    (unwind-protect
        (cl-letf (((symbol-function
                    'ai-code-backends-infra--session-working-directory)
                   (lambda () "/tmp/test-open-interpreter"))
                  ((symbol-function
                    'ai-code-backends-infra--resolve-start-command)
                   (lambda (&rest _args)
                     (list :command "interpreter --model test")))
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
                  ((symbol-function
                    'ai-code-backends-infra--toggle-or-create-session)
                   (lambda (&rest args)
                     (cl-destructuring-bind
                         (_working-dir _buffer-name _process-table command
                                       &optional _escape-fn cleanup-fn
                                       _instance-name _prefix _force-prompt
                                       _env-vars _multiline-input-sequence
                                       post-start-fn)
                         args
                       (setq captured-command command
                             captured-cleanup-fn cleanup-fn
                             captured-post-start-fn post-start-fn))
                     nil)))
          (ai-code-open-interpreter-cli)
          (should builtins-called)
          (should ensure-called)
          (should (string-match-p
                   "\\`interpreter --model test " captured-command))
          (should (string-match-p
                   "mcp_servers\\.emacs_tools" captured-command))
          (should (string-match-p
                   "mcp/open-interpreter-" captured-command))
          (should (functionp captured-cleanup-fn))
          (should (functionp captured-post-start-fn))
          (funcall captured-post-start-fn session-buffer nil "default")
          (should (string-prefix-p "open-interpreter-" (car registered)))
          (should (equal "/tmp/test-open-interpreter" (nth 1 registered)))
          (should (eq session-buffer (nth 2 registered)))
          (with-current-buffer session-buffer
            (let ((status (ai-code-mcp-agent-buffer-status)))
              (should (eq 'open-interpreter (plist-get status :backend)))
              (should (string-match-p
                       "^http://127\\.0\\.0\\.1:8765/mcp/open-interpreter-"
                       (plist-get status :server-url)))))
          (funcall captured-cleanup-fn)
          (should (equal (car registered) unregistered)))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(provide 'test_ai-code-open-interpreter-cli)

;;; test_ai-code-open-interpreter-cli.el ends here
