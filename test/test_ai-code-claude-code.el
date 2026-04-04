;;; test_ai-code-claude-code.el --- Tests for ai-code-claude-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-claude-code module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-claude-code)

(ert-deftest ai-code-test-claude-code-start-passes-multiline-sequence ()
  "Starting Claude Code should pass the configured multiline sequence to infra."
  (let ((captured-sequence :unset))
    (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test-claude"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args)
                 (list :command "claude")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest args)
                 (cl-destructuring-bind
                     (_working-dir _buffer-name _process-table _command
                                   &optional _escape-fn _cleanup-fn
                                   _instance-name _prefix _force-prompt
                                   _env-vars multiline-input-sequence
                                   _post-start-fn)
                     args
                   (setq captured-sequence multiline-input-sequence))
                 nil)))
      (let ((ai-code-claude-code-multiline-input-sequence "\e\r"))
        (ai-code-claude-code)
        (should (equal captured-sequence "\e\r"))))))

(ert-deftest ai-code-test-claude-code-start-passes-no-flicker-env ()
  "Starting Claude Code should pass CLAUDE_CODE_NO_FLICKER env var based on config."
  (let ((captured-env-vars :unset))
    (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test-claude"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args)
                 (list :command "claude")))
              ((symbol-function 'ai-code-mcp-agent-prepare-launch)
               (lambda (&rest _args) nil))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest args)
                 (let ((env-vars (nth 9 args)))
                   (setq captured-env-vars env-vars))
                 nil)))
      (let ((ai-code-claude-code-no-flicker t))
        (ai-code-claude-code)
        (should (member "CLAUDE_CODE_NO_FLICKER=1" captured-env-vars)))
      (let ((ai-code-claude-code-no-flicker nil))
        (ai-code-claude-code)
        (should (member "CLAUDE_CODE_NO_FLICKER=0" captured-env-vars))))))

(provide 'test_ai-code-claude-code)

;;; test_ai-code-claude-code.el ends here
