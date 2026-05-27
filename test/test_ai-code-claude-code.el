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

(defvar ghostel-full-redraw)

(ert-deftest ai-code-test-claude-code-start-uses-generic-helper ()
  "Claude Code startup should delegate session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test-claude"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "claude --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-claude-code-program "claude-test")
            (ai-code-claude-code-program-switches '("--debug"))
            (ai-code-claude-code-no-flicker t)
            (ai-code-claude-code-multiline-input-sequence "\e\r"))
        (ai-code-claude-code 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "claude-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Claude Code"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-claude-code--processes))
    (should (equal (plist-get captured-options :session-prefix) "claude"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-claude-code-send-escape))
    (should (equal (plist-get captured-options :env-vars)
                   '("TERM_PROGRAM=emacs"
                     "FORCE_CODE_TERMINAL=true"
                     "CLAUDE_CODE_NO_FLICKER=1")))
    (should (equal (plist-get captured-options :multiline-input-sequence) "\e\r"))
    (should (functionp (plist-get captured-options :prepare-launch)))))

(ert-deftest ai-code-test-claude-code-prepare-launch-preserves-rendering-hooks ()
  "Claude Code launch preparation should preserve MCP and rendering hooks."
  (let (captured-options
        mcp-post-start-called
        cleanup-called
        (session-buffer (generate-new-buffer " *ai-code-claude-rendering*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
                     (lambda (options _arg)
                       (setq captured-options options)))
                    ((symbol-function 'ai-code-backends-infra--session-working-directory)
                     (lambda () "/tmp/test-claude"))
                    ((symbol-function 'ai-code-backends-infra--resolve-start-command)
                     (lambda (&rest _args) '(:command "claude")))
                    ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-mcp-agent-prepare-launch)
                     (lambda (backend working-dir command)
                       (should (eq backend 'claude-code))
                       (should (equal working-dir "/tmp/test-claude"))
                       (should (equal command "claude"))
                       (list :command "claude --mcp"
                             :cleanup-fn (lambda () (setq cleanup-called t))
                             :post-start-fn
                             (lambda (buffer process instance-name)
                               (setq mcp-post-start-called
                                     (list buffer process instance-name)))))))
            (ai-code-claude-code)
          (let* ((launch (funcall (plist-get captured-options :prepare-launch)
                                  "/tmp/test-claude"
                                  "claude"))
                 (post-start-fn (plist-get launch :post-start-fn))
                 (cleanup-fn (plist-get launch :cleanup-fn)))
            (should (equal (plist-get launch :command) "claude --mcp"))
            (should (functionp cleanup-fn))
            (should (functionp post-start-fn))
            (let ((ai-code-backends-infra-terminal-backend 'vterm))
              (funcall post-start-fn session-buffer 'process 'default)
              (with-current-buffer session-buffer
                (should ai-code-backends-infra-strip-alternate-screen)))
            (let ((ai-code-backends-infra-terminal-backend 'ghostel))
              (funcall post-start-fn session-buffer 'process 'ghost)
              (with-current-buffer session-buffer
                (should-not ai-code-backends-infra-strip-alternate-screen)
                (should ghostel-full-redraw)))
            (should (equal mcp-post-start-called
                           (list session-buffer 'process 'ghost)))
            (funcall cleanup-fn)
            (should cleanup-called))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest ai-code-test-claude-code-start-passes-multiline-sequence ()
  "Starting Claude Code should pass the configured multiline sequence to infra."
  (let ((captured-sequence :unset))
    (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test-claude"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args)
                 (list :command "claude")))
              ((symbol-function 'ai-code-mcp-agent-prepare-launch)
               (lambda (&rest _args) nil))
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
        (should-not (member "CLAUDE_CODE_NO_FLICKER=0" captured-env-vars))
        (should-not (member "CLAUDE_CODE_NO_FLICKER=1" captured-env-vars))))))

(provide 'test_ai-code-claude-code)

;;; test_ai-code-claude-code.el ends here
