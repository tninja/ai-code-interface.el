;;; test_ai-code-agent-shell.el --- Tests for ai-code-agent-shell.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-agent-shell backend bridge.

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))

(require 'ai-code-agent-shell)

(ert-deftest ai-code-test-agent-shell-start-forwards-prefix-arg ()
  "Ensure start forwards prefix args to `agent-shell'."
  (let (called-fn seen-prefix)
    (cl-letf (((symbol-function 'ai-code-agent-shell--ensure-available)
               (lambda ()))
              ((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq called-fn fn
                       seen-prefix current-prefix-arg))))
      (ai-code-agent-shell '(4))
      (should (eq called-fn 'agent-shell))
      (should (equal seen-prefix '(4))))))

(ert-deftest ai-code-test-agent-shell-switch-uses-existing-shell-buffer ()
  "Ensure switch jumps to existing shell buffer when available."
  (let ((shell-buffer (get-buffer-create " *ai-code-agent-shell-test*"))
        (switched-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-agent-shell--ensure-available)
                   (lambda ()))
                  ((symbol-function 'agent-shell--shell-buffer)
                   (lambda (&rest _keys) shell-buffer))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _args)
                     (setq switched-buffer buffer))))
          (ai-code-agent-shell-switch-to-buffer nil)
          (should (eq switched-buffer shell-buffer)))
      (when (buffer-live-p shell-buffer)
        (kill-buffer shell-buffer)))))

(ert-deftest ai-code-test-agent-shell-send-command-queues-request ()
  "Ensure send command delegates to `agent-shell-queue-request'."
  (let ((shell-buffer (get-buffer-create " *ai-code-agent-shell-send*"))
        (queued-text nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-agent-shell--ensure-available)
                   (lambda ()))
                  ((symbol-function 'agent-shell--shell-buffer)
                   (lambda (&rest _keys) shell-buffer))
                  ((symbol-function 'agent-shell-queue-request)
                   (lambda (prompt)
                     (setq queued-text prompt))))
          (ai-code-agent-shell-send-command "hello")
          (should (equal queued-text "hello")))
      (when (buffer-live-p shell-buffer)
        (kill-buffer shell-buffer)))))

(ert-deftest ai-code-test-agent-shell-send-command-errors-without-session ()
  "Ensure send command errors when no session exists."
  (cl-letf (((symbol-function 'ai-code-agent-shell--ensure-available)
             (lambda ()))
            ((symbol-function 'agent-shell--shell-buffer)
             (lambda (&rest _keys) nil)))
    (should-error (ai-code-agent-shell-send-command "hello")
                  :type 'user-error)))

(ert-deftest ai-code-test-agent-shell-resume-uses-latest-strategy ()
  "Ensure resume uses latest strategy and forces new shell creation."
  (let (called-fn seen-prefix seen-strategy)
    (cl-letf (((symbol-function 'ai-code-agent-shell--ensure-available)
               (lambda ()))
              ((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq called-fn fn
                       seen-prefix current-prefix-arg
                       seen-strategy agent-shell-session-strategy))))
      (ai-code-agent-shell-resume nil)
      (should (eq called-fn 'agent-shell))
      (should (equal seen-prefix '(4)))
      (should (eq seen-strategy 'latest)))))

(ert-deftest ai-code-test-agent-shell-resume-with-prefix-uses-prompt-strategy ()
  "Ensure prefixed resume prompts for session selection."
  (let (seen-strategy)
    (cl-letf (((symbol-function 'ai-code-agent-shell--ensure-available)
               (lambda ()))
              ((symbol-function 'call-interactively)
               (lambda (&rest _args)
                 (setq seen-strategy agent-shell-session-strategy))))
      (ai-code-agent-shell-resume '(4))
      (should (eq seen-strategy 'prompt)))))

(provide 'test_ai-code-agent-shell)

;;; test_ai-code-agent-shell.el ends here
