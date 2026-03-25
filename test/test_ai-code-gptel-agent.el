;;; test_ai-code-gptel-agent.el --- Tests for ai-code-gptel-agent.el -*- lexical-binding: t; -*-

;; Author: davidwuchn
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-gptel-agent backend bridge.

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))

(require 'ai-code-gptel-agent)
(require 'ai-code-behaviors)

(ert-deftest ai-code-test-gptel-agent-start-forwards-prefix-arg ()
  "Ensure start forwards prefix args to `gptel-agent'."
  (let (called-fn seen-prefix)
    (cl-letf (((symbol-function 'ai-code-gptel-agent--ensure-available)
               (lambda ()))
              ((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq called-fn fn
                       seen-prefix current-prefix-arg))))
      (ai-code-gptel-agent '(4))
      (should (eq called-fn 'gptel-agent))
      (should (equal seen-prefix '(4))))))

(ert-deftest ai-code-test-gptel-agent-switch-uses-existing-buffer ()
  "Ensure switch jumps to existing gptel-agent buffer when available."
  (let* ((project-root default-directory)
         (project-name (file-name-nondirectory
                        (directory-file-name project-root)))
         (buf-name (format "*gptel-agent:%s*" project-name))
         (shell-buffer (get-buffer-create buf-name))
         (switched-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-gptel-agent--ensure-available)
                   (lambda ()))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _args)
                     (setq switched-buffer buffer))))
          (ai-code-gptel-agent-switch-to-buffer nil)
          (should (eq switched-buffer shell-buffer)))
      (when (buffer-live-p shell-buffer)
        (kill-buffer shell-buffer)))))

(ert-deftest ai-code-test-gptel-agent-switch-errors-without-session ()
  "Ensure switch errors when no session exists."
  (cl-letf (((symbol-function 'ai-code-gptel-agent--ensure-available)
             (lambda ()))
            ((symbol-function 'ai-code-gptel-agent--get-buffer)
             (lambda () nil)))
    (should-error (ai-code-gptel-agent-switch-to-buffer nil)
                  :type 'user-error)))

(ert-deftest ai-code-test-gptel-agent-send-command-inserts-and-sends ()
  "Ensure send command inserts prompt and calls gptel-send."
  (let* ((project-root default-directory)
         (project-name (file-name-nondirectory
                        (directory-file-name project-root)))
         (buf-name (format "*gptel-agent:%s*" project-name))
         (shell-buffer (get-buffer-create buf-name))
         (send-called nil)
         (inserted-text nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-gptel-agent--ensure-available)
                   (lambda ()))
                  ((symbol-function 'gptel-send)
                   (lambda (&optional _arg)
                     (setq send-called t)
                     (setq inserted-text (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (point-max))))))
          (ai-code-gptel-agent-send-command "test prompt")
          (should send-called)
          (should (string= inserted-text "test prompt")))
      (when (buffer-live-p shell-buffer)
        (kill-buffer shell-buffer)))))

(ert-deftest ai-code-test-gptel-agent-send-command-errors-without-session ()
  "Ensure send command errors when no session exists."
  (cl-letf (((symbol-function 'ai-code-gptel-agent--ensure-available)
             (lambda ()))
            ((symbol-function 'ai-code-gptel-agent--get-buffer)
             (lambda () nil)))
    (should-error (ai-code-gptel-agent-send-command "hello")
                  :type 'user-error)))

(ert-deftest ai-code-test-gptel-agent-ensure-available-errors-without-package ()
  "Ensure ensure-available errors when gptel-agent is not available."
  (let* ((original-require (symbol-function 'require))
         (result nil))
    (unwind-protect
        (progn
          (fset 'require (lambda (feature &optional _filename noerror)
                           (if (eq feature 'gptel-agent)
                               nil
                             (funcall original-require feature nil noerror))))
          (condition-case err
              (ai-code-gptel-agent--ensure-available)
            (user-error (setq result err))))
      (fset 'require original-require))
    (should result)
    (should (string-match-p "gptel-agent" (error-message-string result)))))

(ert-deftest ai-code-test-gptel-agent-backend-in-backends-list ()
  "Ensure gptel-agent is registered in ai-code-backends."
  (let ((backend-entry (assoc 'gptel-agent ai-code-backends)))
    (should backend-entry)
    (should (string= (plist-get (cdr backend-entry) :label) "GPTel Agent"))
    (should (eq (plist-get (cdr backend-entry) :require) 'ai-code-gptel-agent))
    (should (eq (plist-get (cdr backend-entry) :start) 'ai-code-gptel-agent))
    (should (eq (plist-get (cdr backend-entry) :switch) 'ai-code-gptel-agent-switch-to-buffer))
    (should (eq (plist-get (cdr backend-entry) :send) 'ai-code-gptel-agent-send-command))
    (should (null (plist-get (cdr backend-entry) :resume)))))

(ert-deftest ai-code-test-gptel-agent-session-exists-with-buffer ()
  "Ensure session-exists-p returns non-nil when gptel-agent buffer exists."
  (let* ((project-root default-directory)
          (project-name (file-name-nondirectory
                         (directory-file-name project-root)))
          (buf-name (format "*gptel-agent:%s*" project-name))
          (shell-buffer (get-buffer-create buf-name))
          (old-backend ai-code-selected-backend))
     (unwind-protect
         (cl-letf (((symbol-function 'ai-code--git-root) (lambda () project-root)))
           (setq ai-code-selected-backend 'gptel-agent)
           (should (ai-code--session-exists-p)))
       (setq ai-code-selected-backend old-backend)
       (when (buffer-live-p shell-buffer)
         (kill-buffer shell-buffer)))))

(ert-deftest ai-code-test-gptel-agent-session-not-exists-without-buffer ()
  "Ensure session-exists-p returns nil when no gptel-agent buffer exists."
  (let ((old-backend ai-code-selected-backend))
    (unwind-protect
        (progn
          (setq ai-code-selected-backend 'gptel-agent)
          (should-not (ai-code--session-exists-p)))
      (setq ai-code-selected-backend old-backend))))

(provide 'test_ai-code-gptel-agent)

;;; test_ai-code-gptel-agent.el ends here