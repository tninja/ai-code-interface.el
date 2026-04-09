;;; test_ai-code-onboarding.el --- Tests for ai-code onboarding -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for onboarding quickstart behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(defun ai-code-test-onboarding--maybe-prefer-packaged-transient ()
  "Prefer the newest packaged Transient when one is installed."
  (let* ((pattern (expand-file-name "transient-*" package-user-dir))
         (candidates (sort (file-expand-wildcards pattern) #'version<))
         (latest (car (last candidates))))
    (when latest
      (add-to-list 'load-path latest))))

(ai-code-test-onboarding--maybe-prefer-packaged-transient)

(require 'transient)
(require 'ai-code)

(ert-deftest ai-code-test-onboarding-open-quickstart-renders-core-sections ()
  "Opening quickstart should render the core onboarding sections."
  (let ((rendered-buffer nil))
    (cl-letf (((symbol-function 'ai-code-current-backend-label)
               (lambda () "OpenAI Codex CLI"))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args)
                 (setq rendered-buffer (get-buffer buffer))
                 rendered-buffer)))
      (unwind-protect
          (progn
            (ai-code-onboarding-open-quickstart)
            (should (buffer-live-p rendered-buffer))
            (with-current-buffer rendered-buffer
              (should (string-match-p "OpenAI Codex CLI" (buffer-string)))
              (should (string-match-p "Start Here" (buffer-string)))
              (should (string-match-p "Most Useful Actions" (buffer-string)))
              (should (string-match-p "How Context Works" (buffer-string)))))
        (when (buffer-live-p rendered-buffer)
          (kill-buffer rendered-buffer))))))

(ert-deftest ai-code-test-onboarding-maybe-show-quickstart-persists-seen-state ()
  "Auto-showing quickstart should persist the seen state in interactive sessions."
  (let ((ai-code-onboarding-auto-show t)
        (ai-code-onboarding-seen nil)
        (noninteractive nil)
        saved-variables)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol value)
                 (push (cons symbol value) saved-variables)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args)
                 (get-buffer buffer))))
      (ai-code-onboarding-maybe-show-quickstart)
      (should ai-code-onboarding-seen)
      (should (member '(ai-code-onboarding-seen . t) saved-variables)))))

(ert-deftest ai-code-test-onboarding-current-backend-label-falls-back-on-error ()
  "Backend label helper should degrade gracefully when backend lookup fails."
  (cl-letf (((symbol-function 'ai-code-current-backend-label)
             (lambda ()
               (error "backend lookup failed"))))
    (should (string= "<none>"
                     (ai-code-onboarding--current-backend-label)))))

(ert-deftest ai-code-test-onboarding-disable-auto-show-turns-off-future-auto-display ()
  "Disabling auto-show should persist the opt-out state and close the window."
  (let ((ai-code-onboarding-auto-show t)
        (ai-code-onboarding-seen nil)
        (quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (&rest _args)
                 (setq quit-called t))))
      (ai-code-onboarding-disable-auto-show)
      (should-not ai-code-onboarding-auto-show)
      (should ai-code-onboarding-seen)
      (should quit-called))))

(provide 'test_ai-code-onboarding)

;;; test_ai-code-onboarding.el ends here
