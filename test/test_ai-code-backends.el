;;; test_ai-code-backends.el --- Tests for ai-code-backends.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-backends.el behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends)

(ert-deftest ai-code-test-open-backend-agent-file-opens-path ()
  "Test that agent file opens from git root using backend config."
  (let* ((temp-dir (make-temp-file "ai-code-agent-" t))
         (backend-key 'test-backend)
         (ai-code-backends `((,backend-key
                              :label "Test Backend"
                              :agent-file "AGENTS.md")))
         (ai-code-selected-backend backend-key)
         (opened-path nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () temp-dir))
                  ((symbol-function 'find-file-other-window)
                   (lambda (path) (setq opened-path path))))
          (ai-code-open-backend-agent-file)
          (should (string= opened-path
                           (expand-file-name "AGENTS.md" temp-dir))))
      (when (and temp-dir (file-directory-p temp-dir))
        (delete-directory temp-dir t))))

(ert-deftest ai-code-test-cli-send-command-nil-errors-noninteractive ()
  "Ensure nil COMMAND errors in noninteractive calls."
  (let ((ai-code--cli-send-fn (lambda (_command)
                                (ert-fail "Should not be called"))))
    (should-error (ai-code-cli-send-command nil)
                  :type 'user-error)))

(ert-deftest ai-code-test-cli-resume-preserves-prefix-arg ()
  "Ensure `current-prefix-arg' reaches backend resume when ARG is nil."
  (let* ((backend-key 'test-backend)
         (ai-code-backends `((,backend-key
                              :label "Test Backend"
                              :start ai-code-test-start
                              :switch ai-code-test-switch
                              :send ai-code-test-send
                              :resume ai-code-test-resume
                              :cli "test")))
         (saved-start ai-code--cli-start-fn)
         (saved-switch ai-code--cli-switch-fn)
         (saved-send ai-code--cli-send-fn)
         (saved-resume ai-code--cli-resume-fn)
         (saved-backend ai-code-selected-backend)
         (saved-cli ai-code-cli)
         (resume-arg nil))
    (cl-letf (((symbol-function 'ai-code-test-start) (lambda (&optional _arg)))
              ((symbol-function 'ai-code-test-switch) (lambda (&optional _arg)))
              ((symbol-function 'ai-code-test-send) (lambda (&optional _arg)))
              ((symbol-function 'ai-code-test-resume)
               (lambda (&optional arg)
                 (interactive "P")
                 (setq resume-arg arg))))
      (unwind-protect
          (progn
            (ai-code--apply-backend backend-key)
            (let ((current-prefix-arg '(4)))
              (ai-code-cli-resume nil))
            (should (equal resume-arg '(4))))
        (setq ai-code--cli-start-fn saved-start
              ai-code--cli-switch-fn saved-switch
              ai-code--cli-send-fn saved-send
              ai-code--cli-resume-fn saved-resume
              ai-code-selected-backend saved-backend
              ai-code-cli saved-cli)))))

(provide 'test_ai-code-backends)

;;; test_ai-code-backends.el ends here
