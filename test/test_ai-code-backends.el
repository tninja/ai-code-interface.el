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

(provide 'test_ai-code-backends)

;;; test_ai-code-backends.el ends here
