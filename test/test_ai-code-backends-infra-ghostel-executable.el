;;; test_ai-code-backends-infra-ghostel-executable.el --- Ghostel executable tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for Ghostel executable resolution.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra-ghostel)

(ert-deftest test-ai-code-backends-infra-ghostel-resolve-program-finds-bare-executable ()
  "Bare executable names should be resolved before Ghostel starts them."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program)
               (when (string= program "claude")
                 "/opt/homebrew/bin/claude"))))
    (should
     (equal (ai-code-backends-infra-ghostel--resolve-program "claude")
            "/opt/homebrew/bin/claude"))))

(ert-deftest test-ai-code-backends-infra-ghostel-resolve-program-preserves-explicit-path ()
  "Explicit executable paths should not be searched again."
  (let (searched)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program)
                 (setq searched t)
                 nil)))
      (should
       (equal (ai-code-backends-infra-ghostel--resolve-program "./bin/claude")
              "./bin/claude"))
      (should-not searched))))

(ert-deftest test-ai-code-backends-infra-ghostel-start-process-uses-resolved-program ()
  "Ghostel startup should pass the resolved executable to `ghostel-exec'."
  (with-temp-buffer
    (let (program-seen args-seen)
      (cl-letf (((symbol-function 'ai-code-backends-infra--configure-ghostel-buffer)
                 #'ignore)
                ((symbol-function 'ai-code-backends-infra-ghostel--resolve-program)
                 (lambda (program)
                   (should (equal program "claude"))
                   "/usr/local/bin/claude"))
                ((symbol-function 'ghostel-exec)
                 (lambda (_buffer program args)
                   (setq program-seen program
                         args-seen args)
                   nil)))
        (ai-code-backends-infra--start-ghostel-process
         (current-buffer) "claude --dangerously-skip-permissions"))
      (should (equal program-seen "/usr/local/bin/claude"))
      (should (equal args-seen '("--dangerously-skip-permissions"))))))

(provide 'test_ai-code-backends-infra-ghostel-executable)

;;; test_ai-code-backends-infra-ghostel-executable.el ends here
