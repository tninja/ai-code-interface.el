;;; test_ai-code-agile.el --- Tests for ai-code-agile.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-agile.el, focusing on TDD workflow helper functions.

;;; Code:

(require 'ert)
(require 'ai-code-agile)

;;; Tests for ai-code--tdd-source-function-context-p

(ert-deftest ai-code-test-tdd-source-function-context-p-source-file ()
  "Return non-nil for a non-test source file in prog-mode with a function name."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (should (ai-code--tdd-source-function-context-p "my-function"))))

(ert-deftest ai-code-test-tdd-source-function-context-p-test-file ()
  "Return nil when buffer-file-name contains \"test\"."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/test/test_my-module.el")
    (should-not (ai-code--tdd-source-function-context-p "my-function"))))

(ert-deftest ai-code-test-tdd-source-function-context-p-no-file ()
  "Return nil when buffer has no associated file."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name nil)
    (should-not (ai-code--tdd-source-function-context-p "my-function"))))

(ert-deftest ai-code-test-tdd-source-function-context-p-nil-function ()
  "Return nil when function-name is nil."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (should-not (ai-code--tdd-source-function-context-p nil))))

(ert-deftest ai-code-test-tdd-source-function-context-p-non-prog-mode ()
  "Return nil when buffer is not in a prog-mode derived mode."
  (with-temp-buffer
    (text-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (should-not (ai-code--tdd-source-function-context-p "my-function"))))

;;; Tests for ai-code--write-test

(ert-deftest ai-code-test-write-test-prompt-includes-function-name ()
  "Verify prompt passed to ai-code--insert-prompt contains the function name."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "my-function" captured-prompt))))))

(ert-deftest ai-code-test-write-test-prompt-includes-source-file-hint ()
  "Verify prompt includes a hint about the source file when buffer has a file name."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "my-module.el" captured-prompt))))))

(ert-deftest ai-code-test-write-test-prompt-no-file-fallback ()
  "Verify fallback hint when buffer has no file name."
  (with-temp-buffer
    (setq-local buffer-file-name nil)
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "corresponding test file" captured-prompt))))))

(ert-deftest ai-code-test-write-test-prompt-includes-tdd-instruction ()
  "Verify TDD instruction appears in the final prompt."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "TDD" captured-prompt))))))

(provide 'test_ai-code-agile)
;;; test_ai-code-agile.el ends here
