;;; test_ai-code-annotation.el --- Annotation action unit tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional dependency and menu tests, without requiring annotate.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-annotation)

(ert-deftest ai-code-annotation-missing-package-explains-installation ()
  "Missing annotate.el must stop before any AI handoff."
  (let ((original (symbol-function 'require)) sent)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest args)
                 (if (eq feature 'annotate) nil (apply original feature args))))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (&rest _) (setq sent t))))
      (should (string-match-p "Install annotate.el"
                              (error-message-string
                               (should-error (ai-code-address-code-annotation)
                                             :type 'user-error))))
      (should-not sent))))

(ert-deftest ai-code-annotation-context-menu-exposes-action ()
  "The shared context action group exposes the new entry."
  (with-temp-buffer
    (insert-file-contents "ai-code.el")
    (goto-char (point-min))
    (re-search-forward "^(transient-define-group ai-code--menu-actions-with-context")
    (goto-char (match-beginning 0))
    (should (member '("m" "Address code annotation" ai-code-address-code-annotation)
                    (read (current-buffer))))))

(provide 'test_ai-code-annotation)
;;; test_ai-code-annotation.el ends here
