;;; test_00-bootstrap.el --- Batch test bootstrap -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Normalize batch test load-path so project tests do not accidentally pick
;; stale compatibility packages over the built-in libraries they expect.

;;; Code:

(require 'cl-lib)
(require 'package)

(setq load-prefer-newer t)

(let ((stubs-dir (expand-file-name "test/stubs" default-directory)))
  (when (file-directory-p stubs-dir)
    (add-to-list 'load-path stubs-dir)))

(defun ai-code-test--prefer-packaged-library (prefix)
  "Put the newest installed package matching PREFIX at the front of `load-path'."
  (let* ((pattern (expand-file-name (format "%s-*" prefix) package-user-dir))
         (candidates (sort (file-expand-wildcards pattern) #'version<))
         (latest (car (last candidates))))
    (when latest
      (add-to-list 'load-path latest))))

(defun ai-code-test--remove-shadowing-compat-libraries ()
  "Drop old ELPA compatibility libraries that shadow built-in packages."
  (setq load-path
        (cl-remove-if
         (lambda (path)
           (string-match-p "/\\(cl-lib\\|flymake\\)-" path))
         load-path)))

(ai-code-test--prefer-packaged-library "transient")
(ai-code-test--remove-shadowing-compat-libraries)

(provide 'test_00-bootstrap)

;;; test_00-bootstrap.el ends here
