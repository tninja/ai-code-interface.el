;;; test_ai-code-git.el --- Tests for ai-code-git.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-git module, specifically testing
;; the .gitignore update logic.

;;; Code:

(require 'ert)
(require 'ai-code-git)
(require 'ai-code-prompt-mode)
(require 'ai-code-discussion)

(ert-deftest test-ai-code-update-git-ignore-no-duplicates ()
  "Test that ai-code-update-git-ignore does not add duplicate entries.
When .gitignore already contains the required entries, they should
not be added again."
  (let* ((temp-dir (make-temp-file "ai-code-test-" t))
         (gitignore-path (expand-file-name ".gitignore" temp-dir))
         (required-entries (list ai-code-prompt-file-name
                                ai-code-notes-file-name
                                ".projectile"
                                "GTAGS"
                                "GRTAGS"
                                "GPATH")))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory temp-dir))
            (shell-command "git init"))
          
          ;; Create .gitignore with entries already present
          (with-temp-file gitignore-path
            (insert "# Existing entries\n")
            (dolist (entry required-entries)
              (insert entry "\n"))
            (insert "# End of file\n"))
          
          ;; Store original content
          (let ((original-content (with-temp-buffer
                                    (insert-file-contents gitignore-path)
                                    (buffer-string))))
            
            ;; Mock magit-toplevel to return temp-dir
            (cl-letf (((symbol-function 'magit-toplevel)
                       (lambda () temp-dir)))
              ;; Call the function
              (ai-code-update-git-ignore))
            
            ;; Read the updated content
            (let ((updated-content (with-temp-buffer
                                     (insert-file-contents gitignore-path)
                                     (buffer-string))))
              ;; Content should be the same (no duplicates added)
              (should (string= original-content updated-content))
              
              ;; Each entry should appear exactly once
              (dolist (entry required-entries)
                (let ((count 0))
                  (with-temp-buffer
                    (insert updated-content)
                    (goto-char (point-min))
                    (while (re-search-forward (concat "^\\s-*" (regexp-quote entry) "\\s-*$") nil t)
                      (setq count (1+ count))))
                  (should (= count 1)))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest test-ai-code-update-git-ignore-adds-missing ()
  "Test that ai-code-update-git-ignore adds missing entries.
When .gitignore is missing some entries, they should be added."
  (let* ((temp-dir (make-temp-file "ai-code-test-" t))
         (gitignore-path (expand-file-name ".gitignore" temp-dir)))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory temp-dir))
            (shell-command "git init"))
          
          ;; Create .gitignore with only some entries
          (with-temp-file gitignore-path
            (insert "# Existing entries\n")
            (insert ".projectile\n")
            (insert "GTAGS\n"))
          
          ;; Mock magit-toplevel to return temp-dir
          (cl-letf (((symbol-function 'magit-toplevel)
                     (lambda () temp-dir)))
            ;; Call the function
            (ai-code-update-git-ignore))
          
          ;; Read the updated content
          (let ((updated-content (with-temp-buffer
                                   (insert-file-contents gitignore-path)
                                   (buffer-string))))
            ;; All required entries should be present
            (should (string-match-p (regexp-quote ai-code-prompt-file-name) updated-content))
            (should (string-match-p (regexp-quote ai-code-notes-file-name) updated-content))
            (should (string-match-p (regexp-quote ".projectile") updated-content))
            (should (string-match-p (regexp-quote "GTAGS") updated-content))
            (should (string-match-p (regexp-quote "GRTAGS") updated-content))
            (should (string-match-p (regexp-quote "GPATH") updated-content))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(provide 'test_ai-code-git)

;;; test_ai-code-git.el ends here
