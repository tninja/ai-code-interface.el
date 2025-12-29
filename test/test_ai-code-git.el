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

(ert-deftest ai-code-test-ai-code-gitignore-regex-pattern ()
  "Test that the regex pattern correctly matches entries in .gitignore.
This is a unit test for the regex pattern used in ai-code-update-git-ignore."
  (let ((gitignore-content "# Test .gitignore file
.ai.code.prompt.org
.ai.code.notes.org
.projectile
GTAGS
GRTAGS
GPATH
# End of file
"))
    ;; Test that existing entries are found
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".ai.code.prompt.org")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".ai.code.notes.org")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".projectile")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GTAGS")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GRTAGS")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GPATH")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    
    ;; Test that a missing entry is not found
    (should-not (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                       (regexp-quote "MISSING_ENTRY")
                                       "\\s-*\\(?:\n\\|$\\)")
                               gitignore-content))
    
    ;; Test entries with whitespace
    (let ((gitignore-with-whitespace "  .projectile  
GTAGS
"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".projectile")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-with-whitespace))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote "GTAGS")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-with-whitespace)))
    
    ;; Test entry at beginning of file (no leading newline)
    (let ((gitignore-start ".ai.code.prompt.org
other-file"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".ai.code.prompt.org")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-start)))
    
    ;; Test entry at end of file (no trailing newline)
    (let ((gitignore-end "other-file
.ai.code.prompt.org"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".ai.code.prompt.org")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-end)))))


(ert-deftest ai-code-test-ai-code-update-git-ignore-no-duplicates ()
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

(ert-deftest ai-code-test-ai-code-update-git-ignore-adds-missing ()
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
