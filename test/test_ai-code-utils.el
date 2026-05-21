;;; test_ai-code-utils.el --- Tests for ai-code-utils -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-utils.el shared utility functions.

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil))

(require 'ai-code-utils)

;;; --- Path utilities ---

(ert-deftest test-ai-code-utils-git-root-returns-nil-outside-repo ()
  "git-root should return nil when not in a git repository."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) nil)))
    (should-not (ai-code--git-root))))

(ert-deftest test-ai-code-utils-git-root-returns-truename ()
  "git-root should apply file-truename to magit-toplevel result."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) "/tmp/repo/")))
    (should (stringp (ai-code--git-root)))))

(ert-deftest test-ai-code-utils-git-root-handles-errors ()
  "git-root should return nil when magit-toplevel errors."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) (error "Not a git repo"))))
    (should-not (ai-code--git-root))))

(ert-deftest test-ai-code-utils-project-root-prefers-projectile ()
  "project-root should prefer Projectile when available."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () "/projectile/root/"))
            ((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) "/git/root/")))
    (should (equal (ai-code--project-root) "/projectile/root/"))))

(ert-deftest test-ai-code-utils-project-root-falls-back-to-git ()
  "project-root should fall back to git root when Projectile unavailable."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () (error "No project")))
            ((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) "/git/root/")))
    (should (equal (ai-code--project-root)
                   (file-truename "/git/root/")))))

(ert-deftest test-ai-code-utils-session-project-root-cascade ()
  "session-project-root should cascade: project.el -> git -> default-directory."
  (let ((default-directory "/tmp/fallback/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt _dir) nil))
              ((symbol-function 'magit-toplevel)
               (lambda (&optional _dir) nil)))
      (should (equal (ai-code--session-project-root) "/tmp/fallback/")))))

(ert-deftest test-ai-code-utils-files-dir-name-is-string ()
  "ai-code-files-dir-name should be a non-empty string constant."
  (should (stringp ai-code-files-dir-name))
  (should (not (string-empty-p ai-code-files-dir-name))))

(ert-deftest test-ai-code-utils-get-files-directory-under-git-root ()
  "get-files-directory should return files dir under git root."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) "/repo/")))
    (should (string-match-p "\\.ai\\.code\\.files"
                            (ai-code--get-files-directory)))))

(ert-deftest test-ai-code-utils-get-files-directory-fallback ()
  "get-files-directory should fall back to default-directory outside git."
  (let ((default-directory "/tmp/no-git/"))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda (&optional _dir) nil)))
      (should (equal (ai-code--get-files-directory) "/tmp/no-git/")))))

(ert-deftest test-ai-code-utils-ensure-files-directory-creates-dir ()
  "ensure-files-directory should create the directory if needed."
  (let* ((tmp-root (file-truename (make-temp-file "ai-code-utils-test-" t)))
         (expected-dir (expand-file-name ".ai.code.files" tmp-root)))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-toplevel)
                   (lambda (&optional _dir) tmp-root)))
          (should-not (file-directory-p expected-dir))
          (let ((result (ai-code--ensure-files-directory)))
            (should (file-directory-p expected-dir))
            (should (equal result expected-dir))))
      (ignore-errors (delete-directory tmp-root t)))))

;;; --- Text utilities ---

(ert-deftest test-ai-code-utils-get-clipboard-text-returns-string-or-nil ()
  "get-clipboard-text should return a plain string or nil."
  (cl-letf (((symbol-function 'gui-get-selection)
             (lambda (&rest _args) nil))
            ((symbol-function 'current-kill)
             (lambda (&rest _args) "kill ring text")))
    (let ((result (ai-code--get-clipboard-text)))
      (should (stringp result))
      (should (equal result "kill ring text")))))

(ert-deftest test-ai-code-utils-get-clipboard-text-strips-properties ()
  "get-clipboard-text should strip text properties."
  (let ((propertied (propertize "hello" 'face 'bold)))
    (cl-letf (((symbol-function 'gui-get-selection)
               (lambda (&rest _args) nil))
              ((symbol-function 'current-kill)
               (lambda (&rest _args) propertied)))
      (let ((result (ai-code--get-clipboard-text)))
        (should (equal result "hello"))
        (should-not (text-properties-at 0 result))))))

(ert-deftest test-ai-code-utils-get-clipboard-text-nil-when-empty ()
  "get-clipboard-text should return nil when clipboard and kill ring are empty."
  (cl-letf (((symbol-function 'gui-get-selection)
             (lambda (&rest _args) nil))
            ((symbol-function 'current-kill)
             (lambda (&rest _args) (error "Kill ring empty"))))
    (should-not (ai-code--get-clipboard-text))))

(ert-deftest test-ai-code-utils-get-window-files-returns-list ()
  "get-window-files should return a list of file paths from visible windows."
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/test.el")
    (cl-letf (((symbol-function 'window-list)
               (lambda () (list (selected-window))))
              ((symbol-function 'window-buffer)
               (lambda (_w) (current-buffer))))
      (let ((result (ai-code--get-window-files)))
        (should (listp result))
        (should (member "/tmp/test.el" result))))))

(ert-deftest test-ai-code-utils-get-context-files-string-with-file ()
  "get-context-files-string should include current buffer file."
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/main.el")
    (cl-letf (((symbol-function 'ai-code--get-window-files)
               (lambda () '("/tmp/main.el" "/tmp/other.el"))))
      (let ((result (ai-code--get-context-files-string)))
        (should (string-match-p "/tmp/main.el" result))
        (should (string-match-p "/tmp/other.el" result))))))

(ert-deftest test-ai-code-utils-get-context-files-string-empty-without-file ()
  "get-context-files-string should return empty string without buffer-file-name."
  (with-temp-buffer
    (should (equal (ai-code--get-context-files-string) ""))))

(ert-deftest test-ai-code-utils-format-repo-context-info-nil-when-empty ()
  "format-repo-context-info should return nil when no context stored."
  (let ((ai-code--repo-context-info (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda (&optional _dir) "/repo/")))
      (should-not (ai-code--format-repo-context-info)))))

(ert-deftest test-ai-code-utils-format-repo-context-info-with-entries ()
  "format-repo-context-info should format stored context entries."
  (let ((ai-code--repo-context-info (make-hash-table :test #'equal)))
    (puthash "/repo/root/" '("entry2" "entry1")
             ai-code--repo-context-info)
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/repo/root/")))
      (let ((result (ai-code--format-repo-context-info)))
        (should (stringp result))
        (should (string-match-p "entry1" result))
        (should (string-match-p "entry2" result))))))

(provide 'test_ai-code-utils)

;;; test_ai-code-utils.el ends here
