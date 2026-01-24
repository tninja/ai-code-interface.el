;;; test_ai-code-prompt-mode.el --- Tests for ai-code-prompt-mode -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-prompt-mode.

;;; Code:

(require 'ert)
(require 'ai-code-prompt-mode)
(require 'magit)
(require 'cl-lib)

;; Helper macro to set up and tear down the test environment
(defmacro ai-code-with-test-repo (&rest body)
  "Set up a temporary git repository environment for testing.
This macro creates a temporary directory structure, mocks `magit-toplevel`,
and ensures everything is cleaned up afterward."
  `(let* ((git-root (expand-file-name "test-repo/" temporary-file-directory))
          (mock-file-in-repo (expand-file-name "src/main.js" git-root))
          (outside-file (expand-file-name "other-file.txt" temporary-file-directory)))
     (unwind-protect
         (progn
           ;; Setup: Create dummy files and directories
           (make-directory (file-name-directory mock-file-in-repo) t)
           (with-temp-file mock-file-in-repo (insert "content"))
           (with-temp-file outside-file (insert "content"))
           ;; Execute test body with mocks
           (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
             ,@body))
       ;; Teardown: Clean up dummy files and directories
       (when (file-exists-p mock-file-in-repo) (delete-file mock-file-in-repo))
       (when (file-exists-p outside-file) (delete-file outside-file))
       (when (file-directory-p (file-name-directory mock-file-in-repo))
         (delete-directory (file-name-directory mock-file-in-repo)))
       (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-preprocess-path-in-repo ()
  "Test that a file path inside the git repo is made relative with an @-prefix."
  (ai-code-with-test-repo
   (let ((prompt (format "check file %s" mock-file-in-repo)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      "check file @src/main.js")))))

(ert-deftest ai-code-test-preprocess-path-outside-repo ()
  "Test that a file path outside the git repo remains unchanged."
  (ai-code-with-test-repo
   (let ((prompt (format "check file %s" outside-file)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest ai-code-test-preprocess-non-existent-path ()
  "Test that a non-existent file path remains unchanged."
  (ai-code-with-test-repo
   (let ((prompt "check file /tmp/non-existent-file.txt"))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest ai-code-test-preprocess-prompt-without-path ()
  "Test that a prompt with no file paths remains unchanged."
  (ai-code-with-test-repo
   (let ((prompt "this is a simple prompt"))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest ai-code-test-preprocess-multiple-paths ()
  "Test a prompt with multiple file paths (inside and outside the repo)."
  (ai-code-with-test-repo
   (let ((prompt (format "compare %s and %s" mock-file-in-repo outside-file)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      (format "compare @src/main.js and %s" outside-file))))))

(ert-deftest ai-code-test-preprocess-not-in-git-repo ()
  "Test that paths are not modified when not in a git repository."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) nil)))
    (let ((prompt "check file /some/file.txt"))
      (should (string= (ai-code--preprocess-prompt-text prompt)
                       prompt)))))

;;; Tests for task file functions

(ert-deftest ai-code-test-get-files-directory-in-git-repo ()
  "Test that ai-code--get-files-directory returns .ai.code.files/ in git repo."
  (ai-code-with-test-repo
   (let ((expected-dir (expand-file-name ".ai.code.files" git-root)))
     (should (string= (ai-code--get-files-directory) expected-dir)))))

(ert-deftest ai-code-test-get-files-directory-not-in-git-repo ()
  "Test that ai-code--get-files-directory returns default-directory when not in git repo."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) nil)))
    (let ((default-directory "/tmp/test-dir/"))
      (should (string= (ai-code--get-files-directory) default-directory)))))

(ert-deftest ai-code-test-ensure-files-directory-creates-directory ()
  "Test that ai-code--ensure-files-directory creates the directory if it doesn't exist."
  (ai-code-with-test-repo
   (let ((files-dir (expand-file-name ".ai.code.files" git-root)))
     ;; Ensure directory doesn't exist initially
     (when (file-directory-p files-dir)
       (delete-directory files-dir t))
     ;; Call function
     (let ((result (ai-code--ensure-files-directory)))
       ;; Check directory was created
       (should (file-directory-p files-dir))
       ;; Check return value
       (should (string= result files-dir)))
     ;; Cleanup
     (when (file-directory-p files-dir)
       (delete-directory files-dir t)))))

(ert-deftest ai-code-test-ensure-files-directory-returns-existing ()
  "Test that ai-code--ensure-files-directory returns path of existing directory."
  (ai-code-with-test-repo
   (let ((files-dir (expand-file-name ".ai.code.files" git-root)))
     ;; Create directory first
     (make-directory files-dir t)
     ;; Call function
     (let ((result (ai-code--ensure-files-directory)))
       ;; Check return value
       (should (string= result files-dir))
       ;; Check directory still exists
       (should (file-directory-p files-dir)))
     ;; Cleanup
     (when (file-directory-p files-dir)
       (delete-directory files-dir t)))))

(ert-deftest ai-code-test-generate-task-filename-without-gptel ()
  "Test filename generation without gptel (basic cleanup)."
  (let ((ai-code-task-use-gptel-filename nil))
    ;; Test basic task name
    (let ((filename (ai-code--generate-task-filename "Fix Login Bug")))
      (should (string-match-p "^task_[0-9]\\{8\\}_fix_login_bug\\.org$" filename)))
    
    ;; Test special characters are cleaned
    (let ((filename (ai-code--generate-task-filename "Add@Feature#123!")))
      (should (string-match-p "^task_[0-9]\\{8\\}_add_feature_123\\.org$" filename)))
    
    ;; Test multiple underscores are collapsed
    (let ((filename (ai-code--generate-task-filename "Test   Multiple   Spaces")))
      (should (string-match-p "^task_[0-9]\\{8\\}_test_multiple_spaces\\.org$" filename)))
    
    ;; Test leading/trailing underscores are removed
    (let ((filename (ai-code--generate-task-filename "  Trim Spaces  ")))
      (should (string-match-p "^task_[0-9]\\{8\\}_trim_spaces\\.org$" filename)))))

(ert-deftest ai-code-test-generate-task-filename-with-gptel ()
  "Test filename generation with gptel (mocked)."
  (let ((ai-code-task-use-gptel-filename t))
    ;; Mock ai-code-call-gptel-sync to return a predictable value
    (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
               (lambda (question) "implement_user_authentication")))
      (let ((filename (ai-code--generate-task-filename "Add user login feature")))
        (should (string-match-p "^task_[0-9]\\{8\\}_implement_user_authentication\\.org$" filename))))
    
    ;; Test that gptel errors fall back to basic cleanup
    (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
               (lambda (question) (error "GPTel not available"))))
      (let ((filename (ai-code--generate-task-filename "Fix Bug")))
        (should (string-match-p "^task_[0-9]\\{8\\}_fix_bug\\.org$" filename))))))

(ert-deftest ai-code-test-generate-task-filename-with-rdar ()
  "Test filename generation with rdar:// ID."
  (let ((ai-code-task-use-gptel-filename nil))
    ;; Test rdar:// ID is extracted and used as prefix
    (let ((filename (ai-code--generate-task-filename "rdar://12345678 Fix crash on startup")))
      (should (string-match-p "^rdar_12345678_rdar_12345678_fix_crash_on_startup\\.org$" filename)))
    
    ;; Test rdar:// in middle of task name
    (let ((filename (ai-code--generate-task-filename "Fix crash rdar://99999 in login")))
      (should (string-match-p "^rdar_99999_fix_crash_rdar_99999_in_login\\.org$" filename)))))

(ert-deftest ai-code-test-generate-task-filename-org-extension ()
  "Test that .org extension is always added."
  (let ((ai-code-task-use-gptel-filename nil))
    ;; Test normal case
    (let ((filename (ai-code--generate-task-filename "Test Task")))
      (should (string-suffix-p ".org" filename)))
    
    ;; Test with rdar://
    (let ((filename (ai-code--generate-task-filename "rdar://12345 Test")))
      (should (string-suffix-p ".org" filename)))))

(ert-deftest ai-code-test-generate-task-filename-length-truncation ()
  "Test that filename is truncated to reasonable length."
  (let ((ai-code-task-use-gptel-filename nil)
        (long-name (make-string 100 ?a)))
    (let ((filename (ai-code--generate-task-filename long-name)))
      ;; Extract the generated part (after prefix and before .org)
      (string-match "^task_[0-9]\\{8\\}_\\(.*\\)\\.org$" filename)
      (let ((generated-part (match-string 1 filename)))
        ;; Should be truncated to 60 chars
        (should (<= (length generated-part) 60))))))

(ert-deftest ai-code-test-create-or-open-task-file-open-directory ()
  "Test that ai-code-create-or-open-task-file opens directory when task name is empty."
  (ai-code-with-test-repo
   (let ((files-dir (expand-file-name ".ai.code.files" git-root))
         (dired-called nil)
         (dired-dir nil))
     ;; Mock read-string to return empty string
     (cl-letf (((symbol-function 'read-string)
                (lambda (prompt &optional initial-input) ""))
               ((symbol-function 'dired-other-window)
                (lambda (dirname)
                  (setq dired-called t)
                  (setq dired-dir dirname)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call function
       (ai-code-create-or-open-task-file)
       ;; Check that dired was called
       (should dired-called)
       ;; Check that directory was created and passed to dired
       (should (string= dired-dir files-dir))
       (should (file-directory-p files-dir)))
     ;; Cleanup
     (when (file-directory-p files-dir)
       (delete-directory files-dir t)))))

(ert-deftest ai-code-test-create-or-open-task-file-create-new ()
  "Test that ai-code-create-or-open-task-file creates new task file with metadata."
  (ai-code-with-test-repo
   (let ((files-dir (expand-file-name ".ai.code.files" git-root))
         (task-file nil)
         (ai-code-task-use-gptel-filename nil))
     (unwind-protect
         (cl-letf (((symbol-function 'read-string)
                    (lambda (prompt &optional initial-input)
                      (cond
                       ((string-match-p "Task name" prompt) "Test Task")
                       ((string-match-p "URL" prompt) "https://example.com")
                       ((string-match-p "Confirm task filename" prompt) initial-input))))
                   ((symbol-function 'find-file-other-window)
                    (lambda (filename)
                      (setq task-file filename)
                      (set-buffer (get-buffer-create "*test-task-buffer*"))
                      (erase-buffer)))
                   ((symbol-function 'message)
                    (lambda (&rest args) nil)))
           ;; Call function
           (ai-code-create-or-open-task-file)
           ;; Check that task file path was set
           (should task-file)
           (should (string-prefix-p files-dir task-file))
           (should (string-suffix-p ".org" task-file))
           ;; Check buffer content
           (with-current-buffer "*test-task-buffer*"
             (let ((content (buffer-string)))
               (should (string-match-p "#+TITLE: Test Task" content))
               (should (string-match-p "#+DATE: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" content))
               (should (string-match-p "#+URL: https://example.com" content))
               (should (string-match-p "\\* Task Description" content))
               (should (string-match-p "\\* Investigation" content))
               (should (string-match-p "\\* Code Change" content)))))
       ;; Cleanup
       (when (buffer-live-p (get-buffer "*test-task-buffer*"))
         (kill-buffer "*test-task-buffer*"))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-create-or-open-task-file-adds-org-extension ()
  "Test that .org extension is added if missing from confirmed filename."
  (ai-code-with-test-repo
   (let ((files-dir (expand-file-name ".ai.code.files" git-root))
         (task-file nil)
         (ai-code-task-use-gptel-filename nil))
     (unwind-protect
         (cl-letf (((symbol-function 'read-string)
                    (lambda (prompt &optional initial-input)
                      (cond
                       ((string-match-p "Task name" prompt) "Test Task")
                       ((string-match-p "URL" prompt) "")
                       ;; User removes .org extension
                       ((string-match-p "Confirm task filename" prompt) "my_task"))))
                   ((symbol-function 'find-file-other-window)
                    (lambda (filename)
                      (setq task-file filename)
                      (set-buffer (get-buffer-create "*test-task-buffer*"))
                      (erase-buffer)))
                   ((symbol-function 'message)
                    (lambda (&rest args) nil)))
           ;; Call function
           (ai-code-create-or-open-task-file)
           ;; Check that .org extension was added
           (should (string-suffix-p ".org" task-file)))
       ;; Cleanup
       (when (buffer-live-p (get-buffer "*test-task-buffer*"))
         (kill-buffer "*test-task-buffer*"))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(provide 'test-ai-code-prompt-mode)
;;; test_ai-code-prompt-mode.el ends here