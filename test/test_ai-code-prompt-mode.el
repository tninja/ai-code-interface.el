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


(ert-deftest ai-code-test-setup-snippets-finds-directory ()
  "Test that ai-code--setup-snippets can locate the snippets directory."
  ;; This test verifies that locate-library can find the correct library
  ;; and that the snippets directory path is constructed correctly
  (let ((lib-path (locate-library "ai-code")))
    ;; Library should be found
    (should lib-path)
    ;; Construct expected snippet directory path
    (let ((snippet-dir (expand-file-name "snippets"
                                         (file-name-directory (file-truename lib-path)))))
      ;; Snippet directory should exist
      (should (file-directory-p snippet-dir))
      ;; Snippet directory should contain the ai-code-prompt-mode subdirectory
      (let ((ai-code-prompt-mode-dir (expand-file-name "ai-code-prompt-mode" snippet-dir)))
        (should (file-directory-p ai-code-prompt-mode-dir))))))

(ert-deftest ai-code-test-auto-mode-alist-pattern ()
  "Test that auto-mode-alist correctly matches .ai.code.prompt.org file."
  (let* ((test-file-name ".ai.code.prompt.org")
         (test-path (concat "/some/path/" test-file-name))
         ;; Find the first matching entry in auto-mode-alist
         (matched-mode (cl-some (lambda (entry)
                                  (when (string-match (car entry) test-path)
                                    (cdr entry)))
                                auto-mode-alist)))
    ;; Verify that the matched mode is ai-code-prompt-mode
    (should (eq matched-mode 'ai-code-prompt-mode))))

;;; Tests for filepath completion functionality

(ert-deftest ai-code-test-dedupe-preserve-order ()
  "Test that ai-code--dedupe-preserve-order removes duplicates while preserving order."
  ;; Test basic deduplication
  (let ((items '("a" "b" "c" "b" "a" "d")))
    (should (equal (ai-code--dedupe-preserve-order items)
                   '("a" "b" "c" "d"))))
  
  ;; Test empty list
  (should (equal (ai-code--dedupe-preserve-order '()) '()))
  
  ;; Test list with no duplicates
  (let ((items '("x" "y" "z")))
    (should (equal (ai-code--dedupe-preserve-order items)
                   '("x" "y" "z"))))
  
  ;; Test all duplicates
  (let ((items '("same" "same" "same")))
    (should (equal (ai-code--dedupe-preserve-order items)
                   '("same")))))

(ert-deftest ai-code-test-file-in-git-repo-p ()
  "Test that ai-code--file-in-git-repo-p correctly identifies files in git repo."
  (ai-code-with-test-repo
   ;; File in repo should return non-nil
   (should (ai-code--file-in-git-repo-p mock-file-in-repo git-root))
   
   ;; File outside repo should return nil
   (should-not (ai-code--file-in-git-repo-p outside-file git-root))
   
   ;; Non-existent file should return nil
   (should-not (ai-code--file-in-git-repo-p "/tmp/non-existent-file.txt" git-root))
   
   ;; nil file should return nil
   (should-not (ai-code--file-in-git-repo-p nil git-root))))

(ert-deftest ai-code-test-relative-filepath ()
  "Test that ai-code--relative-filepath returns correct relative paths with @ prefix."
  (ai-code-with-test-repo
   ;; Test file in subdirectory
   (let ((result (ai-code--relative-filepath mock-file-in-repo git-root)))
     (should (string= result "@src/main.js")))))

(ert-deftest ai-code-test-visible-window-files ()
  "Test that ai-code--visible-window-files returns files from visible windows."
  (ai-code-with-test-repo
   (let ((test-file-1 (expand-file-name "file1.el" git-root))
         (test-file-2 (expand-file-name "file2.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test files
           (with-temp-file test-file-1 (insert "content1"))
           (with-temp-file test-file-2 (insert "content2"))
           
           ;; Mock ai-code--git-ignored-repo-file-p to return nil for all files
           (cl-letf (((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (file root) nil)))
             
             ;; Open files in buffers but don't display them
             (with-current-buffer (find-file-noselect test-file-1)
               (with-current-buffer (find-file-noselect test-file-2)
                 ;; Mock window-list to return windows with these buffers
                 (cl-letf (((symbol-function 'window-list)
                            (lambda (&optional frame no-minibuf)
                              (list (selected-window))))
                           ((symbol-function 'window-buffer)
                            (lambda (win)
                              (if (eq win (selected-window))
                                  (get-buffer (file-name-nondirectory test-file-1))
                                (get-buffer (file-name-nondirectory test-file-2))))))
                   (let ((result (ai-code--visible-window-files git-root)))
                     ;; Should contain the file from the selected window
                     (should (member test-file-1 result))))))))
       
       ;; Cleanup
       (when (file-exists-p test-file-1) (delete-file test-file-1))
       (when (file-exists-p test-file-2) (delete-file test-file-2))))))

(ert-deftest ai-code-test-buffer-file-list ()
  "Test that ai-code--buffer-file-list returns buffer files excluding skip-files."
  (ai-code-with-test-repo
   (let ((test-file-1 (expand-file-name "buf1.el" git-root))
         (test-file-2 (expand-file-name "buf2.el" git-root))
         (test-file-3 (expand-file-name "buf3.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test files
           (with-temp-file test-file-1 (insert "content1"))
           (with-temp-file test-file-2 (insert "content2"))
           (with-temp-file test-file-3 (insert "content3"))
           
           ;; Mock ai-code--git-ignored-repo-file-p
           (cl-letf (((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (file root) nil)))
             
             ;; Open files in buffers
             (let ((buf1 (find-file-noselect test-file-1))
                   (buf2 (find-file-noselect test-file-2))
                   (buf3 (find-file-noselect test-file-3)))
               (unwind-protect
                   (progn
                     ;; Test without skip-files
                     (let ((result (ai-code--buffer-file-list git-root)))
                       (should (member test-file-1 result))
                       (should (member test-file-2 result))
                       (should (member test-file-3 result)))
                     
                     ;; Test with skip-files
                     (let ((result (ai-code--buffer-file-list 
                                   git-root 
                                   (list (file-truename test-file-1)))))
                       (should-not (member test-file-1 result))
                       (should (member test-file-2 result))
                       (should (member test-file-3 result))))
                 
                 ;; Kill buffers
                 (when (buffer-live-p buf1) (kill-buffer buf1))
                 (when (buffer-live-p buf2) (kill-buffer buf2))
                 (when (buffer-live-p buf3) (kill-buffer buf3))))))
       
       ;; Cleanup files
       (when (file-exists-p test-file-1) (delete-file test-file-1))
       (when (file-exists-p test-file-2) (delete-file test-file-2))
       (when (file-exists-p test-file-3) (delete-file test-file-3))))))

(ert-deftest ai-code-test-prompt-filepath-candidates-excludes-current-file ()
  "Test that ai-code--prompt-filepath-candidates excludes the current file."
  (ai-code-with-test-repo
   (let ((test-file (expand-file-name "current.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test file
           (with-temp-file test-file (insert "content"))
           
           ;; Mock dependencies
           (cl-letf (((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (file root) nil))
                     ((symbol-function 'ai-code--repo-recent-files)
                      (lambda (root) (list test-file))))
             
             ;; Test with current buffer being the test file
             (with-current-buffer (find-file-noselect test-file)
               (unwind-protect
                   (let ((candidates (ai-code--prompt-filepath-candidates)))
                     ;; Current file should be excluded
                     (should-not (member "@current.el" candidates)))
                 (kill-buffer)))))
       
       ;; Cleanup
       (when (file-exists-p test-file) (delete-file test-file))))))

(ert-deftest ai-code-test-prompt-filepath-candidates-excludes-ai-code-files ()
  "Test that ai-code--prompt-filepath-candidates excludes files under .ai.code.files."
  (ai-code-with-test-repo
   (let ((ai-files-dir (expand-file-name ".ai.code.files" git-root))
         (task-file (expand-file-name ".ai.code.files/task.org" git-root))
         (normal-file (expand-file-name "normal.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test files
           (make-directory ai-files-dir t)
           (with-temp-file task-file (insert "task"))
           (with-temp-file normal-file (insert "normal"))
           
           ;; Mock dependencies
           (cl-letf (((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (file root) nil))
                     ((symbol-function 'ai-code--repo-recent-files)
                      (lambda (root) (list task-file normal-file))))
             
             (let ((candidates (ai-code--prompt-filepath-candidates)))
               ;; Task file should be excluded
               (should-not (cl-some (lambda (c) (string-prefix-p "@.ai.code.files/" c))
                                    candidates))
               ;; Normal file should be included
               (should (member "@normal.el" candidates)))))
       
       ;; Cleanup
       (when (file-exists-p task-file) (delete-file task-file))
       (when (file-exists-p normal-file) (delete-file normal-file))
       (when (file-directory-p ai-files-dir) (delete-directory ai-files-dir))))))

(ert-deftest ai-code-test-prompt-filepath-candidates-prioritizes-visible-windows ()
  "Test that ai-code--prompt-filepath-candidates prioritizes visible window files."
  (ai-code-with-test-repo
   (let ((visible-file (expand-file-name "visible.el" git-root))
         (buffer-file (expand-file-name "buffer.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test files
           (with-temp-file visible-file (insert "visible"))
           (with-temp-file buffer-file (insert "buffer"))
           
           ;; Mock dependencies
           (cl-letf (((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (file root) nil))
                     ((symbol-function 'ai-code--visible-window-files)
                      (lambda (root) (list visible-file)))
                     ((symbol-function 'ai-code--buffer-file-list)
                      (lambda (root skip) (list buffer-file)))
                     ((symbol-function 'ai-code--repo-recent-files)
                      (lambda (root) '())))
             
             (let ((candidates (ai-code--prompt-filepath-candidates)))
               ;; Visible file should come before buffer file
               (should (equal candidates '("@visible.el" "@buffer.el"))))))
       
       ;; Cleanup
       (when (file-exists-p visible-file) (delete-file visible-file))
       (when (file-exists-p buffer-file) (delete-file buffer-file))))))

(ert-deftest ai-code-test-prompt-filepath-capf-returns-candidates-after-at ()
  "Test that ai-code--prompt-filepath-capf returns candidates when '@' is typed."
  (ai-code-with-test-repo
   (with-temp-buffer
     ;; Insert text with @ symbol
     (insert "Check @")
     
     ;; Mock dependencies
     (cl-letf (((symbol-function 'ai-code--prompt-filepath-candidates)
                (lambda () '("@file1.el" "@file2.el"))))
       
       (let* ((result (ai-code--prompt-filepath-capf))
              (start (nth 0 result))
              (end (nth 1 result))
              (candidates (nth 2 result))
              (props (nthcdr 3 result)))
         ;; Should return completion table
         (should result)
         (should (= start (- (point) 1)))  ; start position at @
         (should (= end (point)))          ; end position at current point
         (should (equal candidates '("@file1.el" "@file2.el"))) ; candidates
         (should (eq (plist-get props :exclusive) 'no)))))))

(ert-deftest ai-code-test-prompt-filepath-capf-no-candidates-without-at ()
  "Test that ai-code--prompt-filepath-capf returns nil when '@' is not present."
  (ai-code-with-test-repo
   (with-temp-buffer
     ;; Insert text without @ symbol
     (insert "Check file")
     
     ;; Should return nil
     (should-not (ai-code--prompt-filepath-capf)))))

(ert-deftest ai-code-test-prompt-filepath-capf-partial-match ()
  "Test that ai-code--prompt-filepath-capf works with partial file paths after '@'."
  (ai-code-with-test-repo
   (with-temp-buffer
     ;; Insert text with @ and partial path
     (insert "Check @src/ma")
     (let ((at-position (- (point) (length "src/ma"))))
       ;; Mock dependencies
       (cl-letf (((symbol-function 'ai-code--prompt-filepath-candidates)
                  (lambda () '("@src/main.el" "@src/main.js"))))
         
         (let* ((result (ai-code--prompt-filepath-capf))
                (start (nth 0 result))
                (end (nth 1 result))
                (candidates (nth 2 result)))
           ;; Should return completion table
           (should result)
           (should (= start (1- at-position))) ; start at @ (one before 's')
           (should (= end (point)))            ; end at current position
           (should (equal candidates '("@src/main.el" "@src/main.js"))))))))))

(ert-deftest ai-code-test-prompt-auto-trigger-filepath-completion ()
  "Test that ai-code--prompt-auto-trigger-filepath-completion triggers completion after '@'."
  (ai-code-with-test-repo
   (with-temp-buffer
     ;; Insert @ symbol
     (insert "@")
     
     ;; Mock completion-at-point
     (let ((completion-called nil))
       (cl-letf (((symbol-function 'completion-at-point)
                  (lambda () (setq completion-called t))))
         
         ;; Call auto-trigger function
         (ai-code--prompt-auto-trigger-filepath-completion)
         
         ;; Should have called completion-at-point
         (should completion-called))))))

(ert-deftest ai-code-test-prompt-auto-trigger-no-trigger-without-at ()
  "Test that ai-code--prompt-auto-trigger-filepath-completion doesn't trigger without '@'."
  (ai-code-with-test-repo
   (with-temp-buffer
     ;; Insert text without @
     (insert "text")
     
     ;; Mock completion-at-point
     (let ((completion-called nil))
       (cl-letf (((symbol-function 'completion-at-point)
                  (lambda () (setq completion-called t))))
         
         ;; Call auto-trigger function
         (ai-code--prompt-auto-trigger-filepath-completion)
         
         ;; Should NOT have called completion-at-point
         (should-not completion-called))))))

;;; Tests for # symbol completion in prompt mode

(ert-deftest ai-code-test-prompt-auto-trigger-hash-with-file ()
  "Test that # auto-trigger completes symbols from @file in prompt mode."
  (let ((git-root (expand-file-name "test-repo/" temporary-file-directory))
        (test-file (expand-file-name "src/test.el" (expand-file-name "test-repo/" temporary-file-directory))))
    (unwind-protect
        (progn
          ;; Setup: Create test file with symbols
          (make-directory (file-name-directory test-file) t)
          (with-temp-file test-file
            (insert "(defun prompt-test-symbol () nil)\n"))
          
          (require 'ai-code-input nil t)
          (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root))
                    ((symbol-function 'completing-read)
                     (lambda (prompt candidates &rest args)
                       "prompt-test-symbol")))
            (with-temp-buffer
              (insert "@src/test.el#")
              
              ;; Call auto-trigger
              (ai-code--prompt-auto-trigger-filepath-completion)
              
              ;; Should have replaced # with #symbol
              (should (string-match-p "#prompt-test-symbol" (buffer-string))))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file))
      (when (file-directory-p (file-name-directory test-file))
        (delete-directory (file-name-directory test-file)))
      (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-prompt-auto-trigger-hash-without-file ()
  "Test that # auto-trigger does nothing without valid @file in prompt mode."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) "/tmp/repo/")))
    (with-temp-buffer
      (insert "#")
      
      (let ((original-content (buffer-string)))
        (ai-code--prompt-auto-trigger-filepath-completion)
        
        ;; Content should be unchanged (no completion without @file)
        (should (string= original-content (buffer-string)))))))

(ert-deftest ai-code-test-prompt-auto-trigger-hash-nonexistent-file ()
  "Test that # auto-trigger handles nonexistent files gracefully in prompt mode."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) "/tmp/repo/")))
    (with-temp-buffer
      (insert "@nonexistent/file.el#")
      
      (let ((original-content (buffer-string)))
        (ai-code--prompt-auto-trigger-filepath-completion)
        
        ;; Content should be unchanged (file doesn't exist)
        (should (string= original-content (buffer-string)))))))

(ert-deftest ai-code-test-prompt-auto-trigger-hash-no-symbols ()
  "Test that # auto-trigger handles files with no symbols in prompt mode."
  (let ((git-root (expand-file-name "test-repo/" temporary-file-directory))
        (test-file (expand-file-name "src/empty.txt" (expand-file-name "test-repo/" temporary-file-directory))))
    (unwind-protect
        (progn
          ;; Setup: Create empty test file
          (make-directory (file-name-directory test-file) t)
          (with-temp-file test-file
            (insert "no symbols here"))
          
          (require 'ai-code-input nil t)
          (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
            (with-temp-buffer
              (insert "@src/empty.txt#")
              
              (let ((original-content (buffer-string)))
                (ai-code--prompt-auto-trigger-filepath-completion)
                
                ;; Content should be unchanged (no symbols to complete)
                (should (string= original-content (buffer-string)))))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file))
      (when (file-directory-p (file-name-directory test-file))
        (delete-directory (file-name-directory test-file)))
      (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-prompt-auto-trigger-hash-user-quit ()
  "Test that # auto-trigger handles user quit gracefully in prompt mode."
  (let ((git-root (expand-file-name "test-repo/" temporary-file-directory))
        (test-file (expand-file-name "src/test.el" (expand-file-name "test-repo/" temporary-file-directory))))
    (unwind-protect
        (progn
          ;; Setup: Create test file
          (make-directory (file-name-directory test-file) t)
          (with-temp-file test-file
            (insert "(defun some-func () nil)\n"))
          
          (require 'ai-code-input nil t)
          (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root))
                    ((symbol-function 'completing-read)
                     (lambda (prompt candidates &rest args)
                       (signal 'quit nil))))
            (with-temp-buffer
              (insert "@src/test.el#")
              
              (let ((original-content (buffer-string)))
                (ai-code--prompt-auto-trigger-filepath-completion)
                
                ;; Content should be unchanged (user quit)
                (should (string= original-content (buffer-string)))))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file))
      (when (file-directory-p (file-name-directory test-file))
        (delete-directory (file-name-directory test-file)))
      (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-prompt-auto-trigger-hash-in-minibuffer ()
  "Test that # auto-trigger doesn't work in minibuffer in prompt mode."
  (cl-letf (((symbol-function 'minibufferp) (lambda (&optional buffer) t)))
    (with-temp-buffer
      (insert "@src/test.el#")
      
      (let ((original-content (buffer-string)))
        (ai-code--prompt-auto-trigger-filepath-completion)
        
        ;; Should be unchanged in minibuffer
        (should (string= original-content (buffer-string)))))))

(ert-deftest ai-code-test-prompt-filepath-capf-at-completion ()
  "Test that @ filepath completion works via capf in prompt mode."
  (ai-code-with-test-repo
   (with-temp-buffer
     (insert "@src/")
     (goto-char (point-max))
     
     ;; Get completion candidates
     (let ((result (ai-code--prompt-filepath-capf)))
       ;; Should return completion list
       (should result)
       (should (listp result))
       ;; First element should be start position
       (should (numberp (car result)))
       ;; Second element should be end position
       (should (numberp (cadr result)))
       ;; Third element should be candidate list
       (should (listp (caddr result)))))))

(ert-deftest ai-code-test-prompt-filepath-capf-no-at ()
  "Test that capf returns nil without @ prefix in prompt mode."
  (ai-code-with-test-repo
   (with-temp-buffer
     (insert "src/")
     (goto-char (point-max))
     
     ;; Get completion candidates
     (let ((result (ai-code--prompt-filepath-capf)))
       ;; Should return nil (no @ prefix)
       (should-not result)))))

(ert-deftest ai-code-test-prompt-filepath-capf-in-minibuffer ()
  "Test that capf returns nil in minibuffer in prompt mode."
  (ai-code-with-test-repo
   (cl-letf (((symbol-function 'minibufferp) (lambda (&optional buffer) t)))
     (with-temp-buffer
       (insert "@src/")
       (goto-char (point-max))
       
       ;; Get completion candidates
       (let ((result (ai-code--prompt-filepath-capf)))
         ;; Should return nil (in minibuffer)
         (should-not result))))))

(ert-deftest ai-code-test-prompt-filepath-candidates-includes-dired-directories ()
  "Test that ai-code--prompt-filepath-candidates includes dired directories from current frame."
  (ai-code-with-test-repo
   (let ((dired-dir-1 (expand-file-name "src/" git-root))
         (dired-dir-2 (expand-file-name "test/" git-root))
         (test-file (expand-file-name "file.el" git-root))
         (dired-buf-1 nil)
         (dired-buf-2 nil))
     (unwind-protect
         (progn
           ;; Create test directories and file
           (make-directory dired-dir-1 t)
           (make-directory dired-dir-2 t)
           (with-temp-file test-file (insert "content"))
           
           ;; Open dired buffers and display them in windows
           (setq dired-buf-1 (dired-noselect dired-dir-1))
           (setq dired-buf-2 (dired-noselect dired-dir-2))
           
           ;; Display dired buffers in windows to simulate visible frame
           (switch-to-buffer dired-buf-1)
           (split-window)
           (other-window 1)
           (switch-to-buffer dired-buf-2)
           
           ;; Mock dependencies
           (cl-letf (((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (_file _root) nil))
                     ((symbol-function 'ai-code--visible-window-files)
                      (lambda (_root) '()))
                     ((symbol-function 'ai-code--buffer-file-list)
                      (lambda (_root _skip) (list test-file)))
                     ((symbol-function 'ai-code--repo-recent-files)
                      (lambda (_root) '())))
             
             (let ((candidates (ai-code--prompt-filepath-candidates)))
               ;; Both dired directories should be included in candidates
               (should (member "@src/" candidates))
               (should (member "@test/" candidates))
               ;; Test file should also be included
               (should (member "@file.el" candidates))
               ;; Dired directories should come before buffer files
               (let ((src-pos (cl-position "@src/" candidates :test #'string=))
                     (test-pos (cl-position "@test/" candidates :test #'string=))
                     (file-pos (cl-position "@file.el" candidates :test #'string=)))
                 (should (< src-pos file-pos))
                 (should (< test-pos file-pos))))))
       
       ;; Cleanup
       (when (buffer-live-p dired-buf-1) (kill-buffer dired-buf-1))
       (when (buffer-live-p dired-buf-2) (kill-buffer dired-buf-2))
       (when (file-exists-p test-file) (delete-file test-file))
       (when (file-directory-p dired-dir-1) (delete-directory dired-dir-1))
       (when (file-directory-p dired-dir-2) (delete-directory dired-dir-2))
       ;; Restore window configuration
       (delete-other-windows)))))

(provide 'test-ai-code-prompt-mode)
;;; test_ai-code-prompt-mode.el ends here
