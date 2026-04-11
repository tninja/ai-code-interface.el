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
  `(let* ((git-root (expand-file-name "test-repo/" (file-truename temporary-file-directory)))
          (mock-file-in-repo (expand-file-name "src/main.js" git-root))
          (outside-file (expand-file-name "other-file.txt" (file-truename temporary-file-directory))))
     (unwind-protect
         (progn
           ;; Setup: Create dummy files and directories
           (make-directory (file-name-directory mock-file-in-repo) t)
           (with-temp-file mock-file-in-repo (insert "content"))
           (with-temp-file outside-file (insert "content"))
           ;; Execute test body with mocks
           (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root))
                     ((symbol-function 'ai-code--git-root) (lambda (&optional dir) git-root)))
             ,@body))
       ;; Teardown: Clean up dummy files and directories
       (when (file-exists-p mock-file-in-repo) (delete-file mock-file-in-repo))
       (when (file-exists-p outside-file) (delete-file outside-file))
       (when (file-directory-p (file-name-directory mock-file-in-repo))
         (delete-directory (file-name-directory mock-file-in-repo) t))
       (when (file-directory-p git-root) (delete-directory git-root t)))))

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

(ert-deftest ai-code-test-preprocess-preserves-whitespace ()
  "Test that original whitespace (including newlines) is preserved."
  (ai-code-with-test-repo
   (let ((prompt (format "check file %s\nand also\n  %s" mock-file-in-repo outside-file)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      (format "check file @src/main.js\nand also\n  %s" outside-file))))))

(ert-deftest ai-code-test-preprocess-not-in-git-repo ()
  "Test that paths are not modified when not in a git repository."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) nil)))
    (let ((prompt "check file /some/file.txt"))
      (should (string= (ai-code--preprocess-prompt-text prompt)
                       prompt)))))

(ert-deftest ai-code-test-prompt-send-block-in-prompt-file-sends-directly ()
  "Send block directly when current buffer is the prompt file."
  (let ((sent-prompt nil)
        (read-called nil)
        (insert-called nil))
    (with-temp-buffer
      (insert "line one\nline two\n\nline three")
      (goto-char (point-min))
      (setq-local buffer-file-name
                  (expand-file-name ai-code-prompt-file-name temporary-file-directory))
      (cl-letf (((symbol-function 'ai-code--send-prompt)
                 (lambda (prompt)
                   (setq sent-prompt prompt)))
                ((symbol-function 'ai-code-read-string)
                 (lambda (&rest _args)
                   (setq read-called t)
                   "edited prompt"))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (&rest _args)
                   (setq insert-called t))))
        (ai-code-prompt-send-block)))
    (should (string= sent-prompt "line one\nline two"))
    (should-not read-called)
    (should-not insert-called)))

(ert-deftest ai-code-test-prompt-send-block-in-other-buffer-confirms-before-send ()
  "Ask confirmation and edit prompt before sending when not in prompt file."
  (let ((read-args nil)
        (inserted-prompt nil)
        (sent-directly nil))
    (with-temp-buffer
      (insert "line one\nline two\n\nline three")
      (goto-char (point-min))
      (setq-local buffer-file-name (expand-file-name "notes.org" temporary-file-directory))
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (prompt &optional initial-input candidate-list)
                   (setq read-args (list prompt initial-input candidate-list))
                   "edited prompt"))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt)
                   (setq inserted-prompt prompt)))
                ((symbol-function 'ai-code--send-prompt)
                 (lambda (&rest _args)
                   (setq sent-directly t))))
        (ai-code-prompt-send-block)))
    (should (equal read-args
                   '("Confirm and edit prompt before sending: "
                     "line one\nline two"
                     nil)))
    (should (string= inserted-prompt "edited prompt"))
    (should-not sent-directly)))

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
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest _args)
                  (should (string-match-p "Task name" prompt))
                  (should (member "scratch.org" collection))
                  ""))
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

(ert-deftest ai-code-test-create-or-open-task-file-with-prefix-sends-search-prompt ()
  "Test that prefix arg sends a confirmed search prompt to the AI session."
  (ai-code-with-test-repo
   (let* ((files-dir (expand-file-name ".ai.code.files" git-root))
          (search-dir (expand-file-name "notes" files-dir))
          (read-calls nil)
          (sent-command nil)
          (switch-called nil))
     (make-directory search-dir t)
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'read-string)
                      (lambda (prompt &optional initial-input history default-value _inherit)
                        (push (list prompt initial-input history default-value) read-calls)
                        (if (string-match-p "Directory to search" prompt)
                            search-dir
                          (ert-fail (format "Unexpected prompt: %s" prompt)))))
                     ((symbol-function 'ai-code-read-string)
                      (lambda (prompt &optional initial-input candidate-list)
                        (push (list prompt initial-input candidate-list) read-calls)
                        (cond
                         ((string-match-p "Search description" prompt) "find todos about auth")
                         ((string-match-p "Confirm search prompt" prompt) initial-input)
                         (t (ert-fail (format "Unexpected prompt: %s" prompt))))))
                     ((symbol-function 'ai-code-cli-send-command)
                      (lambda (command)
                        (setq sent-command command)))
                     ((symbol-function 'ai-code-cli-switch-to-buffer)
                      (lambda ()
                        (setq switch-called t)))
                     ((symbol-function 'message)
                      (lambda (&rest _args) nil)))
             (let ((current-prefix-arg '(4)))
               (call-interactively #'ai-code-create-or-open-task-file))
             (should switch-called)
             (should (equal (car (car (last read-calls)))
                            "Directory to search org files: "))
             (should (equal (nth 1 (car (last read-calls))) files-dir))
             (should (eq (nth 2 (car (last read-calls)))
                         'ai-code-task-search-directory-history))
             (should (equal sent-command
                            (concat
                             "Search the content of all .org files recursively under directory: "
                             search-dir
                             "\n"
                             "Search target description: find todos about auth"
                             "\n"
                             "Focus on matching content inside the files, not just file names."
                             "\n"
                             "Return the relevant file paths, matched excerpts, and a concise summary.")))))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-read-task-search-directory-expands-relative-input-from-files-dir ()
  "Relative search directories should resolve from AI-CODE-FILES-DIR."
  (let* ((ai-code-files-dir "/tmp/project/.ai.code.files/")
         (expected-dir (expand-file-name "notes" ai-code-files-dir)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "notes"))
              ((symbol-function 'file-directory-p)
               (lambda (dir)
                 (string= dir expected-dir))))
      (should (equal (ai-code--read-task-search-directory ai-code-files-dir)
                     expected-dir)))))

(ert-deftest ai-code-test-create-or-open-task-file-create-new ()
  "Test that ai-code-create-or-open-task-file creates new task file with metadata."
  (ai-code-with-test-repo
   (let ((files-dir (expand-file-name ".ai.code.files" git-root))
         (task-file nil)
         (ai-code-task-use-gptel-filename nil))
     (unwind-protect
	         (cl-letf (((symbol-function 'completing-read)
	                    (lambda (prompt collection &rest _args)
	                      (cond
	                       ((string-match-p "Task name" prompt)
	                        (should (member "scratch.org" collection))
	                        "Test Task")
	                       ((string-match-p "Create task file in" prompt)
	                        (format "ai-code-files-dir: %s" files-dir)))))
                    ((symbol-function 'read-string)
                     (lambda (prompt &optional initial-input)
                       (cond
                        ((string-match-p "URL" prompt) "https://example.com")
                        ((string-match-p "Confirm task filename" prompt) initial-input))))
	                   ((symbol-function 'ai-code-current-backend-label)
	                    (lambda () "codex"))
 	                   ((symbol-function 'find-file-other-window)
 	                    (lambda (filename)
 	                      (setq task-file filename)
	                      (set-buffer (find-file-noselect filename))
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
	           (with-current-buffer (get-file-buffer task-file)
	             (let ((content (buffer-string)))
	               (should (string-match-p (regexp-quote "#+TITLE: Test Task") content))
	               (should (string-match-p (regexp-quote "#+DATE: ") content))
	               (should (string-match-p (regexp-quote "#+URL: https://example.com") content))
	               (should (string-match-p "\\* Task Description" content))
	               (should (string-match-p "\\* Investigation" content))
	               (should (string-match-p "\\* Code Change" content)))))
       ;; Cleanup
       (when (and task-file (get-file-buffer task-file))
         (kill-buffer (get-file-buffer task-file)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-create-or-open-task-file-opens-existing-candidate-directly ()
  "Test that selecting an existing task file opens it without filename prompts."
  (ai-code-with-test-repo
   (let* ((files-dir (expand-file-name ".ai.code.files" git-root))
          (existing-file (expand-file-name "existing-task.org" files-dir))
          (opened-file nil))
     (make-directory files-dir t)
     (with-temp-file existing-file
       (insert "#+TITLE: Existing Task\n"))
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (prompt collection &rest _args)
                        (should (string-match-p "Task name" prompt))
                        (should (equal collection '("existing-task.org" "scratch.org")))
                        "existing-task.org"))
                     ((symbol-function 'read-string)
                      (lambda (prompt &rest _args)
                        (ert-fail (format "Unexpected prompt: %s" prompt))))
                     ((symbol-function 'ai-code--generate-task-filename)
                      (lambda (&rest _args)
                        (ert-fail "Should not generate filename for existing task")))
                     ((symbol-function 'find-file-other-window)
                      (lambda (filename)
                        (setq opened-file filename)))
                     ((symbol-function 'message)
                      (lambda (&rest _args) nil)))
             (ai-code-create-or-open-task-file))
           (should (equal opened-file existing-file)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-create-or-open-task-file-creates-scratch-candidate-directly ()
  "Test that selecting scratch.org creates it directly with template content."
  (ai-code-with-test-repo
   (let* ((files-dir (expand-file-name ".ai.code.files" git-root))
          (scratch-file (expand-file-name "scratch.org" files-dir))
          (opened-file nil))
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (prompt collection &rest _args)
                        (should (string-match-p "Task name" prompt))
                        (should (member "scratch.org" collection))
                        "scratch.org"))
                     ((symbol-function 'read-string)
                      (lambda (prompt &rest _args)
                        (ert-fail (format "Unexpected prompt: %s" prompt))))
                     ((symbol-function 'ai-code--generate-task-filename)
                      (lambda (&rest _args)
                        (ert-fail "Should not generate filename for scratch.org")))
                     ((symbol-function 'ai-code-current-backend-label)
                      (lambda () "codex"))
                     ((symbol-function 'find-file-other-window)
                      (lambda (filename)
                        (setq opened-file filename)
                        (set-buffer (find-file-noselect filename))
                        (erase-buffer)))
                     ((symbol-function 'message)
                      (lambda (&rest _args) nil)))
             (ai-code-create-or-open-task-file))
           (should (equal opened-file scratch-file))
           (should (file-exists-p scratch-file))
           (with-temp-buffer
             (insert-file-contents scratch-file)
             (let ((content (buffer-string)))
               (should (string-match-p (regexp-quote "#+TITLE: scratch.org") content))
               (should (string-match-p "\\* Task Description" content)))))
       (when (get-file-buffer scratch-file)
         (kill-buffer (get-file-buffer scratch-file)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-task-file-candidates-sort-by-modified-time-with-missing-scratch ()
  "Test that task candidates follow modified time and missing scratch.org is fifth."
  (ai-code-with-test-repo
   (let* ((files-dir (expand-file-name ".ai.code.files" git-root))
          (file-names '("task-1.org"
                        "task-2.org"
                        "task-3.org"
                        "task-4.org"
                        "task-5.org"
                        "task-6.org"))
          (base-time (current-time)))
     (make-directory files-dir t)
     (cl-loop for file-name in file-names
              for offset from 0
              do (let ((file (expand-file-name file-name files-dir)))
                   (with-temp-file file
                     (insert file-name))
                   (set-file-times file
                                   (time-subtract base-time
                                                  (seconds-to-time (* offset 60))))))
     (should
      (equal (ai-code--task-file-candidates files-dir)
             '("task-1.org"
               "task-2.org"
               "task-3.org"
               "task-4.org"
               "scratch.org"
               "task-5.org"
               "task-6.org"))))
   (when (file-directory-p (expand-file-name ".ai.code.files" git-root))
     (delete-directory (expand-file-name ".ai.code.files" git-root) t))))

(ert-deftest ai-code-test-task-file-candidates-excludes-prompt-file ()
  "Test that task candidates exclude `ai-code-prompt-file-name`."
  (ai-code-with-test-repo
   (let* ((files-dir (expand-file-name ".ai.code.files" git-root))
          (task-file (expand-file-name "task-1.org" files-dir))
          (prompt-file (expand-file-name ai-code-prompt-file-name files-dir)))
     (make-directory files-dir t)
     (with-temp-file task-file
       (insert "task"))
     (with-temp-file prompt-file
       (insert "prompt"))
     (should
      (equal (ai-code--task-file-candidates files-dir)
             '("task-1.org" "scratch.org"))))
   (when (file-directory-p (expand-file-name ".ai.code.files" git-root))
     (delete-directory (expand-file-name ".ai.code.files" git-root) t))))

(ert-deftest ai-code-test-initialize-task-file-content-includes-branch ()
  "Test that ai-code--initialize-task-file-content inserts #+BRANCH when branch is available."
  (cl-letf (((symbol-function 'magit-get-current-branch)
             (lambda () "feature/my-branch"))
            ((symbol-function 'ai-code-current-backend-label)
             (lambda () "codex")))
    (with-temp-buffer
      (ai-code--initialize-task-file-content "Test Task" "https://example.com")
      (let ((content (buffer-string)))
        (should (string-match-p (regexp-quote "#+BRANCH: feature/my-branch") content))))))

(ert-deftest ai-code-test-initialize-task-file-content-no-branch ()
  "Test that ai-code--initialize-task-file-content omits #+BRANCH when no branch available."
  (cl-letf (((symbol-function 'magit-get-current-branch)
             (lambda () nil))
            ((symbol-function 'ai-code-current-backend-label)
             (lambda () "codex")))
    (with-temp-buffer
      (ai-code--initialize-task-file-content "Test Task" "")
      (let ((content (buffer-string)))
        (should-not (string-match-p "#+BRANCH:" content))))))

(ert-deftest ai-code-test-create-or-open-task-file-adds-org-extension ()
  "Test that .org extension is added if missing from confirmed filename."
  (ai-code-with-test-repo
   (let ((files-dir (expand-file-name ".ai.code.files" git-root))
         (task-file nil)
         (ai-code-task-use-gptel-filename nil))
     (unwind-protect
         (cl-letf (((symbol-function 'completing-read)
                    (lambda (prompt collection &rest _args)
                      (cond
                       ((string-match-p "Task name" prompt)
                        (should (member "scratch.org" collection))
                        "Test Task")
                       ((string-match-p "Create task file in" prompt)
                        (format "ai-code-files-dir: %s" files-dir)))))
                   ((symbol-function 'read-string)
                    (lambda (prompt &optional _initial-input)
                      (cond
                       ((string-match-p "URL" prompt) "")
                       ;; User removes .org extension
                       ((string-match-p "Confirm task filename" prompt) "my_task"))))
                   ((symbol-function 'ai-code-current-backend-label)
                    (lambda () "codex"))
                   ((symbol-function 'find-file-other-window)
                    (lambda (filename)
                      (setq task-file filename)
                      (set-buffer (find-file-noselect filename))
                      (erase-buffer)))
                   ((symbol-function 'message)
                    (lambda (&rest args) nil)))
           ;; Call function
           (ai-code-create-or-open-task-file)
           ;; Check that .org extension was added
           (should (string-suffix-p ".org" task-file)))
       ;; Cleanup
       (when (and task-file (get-file-buffer task-file))
         (kill-buffer (get-file-buffer task-file)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-create-or-open-task-file-create-subdir-option-dir-only ()
  "Test that confirming filename ending with / opens subdirectory and does not create file."
  (ai-code-with-test-repo
   (let* ((default-directory git-root)
          (ai-code-task-use-gptel-filename nil)
          (files-dir (expand-file-name ".ai.code.files" git-root))
          (generated-filename "task_20260101_my_task.org")
          (expected-subdir (expand-file-name "task_20260101_my_task" default-directory))
          (opened-file nil)
          (opened-dired nil))
     (unwind-protect
         (cl-letf (((symbol-function 'completing-read)
                    (lambda (prompt collection &rest _args)
                      (cond
                       ((string-match-p "Task name" prompt)
                        (should (member "scratch.org" collection))
                        "My Task")
                       ((string-match-p "Create task file in" prompt)
                        (format "current directory: %s" default-directory)))))
                   ((symbol-function 'read-string)
                    (lambda (prompt &optional _initial-input)
                      (cond
                       ((string-match-p "URL" prompt) "")
                       ((string-match-p "Confirm task filename" prompt) "task_20260101_my_task/"))))
                   ((symbol-function 'ai-code--generate-task-filename)
                    (lambda (_task-name) generated-filename))
                   ((symbol-function 'find-file-other-window)
                    (lambda (filename) (setq opened-file filename)))
                   ((symbol-function 'dired-other-window)
                    (lambda (dirname) (setq opened-dired dirname)))
                   ((symbol-function 'message)
                    (lambda (&rest _args) nil)))
           (ai-code-create-or-open-task-file)
           (should (string= opened-dired expected-subdir))
           (should (file-directory-p expected-subdir))
           (should-not opened-file)
           (should-not (file-exists-p (expand-file-name generated-filename expected-subdir)))))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))
       (when (file-directory-p expected-subdir)
         (delete-directory expected-subdir t)))))

(ert-deftest ai-code-test-select-task-target-directory-create-subdir-option ()
  "Test that directory selection returns one of the two target directories."
  (ai-code-with-test-repo
   (let* ((ai-code-files-dir (expand-file-name ".ai.code.files" git-root))
          (current-dir default-directory)
          (selection nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt _collection &rest _args)
                  (format "current directory: %s" current-dir))))
       (setq selection
             (ai-code--select-task-target-directory ai-code-files-dir current-dir))
       (should (string= selection current-dir)))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt _collection &rest _args)
                  (format "ai-code-files-dir: %s" ai-code-files-dir))))
       (setq selection
             (ai-code--select-task-target-directory ai-code-files-dir current-dir))
       (should (string= selection ai-code-files-dir)))))


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

(ert-deftest ai-code-test-normalize-path ()
  "Test that ai-code--normalize-path returns correct normalized paths."
  (ai-code-with-test-repo
   (let ((existing-file mock-file-in-repo)
         (non-existing-file (expand-file-name "non-existent.el" git-root)))
     ;; Test with existing file - should return truename
     (let ((result (ai-code--normalize-path existing-file)))
       (should (string= result (file-truename existing-file))))

     ;; Test with non-existing file - should return expanded path
     (let ((result (ai-code--normalize-path non-existing-file)))
       (should (string= result (expand-file-name non-existing-file)))))))

(ert-deftest ai-code-test-candidate-path-in-repo ()
  "Test that ai-code--candidate-path returns relative path for in-repo files."
  (ai-code-with-test-repo
   (let ((test-file (expand-file-name "src/test.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test file
           (make-directory (file-name-directory test-file) t)
           (with-temp-file test-file (insert "content"))
           
           (let ((result (ai-code--candidate-path test-file (file-truename git-root))))
             ;; Should return relative path with @ prefix
             (should (string= result "@src/test.el"))))
       
       ;; Cleanup
       (when (file-exists-p test-file) (delete-file test-file))))))

(ert-deftest ai-code-test-candidate-path-out-of-repo ()
  "Test that ai-code--candidate-path returns absolute path for out-of-repo files."
  (ai-code-with-test-repo
   (let ((out-file (expand-file-name "outside.el" temporary-file-directory)))
     (unwind-protect
         (progn
           ;; Create file outside repo
           (with-temp-file out-file (insert "content"))
           
           (let ((result (ai-code--candidate-path out-file (file-truename git-root))))
             ;; Should return absolute path (truename)
             (should (string= result (file-truename out-file)))))
       
       ;; Cleanup
       (when (file-exists-p out-file) (delete-file out-file))))))

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
           
           ;; Open files in buffers
           (let ((buf1 (find-file-noselect test-file-1))
                 (buf2 (find-file-noselect test-file-2)))
             (unwind-protect
                 (progn
                   ;; Mock window-list to simulate visible windows
                   (cl-letf (((symbol-function 'window-list)
                              (lambda (&optional frame no-minibuf)
                                (list (selected-window))))
                            ((symbol-function 'window-buffer)
                             (lambda (win)
                               (if (eq win (selected-window))
                                   buf1
                                 buf2)))
                            ((symbol-function 'selected-window)
                             (lambda () 'mock-window)))
                     (let ((result (ai-code--visible-window-files)))
                       ;; Should contain the file from the mocked window
                       (should (member test-file-1 result))
                       ;; Should not filter by git repo (unlike old implementation)
                       (should (= 1 (length result))))))
               
               ;; Kill buffers
               (when (buffer-live-p buf1) (kill-buffer buf1))
               (when (buffer-live-p buf2) (kill-buffer buf2)))))
       
       ;; Cleanup
       (when (file-exists-p test-file-1) (delete-file test-file-1))
       (when (file-exists-p test-file-2) (delete-file test-file-2))))))

(ert-deftest ai-code-test-recent-buffer-paths ()
  "Test that ai-code--recent-buffer-paths returns recent buffer paths."
  (ai-code-with-test-repo
   (let ((test-file-1 (expand-file-name "recent1.el" git-root))
         (test-file-2 (expand-file-name "recent2.el" git-root))
         (test-file-3 (expand-file-name "recent3.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test files
           (with-temp-file test-file-1 (insert "content1"))
           (with-temp-file test-file-2 (insert "content2"))
           (with-temp-file test-file-3 (insert "content3"))
           
           ;; Open files in buffers (most recent first in buffer-list)
           (let ((buf1 (find-file-noselect test-file-1))
                 (buf2 (find-file-noselect test-file-2))
                 (buf3 (find-file-noselect test-file-3)))
             (unwind-protect
                 (progn
                   (let ((result (ai-code--recent-buffer-paths (file-truename git-root))))
                     ;; Should return candidate paths (relative with @ prefix for in-repo)
                     (should (member "@recent1.el" result))
                     (should (member "@recent2.el" result))
                     (should (member "@recent3.el" result))
                     ;; Should limit to 5 files
                     (should (<= (length result) 5))))
               
               ;; Kill buffers
               (when (buffer-live-p buf1) (kill-buffer buf1))
               (when (buffer-live-p buf2) (kill-buffer buf2))
               (when (buffer-live-p buf3) (kill-buffer buf3)))))
       
       ;; Cleanup
       (when (file-exists-p test-file-1) (delete-file test-file-1))
       (when (file-exists-p test-file-2) (delete-file test-file-2))
       (when (file-exists-p test-file-3) (delete-file test-file-3))))))

(ert-deftest ai-code-test-recent-buffer-paths-includes-dired ()
  "Test that ai-code--recent-buffer-paths includes dired directories."
  (ai-code-with-test-repo
   (let ((dired-dir (expand-file-name "testdir/" git-root))
         (dired-buf nil))
     (unwind-protect
         (progn
           ;; Create test directory
           (make-directory dired-dir t)
           
           ;; Open dired buffer
           (setq dired-buf (dired-noselect dired-dir))
           
           (let ((result (ai-code--recent-buffer-paths (file-truename git-root))))
             ;; Should include the dired directory
             (should (member "@testdir/" result))))
       
       ;; Cleanup
       (when (buffer-live-p dired-buf) (kill-buffer dired-buf))
       (when (file-directory-p dired-dir) (delete-directory dired-dir))))))

(ert-deftest ai-code-test-current-frame-dired-paths ()
  "Test that ai-code--current-frame-dired-paths returns dired directories."
  (ai-code-with-test-repo
   (let ((dired-dir-1 (expand-file-name "src/" git-root))
         (dired-dir-2 (expand-file-name "test/" git-root))
         (dired-buf-1 nil)
         (dired-buf-2 nil))
     (unwind-protect
         (progn
           ;; Create test directories
           (make-directory dired-dir-1 t)
           (make-directory dired-dir-2 t)
           
           ;; Open dired buffers
           (setq dired-buf-1 (dired-noselect dired-dir-1))
           (setq dired-buf-2 (dired-noselect dired-dir-2))
           
           ;; Mock window-list and git-ignored check
           (cl-letf (((symbol-function 'window-list)
                      (lambda (&optional frame no-minibuf)
                        (list 'win1 'win2)))
                     ((symbol-function 'window-buffer)
                      (lambda (win)
                        (if (eq win 'win1) dired-buf-1 dired-buf-2)))
                     ((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (file root) nil)))
             
             (let ((result (ai-code--current-frame-dired-paths (file-truename git-root))))
               ;; Should include both dired directories
               (should (member "@src/" result))
               (should (member "@test/" result)))))
       
       ;; Cleanup
       (when (buffer-live-p dired-buf-1) (kill-buffer dired-buf-1))
       (when (buffer-live-p dired-buf-2) (kill-buffer dired-buf-2))
       (when (file-directory-p dired-dir-1) (delete-directory dired-dir-1))
       (when (file-directory-p dired-dir-2) (delete-directory dired-dir-2))))))

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
                      (lambda () (list visible-file)))
                     ((symbol-function 'ai-code--current-frame-dired-paths)
                      (lambda (root) '()))
                     ((symbol-function 'ai-code--recent-buffer-paths)
                      (lambda (root) '()))
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

(ert-deftest ai-code-test-prompt-filepath-candidates-includes-dired-directories ()
  "Test that ai-code--prompt-filepath-candidates includes dired directories from current frame."
  (ai-code-with-test-repo
   (let ((test-file (expand-file-name "file.el" git-root)))
     (unwind-protect
         (progn
           ;; Create test file
           (with-temp-file test-file (insert "content"))
           
           ;; Mock dependencies
           (cl-letf (((symbol-function 'ai-code--git-ignored-repo-file-p)
                      (lambda (_file _root) nil))
                     ((symbol-function 'ai-code--visible-window-files)
                      (lambda () '()))
                     ((symbol-function 'ai-code--current-frame-dired-paths)
                      (lambda (_root) '("@src/" "test/")))
                     ((symbol-function 'ai-code--recent-buffer-paths)
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
                     (test-pos (cl-position "test/" candidates :test #'string=))
                     (file-pos (cl-position "@file.el" candidates :test #'string=)))
                 (should (< src-pos file-pos))
                 (should (< test-pos file-pos))))))
       
       ;; Cleanup
       (when (file-exists-p test-file) (delete-file test-file))))))

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
     
     ;; Mock filepath candidates and selection
     (cl-letf (((symbol-function 'ai-code--prompt-filepath-candidates)
                (lambda () '("@src/main.el")))
               ((symbol-function 'completing-read)
                (lambda (_prompt candidates &rest _args)
                  (car candidates))))
       
       ;; Call auto-trigger function
       (ai-code--prompt-auto-trigger-filepath-completion)
       
       ;; Should replace @ with chosen candidate
       (should (string= (buffer-string) "@src/main.el"))))))

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

(ert-deftest ai-code-test-insert-backend-label-drawer ()
  "Test that ai-code--insert-backend-label-drawer inserts a PROPERTIES drawer with AGENT."
  (cl-letf (((symbol-function 'ai-code-current-backend-label)
             (lambda () "codex")))
    (with-temp-buffer
      (ai-code--insert-backend-label-drawer)
      (let ((content (buffer-string)))
        (should (string-match-p (regexp-quote ":PROPERTIES:") content))
        (should (string-match-p (regexp-quote ":AGENT: codex") content))
        (should (string-match-p (regexp-quote ":END:") content))))))

(ert-deftest ai-code-test-insert-backend-label-drawer-unknown-on-error ()
  "Test that ai-code--insert-backend-label-drawer falls back to 'unknown' on error."
  (cl-letf (((symbol-function 'ai-code-current-backend-label)
             (lambda () (error "no backend"))))
    (with-temp-buffer
      (ai-code--insert-backend-label-drawer)
      (let ((content (buffer-string)))
        (should (string-match-p (regexp-quote ":AGENT: unknown") content))))))

(ert-deftest ai-code-test-append-prompt-to-buffer-includes-drawer ()
  "Test that ai-code--append-prompt-to-buffer inserts PROPERTIES drawer with AGENT."
  (cl-letf (((symbol-function 'ai-code-current-backend-label)
             (lambda () "gemini"))
            ((symbol-function 'ai-code--generate-prompt-headline)
             (lambda (_prompt-text) (insert "** test headline\n"))))
    (with-temp-buffer
      (ai-code--append-prompt-to-buffer "Fix the bug")
      (let ((content (buffer-string)))
        (should (string-match-p (regexp-quote ":PROPERTIES:") content))
        (should (string-match-p (regexp-quote ":AGENT: gemini") content))
        (should (string-match-p (regexp-quote ":END:") content))))))

(provide 'test-ai-code-prompt-mode)
;;; test_ai-code-prompt-mode.el ends here
