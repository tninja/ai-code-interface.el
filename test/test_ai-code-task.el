;;; test_ai-code-task.el --- Tests for ai-code-task -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-task.

;;; Code:

(require 'ert)
(require 'ai-code-task)
(require 'ai-code-prompt-mode)
(require 'magit)
(require 'cl-lib)

(defvar ai-code-prompt-suffix)
(defvar ai-code-auto-test-type)
(defvar ai-code-auto-test-suffix)
(defvar ai-code-discussion-auto-follow-up-enabled)
(defvar ai-code-discussion-auto-follow-up-suffix)
(defvar ai-code-use-prompt-suffix)
(defvar org-roam-directory)

(defmacro ai-code-with-test-repo (&rest body)
  "Set up a temporary git repository environment for testing with BODY."
  `(let* ((git-root (expand-file-name "test-repo/" (file-truename temporary-file-directory)))
          (mock-file-in-repo (expand-file-name "src/main.js" git-root))
          (outside-file (expand-file-name "other-file.txt" (file-truename temporary-file-directory))))
     (unwind-protect
         (progn
           (make-directory (file-name-directory mock-file-in-repo) t)
           (with-temp-file mock-file-in-repo (insert "content"))
           (with-temp-file outside-file (insert "content"))
           (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional _dir) git-root))
                     ((symbol-function 'ai-code--git-root) (lambda (&optional _dir) git-root))
                     ((symbol-function 'magit-git-lines)
                      (lambda (&rest _args)
                        (let ((default-directory git-root))
                          (mapcar (lambda (f) (file-relative-name f git-root))
                                  (directory-files-recursively git-root ""))))))
             ,@body))
       (when (file-exists-p mock-file-in-repo) (delete-file mock-file-in-repo))
       (when (file-exists-p outside-file) (delete-file outside-file))
       (when (file-directory-p (file-name-directory mock-file-in-repo))
         (delete-directory (file-name-directory mock-file-in-repo) t))
       (when (file-directory-p git-root) (delete-directory git-root t)))))

(ert-deftest ai-code-test-generate-task-filename-without-gptel ()
  "Test filename generation without gptel."
  (let ((ai-code-task-use-gptel-filename nil))
    (let ((filename (ai-code--generate-task-filename "Fix Login Bug")))
      (should (string-match-p "^task_[0-9]\\{8\\}_fix_login_bug\\.org$" filename)))
    (let ((filename (ai-code--generate-task-filename "Add@Feature#123!")))
      (should (string-match-p "^task_[0-9]\\{8\\}_add_feature_123\\.org$" filename)))
    (let ((filename (ai-code--generate-task-filename "Test   Multiple   Spaces")))
      (should (string-match-p "^task_[0-9]\\{8\\}_test_multiple_spaces\\.org$" filename)))
    (let ((filename (ai-code--generate-task-filename "  Trim Spaces  ")))
      (should (string-match-p "^task_[0-9]\\{8\\}_trim_spaces\\.org$" filename)))))

(ert-deftest ai-code-test-generate-task-filename-with-gptel ()
  "Test filename generation with mocked gptel."
  (let ((ai-code-task-use-gptel-filename t))
    (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
               (lambda (_question) "implement_user_authentication")))
      (let ((filename (ai-code--generate-task-filename "Add user login feature")))
        (should
         (string-match-p
          "^task_[0-9]\\{8\\}_implement_user_authentication\\.org$"
          filename))))
    (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
               (lambda (_question) (error "GPTel not available"))))
      (let ((filename (ai-code--generate-task-filename "Fix Bug")))
        (should (string-match-p "^task_[0-9]\\{8\\}_fix_bug\\.org$" filename))))))

(ert-deftest ai-code-test-generate-task-filename-with-rdar ()
  "Test filename generation with rdar IDs."
  (let ((ai-code-task-use-gptel-filename nil))
    (let ((filename
           (ai-code--generate-task-filename
            "rdar://12345678 Fix crash on startup")))
      (should
       (string-match-p
        "^rdar_12345678_rdar_12345678_fix_crash_on_startup\\.org$"
        filename)))
    (let ((filename
           (ai-code--generate-task-filename
            "Fix crash rdar://99999 in login")))
      (should
       (string-match-p
        "^rdar_99999_fix_crash_rdar_99999_in_login\\.org$"
        filename)))))

(ert-deftest ai-code-test-generate-task-filename-org-extension ()
  "Test that .org extension is always added."
  (let ((ai-code-task-use-gptel-filename nil))
    (let ((filename (ai-code--generate-task-filename "Test Task")))
      (should (string-suffix-p ".org" filename)))
    (let ((filename (ai-code--generate-task-filename "rdar://12345 Test")))
      (should (string-suffix-p ".org" filename)))))

(ert-deftest ai-code-test-generate-task-filename-length-truncation ()
  "Test that generated filename payload is truncated."
  (let ((ai-code-task-use-gptel-filename nil)
        (long-name (make-string 100 ?a)))
    (let ((filename (ai-code--generate-task-filename long-name)))
      (string-match "^task_[0-9]\\{8\\}_\\(.*\\)\\.org$" filename)
      (let ((generated-part (match-string 1 filename)))
        (should (<= (length generated-part) 60))))))

(ert-deftest ai-code-test-create-or-open-task-file-open-directory ()
  "Create/open task file opens directory when task name is empty."
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
                (lambda (&rest _args) nil)))
       (ai-code-create-or-open-task-file)
       (should dired-called)
       (should (string= dired-dir files-dir))
       (should (file-directory-p files-dir)))
     (when (file-directory-p files-dir)
       (delete-directory files-dir t)))))

(ert-deftest ai-code-test-create-or-open-task-file-prefix-still-opens-directory-for-empty-name ()
  "Prefix arg still opens the task directory for an empty task name."
  (ai-code-with-test-repo
   (let* ((files-dir (expand-file-name ".ai.code.files" git-root))
          (dired-called nil)
          (dired-dir nil)
          (sent-command nil))
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (prompt collection &rest _args)
                        (cond
                         ((string-match-p "Task name" prompt)
                          (should (member "scratch.org" collection))
                          "")
                         (t
                          (ert-fail (format "Unexpected prompt: %s" prompt))))))
                     ((symbol-function 'dired-other-window)
                      (lambda (dirname)
                        (setq dired-called t)
                        (setq dired-dir dirname)))
                     ((symbol-function 'ai-code-cli-send-command)
                      (lambda (command)
                        (setq sent-command command)))
                     ((symbol-function 'message)
                      (lambda (&rest _args) nil)))
             (let ((current-prefix-arg '(4)))
               (call-interactively #'ai-code-create-or-open-task-file))
             (should dired-called)
             (should (equal dired-dir files-dir))
             (should-not sent-command)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-read-task-search-directory-expands-relative-input-from-files-dir ()
  "Relative search directories resolve from AI-CODE-FILES-DIR."
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
  "Create/open task file creates a new task file with metadata."
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
                    (lambda (prompt &optional initial-input &rest _args)
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
                    (lambda (&rest _args) nil)))
           (ai-code-create-or-open-task-file)
           (should task-file)
           (should (string-prefix-p files-dir task-file))
           (should (string-suffix-p ".org" task-file))
           (with-current-buffer (get-file-buffer task-file)
             (let ((content (buffer-string)))
               (should (string-match-p (regexp-quote "#+TITLE: Test Task") content))
               (should (string-match-p (regexp-quote "#+DATE: ") content))
               (should
                (string-match-p
                 (regexp-quote "#+URL: https://example.com") content))
               (should (string-match-p "\\* Task Description" content))
               (should (string-match-p "\\* Investigation" content))
               (should (string-match-p "\\* Code Change" content)))))
       (when (and task-file (get-file-buffer task-file))
         (kill-buffer (get-file-buffer task-file)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-create-or-open-task-file-opens-existing-candidate-directly ()
  "Selecting an existing task file opens it directly."
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
                        (should
                         (equal collection '("existing-task.org" "scratch.org")))
                        "existing-task.org"))
                     ((symbol-function 'read-string)
                      (lambda (prompt &rest _args)
                        (ert-fail (format "Unexpected prompt: %s" prompt))))
                     ((symbol-function 'ai-code--generate-task-filename)
                      (lambda (&rest _args)
                        (ert-fail
                         "Should not generate filename for existing task")))
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
  "Selecting scratch.org creates it directly with template content."
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
                        (ert-fail
                         "Should not generate filename for scratch.org")))
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
               (should
                (string-match-p (regexp-quote "#+TITLE: scratch.org") content))
               (should (string-match-p "\\* Task Description" content)))))
       (when (get-file-buffer scratch-file)
         (kill-buffer (get-file-buffer scratch-file)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-agent-handoff-loads-current-heading-subtree ()
  "Agent handoff loads the current Org heading subtree as context."
  (let (sent-prompt)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/project/.ai.code.files/task.org")
      (insert "* Current Handoff\n")
      (insert "Goal: finish backend-neutral handoff.\n")
      (insert "** Decision\n")
      (insert "Use task files instead of backend session state.\n")
      (insert "* Other Section\n")
      (insert "This content should not be sent.\n")
      (ai-code-prompt-mode)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'ai-code--confirm-and-send)
                 (lambda (_label prompt)
                   (setq sent-prompt prompt))))
        (ai-code-agent-handoff nil)))
    (should (string-match-p "Use this agent handoff context" sent-prompt))
    (should (string-match-p "Goal: finish backend-neutral handoff" sent-prompt))
    (should
     (string-match-p
      "Use task files instead of backend session state" sent-prompt))
    (should-not (string-match-p "This content should not be sent" sent-prompt))))

(ert-deftest ai-code-test-agent-handoff-prefix-loads-whole-task-file ()
  "Agent handoff with prefix loads the whole current task file."
  (let (sent-prompt)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/project/.ai.code.files/task.org")
      (insert "* Task Description\n")
      (insert "Build handoff support.\n")
      (insert "* Prior Context\n")
      (insert "Carry all task notes forward.\n")
      (ai-code-prompt-mode)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'ai-code--confirm-and-send)
                 (lambda (_label prompt)
                   (setq sent-prompt prompt))))
        (ai-code-agent-handoff '(4))))
    (should (string-match-p "Use this whole task file" sent-prompt))
    (should (string-match-p "Build handoff support" sent-prompt))
    (should (string-match-p "Carry all task notes forward" sent-prompt))))

(ert-deftest ai-code-test-agent-handoff-off-heading-dumps-to-task-file ()
  "Agent handoff off a heading asks the agent to append a handoff."
  (let (sent-prompt)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/project/.ai.code.files/task.org")
      (insert "* Task Description\n")
      (insert "Build handoff support.\n")
      (ai-code-prompt-mode)
      (goto-char (point-max))
      (cl-letf (((symbol-function 'ai-code--confirm-and-send)
                 (lambda (_label prompt)
                   (setq sent-prompt prompt))))
        (ai-code-agent-handoff nil)))
    (should (string-match-p "append a new top-level Org section" sent-prompt))
    (should (string-match-p "Agent Handoff" sent-prompt))
    (should (string-match-p "Task objective" sent-prompt))
    (should
     (string-match-p "/tmp/project/.ai.code.files/task.org" sent-prompt))))

(ert-deftest ai-code-test-task-file-candidates-sort-by-modified-time-with-missing-scratch ()
  "Task candidates follow modified time and missing scratch.org is fifth."
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
                   (set-file-times
                    file
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
  "Task candidates exclude `ai-code-prompt-file-name`."
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
  "Task file initialization inserts branch when available."
  (cl-letf (((symbol-function 'magit-get-current-branch)
             (lambda () "feature/my-branch"))
            ((symbol-function 'ai-code-current-backend-label)
             (lambda () "codex")))
    (with-temp-buffer
      (ai-code--initialize-task-file-content "Test Task" "https://example.com")
      (let ((content (buffer-string)))
        (should
         (string-match-p
          (regexp-quote "#+BRANCH: feature/my-branch") content))))))

(ert-deftest ai-code-test-initialize-task-file-content-no-branch ()
  "Task file initialization omits branch when unavailable."
  (cl-letf (((symbol-function 'magit-get-current-branch)
             (lambda () nil))
            ((symbol-function 'ai-code-current-backend-label)
             (lambda () "codex")))
    (with-temp-buffer
      (ai-code--initialize-task-file-content "Test Task" "")
      (let ((content (buffer-string)))
        (should-not (string-match-p "#+BRANCH:" content))))))

(ert-deftest ai-code-test-create-or-open-task-file-adds-org-extension ()
  "Create/open task file adds .org when the user omits it."
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
                    (lambda (prompt &optional _initial-input &rest _args)
                      (cond
                       ((string-match-p "URL" prompt) "")
                       ((string-match-p "Confirm task filename" prompt) "my_task"))))
                   ((symbol-function 'ai-code-current-backend-label)
                    (lambda () "codex"))
                   ((symbol-function 'find-file-other-window)
                    (lambda (filename)
                      (setq task-file filename)
                      (set-buffer (find-file-noselect filename))
                      (erase-buffer)))
                   ((symbol-function 'message)
                    (lambda (&rest _args) nil)))
           (ai-code-create-or-open-task-file)
           (should (string-suffix-p ".org" task-file)))
       (when (and task-file (get-file-buffer task-file))
         (kill-buffer (get-file-buffer task-file)))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))))))

(ert-deftest ai-code-test-create-or-open-task-file-create-subdir-option-dir-only ()
  "Confirming filename ending with / opens subdirectory and does not create file."
  (ai-code-with-test-repo
   (let* ((default-directory git-root)
          (ai-code-task-use-gptel-filename nil)
          (files-dir (expand-file-name ".ai.code.files" git-root))
          (generated-filename "task_20260101_my_task.org")
          (expected-subdir
           (expand-file-name "task_20260101_my_task" default-directory))
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
                    (lambda (prompt &optional _initial-input &rest _args)
                      (cond
                       ((string-match-p "URL" prompt) "")
                       ((string-match-p "Confirm task filename" prompt)
                        "task_20260101_my_task/"))))
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
           (should-not
            (file-exists-p
             (expand-file-name generated-filename expected-subdir)))))
       (when (file-directory-p files-dir)
         (delete-directory files-dir t))
       (when (file-directory-p expected-subdir)
         (delete-directory expected-subdir t)))))

(ert-deftest ai-code-test-select-task-target-directory-create-subdir-option ()
  "Directory selection returns one of the two target directories."
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
       (should (string= selection ai-code-files-dir))))))

(ert-deftest ai-code-test-create-or-open-task-file-in-worktree-links-to-main-repo ()
  "Task file is created in the main repo and symlinked into the worktree."
  (let* ((main-repo-root (file-truename (make-temp-file "main-repo-" t)))
         (worktree-root (file-truename (make-temp-file "worktree-" t)))
         (main-files-dir (expand-file-name ".ai.code.files" main-repo-root))
         (ai-code-files-dir-name ".ai.code.files")
         (ai-code-task-use-gptel-filename nil)
         (task-file nil))
    (make-directory main-files-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) worktree-root))
                  ((symbol-function 'magit-toplevel)
                   (lambda (&optional _dir) worktree-root))
                  ((symbol-function 'ai-code--worktree-main-repo-root)
                   (lambda () main-repo-root))
                  ((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () main-files-dir))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _args)
                     (cond
                      ((string-match-p "Task name" prompt) "worktree-task")
                      ((string-match-p "Create task file in" prompt)
                       (format "ai-code-files-dir: %s" main-files-dir)))))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial-input &rest _args)
                     (or initial-input "")))
                  ((symbol-function 'ai-code-current-backend-label)
                   (lambda () "claude"))
                  ((symbol-function 'find-file-other-window)
                   (lambda (filename) (setq task-file filename)))
                  ((symbol-function 'save-buffer)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'message)
                   (lambda (&rest _args) nil)))
          (ai-code-create-or-open-task-file)
          (should task-file)
          (should (string-prefix-p main-files-dir task-file))
          (let ((symlink
                 (expand-file-name
                  (file-name-nondirectory task-file) worktree-root)))
            (should (file-symlink-p symlink))
            (should
             (string=
              (file-truename symlink)
              (file-truename task-file)))))
      (delete-directory main-repo-root t)
      (delete-directory worktree-root t))))

(provide 'test-ai-code-task)
;;; test_ai-code-task.el ends here
