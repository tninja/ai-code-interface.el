;;; test_ai-code-file.el --- Tests for ai-code-file -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-file.el, particularly for file/directory creation
;; with GPTel integration.

;;; Code:

(require 'ert)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-file)
(require 'cl-lib)

(defvar ai-code-use-prompt-suffix)
(defvar ai-code-prompt-suffix)
(defvar ai-code-auto-test-type)
(defvar ai-code-auto-test-suffix)
(defvar ai-code-cli)
(defvar ai-code-selected-backend)
(defvar ai-code-sed-command)

;; Helper macro to set up and tear down the test environment
(defmacro ai-code-file-with-test-env (&rest body)
  "Set up a temporary environment for testing file operations.
This macro creates a temporary directory structure and ensures
everything is cleaned up afterward."
  `(let* ((test-dir (expand-file-name "test-file-ops/" temporary-file-directory))
          (default-directory test-dir))
     (unwind-protect
         (progn
           ;; Setup: Create test directory
           (make-directory test-dir t)
           ;; Execute test body
           ,@body)
       ;; Teardown: Clean up test directory
       (when (file-directory-p test-dir)
         (delete-directory test-dir t)))))

;;; Tests for ai-code--sanitize-generated-path-name

(ert-deftest ai-code-test-sanitize-basic-name ()
  "Test basic sanitization converts to lowercase and preserves valid characters."
  (should (string= (ai-code--sanitize-generated-path-name "MyFile.txt")
                   "myfile.txt"))
  (should (string= (ai-code--sanitize-generated-path-name "test_file_123.js")
                   "test_file_123.js"))
  (should (string= (ai-code--sanitize-generated-path-name "data.json")
                   "data.json")))

(ert-deftest ai-code-test-sanitize-special-characters ()
  "Test that special characters are replaced with underscores."
  ;; Special chars become underscores, multiple underscores collapse,
  ;; but trailing _ before extension is preserved
  (should (string= (ai-code--sanitize-generated-path-name "my file!@#$%.txt")
                   "my_file_.txt"))
  (should (string= (ai-code--sanitize-generated-path-name "test&file*.js")
                   "test_file_.js"))
  (should (string= (ai-code--sanitize-generated-path-name "file(with)parens.txt")
                   "file_with_parens.txt")))

(ert-deftest ai-code-test-sanitize-multiple-underscores ()
  "Test that multiple consecutive underscores are collapsed to one."
  (should (string= (ai-code--sanitize-generated-path-name "my___file.txt")
                   "my_file.txt"))
  (should (string= (ai-code--sanitize-generated-path-name "test____data.js")
                   "test_data.js")))

(ert-deftest ai-code-test-sanitize-slashes-replaced ()
  "Test that slashes are replaced with underscores."
  (should (string= (ai-code--sanitize-generated-path-name "path//to///file.txt")
                   "path_to_file.txt"))
  (should (string= (ai-code--sanitize-generated-path-name "dir////subdir/file.js")
                   "dir_subdir_file.js")))

(ert-deftest ai-code-test-sanitize-path-traversal-prevention ()
  "Test that path traversal attempts are sanitized."
  ;; Slashes replaced with underscores, leading dots/underscores stripped
  (should (string= (ai-code--sanitize-generated-path-name "../../../etc/passwd")
                   "etc_passwd"))
  (should (string= (ai-code--sanitize-generated-path-name "../../file.txt")
                   "file.txt"))
  ;; But dots in filenames are preserved
  (should (string= (ai-code--sanitize-generated-path-name "config.json")
                   "config.json")))

(ert-deftest ai-code-test-sanitize-newlines ()
  "Test that newlines are handled by taking only the first line."
  (should (string= (ai-code--sanitize-generated-path-name "file.txt\nsome extra text")
                   "file.txt"))
  (should (string= (ai-code--sanitize-generated-path-name "first line\nsecond line\nthird")
                   "first_line")))

(ert-deftest ai-code-test-sanitize-whitespace ()
  "Test that leading and trailing whitespace is removed."
  (should (string= (ai-code--sanitize-generated-path-name "  file.txt  ")
                   "file.txt"))
  (should (string= (ai-code--sanitize-generated-path-name "\t\ntest.js\n\t")
                   "test.js")))

(ert-deftest ai-code-test-sanitize-empty-input ()
  "Test that empty or whitespace-only input returns empty string."
  (should (string= (ai-code--sanitize-generated-path-name "") ""))
  (should (string= (ai-code--sanitize-generated-path-name "   ") ""))
  (should (string= (ai-code--sanitize-generated-path-name "\n\t ") ""))
  (should (string= (ai-code--sanitize-generated-path-name "\n\n\n") ""))
  (should (string= (ai-code--sanitize-generated-path-name nil) "")))

(ert-deftest ai-code-test-sanitize-nested-paths-flattened ()
  "Test that nested paths have slashes replaced with underscores."
  (should (string= (ai-code--sanitize-generated-path-name "src/components/button.js")
                   "src_components_button.js"))
  (should (string= (ai-code--sanitize-generated-path-name "lib/utils/helper_functions.py")
                   "lib_utils_helper_functions.py")))

(ert-deftest ai-code-test-sanitize-trailing-delimiters ()
  "Test that trailing underscores and dots are removed."
  (should (string= (ai-code--sanitize-generated-path-name "file___")
                   "file"))
  (should (string= (ai-code--sanitize-generated-path-name "dir///")
                   "dir"))
  (should (string= (ai-code--sanitize-generated-path-name "test...")
                   "test")))

;;; Tests for ai-code--generate-file-or-dir-name-with-gptel

(ert-deftest ai-code-test-generate-name-with-gptel-success ()
  "Test successful name generation with GPTel."
  (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
             (lambda (prompt)
               ;; Simulate GPTel returning a suggested name
               "user_service.py")))
    (let ((result (ai-code--generate-file-or-dir-name-with-gptel
                   "Create a user service module"
                   "file")))
      (should (string= result "user_service.py")))))

(ert-deftest ai-code-test-generate-name-with-gptel-sanitizes-output ()
  "Test that GPTel output is sanitized."
  (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
             (lambda (prompt)
               ;; Simulate GPTel returning a name with special chars
               "User Service!@#.py")))
    (let ((result (ai-code--generate-file-or-dir-name-with-gptel
                   "Create a user service module"
                   "file")))
      (should (string= result "user_service_.py")))))

(ert-deftest ai-code-test-generate-name-with-gptel-error-fallback ()
  "Test that GPTel errors fall back to description."
  (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
             (lambda (prompt)
               ;; Simulate GPTel error
               (error "GPTel connection failed"))))
    (let ((result (ai-code--generate-file-or-dir-name-with-gptel
                   "my test file"
                   "file")))
      ;; Should fallback to sanitized description
      (should (string= result "my_test_file")))))

(ert-deftest ai-code-test-generate-name-with-gptel-multiline-response ()
  "Test that multiline GPTel responses are handled correctly."
  (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
             (lambda (prompt)
               ;; Simulate GPTel returning multiple lines
               "user_service.py\nThis is a user service module\nSome more text")))
    (let ((result (ai-code--generate-file-or-dir-name-with-gptel
                   "Create a user service module"
                   "file")))
      ;; Should only use first line
      (should (string= result "user_service.py")))))

(ert-deftest ai-code-test-generate-name-with-gptel-nested-path ()
  "Test that GPTel nested path suggestions have slashes flattened."
  (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
             (lambda (prompt)
               "src/services/user_service.py")))
    (let ((result (ai-code--generate-file-or-dir-name-with-gptel
                   "Create a user service in the services directory"
                   "file")))
      (should (string= result "src_services_user_service.py")))))

;;; Tests for ai-code-create-file-or-dir

(ert-deftest ai-code-test-create-file-basic ()
  "Test basic file creation without GPTel."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (opened-in-other-window nil)
         (ai-code-task-use-gptel-filename nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "test file")
                   ((string-match-p "Confirm" prompt) "test.txt"))))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)
                  (setq opened-in-other-window t)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify file was created
       (should opened-in-other-window)
       (should created-file)
       (should (string-suffix-p "test.txt" created-file))
       (should (file-exists-p created-file))))))

(ert-deftest ai-code-test-create-directory-basic ()
  "Test basic directory creation without GPTel."
  (ai-code-file-with-test-env
   (let ((created-dir nil)
         (opened-in-dired nil)
         (ai-code-task-use-gptel-filename nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "directory"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "test directory")
                   ((string-match-p "Confirm" prompt) "test_dir"))))
               ((symbol-function 'dired-other-window)
                (lambda (dirname)
                  (setq created-dir dirname)
                  (setq opened-in-dired t)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify directory was created
       (should opened-in-dired)
       (should created-dir)
       (should (string-suffix-p "test_dir" created-dir))
       (should (file-directory-p created-dir))))))

(ert-deftest ai-code-test-create-file-with-gptel ()
  "Test file creation with GPTel name generation."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (ai-code-task-use-gptel-filename t))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "user authentication module")
                   ((string-match-p "Confirm" prompt) initial-input)))) ; Accept suggested name
               ((symbol-function 'ai-code-call-gptel-sync)
                (lambda (prompt)
                  "auth_module.py"))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify GPTel-generated name was used
       (should created-file)
       (should (string-suffix-p "auth_module.py" created-file))
       (should (file-exists-p created-file))))))

(ert-deftest ai-code-test-create-file-with-gptel-error ()
  "Test file creation when GPTel fails."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (ai-code-task-use-gptel-filename t))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "test file")
                   ((string-match-p "Confirm" prompt) initial-input)))) ; Accept fallback
               ((symbol-function 'ai-code-call-gptel-sync)
                (lambda (prompt)
                  (error "GPTel connection failed")))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify fallback name was used
       (should created-file)
       (should (string-suffix-p "test_file" created-file))
       (should (file-exists-p created-file))))))

(ert-deftest ai-code-test-create-file-uses-radar-style-prefix ()
  "Test radar style text uses the same prefix rule as task files."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (ai-code-task-use-gptel-filename nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt _collection &rest _args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt)
                    "rdar://12345 Fix crash on startup")
                   ((string-match-p "Confirm" prompt)
                    (should (string= initial-input
                                     "rdar_12345_rdar_12345_fix_crash_on_startup"))
                    initial-input))))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest _args) nil)))
       (ai-code-create-file-or-dir)
       (should created-file)
       (should (string-suffix-p
                "rdar_12345_rdar_12345_fix_crash_on_startup"
                created-file))
       (should (file-exists-p created-file))))))

(ert-deftest ai-code-test-create-file-empty-description-error ()
  "Test that empty description raises user-error."
  (ai-code-file-with-test-env
   (let ((ai-code-task-use-gptel-filename nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  ""))) ; Empty description
       ;; Should raise user-error
       (should-error (ai-code-create-file-or-dir) :type 'user-error)))))

(ert-deftest ai-code-test-create-file-empty-confirmed-name-error ()
  "Test that empty confirmed name raises user-error."
  (ai-code-file-with-test-env
   (let ((ai-code-task-use-gptel-filename nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "test file")
                   ((string-match-p "Confirm" prompt) "   "))))) ; Whitespace only
       ;; Should raise user-error
       (should-error (ai-code-create-file-or-dir) :type 'user-error)))))

(ert-deftest ai-code-test-create-nested-file ()
  "Test creating file with nested path input gets flattened."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (ai-code-task-use-gptel-filename nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "nested file")
                   ((string-match-p "Confirm" prompt) "src_lib_utils.js"))))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify file was created with flattened name
       (should created-file)
       (should (string-suffix-p "src_lib_utils.js" created-file))
       (should (file-exists-p created-file))))))

(ert-deftest ai-code-test-create-file-user-confirmation-flow ()
  "Test that user can modify GPTel-suggested name."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (ai-code-task-use-gptel-filename t)
         (confirmation-shown nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "database connection")
                   ((string-match-p "Confirm" prompt)
                    (progn
                      (setq confirmation-shown t)
                      ;; User modifies the suggested name
                      (should (string= initial-input "db_connection.py"))
                      "database.py")))))
               ((symbol-function 'ai-code-call-gptel-sync)
                (lambda (prompt)
                  "db_connection.py"))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify user was shown confirmation and their choice was used
       (should confirmation-shown)
       (should created-file)
       (should (string-suffix-p "database.py" created-file))
       (should (file-exists-p created-file))))))

(ert-deftest ai-code-test-create-file-fallback-when-gptel-returns-empty ()
  "Test fallback to default name when GPTel returns empty string."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (ai-code-task-use-gptel-filename t))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "test")
                   ((string-match-p "Confirm" prompt)
                    ;; Should get default fallback
                    (should (string= initial-input "new_file.txt"))
                    initial-input))))
               ((symbol-function 'ai-code-call-gptel-sync)
                (lambda (prompt)
                  "")) ; GPTel returns empty
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify fallback was used
       (should created-file)
       (should (string-suffix-p "new_file.txt" created-file))))))

(ert-deftest ai-code-test-create-directory-fallback-when-gptel-returns-empty ()
  "Test fallback to default name when GPTel returns empty string for directory."
  (ai-code-file-with-test-env
   (let ((created-dir nil)
         (ai-code-task-use-gptel-filename t))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "directory"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "test")
                   ((string-match-p "Confirm" prompt)
                    ;; Should get default fallback
                    (should (string= initial-input "new_dir"))
                    initial-input))))
               ((symbol-function 'ai-code-call-gptel-sync)
                (lambda (prompt)
                  "")) ; GPTel returns empty
               ((symbol-function 'dired-other-window)
                (lambda (dirname)
                  (setq created-dir dirname)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify fallback was used
       (should created-dir)
       (should (string-suffix-p "new_dir" created-dir))))))

(ert-deftest ai-code-test-create-file-from-dired-mode ()
  "Test file creation from dired-mode uses dired directory."
  (ai-code-file-with-test-env
   (let ((created-file nil)
         (ai-code-task-use-gptel-filename nil)
         (dired-dir (expand-file-name "subdir" test-dir)))
     ;; Create a subdirectory
     (make-directory dired-dir t)
     (cl-letf (((symbol-function 'derived-mode-p)
                (lambda (mode)
                  (eq mode 'dired-mode)))
               ((symbol-function 'dired-current-directory)
                (lambda ()
                  dired-dir))
               ((symbol-function 'completing-read)
                (lambda (prompt collection &rest args)
                  "file"))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input)
                  (cond
                   ((string-match-p "Describe" prompt) "test")
                   ((string-match-p "Confirm" prompt)
                    ;; Verify prompt shows dired directory
                    (should (string-match-p "subdir" prompt))
                    "test.txt"))))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify file was created in dired directory
       (should created-file)
       (should (string-prefix-p dired-dir created-file))
       (should (file-exists-p created-file))))))

(ert-deftest ai-code-test-apply-prompt-on-current-file-uses-send-time-auto-test-suffix ()
  "Test that apply-prompt uses send-time resolved suffix when auto test is enabled."
  (let ((captured-command nil)
        (resolved-called nil)
        (ai-code-use-prompt-suffix t)
        (ai-code-prompt-suffix nil)
        (ai-code-auto-test-type 'ask-me)
        (ai-code-auto-test-suffix nil)
        (ai-code-cli "claude")
        (ai-code-sed-command "sed"))
    (cl-letf (((symbol-function 'ai-code-read-string)
               (lambda (&rest _args) "Refactor this"))
              ((symbol-function 'ai-code--resolve-auto-test-suffix-for-send)
               (lambda ()
                 (setq resolved-called t)
                 "RUN TESTS"))
              ((symbol-function 'compilation-start)
               (lambda (command &rest _args)
                 (setq captured-command command)
                 nil)))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/sample.py")
        (ai-code-apply-prompt-on-current-file))
      (should resolved-called)
      (should (string-match-p "RUN\\\\ TESTS" captured-command)))))

(ert-deftest ai-code-test-apply-prompt-on-current-file-errors-for-agent-shell-backend ()
  "Ensure `ai-code-apply-prompt-on-current-file' rejects the agent-shell backend."
  (let ((compilation-called nil)
        (ai-code-selected-backend 'agent-shell)
        (ai-code-cli "agent-shell")
        (ai-code-sed-command "sed"))
    (cl-letf (((symbol-function 'ai-code-read-string)
               (lambda (&rest _args) "Refactor this"))
              ((symbol-function 'compilation-start)
               (lambda (&rest _args)
                 (setq compilation-called t)
                 nil)))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/sample.py")
        (should-error (ai-code-apply-prompt-on-current-file)
                      :type 'user-error)))
    (should (null compilation-called))))

(ert-deftest ai-code-test-apply-prompt-on-current-file-errors-for-eca-backend ()
  "Ensure `ai-code-apply-prompt-on-current-file' rejects the eca backend."
  (let ((compilation-called nil)
        (ai-code-selected-backend 'eca)
        (ai-code-cli nil)
        (ai-code-sed-command "sed"))
    (cl-letf (((symbol-function 'ai-code-read-string)
               (lambda (&rest _args) "Refactor this"))
              ((symbol-function 'compilation-start)
               (lambda (&rest _args)
                 (setq compilation-called t)
                 nil)))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/sample.py")
        (should-error (ai-code-apply-prompt-on-current-file)
                      :type 'user-error)))
    (should (null compilation-called))))

;;; Tests for ai-code--git-root

(ert-deftest ai-code-test-git-root-returns-truename ()
  "Test that `ai-code--git-root' returns file-truename of magit-toplevel."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) "/some/symlinked/path/")))
    (cl-letf (((symbol-function 'file-truename)
               (lambda (path) "/some/real/path/")))
      (should (string= (ai-code--git-root) "/some/real/path/")))))

(ert-deftest ai-code-test-git-root-returns-nil-when-not-in-repo ()
  "Test that `ai-code--git-root' returns nil when not in a git repo."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) nil)))
    (should (null (ai-code--git-root)))))

(ert-deftest ai-code-test-git-root-passes-dir-argument ()
  "Test that `ai-code--git-root' passes DIR argument to magit-toplevel."
  (let ((received-dir nil))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda (&optional dir) (setq received-dir dir) "/path/"))
             ((symbol-function 'file-truename)
              (lambda (path) path)))
      (ai-code--git-root "/custom/dir/")
      (should (string= received-dir "/custom/dir/")))))

(ert-deftest ai-code-test-git-root-handles-error-gracefully ()
  "Test that `ai-code--git-root' returns nil when magit-toplevel errors."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda (&optional _dir) (error "Not a git repository"))))
    (should (null (ai-code--git-root)))))

;;; Tests for ai-code-context-action with completing-read

(ert-deftest ai-code-test-context-action-add-calls-add-context ()
  "Test that selecting 'Add context' calls `ai-code-add-context' and lists."
  (let ((add-called nil)
        (list-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _args)
                 "Add context"))
              ((symbol-function 'ai-code-add-context)
               (lambda ()
                 (interactive)
                 (setq add-called t)))
              ((symbol-function 'ai-code-list-context)
               (lambda ()
                 (interactive)
                 (setq list-called t))))
      (ai-code-context-action nil)
      (should add-called)
      (should list-called))))

(ert-deftest ai-code-test-context-action-show-calls-list-context ()
  "Test that selecting 'Show context' calls `ai-code-list-context'."
  (let ((list-called nil)
        (add-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _args)
                 "Show context"))
              ((symbol-function 'ai-code-list-context)
               (lambda ()
                 (interactive)
                 (setq list-called t)))
              ((symbol-function 'ai-code-add-context)
               (lambda ()
                 (interactive)
                 (setq add-called t))))
      (ai-code-context-action nil)
      (should list-called)
      (should-not add-called))))

(ert-deftest ai-code-test-context-action-clear-calls-clear-context ()
  "Test that selecting 'Clear context' calls `ai-code-clear-context'."
  (let ((clear-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _args)
                 "Clear context"))
              ((symbol-function 'ai-code-clear-context)
               (lambda ()
                 (interactive)
                 (setq clear-called t))))
      (ai-code-context-action nil)
      (should clear-called))))

(ert-deftest ai-code-test-context-action-ignores-prefix-arg ()
  "Test that prefix arg no longer changes behavior; completing-read is used."
  (let ((completing-read-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _args)
                 (setq completing-read-called t)
                 "Show context"))
              ((symbol-function 'ai-code-list-context)
               (lambda ()
                 (interactive)
                 nil)))
      (ai-code-context-action '(4))
      (should completing-read-called))))

(ert-deftest ai-code-test-build-or-test-project-dispatches-test-project ()
  "Test selecting \"Test project\" dispatches to `ai-code-test-project'."
  (let ((test-project-called nil)
        (build-called nil)
        (run-test-called nil)
        (lint-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Test project"))
              ((symbol-function 'ai-code-test-project)
               (lambda ()
                 (setq test-project-called t)))
              ((symbol-function 'ai-code-build-project)
               (lambda ()
                 (setq build-called t)))
              ((symbol-function 'ai-code-run-test)
               (lambda ()
                 (setq run-test-called t)))
              ((symbol-function 'ai-code-lint-current-file)
               (lambda ()
                 (setq lint-called t))))
      (ai-code-build-or-test-project)
      (should test-project-called)
      (should-not build-called)
      (should-not run-test-called)
      (should-not lint-called))))

(ert-deftest ai-code-test-build-or-test-project-dispatches-lint-current-file ()
  "Test selecting \"Lint current file\" dispatches to `ai-code-lint-current-file'."
  (let ((test-project-called nil)
        (build-called nil)
        (run-test-called nil)
        (lint-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Lint current file"))
              ((symbol-function 'ai-code-test-project)
               (lambda ()
                 (setq test-project-called t)))
              ((symbol-function 'ai-code-build-project)
               (lambda ()
                 (setq build-called t)))
              ((symbol-function 'ai-code-run-test)
               (lambda ()
                 (setq run-test-called t)))
              ((symbol-function 'ai-code-lint-current-file)
               (lambda ()
                 (setq lint-called t))))
      (ai-code-build-or-test-project)
      (should lint-called)
      (should-not test-project-called)
      (should-not build-called)
      (should-not run-test-called))))

(ert-deftest ai-code-test-test-project-builds-ai-prompt-with-at-test-and-failure-follow-up ()
  "Test `ai-code-test-project' sends a project-wide @test prompt with follow-up instructions."
  (let ((captured-initial-input nil)
        (captured-prompt nil))
    (cl-letf (((symbol-function 'projectile-project-root)
               (lambda () "/tmp/demo-project/"))
              ((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/tmp/demo-project/"))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda () "Repo context goes here"))
              ((symbol-function 'ai-code-read-string)
               (lambda (_prompt initial-input)
                 (setq captured-initial-input initial-input)
                 initial-input))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq captured-prompt prompt))))
      (ai-code-test-project)
      (should (string-match-p "Run test on the whole project" captured-initial-input))
      (should (string-match-p "Project root: /tmp/demo-project/" captured-initial-input))
      (should (string-match-p "Repo context goes here" captured-initial-input))
      (should (string-match-p "If test fails" captured-initial-input))
      (should (string-match-p "Analyze the test results" captured-initial-input))
      (should (string-match-p "code fix suggestions" captured-initial-input))
      (should (string= captured-prompt captured-initial-input)))))

(ert-deftest ai-code-test-lint-current-file-builds-ai-prompt-with-diagnostics-follow-up ()
  "Test `ai-code-lint-current-file' prompts AI to use diagnostics on current file."
  (let ((captured-initial-input nil)
        (captured-prompt nil))
    (cl-letf (((symbol-function 'ai-code--format-repo-context-info)
               (lambda () "Repo context goes here"))
              ((symbol-function 'ai-code-read-string)
               (lambda (_prompt initial-input)
                 (setq captured-initial-input initial-input)
                 initial-input))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq captured-prompt prompt))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/demo-project/sample.el")
        (ai-code-lint-current-file))
      (should (string-match-p "get_diagnostics" captured-initial-input))
      (should (string-match-p "Current file: /tmp/demo-project/sample.el" captured-initial-input))
      (should (string-match-p "If lint errors are found" captured-initial-input))
      (should (string-match-p "error analysis" captured-initial-input))
      (should (string-match-p "code fix suggestions" captured-initial-input))
      (should (string= captured-prompt captured-initial-input)))))

(provide 'test_ai-code-file)

;;; test_ai-code-file.el ends here
