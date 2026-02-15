;;; test_ai-code-file.el --- Tests for ai-code-file -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-file.el, particularly for file/directory creation
;; with GPTel integration.

;;; Code:

(require 'ert)
(require 'ai-code-file)
(require 'cl-lib)

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

(ert-deftest ai-code-test-sanitize-multiple-slashes ()
  "Test that multiple consecutive slashes are collapsed to one."
  (should (string= (ai-code--sanitize-generated-path-name "path//to///file.txt")
                   "path/to/file.txt"))
  (should (string= (ai-code--sanitize-generated-path-name "dir////subdir/file.js")
                   "dir/subdir/file.js")))

(ert-deftest ai-code-test-sanitize-path-traversal-prevention ()
  "Test that path traversal attempts are sanitized."
  ;; Leading dots and slashes should be removed
  (should (string= (ai-code--sanitize-generated-path-name "../../../etc/passwd")
                   "etc/passwd"))
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

(ert-deftest ai-code-test-sanitize-nested-paths ()
  "Test that nested paths are properly sanitized."
  (should (string= (ai-code--sanitize-generated-path-name "src/components/button.js")
                   "src/components/button.js"))
  (should (string= (ai-code--sanitize-generated-path-name "lib/utils/helper_functions.py")
                   "lib/utils/helper_functions.py")))

(ert-deftest ai-code-test-sanitize-trailing-delimiters ()
  "Test that trailing underscores, dots, and slashes are removed."
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
  "Test that GPTel can suggest nested paths."
  (cl-letf (((symbol-function 'ai-code-call-gptel-sync)
             (lambda (prompt)
               "src/services/user_service.py")))
    (let ((result (ai-code--generate-file-or-dir-name-with-gptel
                   "Create a user service in the services directory"
                   "file")))
      (should (string= result "src/services/user_service.py")))))

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
  "Test creating file in nested directory structure."
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
                   ((string-match-p "Confirm" prompt) "src/lib/utils.js"))))
               ((symbol-function 'find-file-other-window)
                (lambda (file)
                  (setq created-file file)))
               ((symbol-function 'message)
                (lambda (&rest args) nil)))
       ;; Call the function
       (ai-code-create-file-or-dir)
       ;; Verify nested structure was created
       (should created-file)
       (should (string-suffix-p "src/lib/utils.js" created-file))
       (should (file-exists-p created-file))
       ;; Verify parent directories were created
       (should (file-directory-p (expand-file-name "src" test-dir)))
       (should (file-directory-p (expand-file-name "src/lib" test-dir)))))))

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

(provide 'test_ai-code-file)

;;; test_ai-code-file.el ends here
