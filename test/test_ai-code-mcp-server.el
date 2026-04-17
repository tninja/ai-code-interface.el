;;; test_ai-code-mcp-server.el --- Tests for ai-code-mcp-server.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the MCP tools server core and built-in Emacs tools.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'project)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-input)
(require 'ai-code-mcp-server nil t)

(defun ai-code-test-mcp--content-text (result)
  "Extract text content from RESULT."
  (alist-get 'text
             (car (alist-get 'content result))))

(defun ai-code-test-mcp--read-json-payload (result)
  "Decode the JSON text content from RESULT."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (json-read-from-string
     (ai-code-test-mcp--content-text result))))

(cl-defstruct ai-code-test-mcp-mock-diagnostic
  beg end type text backend)

(defconst ai-code-test-mcp--builtin-tool-names
  '("buffer_query"
    "get_diagnostics"
    "get_feature_load_state"
    "get_function_info"
    "get_last_error_backtrace"
    "get_project_buffers"
    "get_project_files"
    "get_recent_messages"
    "get_variable_binding_info"
    "get_variable_value"
    "imenu_list_symbols"
    "notify_user"
    "project_info"
    "treesit_info"
    "xref_find_definitions_at_point"
    "xref_find_references")
  "Expected built-in MCP tool names.")

(ert-deftest ai-code-test-mcp-dispatch-initialize-returns-server-info ()
  "Initialize should expose MCP protocol metadata."
  (should (fboundp 'ai-code-mcp-dispatch))
  (let ((result (ai-code-mcp-dispatch "initialize")))
    (should (equal "2024-11-05"
                   (alist-get 'protocolVersion result)))
    (should (alist-get 'tools (alist-get 'capabilities result)))
    (should (equal "ai-code-mcp-tools"
                   (alist-get 'name (alist-get 'serverInfo result))))))

(ert-deftest ai-code-test-mcp-make-tool-registers-schema-and-dispatches-call ()
  "Custom tools should appear in tools/list and run through tools/call."
  (let ((ai-code-mcp-server-tools nil))
    (ai-code-mcp-make-tool
     :function (lambda (name punctuation)
                 (concat "Hello, " name punctuation))
     :name "greet_user"
     :description "Return a greeting."
     :args '((:name "name"
              :type string
              :description "Name to greet.")
             (:name "punctuation"
              :type string
              :description "Trailing punctuation."
              :optional t)))
    (let* ((tool-entry (car (alist-get 'tools (ai-code-mcp-dispatch "tools/list"))))
           (input-schema (alist-get 'inputSchema tool-entry))
           (properties (alist-get 'properties input-schema))
           (required (append (alist-get 'required input-schema) nil)))
      (should (equal "greet_user" (alist-get 'name tool-entry)))
      (should (equal "string"
                     (alist-get 'type (alist-get 'name properties))))
      (should (equal '("name") required)))
    (let ((result (ai-code-mcp-dispatch
                   "tools/call"
                   '((name . "greet_user")
                     (arguments . ((name . "Codex")
                                   (punctuation . "!")))))))
      (should (equal "Hello, Codex!"
                     (ai-code-test-mcp--content-text result))))))

(ert-deftest ai-code-test-mcp-tools-call-missing-required-argument-errors ()
  "Missing required arguments should fail with a clear error."
  (let ((ai-code-mcp-server-tools nil))
    (ai-code-mcp-make-tool
     :function (lambda (name) name)
     :name "echo_name"
     :description "Echo a name."
     :args '((:name "name"
              :type string
              :description "Name to echo.")))
    (should-error
     (ai-code-mcp-dispatch
      "tools/call"
      '((name . "echo_name")
        (arguments . ())))
     :type 'error)))

(ert-deftest ai-code-test-mcp-session-context-roundtrip ()
  "Session registration should provide project-local execution context."
  (should (fboundp 'ai-code-mcp-register-session))
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (session-id "session-1")
        (project-dir (make-temp-file "ai-code-mcp-project-" t))
        (buffer (generate-new-buffer " *ai-code-mcp-session*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session session-id project-dir buffer)
          (should (equal project-dir
                         (plist-get (ai-code-mcp-get-session-context session-id)
                                    :project-dir)))
          (let ((captured-directory nil))
            (let ((ai-code-mcp--current-session-id session-id))
              (ai-code-mcp-with-session-context nil
                (setq captured-directory default-directory)))
            (should (equal (file-name-as-directory project-dir)
                           captured-directory))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-builtins-setup-registers-common-tools-once ()
  "Built-in setup should register the common Emacs tools without duplicates."
  (let ((ai-code-mcp-server-tools nil))
    (ai-code-mcp-builtins-setup)
    (ai-code-mcp-builtins-setup)
    (let ((tool-names (sort (mapcar (lambda (tool)
                                      (plist-get tool :name))
                                    ai-code-mcp-server-tools)
                            #'string<)))
       (should (equal '("buffer_query"
                        "get_diagnostics"
                        "get_feature_load_state"
                        "get_function_info"
                        "get_last_error_backtrace"
                        "get_project_buffers"
                        "get_project_files"
                        "get_recent_messages"
                        "get_variable_binding_info"
                        "get_variable_value"
                        "imenu_list_symbols"
                        "notify_user"
                        "project_info"
                        "treesit_info"
                        "xref_find_definitions_at_point"
                       "xref_find_references")
                     tool-names)))))

(ert-deftest ai-code-test-mcp-tools-list-registers-builtins-by-default ()
  "Tools list should expose built-in tools without manual setup."
  (let ((ai-code-mcp-server-tools nil))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (tool-names (sort (mapcar (lambda (tool)
                                       (alist-get 'name tool))
                                     (alist-get 'tools tools-result))
                             #'string<)))
      (should (equal ai-code-test-mcp--builtin-tool-names
                     tool-names)))))

(ert-deftest ai-code-test-mcp-notify-user-calls-message-and-beep ()
  "Notification tool should relay the message text and beep."
  (let ((ai-code-mcp-server-tools nil)
        captured-message
        beep-called)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq captured-message
                       (apply #'format format-string args))
                 captured-message))
              ((symbol-function 'beep)
               (lambda (&rest _args)
                 (setq beep-called t))))
       (let ((result (ai-code-mcp-dispatch
                      "tools/call"
                      '((name . "notify_user")
                        (arguments . ((message_text . "Build finished")))))))
        (should (equal "Build finished" captured-message))
        (should beep-called)
        (should (equal "Notified user: Build finished"
                       (ai-code-test-mcp--content-text result)))))))

(ert-deftest ai-code-test-mcp-get-variable-value-returns-bound-variable ()
  "Variable value tool should stringify the requested Emacs variable."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-diagnostics-backend 'flymake))
    (let ((result (ai-code-mcp-dispatch
                   "tools/call"
                   '((name . "get_variable_value")
                     (arguments . ((variable_name . "ai-code-mcp-diagnostics-backend")))))))
      (should (equal "flymake"
                     (ai-code-test-mcp--content-text result))))))

(ert-deftest ai-code-test-mcp-get-variable-value-reports-missing-variable-without-interning ()
  "Unknown variable names should not be interned and should return a friendly error."
  (let* ((ai-code-mcp-server-tools nil)
         (variable-name "ai-code-test-mcp-missing-variable")
         (result nil))
    (when (intern-soft variable-name)
      (ert-fail "Test requires a missing symbol name"))
    (setq result
          (ai-code-mcp-dispatch
           "tools/call"
           `((name . "get_variable_value")
             (arguments . ((variable_name . ,variable-name))))))
    (should (equal (format "Variable not found: %s" variable-name)
                   (ai-code-test-mcp--content-text result)))
    (should-not (intern-soft variable-name))))

(ert-deftest ai-code-test-mcp-get-variable-value-reports-unbound-variable ()
  "Unbound variable names should return a friendly error."
  (let ((ai-code-mcp-server-tools nil)
        (variable-name "ai-code-test-mcp-unbound-variable"))
    (unwind-protect
        (let ((symbol (intern variable-name)))
          (setplist symbol nil)
          (makunbound symbol)
          (let ((result (ai-code-mcp-dispatch
                         "tools/call"
                         `((name . "get_variable_value")
                           (arguments . ((variable_name . ,variable-name)))))))
            (should (equal (format "Variable is unbound: %s" variable-name)
                           (ai-code-test-mcp--content-text result)))))
      (unintern variable-name obarray))))

(ert-deftest ai-code-test-mcp-tools-list-describes-variable-value-as-printed-representation ()
  "Variable value tool metadata should match the returned representation."
  (let ((ai-code-mcp-server-tools nil))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (variable-tool (seq-find
                           (lambda (tool)
                             (equal "get_variable_value" (alist-get 'name tool)))
                           (alist-get 'tools tools-result))))
      (should variable-tool)
      (should (equal "Get the printed representation of an Emacs variable value by name."
                     (alist-get 'description variable-tool))))))

(ert-deftest ai-code-test-mcp-get-variable-binding-info-reports-default-and-local-values ()
  "Variable binding info should report current and default values."
  (let ((ai-code-mcp-server-tools nil)
        (variable-name "ai-code-test-mcp-buffer-local-variable")
        (buffer (generate-new-buffer " *ai-code-mcp-binding-info*")))
    (unwind-protect
        (progn
          (set-default (intern variable-name) 2)
          (with-current-buffer buffer
            (setq-local ai-code-test-mcp-buffer-local-variable 8))
          (let* ((payload
                  (ai-code-test-mcp--read-json-payload
                   (ai-code-mcp-dispatch
                    "tools/call"
                    `((name . "get_variable_binding_info")
                      (arguments . ((variable_name . ,variable-name)
                                    (buffer_name . ,(buffer-name buffer))))))))
                 (documentation-summary
                  (alist-get 'documentation_summary payload)))
            (should (equal t (alist-get 'exists payload)))
            (should (equal t (alist-get 'buffer_local payload)))
            (should (equal "8" (alist-get 'current_value_repr payload)))
            (should (equal "2" (alist-get 'default_value_repr payload)))
            (should (equal (buffer-name buffer)
                           (alist-get 'buffer_name payload)))
            (should (stringp documentation-summary))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (intern-soft variable-name)
        (unintern variable-name obarray)))))

(ert-deftest ai-code-test-mcp-get-variable-binding-info-reports-missing-variable ()
  "Variable binding info should report a missing variable without interning it."
  (let ((ai-code-mcp-server-tools nil)
        (variable-name "ai-code-test-mcp-missing-binding-variable"))
    (when (intern-soft variable-name)
      (ert-fail "Test requires a missing symbol name"))
    (let ((payload
           (ai-code-test-mcp--read-json-payload
            (ai-code-mcp-dispatch
             "tools/call"
             `((name . "get_variable_binding_info")
               (arguments . ((variable_name . ,variable-name))))))))
      (should (equal :json-false (alist-get 'exists payload)))
      (should-not (alist-get 'current_value_repr payload))
      (should-not (intern-soft variable-name)))))

(ert-deftest ai-code-test-mcp-get-function-info-reports-alias-and-advice-state ()
  "Function info should report alias and advice metadata."
  (let ((ai-code-mcp-server-tools nil)
        (base-name "ai-code-test-mcp-base-function")
        (alias-name "ai-code-test-mcp-aliased-function")
        (advice-name "ai-code-test-mcp-around-advice"))
    (unwind-protect
        (progn
          (fset (intern base-name) (lambda () :ok))
          (defalias (intern alias-name) (intern base-name))
          (fset (intern advice-name)
                (lambda (fn &rest args)
                  (apply fn args)))
          (advice-add (intern alias-name) :around (intern advice-name))
          (let ((payload
                 (ai-code-test-mcp--read-json-payload
                  (ai-code-mcp-dispatch
                   "tools/call"
                   `((name . "get_function_info")
                     (arguments . ((function_name . ,alias-name))))))))
            (should (equal t (alist-get 'exists payload)))
            (should (equal "lambda" (alist-get 'kind payload)))
            (should (equal t (alist-get 'advised payload)))
            (should (equal base-name (alist-get 'aliased_to payload)))))
      (when (fboundp (intern-soft alias-name))
        (ignore-errors
          (advice-remove (intern alias-name) (intern advice-name)))
        (fmakunbound (intern alias-name)))
      (when (fboundp (intern-soft base-name))
        (fmakunbound (intern base-name)))
      (when (fboundp (intern-soft advice-name))
        (fmakunbound (intern advice-name)))
      (when (intern-soft alias-name)
        (unintern alias-name obarray))
      (when (intern-soft base-name)
        (unintern base-name obarray))
      (when (intern-soft advice-name)
        (unintern advice-name obarray)))))

(ert-deftest ai-code-test-mcp-get-function-info-reports-missing-functions ()
  "Function info should report missing function symbols cleanly."
  (let ((ai-code-mcp-server-tools nil)
        (function-name "ai-code-test-mcp-missing-function"))
    (when (intern-soft function-name)
      (ert-fail "Test requires a missing function symbol"))
    (let ((payload
           (ai-code-test-mcp--read-json-payload
            (ai-code-mcp-dispatch
             "tools/call"
             `((name . "get_function_info")
               (arguments . ((function_name . ,function-name))))))))
      (should (equal :json-false (alist-get 'exists payload)))
      (should-not (intern-soft function-name)))))

(ert-deftest ai-code-test-mcp-get-feature-load-state-reports-loaded-feature-details ()
  "Feature load state should report loaded features and their providers."
  (let ((ai-code-mcp-server-tools nil))
    (let* ((payload
            (ai-code-test-mcp--read-json-payload
             (ai-code-mcp-dispatch
              "tools/call"
              '((name . "get_feature_load_state")
                (arguments . ((feature_name . "json")))))))
           (provided-by-files (append (alist-get 'provided_by_files payload) nil))
           (load-path-matches (append (alist-get 'load_path_matches payload) nil)))
      (should (equal t (alist-get 'loaded payload)))
      (should (stringp (alist-get 'library_path payload)))
      (should provided-by-files)
      (should load-path-matches))))

(ert-deftest ai-code-test-mcp-get-feature-load-state-reports-missing-features ()
  "Feature load state should report missing features without errors."
  (let ((ai-code-mcp-server-tools nil)
        (feature-name "ai-code-test-mcp-missing-feature"))
    (when (intern-soft feature-name)
      (ert-fail "Test requires a missing feature symbol"))
    (let ((payload
           (ai-code-test-mcp--read-json-payload
            (ai-code-mcp-dispatch
             "tools/call"
             `((name . "get_feature_load_state")
               (arguments . ((feature_name . ,feature-name))))))))
      (should (equal :json-false (alist-get 'loaded payload)))
      (should-not (alist-get 'library_path payload)))))

(ert-deftest ai-code-test-mcp-get-recent-messages-returns-latest-messages ()
  "Recent messages should return the latest entries from `*Messages*'."
  (let ((ai-code-mcp-server-tools nil))
    (message "ai-code-mcp-server-test-message")
    (let* ((payload
            (ai-code-test-mcp--read-json-payload
             (ai-code-mcp-dispatch
              "tools/call"
              '((name . "get_recent_messages")
                (arguments . ((limit . 1)))))))
           (messages (alist-get 'messages payload)))
      (should (equal t (alist-get 'ok payload)))
      (should (= 1 (length messages)))
      (should (string-match-p "ai-code-mcp-server-test-message"
                              (aref messages 0))))))

(ert-deftest ai-code-test-mcp-get-last-error-backtrace-reports-empty-state ()
  "Last error backtrace should report when no error has been captured."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp--last-error-record nil))
    (let ((payload
           (ai-code-test-mcp--read-json-payload
            (ai-code-mcp-dispatch
             "tools/call"
             '((name . "get_last_error_backtrace")
               (arguments . ()))))))
      (should (equal :json-false (alist-get 'recorded payload)))
      (should-not (alist-get 'error_message payload))
      (should-not (alist-get 'frames payload)))))

(ert-deftest ai-code-test-mcp-get-last-error-backtrace-returns-recorded-error ()
  "Last error backtrace should return the recorded error snapshot."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp--last-error-record nil))
    (cl-letf (((symbol-function 'backtrace-frames)
               (lambda (&optional _base)
                 '((t ai-code-test-mcp-frame-a nil nil)
                   (t ai-code-test-mcp-frame-b ("x") nil)))))
      (ai-code-mcp--record-command-error '(error "Boom") 'command t))
    (let* ((payload
            (ai-code-test-mcp--read-json-payload
             (ai-code-mcp-dispatch
              "tools/call"
              '((name . "get_last_error_backtrace")
                (arguments . ())))))
           (frames (append (alist-get 'frames payload) nil)))
      (should (equal t (alist-get 'recorded payload)))
      (should (equal "error" (alist-get 'error_symbol payload)))
      (should (equal "Boom" (alist-get 'error_message payload)))
      (should (equal "command" (alist-get 'context payload)))
      (should (= 2 (alist-get 'frame_count payload)))
      (should (string-match-p "ai-code-test-mcp-frame-a"
                              (car frames))))))

(ert-deftest ai-code-test-mcp-tools-list-encodes-empty-input-schema-properties ()
  "No-argument tools should encode empty schema properties as an object."
  (let ((ai-code-mcp-server-tools nil))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (project-tool (seq-find
                          (lambda (tool)
                            (equal "project_info" (alist-get 'name tool)))
                          (alist-get 'tools tools-result)))
           (encoded (json-encode tools-result)))
      (should project-tool)
      (should (string-match-p
               "\"properties\":{}"
               encoded)))))

(ert-deftest ai-code-test-mcp-tools-call-runs-inside-session-context ()
  "Tool calls should run with the registered session buffer and directory."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (session-id "session-tools-call")
        (project-dir (make-temp-file "ai-code-mcp-tools-call-" t))
        (session-buffer (generate-new-buffer " *ai-code-mcp-tools-call*")))
    (unwind-protect
        (progn
          (with-current-buffer session-buffer
            (rename-buffer "session-context-buffer" t))
          (ai-code-mcp-register-session session-id project-dir session-buffer)
          (ai-code-mcp-make-tool
           :function (lambda ()
                       (format "buffer=%s dir=%s"
                               (buffer-name (current-buffer))
                               default-directory))
           :name "session_probe"
           :description "Report session buffer and directory."
           :args nil)
          (with-temp-buffer
            (let* ((ai-code-mcp--current-session-id session-id)
                   (result (ai-code-mcp-dispatch
                            "tools/call"
                            '((name . "session_probe")
                              (arguments . ()))))
                   (text (ai-code-test-mcp--content-text result)))
              (should (string-match-p "buffer=session-context-buffer" text))
              (should (string-match-p
                       (regexp-quote (file-name-as-directory project-dir))
                       text)))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-tools-call-get-diagnostics-returns-json-for-target-uri ()
  "Diagnostics tool should return JSON diagnostics for the requested file URI."
  (let* ((project-dir (make-temp-file "ai-code-mcp-diagnostics-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (file-uri (concat "file://" file-path))
         (session-buffer (generate-new-buffer " *ai-code-mcp-diagnostics-session*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-diagnostics")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (let ((diagnostic (make-ai-code-test-mcp-mock-diagnostic
                               :beg (point-min)
                               :end (line-end-position)
                               :type :warning
                               :text "Unused value"
                               :backend 'mock-backend)))
              (ai-code-mcp-register-session "session-diagnostics" project-dir session-buffer)
              (cl-letf (((symbol-function 'flymake-diagnostics)
                         (lambda (&rest _) (list diagnostic)))
                        ((symbol-function 'flymake-diagnostic-beg)
                         #'ai-code-test-mcp-mock-diagnostic-beg)
                        ((symbol-function 'flymake-diagnostic-end)
                         #'ai-code-test-mcp-mock-diagnostic-end)
                        ((symbol-function 'flymake-diagnostic-type)
                         #'ai-code-test-mcp-mock-diagnostic-type)
                        ((symbol-function 'flymake-diagnostic-backend)
                         #'ai-code-test-mcp-mock-diagnostic-backend)
                        ((symbol-function 'flymake-diagnostic-text)
                         #'ai-code-test-mcp-mock-diagnostic-text))
                (let ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol))
                  (let* ((payload (ai-code-test-mcp--content-text
                                   (ai-code-mcp-dispatch
                                    "tools/call"
                                    `((name . "get_diagnostics")
                                      (arguments . ((uri . ,file-uri)))))))
                         (items (json-read-from-string payload))
                         (entry (aref items 0))
                         (diagnostics (alist-get 'diagnostics entry))
                         (first-diagnostic (aref diagnostics 0))
                         (range (alist-get 'range first-diagnostic))
                         (start (alist-get 'start range)))
                    (should (equal file-uri (alist-get 'uri entry)))
                    (should (equal "Warning"
                                   (alist-get 'severity first-diagnostic)))
                    (should (equal "mock-backend"
                                   (alist-get 'source first-diagnostic)))
                    (should (equal "Unused value"
                                   (alist-get 'message first-diagnostic)))
                    (should (= 1 (alist-get 'line start)))
                    (should (= 0 (alist-get 'character start)))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-tools-call-get-diagnostics-project-results-use-canonical-file-uri ()
  "Project diagnostics should emit canonical file URIs."
  (let* ((project-dir (make-temp-file "ai-code-mcp-diagnostics-project-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (expected-uri (concat "file://" file-path))
         (session-buffer (generate-new-buffer " *ai-code-mcp-diagnostics-project*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-diagnostics-project")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (let ((diagnostic (make-ai-code-test-mcp-mock-diagnostic
                               :beg (point-min)
                               :end (line-end-position)
                               :type :warning
                               :text "Unused value"
                               :backend 'mock-backend)))
              (ai-code-mcp-register-session
               "session-diagnostics-project"
               project-dir
               session-buffer)
              (cl-letf (((symbol-function 'flymake-diagnostics)
                         (lambda (&rest _) (list diagnostic)))
                        ((symbol-function 'flymake-diagnostic-beg)
                         #'ai-code-test-mcp-mock-diagnostic-beg)
                        ((symbol-function 'flymake-diagnostic-end)
                         #'ai-code-test-mcp-mock-diagnostic-end)
                        ((symbol-function 'flymake-diagnostic-type)
                         #'ai-code-test-mcp-mock-diagnostic-type)
                        ((symbol-function 'flymake-diagnostic-backend)
                         #'ai-code-test-mcp-mock-diagnostic-backend)
                        ((symbol-function 'flymake-diagnostic-text)
                         #'ai-code-test-mcp-mock-diagnostic-text))
                (let ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol))
                  (let* ((payload (ai-code-test-mcp--content-text
                                   (ai-code-mcp-dispatch
                                    "tools/call"
                                    '((name . "get_diagnostics")
                                      (arguments . ())))))
                         (items (json-read-from-string payload))
                         (entry (aref items 0)))
                    (should (equal expected-uri
                                   (alist-get 'uri entry)))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-tools-call-get-diagnostics-accepts-localhost-file-uri ()
  "Diagnostics lookup should accept file URIs with localhost authority."
  (let* ((project-dir (make-temp-file "ai-code-mcp-diagnostics-localhost-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (file-uri (concat "file://localhost" file-path))
         (session-buffer (generate-new-buffer " *ai-code-mcp-diagnostics-localhost*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-diagnostics-localhost")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (let ((diagnostic (make-ai-code-test-mcp-mock-diagnostic
                               :beg (point-min)
                               :end (line-end-position)
                               :type :warning
                               :text "Unused value"
                               :backend 'mock-backend)))
              (ai-code-mcp-register-session
               "session-diagnostics-localhost"
               project-dir
               session-buffer)
              (cl-letf (((symbol-function 'flymake-diagnostics)
                         (lambda (&rest _) (list diagnostic)))
                        ((symbol-function 'flymake-diagnostic-beg)
                         #'ai-code-test-mcp-mock-diagnostic-beg)
                        ((symbol-function 'flymake-diagnostic-end)
                         #'ai-code-test-mcp-mock-diagnostic-end)
                        ((symbol-function 'flymake-diagnostic-type)
                         #'ai-code-test-mcp-mock-diagnostic-type)
                        ((symbol-function 'flymake-diagnostic-backend)
                         #'ai-code-test-mcp-mock-diagnostic-backend)
                        ((symbol-function 'flymake-diagnostic-text)
                         #'ai-code-test-mcp-mock-diagnostic-text))
                (let ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol))
                  (let* ((payload (ai-code-test-mcp--content-text
                                   (ai-code-mcp-dispatch
                                    "tools/call"
                                    `((name . "get_diagnostics")
                                      (arguments . ((uri . ,file-uri)))))))
                         (items (json-read-from-string payload)))
                    (should (= 1 (length items)))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-project-info-uses-session-project-dir ()
  "Project info should report the session project directory."
  (let* ((project-dir (make-temp-file "ai-code-mcp-project-info-" t))
         (file-a (expand-file-name "a.el" project-dir))
         (file-b (expand-file-name "nested/b.el" project-dir))
         (buffer (generate-new-buffer " *ai-code-mcp-project-info*"))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-2"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file-b) t)
          (with-temp-file file-a (insert "(message \"a\")\n"))
          (with-temp-file file-b (insert "(message \"b\")\n"))
          (ai-code-mcp-register-session "session-2" project-dir buffer)
          (let ((result (ai-code-mcp-project-info)))
            (should (string-match-p (regexp-quote project-dir) result))
            (should (string-match-p "Files: 2" result))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-imenu-list-symbols-returns-symbol-lines ()
  "Imenu tool should return named symbols with file-relative line numbers."
  (let* ((project-dir (make-temp-file "ai-code-mcp-imenu-" t))
         (file-path (expand-file-name "sample.el" project-dir)))
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "(defun alpha ()\n  t)\n\n")
            (insert "(defun beta ()\n  nil)\n"))
          (let ((result (ai-code-mcp-imenu-list-symbols file-path)))
            (should (member "sample.el:1: alpha" result))
            (should (member "sample.el:4: beta" result))))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-server-source-requires-seq-explicitly ()
  "The MCP server source should declare its seq dependency explicitly."
  (with-temp-buffer
    (insert-file-contents "ai-code-mcp-server.el")
    (goto-char (point-min))
    (should (search-forward "(require 'seq)" nil t))))

(ert-deftest ai-code-test-mcp-buffer-query-returns-selected-buffer-lines ()
  "Buffer query should return the requested line range from a live buffer."
  (let ((buffer (generate-new-buffer " *ai-code-mcp-buffer-query*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "alpha\nbeta\ngamma\ndelta\n")
          (should (equal "beta\ngamma"
                         (ai-code-mcp-buffer-query
                          (buffer-name buffer)
                          2
                          2))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-mcp-buffer-query-preserves-trailing-whitespace ()
  "Buffer query should preserve trailing whitespace in the selected text."
  (let ((buffer (generate-new-buffer " *ai-code-mcp-buffer-query-whitespace*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "alpha\nbeta  \n")
          (should (equal "beta  "
                         (ai-code-mcp-buffer-query
                          (buffer-name buffer)
                          2
                          1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-mcp-buffer-query-requires-positive-line-range ()
  "Buffer query should reject non-positive line range arguments."
  (let ((buffer (generate-new-buffer " *ai-code-mcp-buffer-query-range*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "alpha\nbeta\n")
          (should-error
           (ai-code-mcp-buffer-query (buffer-name buffer) 0 1))
          (should-error
           (ai-code-mcp-buffer-query (buffer-name buffer) 1 0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-mcp-get-project-files-returns-relative-project-paths ()
  "Project files should list regular files relative to the session project root."
  (let* ((project-dir (make-temp-file "ai-code-mcp-project-files-" t))
         (file-a (expand-file-name "alpha.el" project-dir))
         (file-b (expand-file-name "nested/beta.el" project-dir))
         (buffer (generate-new-buffer " *ai-code-mcp-project-files*"))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-project-files"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file-b) t)
          (with-temp-file file-a
            (insert "(message \"alpha\")\n"))
          (with-temp-file file-b
            (insert "(message \"beta\")\n"))
          (ai-code-mcp-register-session "session-project-files" project-dir buffer)
          (should (equal '("alpha.el" "nested/beta.el")
                         (sort (ai-code-mcp-get-project-files) #'string<))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-project-files-skips-hidden-directories ()
  "Project files should skip hidden directories such as .git."
  (let* ((project-dir (make-temp-file "ai-code-mcp-project-files-hidden-" t))
         (file-a (expand-file-name "alpha.el" project-dir))
         (file-b (expand-file-name "nested/beta.el" project-dir))
         (hidden-file (expand-file-name ".git/HEAD" project-dir))
         (buffer (generate-new-buffer " *ai-code-mcp-project-files-hidden*"))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-project-files-hidden"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file-b) t)
          (make-directory (file-name-directory hidden-file) t)
          (with-temp-file file-a
            (insert "(message \"alpha\")\n"))
          (with-temp-file file-b
            (insert "(message \"beta\")\n"))
          (with-temp-file hidden-file
            (insert "ref: refs/heads/main\n"))
          (ai-code-mcp-register-session
           "session-project-files-hidden"
           project-dir
           buffer)
          (should (equal '("alpha.el" "nested/beta.el")
                         (sort (ai-code-mcp-get-project-files) #'string<))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-project-buffers-lists-open-buffers-in-project ()
  "Project buffers should include file-visiting buffers under the active project."
  (let* ((project-dir (make-temp-file "ai-code-mcp-project-buffers-" t))
         (project-file (expand-file-name "alpha.el" project-dir))
         (other-dir (make-temp-file "ai-code-mcp-other-project-" t))
         (other-file (expand-file-name "other.el" other-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-project-buffers*"))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-project-buffers")
         project-buffer
         other-buffer)
    (unwind-protect
        (progn
          (with-temp-file project-file
            (insert "(message \"project\")\n"))
          (with-temp-file other-file
            (insert "(message \"other\")\n"))
          (setq project-buffer (find-file-noselect project-file t)
                other-buffer (find-file-noselect other-file t))
          (ai-code-mcp-register-session
           "session-project-buffers"
           project-dir
           session-buffer)
          (let ((result (ai-code-mcp-get-project-buffers)))
            (should (seq-some
                     (lambda (entry)
                       (equal project-file (alist-get 'file entry)))
                     result))
            (should-not (seq-some
                         (lambda (entry)
                           (equal other-file (alist-get 'file entry)))
                         result))))
      (when (buffer-live-p project-buffer)
        (kill-buffer project-buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t)
      (delete-directory other-dir t))))

(ert-deftest ai-code-test-mcp-xref-find-definitions-at-point-uses-location-context ()
  "Definitions-at-point should resolve via the xref backend at a file location."
  (let* ((project-dir (make-temp-file "ai-code-mcp-xref-defs-" t))
         (file-path (expand-file-name "defs.el" project-dir))
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "(defun alpha ()\n")
            (insert "  (beta))\n\n")
            (insert "(defun beta ()\n")
            (insert "  t)\n"))
          (cl-letf (((symbol-function 'xref-find-backend)
                     (lambda () 'mock-backend))
                    ((symbol-function 'xref-backend-identifier-at-point)
                     (lambda (_backend) "beta"))
                    ((symbol-function 'xref-backend-definitions)
                     (lambda (_backend identifier)
                       (list (xref-make
                              (format "%s definition" identifier)
                              (xref-make-file-location file-path 4 0))))))
            (should (equal '("defs.el:4: beta definition")
                           (ai-code-mcp-xref-find-definitions-at-point
                            file-path
                            2
                            3))))
          (setq visited-buffer (find-buffer-visiting file-path)))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-display-path-keeps-external-sibling-absolute ()
  "Display path should keep sibling paths outside the project absolute."
  (let* ((project-dir (make-temp-file "ai-code-mcp-display-path-" t))
         (sibling-dir (concat project-dir "-sibling"))
         (external-file (expand-file-name "other.el" sibling-dir))
         (buffer (generate-new-buffer " *ai-code-mcp-display-path*"))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-display-path"))
    (unwind-protect
        (progn
          (make-directory sibling-dir t)
          (with-temp-file external-file
            (insert "(message \"other\")\n"))
          (ai-code-mcp-register-session "session-display-path" project-dir buffer)
          (should (equal (expand-file-name external-file)
                         (ai-code-mcp--display-path external-file))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (let ((visited-buffer (find-buffer-visiting external-file)))
        (when (buffer-live-p visited-buffer)
          (kill-buffer visited-buffer)))
      (delete-directory project-dir t)
      (delete-directory sibling-dir t))))

(ert-deftest ai-code-test-mcp-format-xref-item-preserves-external-absolute-path ()
  "Xref items outside the project should keep their absolute file path."
  (let* ((project-dir (make-temp-file "ai-code-mcp-xref-project-" t))
         (external-dir (make-temp-file "ai-code-mcp-xref-external-" t))
         (external-file (expand-file-name "index.el" external-dir))
         (buffer (generate-new-buffer " *ai-code-mcp-xref-format*"))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-xref-format"))
    (unwind-protect
        (progn
          (with-temp-file external-file
            (insert "(message \"external\")\n"))
          (ai-code-mcp-register-session "session-xref-format" project-dir buffer)
          (should (equal
                   (format "%s:1: external summary"
                           (expand-file-name external-file))
                   (ai-code-mcp--format-xref-item
                    (xref-make
                     "external summary"
                     (xref-make-file-location external-file 1 0))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (let ((visited-buffer (find-buffer-visiting external-file)))
        (when (buffer-live-p visited-buffer)
          (kill-buffer visited-buffer)))
      (delete-directory project-dir t)
      (delete-directory external-dir t))))

(provide 'test_ai-code-mcp-server)

;;; test_ai-code-mcp-server.el ends here
