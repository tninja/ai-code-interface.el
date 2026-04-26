;;; test_ai-code-mcp-debug-tools.el --- Tests for ai-code-mcp-debug-tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for optional MCP debugging tools.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'seq)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))

(require 'ai-code-mcp-server nil t)
(require 'ai-code-mcp-debug-tools nil t)

(defun ai-code-test-mcp-debug-tools--content-text (result)
  "Extract text content from RESULT."
  (alist-get 'text
             (car (alist-get 'content result))))

(defun ai-code-test-mcp-debug-tools--read-json-payload (result)
  "Decode the JSON text content from RESULT."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (json-read-from-string
     (ai-code-test-mcp-debug-tools--content-text result))))

(defconst ai-code-test-mcp-debug-tools--tool-names
  '("get_feature_load_state"
    "get_function_info"
    "get_last_error_backtrace"
    "get_recent_messages"
    "get_variable_binding_info"
    "get_variable_value")
  "Expected MCP debug tool names.")

(ert-deftest ai-code-test-mcp-debug-tools-source-uses-common-module ()
  "Debug MCP modules should depend on the shared common module."
  (with-temp-buffer
    (insert-file-contents "ai-code-mcp-debug-tools.el")
    (goto-char (point-min))
    (should (search-forward "(require 'ai-code-mcp-common" nil t))
    (goto-char (point-min))
    (should-not (search-forward "(require 'seq)" nil t))
    (should-not (search-forward "(defun ai-code-mcp--json-bool" nil t))
    (goto-char (point-min))
    (should-not (search-forward "(defun ai-code-mcp--message-lines" nil t))
    (goto-char (point-min))
    (should-not (search-forward "(defvar ai-code-mcp-server-tool-setup-functions nil)" nil t))))

(ert-deftest ai-code-test-mcp-common-module-defines-shared-helpers ()
  "The shared MCP common module should define the extracted helpers."
  (with-temp-buffer
    (insert-file-contents "ai-code-mcp-common.el")
    (goto-char (point-min))
    (should (search-forward "(defvar ai-code-mcp-server-tool-setup-functions" nil t))
    (goto-char (point-min))
    (should (search-forward "(defun ai-code-mcp--json-bool" nil t))
    (goto-char (point-min))
    (should (search-forward "(defun ai-code-mcp--message-lines" nil t))))

(ert-deftest ai-code-test-mcp-server-source-uses-common-module-for-setup-registry ()
  "The MCP server source should use the shared setup registry declaration."
  (with-temp-buffer
    (insert-file-contents "ai-code-mcp-server.el")
    (goto-char (point-min))
    (should (search-forward "(require 'ai-code-mcp-common" nil t))
    (goto-char (point-min))
    (should-not (search-forward "(defvar ai-code-mcp-server-tool-setup-functions" nil t))))

(ert-deftest ai-code-test-mcp-debug-tools-register-by-default ()
  "Optional debug tools should register by default."
  (let ((ai-code-mcp-server-tools nil))
    (should ai-code-mcp-debug-tools-enabled)
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (tool-names (sort (mapcar (lambda (tool)
                                       (alist-get 'name tool))
                                     (alist-get 'tools tools-result))
                             #'string<)))
      (dolist (tool-name ai-code-test-mcp-debug-tools--tool-names)
        (should (member tool-name tool-names))))))

(ert-deftest ai-code-test-mcp-debug-tools-do-not-register-eval-elisp-by-default ()
  "Eval should remain opt-in even when debug tools are enabled."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (tool-names (mapcar (lambda (tool)
                                 (alist-get 'name tool))
                               (alist-get 'tools tools-result))))
      (should-not (member "eval_elisp" tool-names)))))

(ert-deftest ai-code-test-mcp-debug-tools-skip-registration-when-disabled ()
  "Optional debug tools should not register when disabled."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled nil))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (tool-names (sort (mapcar (lambda (tool)
                                       (alist-get 'name tool))
                                     (alist-get 'tools tools-result))
                             #'string<)))
      (dolist (tool-name ai-code-test-mcp-debug-tools--tool-names)
        (should-not (member tool-name tool-names))))))

(ert-deftest ai-code-test-mcp-debug-tools-register-eval-when-enabled ()
  "Eval should register when the explicit opt-in is enabled."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp-debug-tools-enable-eval-elisp t))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (tool-names (mapcar (lambda (tool)
                                 (alist-get 'name tool))
                               (alist-get 'tools tools-result))))
      (should (member "eval_elisp" tool-names)))))

(ert-deftest ai-code-test-mcp-eval-elisp-query-uses-target-buffer ()
  "Query evaluation should run against the requested buffer context."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp-debug-tools-enable-eval-elisp t)
        (buffer (generate-new-buffer " *ai-code-mcp-eval*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "hello\n"))
          (let ((payload
                 (ai-code-test-mcp-debug-tools--read-json-payload
                  (ai-code-mcp-dispatch
                   "tools/call"
                   `((name . "eval_elisp")
                     (arguments . ((code . "(buffer-name)")
                                   (buffer_name . ,(buffer-name buffer)))))))))
            (should (equal t (alist-get 'ok payload)))
            (should (equal "\" *ai-code-mcp-eval*\""
                           (alist-get 'value_repr payload)))
            (should (equal "string"
                           (alist-get 'value_type payload)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-mcp-eval-elisp-defaults-to-selected-window-buffer ()
  "Eval should use the selected window buffer when no buffer context is given."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp-debug-tools-enable-eval-elisp t)
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (session-id "mcp-eval-session")
        (session-buffer (generate-new-buffer " *ai-code-mcp-session*"))
        (target-buffer (generate-new-buffer " *ai-code-mcp-target*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer target-buffer)
          (with-current-buffer session-buffer
            (rename-buffer "mcp-session-buffer" t))
          (with-current-buffer target-buffer
            (rename-buffer "mcp-target-buffer" t))
          (ai-code-mcp-register-session session-id default-directory session-buffer)
          (let* ((ai-code-mcp--current-session-id session-id)
                 (payload
                  (ai-code-test-mcp-debug-tools--read-json-payload
                   (ai-code-mcp-dispatch
                    "tools/call"
                    '((name . "eval_elisp")
                      (arguments . ((code . "(buffer-name)"))))))))
            (should (equal t (alist-get 'ok payload)))
            (should (equal "\"mcp-target-buffer\""
                           (alist-get 'value_repr payload)))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))))

(ert-deftest ai-code-test-mcp-eval-elisp-query-rejects-denied-symbols ()
  "Query evaluation should reject denied symbols before running them."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp-debug-tools-enable-eval-elisp t))
    (let* ((payload
            (ai-code-test-mcp-debug-tools--read-json-payload
             (ai-code-mcp-dispatch
              "tools/call"
              '((name . "eval_elisp")
                (arguments . ((code . "(insert \"boom\")")))))))
           (error-object (alist-get 'error payload)))
      (should (equal :json-false (alist-get 'ok payload)))
      (should (equal "query_symbol_denied"
                     (alist-get 'type error-object))))))

(ert-deftest ai-code-test-mcp-get-variable-value-returns-bound-variable ()
  "Variable value tool should stringify the requested Emacs variable."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp-diagnostics-backend 'flymake))
    (let ((result (ai-code-mcp-dispatch
                   "tools/call"
                   '((name . "get_variable_value")
                     (arguments . ((variable_name . "ai-code-mcp-diagnostics-backend")))))))
      (should (equal "flymake"
                     (ai-code-test-mcp-debug-tools--content-text result))))))

(ert-deftest ai-code-test-mcp-get-variable-value-reports-missing-variable-without-interning ()
  "Unknown variable names should not be interned and should return a friendly error."
  (let* ((ai-code-mcp-server-tools nil)
         (ai-code-mcp-debug-tools-enabled t)
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
                   (ai-code-test-mcp-debug-tools--content-text result)))
    (should-not (intern-soft variable-name))))

(ert-deftest ai-code-test-mcp-get-variable-value-reports-unbound-variable ()
  "Unbound variable names should return a friendly error."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t)
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
                           (ai-code-test-mcp-debug-tools--content-text result)))))
      (unintern variable-name obarray))))

(ert-deftest ai-code-test-mcp-tools-list-describes-variable-value-as-printed-representation ()
  "Variable value tool metadata should match the returned representation."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t))
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
        (ai-code-mcp-debug-tools-enabled t)
        (variable-name "ai-code-test-mcp-buffer-local-variable")
        (buffer (generate-new-buffer " *ai-code-mcp-binding-info*")))
    (unwind-protect
        (progn
          (set-default (intern variable-name) 2)
          (with-current-buffer buffer
            (setq-local ai-code-test-mcp-buffer-local-variable 8))
          (let* ((payload
                  (ai-code-test-mcp-debug-tools--read-json-payload
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
        (ai-code-mcp-debug-tools-enabled t)
        (variable-name "ai-code-test-mcp-missing-binding-variable"))
    (when (intern-soft variable-name)
      (ert-fail "Test requires a missing symbol name"))
    (let ((payload
           (ai-code-test-mcp-debug-tools--read-json-payload
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
        (ai-code-mcp-debug-tools-enabled t)
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
                 (ai-code-test-mcp-debug-tools--read-json-payload
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
        (ai-code-mcp-debug-tools-enabled t)
        (function-name "ai-code-test-mcp-missing-function"))
    (when (intern-soft function-name)
      (ert-fail "Test requires a missing function symbol"))
    (let ((payload
           (ai-code-test-mcp-debug-tools--read-json-payload
            (ai-code-mcp-dispatch
             "tools/call"
             `((name . "get_function_info")
               (arguments . ((function_name . ,function-name))))))))
      (should (equal :json-false (alist-get 'exists payload)))
      (should-not (intern-soft function-name)))))

(ert-deftest ai-code-test-mcp-function-kind-does-not-resolve-autoloads ()
  "Autoload classification should not force indirect resolution."
  (let ((symbol (make-symbol "ai-code-test-mcp-autoload-function")))
    (unwind-protect
        (progn
          (fset symbol '(autoload "ai-code-test-autoload-lib" nil nil nil))
          (cl-letf (((symbol-function 'indirect-function)
                     (lambda (&rest _args)
                       (ert-fail "autoload classification should not resolve"))))
            (should (equal "autoload"
                           (ai-code-mcp--function-kind symbol)))))
      (when (fboundp symbol)
        (fmakunbound symbol)))))

(ert-deftest ai-code-test-mcp-get-feature-load-state-reports-loaded-feature-details ()
  "Feature load state should report loaded features and their providers."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t))
    (let* ((payload
            (ai-code-test-mcp-debug-tools--read-json-payload
             (ai-code-mcp-dispatch
              "tools/call"
              '((name . "get_feature_load_state")
                (arguments . ((feature_name . "json")))))))
           (provided-by-files (append (alist-get 'provided_by_files payload) nil))
           (load-path-matches (append (alist-get 'load_path_matches payload) nil)))
      (should (equal t (alist-get 'loaded payload)))
      (should (stringp (alist-get 'library_path payload)))
      (should provided-by-files)
      (should load-path-matches)
      (should-not (alist-get 'load_history_entries payload)))))

(ert-deftest ai-code-test-mcp-get-feature-load-state-reports-missing-features ()
  "Feature load state should report missing features without errors."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t)
        (feature-name "ai-code-test-mcp-missing-feature"))
    (when (intern-soft feature-name)
      (ert-fail "Test requires a missing feature symbol"))
    (let ((payload
           (ai-code-test-mcp-debug-tools--read-json-payload
            (ai-code-mcp-dispatch
             "tools/call"
             `((name . "get_feature_load_state")
               (arguments . ((feature_name . ,feature-name))))))))
      (should (equal :json-false (alist-get 'loaded payload)))
      (should-not (alist-get 'library_path payload)))))

(ert-deftest ai-code-test-mcp-get-feature-load-state-rejects-invalid-feature-name ()
  "Feature load state should reject non-string feature names clearly."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t))
    (let ((payload
           (ai-code-test-mcp-debug-tools--read-json-payload
            (ai-code-mcp-dispatch
             "tools/call"
             '((name . "get_feature_load_state")
               (arguments . ((feature_name . 7))))))))
      (should (equal :json-false (alist-get 'loaded payload)))
      (should (equal "feature_name must be a non-empty string"
                     (alist-get 'error_message payload)))
      (should-not (alist-get 'library_path payload))
      (should-not (alist-get 'load_path_matches payload)))))

(ert-deftest ai-code-test-mcp-get-recent-messages-returns-latest-messages ()
  "Recent messages should return the latest entries from `*Messages*'."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled t))
    (message "ai-code-mcp-server-test-message")
    (let* ((payload
            (ai-code-test-mcp-debug-tools--read-json-payload
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
        (ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp--last-error-record nil))
    (let ((payload
           (ai-code-test-mcp-debug-tools--read-json-payload
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
        (ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp--last-error-record nil))
    (cl-letf (((symbol-function 'backtrace-frames)
               (lambda (&optional _base)
                 '((t ai-code-test-mcp-frame-a nil nil)
                   (t ai-code-test-mcp-frame-b ("x") nil)))))
      (ai-code-mcp--record-command-error '(error "Boom") 'command t))
    (let* ((payload
            (ai-code-test-mcp-debug-tools--read-json-payload
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

(provide 'test_ai-code-mcp-debug-tools)

;;; test_ai-code-mcp-debug-tools.el ends here
