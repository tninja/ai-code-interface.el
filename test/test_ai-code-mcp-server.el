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

(cl-defstruct ai-code-test-mcp-mock-diagnostic
  beg end type text backend)

(defconst ai-code-test-mcp--builtin-tool-names
  '("buffer_query"
    "editor_state"
    "get_diagnostics"
    "get_project_buffers"
    "get_project_files"
    "imenu_list_symbols"
    "notify_user"
    "project_info"
    "treesit_info"
    "visible_buffers"
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
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled nil))
    (ai-code-mcp-builtins-setup)
    (ai-code-mcp-builtins-setup)
    (let ((tool-names (sort (mapcar (lambda (tool)
                                      (plist-get tool :name))
                                    ai-code-mcp-server-tools)
                            #'string<)))
       (should (equal '("buffer_query"
                        "editor_state"
                        "get_diagnostics"
                        "get_project_buffers"
                        "get_project_files"
                        "imenu_list_symbols"
                        "notify_user"
                        "project_info"
                        "treesit_info"
                        "visible_buffers"
                        "xref_find_definitions_at_point"
                       "xref_find_references")
                     tool-names)))))

(ert-deftest ai-code-test-mcp-tools-list-registers-builtins-by-default ()
  "Tools list should expose built-in tools without manual setup."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-debug-tools-enabled nil))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (tool-names (sort (mapcar (lambda (tool)
                                       (alist-get 'name tool))
                                     (alist-get 'tools tools-result))
                             #'string<)))
      (should (equal ai-code-test-mcp--builtin-tool-names
                     tool-names)))))

(ert-deftest ai-code-test-mcp-editor-state-reports-selected-buffer ()
  "Editor state should describe the selected window buffer."
  (let ((ai-code-mcp-server-tools nil)
        (buffer (generate-new-buffer " *ai-code-mcp-editor-state*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (emacs-lisp-mode)
            (setq-local default-directory "/tmp/")
            (insert "alpha\nbeta\n")
            (goto-char (point-min))
            (forward-line 1)
            (move-to-column 2))
          (let* ((result (ai-code-mcp-dispatch "tools/call"
                                               '((name . "editor_state")
                                                 (arguments . ()))))
                 (payload (let ((json-object-type 'alist)
                                (json-array-type 'vector)
                                (json-key-type 'symbol))
                            (json-read-from-string
                             (ai-code-test-mcp--content-text result)))))
            (should (equal t (alist-get 'ok payload)))
            (should (equal (buffer-name buffer)
                           (alist-get 'buffer_name payload)))
            (should (equal "emacs-lisp-mode"
                           (alist-get 'major_mode payload)))
            (should (equal t (alist-get 'modified payload)))
            (should (= 2 (alist-get 'line payload)))
            (should (= 2 (alist-get 'column payload)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-mcp-visible-buffers-lists-current-windows ()
  "Visible buffers should mirror the selected frame windows."
  (let ((ai-code-mcp-server-tools nil)
        (left-buffer (generate-new-buffer " *ai-code-mcp-left*"))
        (right-buffer (generate-new-buffer " *ai-code-mcp-right*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer left-buffer)
          (set-window-buffer (split-window-right) right-buffer)
          (let* ((result (ai-code-mcp-dispatch "tools/call"
                                               '((name . "visible_buffers")
                                                 (arguments . ()))))
                 (payload (let ((json-object-type 'alist)
                                (json-array-type 'vector)
                                (json-key-type 'symbol))
                            (json-read-from-string
                             (ai-code-test-mcp--content-text result))))
                 (items (alist-get 'items payload))
                 (names (sort (mapcar (lambda (item)
                                        (alist-get 'buffer_name item))
                                      (append items nil))
                              #'string<)))
            (should (equal t (alist-get 'ok payload)))
            (should (equal '(" *ai-code-mcp-left*" " *ai-code-mcp-right*")
                           names))))
      (when (buffer-live-p left-buffer)
        (kill-buffer left-buffer))
      (when (buffer-live-p right-buffer)
        (kill-buffer right-buffer)))))

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
