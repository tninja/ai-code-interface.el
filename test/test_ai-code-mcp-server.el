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

(defun ai-code-test-mcp--read-json (payload)
  "Parse PAYLOAD as JSON using alist objects and vector arrays."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (json-read-from-string payload)))

(defmacro ai-code-test-mcp--with-flymake-diagnostics (diagnostics &rest body)
  "Evaluate BODY with Flymake diagnostic accessors mocked.
DIAGNOSTICS is an expression returning a list of mock diagnostic structs."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'flymake-diagnostics)
              (lambda (&rest _) ,diagnostics))
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
     ,@body))

(defconst ai-code-test-mcp--builtin-tool-names
  '("buffer_query"
    "diagnostics_baseline"
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
                        "diagnostics_baseline"
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
                         (envelope (json-read-from-string payload))
                         (items (alist-get 'files envelope))
                         (entry (aref items 0))
                         (diagnostics (alist-get 'diagnostics entry))
                         (first-diagnostic (aref diagnostics 0))
                         (range (alist-get 'range first-diagnostic))
                         (start (alist-get 'start range)))
                    (should (equal "issues" (alist-get 'status envelope)))
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
                         (envelope (json-read-from-string payload))
                         (items (alist-get 'files envelope))
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
                         (envelope (json-read-from-string payload))
                         (items (alist-get 'files envelope)))
                    (should (= 1 (length items)))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-diagnostics-envelope-reports-issues ()
  "The get_diagnostics tool should wrap results in an observation envelope."
  (let* ((project-dir (make-temp-file "ai-code-mcp-envelope-issues-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (file-uri (concat "file://" file-path))
         (session-buffer (generate-new-buffer " *ai-code-mcp-envelope-issues*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-envelope-issues")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-envelope-issues" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics
                (list (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (line-end-position)
                       :type :warning :text "Unused value" :backend 'mock-backend))
              (let* ((payload (ai-code-test-mcp--content-text
                               (ai-code-mcp-dispatch
                                "tools/call"
                                `((name . "get_diagnostics")
                                  (arguments . ((uri . ,file-uri)))))))
                     (envelope (ai-code-test-mcp--read-json payload))
                     (files (alist-get 'files envelope))
                     (actions (alist-get 'next_actions envelope)))
                (should (equal "issues" (alist-get 'status envelope)))
                (should (stringp (alist-get 'summary envelope)))
                (should (= 1 (length files)))
                (should (equal file-uri (alist-get 'uri (aref files 0))))
                (should (vectorp actions))
                (should (> (length actions) 0))
                (should (string-match-p "Unused value" (aref actions 0)))
                (should (vectorp (alist-get 'artifacts envelope)))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-diagnostics-envelope-reports-clean ()
  "The get_diagnostics tool should report clean when there are no diagnostics."
  (let* ((project-dir (make-temp-file "ai-code-mcp-envelope-clean-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (file-uri (concat "file://" file-path))
         (session-buffer (generate-new-buffer " *ai-code-mcp-envelope-clean*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-envelope-clean")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-envelope-clean" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics nil
              (let* ((payload (ai-code-test-mcp--content-text
                               (ai-code-mcp-dispatch
                                "tools/call"
                                `((name . "get_diagnostics")
                                  (arguments . ((uri . ,file-uri)))))))
                     (envelope (ai-code-test-mcp--read-json payload)))
                (should (equal "clean" (alist-get 'status envelope)))
                (should (= 0 (length (alist-get 'files envelope))))
                (should (= 0 (length (alist-get 'next_actions envelope))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-diagnostics-baseline-then-no-new-is-clean ()
  "After recording a baseline, identical diagnostics yield a clean delta."
  (let* ((project-dir (make-temp-file "ai-code-mcp-baseline-clean-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-baseline-clean*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-baseline-clean")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-baseline-clean" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics
                (list (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (line-end-position)
                       :type :warning :text "Existing problem"
                       :backend 'mock-backend))
              (let ((baseline (ai-code-test-mcp--read-json
                               (ai-code-test-mcp--content-text
                                (ai-code-mcp-dispatch
                                 "tools/call"
                                 '((name . "diagnostics_baseline")
                                   (arguments . ())))))))
                (should (equal "baseline_recorded"
                               (alist-get 'status baseline))))
              (let ((delta (ai-code-test-mcp--read-json
                            (ai-code-test-mcp--content-text
                             (ai-code-mcp-dispatch
                              "tools/call"
                              '((name . "get_diagnostics")
                                (arguments . ((since . "baseline")))))))))
                (should (equal "clean" (alist-get 'status delta)))
                (should (= 0 (length (alist-get 'files delta))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-diagnostics-baseline-response-omits-files ()
  "Recording a baseline must not echo the full diagnostics list into context.
The baseline is stored server-side, so the tool response only needs a status
and summary; returning every diagnostic would bloat the model context."
  (let* ((project-dir (make-temp-file "ai-code-mcp-baseline-omit-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-baseline-omit*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-baseline-omit")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-baseline-omit" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics
                (list (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (line-end-position)
                       :type :warning :text "Existing problem"
                       :backend 'mock-backend))
              (let ((baseline (ai-code-test-mcp--read-json
                               (ai-code-test-mcp--content-text
                                (ai-code-mcp-dispatch
                                 "tools/call"
                                 '((name . "diagnostics_baseline")
                                   (arguments . ())))))))
                ;; The baseline is recorded server-side, not in the response body.
                (should (equal "baseline_recorded"
                               (alist-get 'status baseline)))
                ;; So the response must not echo the project diagnostics list.
                (should (= 0 (length (alist-get 'files baseline))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-diagnostics-baseline-response-includes-next-actions ()
  "The baseline response should carry a structured next action for the harness loop.
Per the observation contract, the follow-up step (edit, then verify with
since=\"baseline\") belongs in `next_actions', not only in the summary prose."
  (let* ((project-dir (make-temp-file "ai-code-mcp-baseline-actions-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-baseline-actions*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-baseline-actions")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-baseline-actions" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics
                (list (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (line-end-position)
                       :type :warning :text "Existing problem"
                       :backend 'mock-backend))
              (let* ((baseline (ai-code-test-mcp--read-json
                                (ai-code-test-mcp--content-text
                                 (ai-code-mcp-dispatch
                                  "tools/call"
                                  '((name . "diagnostics_baseline")
                                    (arguments . ()))))))
                     (actions (alist-get 'next_actions baseline)))
                (should (equal "baseline_recorded" (alist-get 'status baseline)))
                ;; The harness's follow-up step is exposed as a structured action.
                (should (> (length actions) 0))
                (should (string-match-p "since=\"baseline\"" (aref actions 0)))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-diagnostics-truncation-note-is-context-aware ()
  "The truncation note must not circularly suggest since=\"baseline\" in a delta
report (the caller already uses it); it points to per-file (uri) narrowing
instead.  The current report may still offer since=\"baseline\" to focus on
newly introduced diagnostics."
  (let* ((ai-code-mcp-diagnostics-max-report-diagnostics 1)
         (entries (list `((uri . "file:///tmp/a.el")
                          (diagnostics . ,(vector
                                           (ai-code-mcp--make-diagnostic 1 0 1 1 'warning "checker" "one")
                                           (ai-code-mcp--make-diagnostic 2 0 2 1 'warning "checker" "two"))))))
         (delta-summary (alist-get 'summary
                                   (ai-code-mcp--diagnostics-envelope entries 'delta)))
         (current-summary (alist-get 'summary
                                     (ai-code-mcp--diagnostics-envelope entries 'current))))
    ;; Both reports are truncated and point to per-file (uri) narrowing.
    (should (string-match-p "uri" delta-summary))
    (should (string-match-p "uri" current-summary))
    ;; The delta note must NOT tell the caller to use since="baseline" again.
    (should-not (string-match-p "since=\"baseline\"" delta-summary))
    ;; The current note may still offer since="baseline" (to focus on regressions).
    (should (string-match-p "since=\"baseline\"" current-summary))))

(ert-deftest ai-code-test-mcp-diagnostics-max-report-type-is-non-negative ()
  "The diagnostics cap customization should restrict input to non-negative integers.
A negative maximum is meaningless and would silently hide every diagnostic."
  (let ((type (get 'ai-code-mcp-diagnostics-max-report-diagnostics 'custom-type)))
    (should (memq 'natnum (flatten-tree type)))
    (should-not (memq 'integer (flatten-tree type)))))

(ert-deftest ai-code-test-mcp-diagnostics-envelope-truncates-large-reports ()
  "The diagnostics envelope caps `files' so a large report cannot overflow context."
  (let* ((ai-code-mcp-diagnostics-max-report-diagnostics 2)
         (entries
          (list `((uri . "file:///tmp/a.el")
                  (diagnostics . ,(vector
                                   (ai-code-mcp--make-diagnostic 1 0 1 1 'warning "checker" "one")
                                   (ai-code-mcp--make-diagnostic 2 0 2 1 'warning "checker" "two")
                                   (ai-code-mcp--make-diagnostic 3 0 3 1 'warning "checker" "three"))))
                `((uri . "file:///tmp/b.el")
                  (diagnostics . ,(vector
                                   (ai-code-mcp--make-diagnostic 1 0 1 1 'error "checker" "four"))))))
         (envelope (ai-code-mcp--diagnostics-envelope entries 'current))
         (files (alist-get 'files envelope))
         (shown (apply #'+ (mapcar (lambda (entry)
                                     (length (alist-get 'diagnostics entry)))
                                   (append files nil))))
         (summary (alist-get 'summary envelope))
         (actions (alist-get 'next_actions envelope)))
    ;; Only the first `limit' diagnostics are listed (and actions follow them) ...
    (should (= 2 shown))
    (should (= 2 (length actions)))
    ;; ... while the true totals and the truncation are reported in the summary.
    (should (string-match-p "4 diagnostic" summary))
    (should (string-match-p "2 of 4" summary))))

(ert-deftest ai-code-test-mcp-diagnostics-baseline-summary-reports-top-sources ()
  "The baseline summary names the dominant diagnostic sources without listing them.
This keeps a useful signal about what produces the baseline noise even though
the full diagnostics list is intentionally omitted from the response."
  (let* ((project-dir (make-temp-file "ai-code-mcp-baseline-sources-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-baseline-sources*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-baseline-sources")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-baseline-sources" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics
                (list (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (point-min)
                       :type :warning :text "style one" :backend 'checkdoc)
                      (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (point-min)
                       :type :warning :text "style two" :backend 'checkdoc)
                      (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (point-min)
                       :type :error :text "compile one" :backend 'byte-compile))
              (let ((summary (alist-get
                              'summary
                              (ai-code-test-mcp--read-json
                               (ai-code-test-mcp--content-text
                                (ai-code-mcp-dispatch
                                 "tools/call"
                                 '((name . "diagnostics_baseline")
                                   (arguments . ()))))))))
                (should (string-match-p "Top sources:" summary))
                (should (string-match-p "checkdoc (2)" summary))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-diagnostics-since-baseline-reports-regression ()
  "Diagnostics that appear after the baseline are reported as a regression."
  (let* ((project-dir (make-temp-file "ai-code-mcp-baseline-regression-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-baseline-regression*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-baseline-regression")
         (existing (make-ai-code-test-mcp-mock-diagnostic
                    :beg (point-min) :end (point-min)
                    :type :warning :text "Existing problem"
                    :backend 'mock-backend))
         (introduced (make-ai-code-test-mcp-mock-diagnostic
                      :beg (point-min) :end (point-min)
                      :type :error :text "New problem"
                      :backend 'mock-backend))
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-baseline-regression" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics (list existing)
              (ai-code-mcp-dispatch
               "tools/call"
               '((name . "diagnostics_baseline")
                 (arguments . ()))))
            (ai-code-test-mcp--with-flymake-diagnostics (list existing introduced)
              (let* ((delta (ai-code-test-mcp--read-json
                             (ai-code-test-mcp--content-text
                              (ai-code-mcp-dispatch
                               "tools/call"
                               '((name . "get_diagnostics")
                                 (arguments . ((since . "baseline"))))))))
                     (files (alist-get 'files delta))
                     (actions (alist-get 'next_actions delta)))
                (should (equal "regression" (alist-get 'status delta)))
                (should (= 1 (length files)))
                (should (= 1 (length (alist-get 'diagnostics (aref files 0)))))
                (should (equal "New problem"
                               (alist-get 'message
                                          (aref (alist-get 'diagnostics
                                                           (aref files 0))
                                                0))))
                (should (string-match-p "New problem" (aref actions 0)))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-diagnostics-since-baseline-counts-duplicates ()
  "A duplicate diagnostic beyond the baseline count should be a regression."
  (let* ((project-dir (make-temp-file "ai-code-mcp-baseline-duplicates-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-baseline-duplicates*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-baseline-duplicates")
         (existing (make-ai-code-test-mcp-mock-diagnostic
                    :beg (point-min) :end (point-min)
                    :type :warning :text "Repeated problem"
                    :backend 'mock-backend))
         (duplicate (make-ai-code-test-mcp-mock-diagnostic
                     :beg (point-min) :end (point-min)
                     :type :warning :text "Repeated problem"
                     :backend 'mock-backend))
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-baseline-duplicates" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics (list existing)
              (ai-code-mcp-dispatch
               "tools/call"
               '((name . "diagnostics_baseline")
                 (arguments . ()))))
            (ai-code-test-mcp--with-flymake-diagnostics (list existing duplicate)
              (let* ((delta (ai-code-test-mcp--read-json
                             (ai-code-test-mcp--content-text
                              (ai-code-mcp-dispatch
                               "tools/call"
                               '((name . "get_diagnostics")
                                 (arguments . ((since . "baseline"))))))))
                     (files (alist-get 'files delta)))
                (should (equal "regression" (alist-get 'status delta)))
                (should (= 1 (length files)))
                (should (= 1 (length (alist-get 'diagnostics (aref files 0)))))
                (should (equal "Repeated problem"
                               (alist-get 'message
                                          (aref (alist-get 'diagnostics
                                                           (aref files 0))
                                                0))))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-unregister-session-clears-diagnostics-baseline ()
  "Unregistering a session should free its recorded diagnostics baseline."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
        (session-id "session-cleanup")
        (buffer (generate-new-buffer " *ai-code-mcp-cleanup*"))
        (project-dir (make-temp-file "ai-code-mcp-cleanup-" t)))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session session-id project-dir buffer)
          (puthash session-id (make-hash-table :test 'equal)
                   ai-code-mcp--diagnostics-baselines)
          (should (gethash session-id ai-code-mcp--diagnostics-baselines))
          (ai-code-mcp-unregister-session session-id)
          (should-not (gethash session-id ai-code-mcp--diagnostics-baselines)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-diagnostics-since-baseline-without-baseline-reports-no-baseline ()
  "Using since=\"baseline\" before recording a baseline must not report regressions."
  (let* ((project-dir (make-temp-file "ai-code-mcp-no-baseline-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (session-buffer (generate-new-buffer " *ai-code-mcp-no-baseline*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-no-baseline")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-no-baseline" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics
                (list (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (line-end-position)
                       :type :warning :text "Existing problem"
                       :backend 'mock-backend))
              (let* ((delta (ai-code-test-mcp--read-json
                             (ai-code-test-mcp--content-text
                              (ai-code-mcp-dispatch
                               "tools/call"
                               '((name . "get_diagnostics")
                                 (arguments . ((since . "baseline"))))))))
                     (actions (alist-get 'next_actions delta)))
                (should (equal "no_baseline" (alist-get 'status delta)))
                (should (= 0 (length (alist-get 'files delta))))
                (should (> (length actions) 0))
                (should (string-match-p "diagnostics_baseline" (aref actions 0)))))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-get-diagnostics-since-baseline-canonicalizes-uri ()
  "Per-file delta should match the baseline regardless of the request URI form."
  (let* ((project-dir (make-temp-file "ai-code-mcp-baseline-uri-" t))
         (file-path (expand-file-name "sample.el" project-dir))
         (localhost-uri (concat "file://localhost" file-path))
         (session-buffer (generate-new-buffer " *ai-code-mcp-baseline-uri*"))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal))
         (ai-code-mcp--current-session-id "session-baseline-uri")
         visited-buffer)
    (unwind-protect
        (progn
          (with-temp-file file-path (insert "(message \"alpha\")\n"))
          (setq visited-buffer (find-file-noselect file-path t))
          (with-current-buffer visited-buffer
            (setq-local flymake-mode t)
            (ai-code-mcp-register-session
             "session-baseline-uri" project-dir session-buffer)
            (ai-code-test-mcp--with-flymake-diagnostics
                (list (make-ai-code-test-mcp-mock-diagnostic
                       :beg (point-min) :end (line-end-position)
                       :type :warning :text "Existing problem"
                       :backend 'mock-backend))
              ;; Baseline is recorded via the project scan (canonical file:// URIs).
              (ai-code-mcp-dispatch
               "tools/call"
               '((name . "diagnostics_baseline")
                 (arguments . ())))
              ;; Query the delta for the same file using a non-canonical localhost URI.
              (let ((delta (ai-code-test-mcp--read-json
                            (ai-code-test-mcp--content-text
                             (ai-code-mcp-dispatch
                              "tools/call"
                              `((name . "get_diagnostics")
                                (arguments . ((uri . ,localhost-uri)
                                              (since . "baseline")))))))))
                (should (equal "clean" (alist-get 'status delta)))
                (should (= 0 (length (alist-get 'files delta))))))))
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
