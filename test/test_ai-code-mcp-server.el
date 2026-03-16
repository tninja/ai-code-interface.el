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
      (should (equal '("imenu_list_symbols"
                       "project_info"
                       "treesit_info"
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
      (should (equal '("imenu_list_symbols"
                       "project_info"
                       "treesit_info"
                       "xref_find_references")
                     tool-names)))))

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

(provide 'test_ai-code-mcp-server)

;;; test_ai-code-mcp-server.el ends here
