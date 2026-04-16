;;; test_ai-code-mcp-editor-tools.el --- Tests for ai-code-mcp-editor-tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for optional Emacs session MCP tools.

;;; Code:

(require 'ert)
(require 'json)
(require 'seq)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))

(require 'ai-code-mcp-server nil t)

(defun ai-code-test-mcp-editor-tools--content-text (result)
  "Extract the first text content entry from RESULT."
  (alist-get 'text
             (car (alist-get 'content result))))

(defun ai-code-test-mcp-editor-tools--read-payload (tool-name &optional arguments)
  "Call TOOL-NAME with ARGUMENTS and decode its JSON payload."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (json-read-from-string
     (ai-code-test-mcp-editor-tools--content-text
      (ai-code-mcp-dispatch
       "tools/call"
       `((name . ,tool-name)
         (arguments . ,(or arguments '()))))))))

(ert-deftest ai-code-test-mcp-editor-tools-register-when-enabled ()
  "Optional editor tools should be registered when explicitly enabled."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-editor-tools-enabled t))
    (let* ((tools-result (ai-code-mcp-dispatch "tools/list"))
           (tool-names (sort (mapcar (lambda (tool)
                                       (alist-get 'name tool))
                                     (alist-get 'tools tools-result))
                             #'string<)))
       (should (equal '("buffer_query"
                        "editor_state"
                        "eval_elisp"
                        "get_diagnostics"
                        "get_project_buffers"
                        "get_project_files"
                        "imenu_list_symbols"
                        "messages_tail"
                        "notify_user"
                        "project_info"
                        "treesit_info"
                        "visible_buffers"
                        "xref_find_definitions_at_point"
                        "xref_find_references")
                     tool-names)))))

(ert-deftest ai-code-test-mcp-editor-state-reports-selected-buffer ()
  "Editor state should describe the selected window buffer."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-editor-tools-enabled t)
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
          (let ((payload (ai-code-test-mcp-editor-tools--read-payload
                          "editor_state")))
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
        (ai-code-mcp-editor-tools-enabled t)
        (left-buffer (generate-new-buffer " *ai-code-mcp-left*"))
        (right-buffer (generate-new-buffer " *ai-code-mcp-right*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer left-buffer)
          (set-window-buffer (split-window-right) right-buffer)
          (let* ((payload (ai-code-test-mcp-editor-tools--read-payload
                           "visible_buffers"))
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

(ert-deftest ai-code-test-mcp-messages-tail-returns-latest-messages ()
  "Messages tail should return the latest entries from *Messages*."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-editor-tools-enabled t))
    (message "ai-code-mcp-editor-tools-test-message")
    (let* ((payload (ai-code-test-mcp-editor-tools--read-payload
                     "messages_tail"
                     '((limit . 1))))
           (messages (alist-get 'messages payload)))
      (should (equal t (alist-get 'ok payload)))
      (should (= 1 (length messages)))
      (should (string-match-p "ai-code-mcp-editor-tools-test-message"
                              (aref messages 0))))))

(ert-deftest ai-code-test-mcp-eval-elisp-query-uses-target-buffer ()
  "Query evaluation should run against the requested buffer context."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-editor-tools-enabled t)
        (buffer (generate-new-buffer " *ai-code-mcp-eval*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "hello\n"))
          (let ((payload
                 (ai-code-test-mcp-editor-tools--read-payload
                  "eval_elisp"
                  `((code . "(buffer-name)")
                    (buffer_name . ,(buffer-name buffer))))))
            (should (equal t (alist-get 'ok payload)))
            (should (equal "\" *ai-code-mcp-eval*\""
                           (alist-get 'value_repr payload)))
            (should (equal "string"
                           (alist-get 'value_type payload)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-mcp-eval-elisp-query-rejects-denied-symbols ()
  "Query evaluation should reject denied symbols before running them."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-editor-tools-enabled t))
    (let* ((payload (ai-code-test-mcp-editor-tools--read-payload
                     "eval_elisp"
                     '((code . "(insert \"boom\")"))))
           (error-object (alist-get 'error payload)))
      (should (equal :json-false (alist-get 'ok payload)))
      (should (equal "query_symbol_denied"
                     (alist-get 'type error-object))))))

(ert-deftest ai-code-test-mcp-eval-elisp-query-rejects-indirect-insert ()
  "Query evaluation should reject indirect calls to denied mutators."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-editor-tools-enabled t)
        (buffer (generate-new-buffer " *ai-code-mcp-indirect*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "hello\n"))
          (let* ((payload
                  (ai-code-test-mcp-editor-tools--read-payload
                   "eval_elisp"
                   `((code . "(apply #'insert '(\"boom\"))")
                     (buffer_name . ,(buffer-name buffer)))))
                 (error-object (alist-get 'error payload)))
            (should (equal :json-false (alist-get 'ok payload)))
            (should (equal "query_symbol_denied"
                           (alist-get 'type error-object)))
            (should (equal "hello\n"
                           (with-current-buffer buffer
                             (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-mcp-eval-elisp-query-rejects-symbol-function-indirection ()
  "Query evaluation should reject quoted mutators passed through `symbol-function'."
  (let ((ai-code-mcp-server-tools nil)
        (ai-code-mcp-editor-tools-enabled t)
        (buffer (generate-new-buffer " *ai-code-mcp-symbol-function*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "hello\n"))
          (let* ((payload
                  (ai-code-test-mcp-editor-tools--read-payload
                   "eval_elisp"
                   `((code . "(apply (symbol-function 'insert) '(\"boom\"))")
                     (buffer_name . ,(buffer-name buffer)))))
                 (error-object (alist-get 'error payload)))
            (should (equal :json-false (alist-get 'ok payload)))
            (should (equal "query_symbol_denied"
                           (alist-get 'type error-object)))
            (should (equal "hello\n"
                           (with-current-buffer buffer
                             (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'test_ai-code-mcp-editor-tools)

;;; test_ai-code-mcp-editor-tools.el ends here
