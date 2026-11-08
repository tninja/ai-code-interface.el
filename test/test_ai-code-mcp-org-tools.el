;;; test_ai-code-mcp-org-tools.el --- Org MCP tool ERT tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Code:
(require 'ert)
(require 'json)
(require 'org)
(require 'ai-code-mcp-server)
(require 'ai-code-mcp-org-tools)

(defun ai-code-test-mcp-org--parse (value)
  "Parse JSON VALUE with stable test object types."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (json-read-from-string value)))

(ert-deftest ai-code-test-mcp-org-heading-preserves-raw-sources ()
  "Return a single heading's original facts without generated summaries."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** TODO Fix bug :review:\n")
    (insert ":PROPERTIES:\n:RADAR: rdar://12345\n:END:\n")
    (insert "See https://github.com/example/repo/issues/1 and https://foo.slack.com/archives/T1\n")
    (insert "*** Child\nUnrelated child text\n")
    (goto-char (point-min))
    (search-forward "See ")
    (let* ((modified (buffer-modified-p))
           (payload (ai-code-test-mcp-org--parse
                     (ai-code-mcp-org-heading-context (buffer-name))))
           (raw (alist-get 'raw_org payload))
           (links (append (alist-get 'references payload) nil)))
      (should (equal "Fix bug" (alist-get 'heading payload)))
      (should (equal "TODO" (alist-get 'todo payload)))
      (should (equal '("Parent")
                     (append (alist-get 'parent_headings payload) nil)))
      (should (string-match-p "rdar://12345" raw))
      (should (string-match-p "issues/1" raw))
      (should-not (string-match-p "Unrelated child text" raw))
      (should (member "rdar://12345" links))
      (should (= 3 (length links)))
      (should (eq modified (buffer-modified-p))))))

(ert-deftest ai-code-test-mcp-org-heading-children-are-opt-in ()
  "Only an explicit true flag should include descendants."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nbody\n** Child\nchild body\n")
    (goto-char (point-min))
    (let ((without (ai-code-test-mcp-org--parse
                    (ai-code-mcp-org-heading-context (buffer-name))))
          (with (ai-code-test-mcp-org--parse
                 (ai-code-mcp-org-heading-context (buffer-name) nil t))))
      (should-not (string-match-p "Child" (alist-get 'raw_org without)))
      (should (string-match-p "Child" (alist-get 'raw_org with))))))

(ert-deftest ai-code-test-mcp-org-heading-uses-source-snapshot ()
  "Switching terminal focus must not change the heading selected at send."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp--current-session-id "org-source"))
    (with-temp-buffer
      (org-mode)
      (insert "* First\n** One\n* Second\n")
      (goto-char (point-min))
      (ai-code-mcp-register-session "org-source" default-directory
                                    (current-buffer))
      (goto-char (point-max))
      (let ((payload (ai-code-test-mcp-org--parse
                      (ai-code-mcp-org-heading-context))))
        (should (equal "First" (alist-get 'heading payload))))
      (ai-code-mcp-unregister-session "org-source"))))

(ert-deftest ai-code-test-mcp-org-heading-validates-input ()
  "Non-Org buffers and invalid limits must be rejected."
  (with-temp-buffer
    (should-error (ai-code-mcp-org-heading-context (buffer-name))
                  :type 'user-error)
    (org-mode)
    (insert "* A\n")
    (should-error (ai-code-mcp-org-heading-context
                   (buffer-name) nil nil 0)
                  :type 'user-error)))

(provide 'test_ai-code-mcp-org-tools)

;;; test_ai-code-mcp-org-tools.el ends here
