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

(ert-deftest ai-code-test-mcp-org-heading-dead-source-is-unavailable ()
  "A killed prompt source must not fall back to another Org buffer."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp--current-session-id "org-dead-source")
        (source (generate-new-buffer " *ai-code-org-dead-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (org-mode)
            (insert "* Origin\n"))
          (ai-code-mcp-register-session "org-dead-source" default-directory
                                        source)
          (kill-buffer source)
          (with-temp-buffer
            (org-mode)
            (insert "* Unrelated\n")
            (let ((payload (ai-code-test-mcp-org--parse
                            (ai-code-mcp-org-heading-context))))
              (should (eq :json-false (alist-get 'ok payload)))
              (should (equal "unavailable" (alist-get 'status payload))))))
      (ai-code-mcp-unregister-session "org-dead-source")
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest ai-code-test-mcp-org-heading-flags-stale-snapshot ()
  "Edits after prompt capture must mark the snapshot position as stale."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp--current-session-id "org-stale"))
    (with-temp-buffer
      (org-mode)
      (insert "* Origin\nbody\n")
      (goto-char (point-min))
      (ai-code-mcp-register-session "org-stale" default-directory
                                    (current-buffer))
      (let ((fresh (ai-code-test-mcp-org--parse
                    (ai-code-mcp-org-heading-context))))
        (should (eq :json-false (alist-get 'snapshot_stale fresh))))
      (insert "* Inserted\n")
      (let ((stale (ai-code-test-mcp-org--parse
                    (ai-code-mcp-org-heading-context))))
        (should (eq t (alist-get 'snapshot_stale stale))))
      (ai-code-mcp-unregister-session "org-stale"))))

(ert-deftest ai-code-test-mcp-org-heading-parents-ignore-previous-calls ()
  "Parent headings must not be reused from an earlier buffer's lookup."
  (let ((org-outline-path-cache nil))
    (dolist (parent '("Alpha" "Beta"))
      (with-temp-buffer
        (org-mode)
        (insert (format "* %s\n** Child\n" parent))
        (goto-char (point-max))
        (let ((payload (ai-code-test-mcp-org--parse
                        (ai-code-mcp-org-heading-context (buffer-name)))))
          (should (equal (list parent)
                         (append (alist-get 'parent_headings payload)
                                 nil))))))))

(ert-deftest ai-code-test-mcp-org-heading-references-ignore-truncation ()
  "A cutoff inside a link must not produce a different reference."
  (with-temp-buffer
    (org-mode)
    (insert "* H\nrdar://12345\n")
    (goto-char (point-min))
    (let* ((payload (ai-code-test-mcp-org--parse
                     (ai-code-mcp-org-heading-context (buffer-name) nil nil 13)))
           (links (append (alist-get 'references payload) nil)))
      (should (eq t (alist-get 'truncated payload)))
      (should (equal "* H\nrdar://12" (alist-get 'raw_org payload)))
      (should (equal '("rdar://12345") links)))))

(ert-deftest ai-code-test-mcp-org-heading-recognizes-radar-problem-links ()
  "Standard rdar://problem/N links are reported as references."
  (with-temp-buffer
    (org-mode)
    (insert "* H\nSee [[rdar://problem/67890][Bug report]]\n")
    (goto-char (point-min))
    (let ((links (append (alist-get 'references
                                    (ai-code-test-mcp-org--parse
                                     (ai-code-mcp-org-heading-context
                                      (buffer-name))))
                         nil)))
      (should (equal '("rdar://problem/67890") links)))))

(provide 'test_ai-code-mcp-org-tools)

;;; test_ai-code-mcp-org-tools.el ends here
