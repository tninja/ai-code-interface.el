;;; test_ai-code-task-evidence.el --- Org evidence links tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: Apache-2.0
;;; Commentary:
;; Source provenance and task-to-evidence relationships must remain distinct.
;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'ai-code-task-evidence)

(defmacro ai-code-task-evidence-test-with-org (&rest body)
  "Run BODY inside a human-authored Org task."
  (declare (indent 0))
  (list 'with-temp-buffer
        '(setq buffer-file-name "/tmp/task-evidence-example.org")
        '(insert "* Customer requirement\nThe original instruction remains.\n"
                 "** Investigation\nHandwritten notes.\n"
                 "* Different task\nDo not edit.\n")
        '(org-mode)
        '(goto-char (point-min))
        (cons 'progn body)))

(ert-deftest ai-code-task-evidence-test-append-human-source ()
  "Store classified provenance with stable IDs without overwriting source."
  (ai-code-task-evidence-test-with-org
    (ai-code-task-evidence-record 'source
                                  "https://example.org/issues/42"
                                  "Original customer request")
    (let ((entries (ai-code-task-evidence-entries)))
      (should (= 1 (length entries)))
      (should (equal "source" (plist-get (car entries) :relation)))
      (should (equal "https://example.org/issues/42"
                     (plist-get (car entries) :reference)))
      (should (stringp (plist-get (car entries) :id))))
    (should (string-match-p "The original instruction remains" (buffer-string)))
    (should (string-match-p "Handwritten notes" (buffer-string)))
    (should (string-match-p "Different task\nDo not edit" (buffer-string)))
    (should (= 1 (how-many "^\\*\\* Evidence Links" (point-min) (point-max))))))

(ert-deftest ai-code-task-evidence-test-duplicate-and-bad-url-rejected ()
  "Do not duplicate a source or accept arbitrary untyped reference text."
  (ai-code-task-evidence-test-with-org
    (ai-code-task-evidence-record 'source "rdar://12345" "Radar issue")
    (let ((text (buffer-string)))
      (should-error (ai-code-task-evidence-record
                     'source "rdar://12345" "Same issue") :type 'user-error)
      (should-error (ai-code-task-evidence-record
                     'source "not a valid URI" "Not a source") :type 'user-error)
      (should (equal text (buffer-string))))))

(ert-deftest ai-code-task-evidence-test-derived-not-authoritative ()
  "AI-generated notes cannot satisfy an original-human-source audit."
  (ai-code-task-evidence-test-with-org
    (ai-code-task-evidence-record 'derived
                                  "https://example.org/ai-plan"
                                  "Agent plan")
    (should (seq-some (lambda (s) (string-match-p "human source" s))
                       (ai-code-task-evidence-audit-report)))))

(ert-deftest ai-code-task-evidence-test-file-and-id-dangling ()
  "Audit local references instead of reporting false verified coverage."
  (ai-code-task-evidence-test-with-org
    (ai-code-task-evidence-record 'source
                                  "https://example.org/requirements"
                                  "Human request")
    (ai-code-task-evidence-record 'verified-by
                                  "file:never-created-verify.org"
                                  "Verification output")
    (ai-code-task-evidence-record 'implements
                                  "id:nonexistent-task-id-123"
                                  "Referenced design")
    (cl-letf (((symbol-function 'org-id-find)
               (lambda (&rest _) nil)))
      (let ((issues (ai-code-task-evidence-audit-report)))
        (should (seq-some (lambda (s) (string-match-p "Missing local file" s))
                          issues))
        (should (seq-some (lambda (s) (string-match-p "Unresolved Org ID" s))
                          issues))))))

(ert-deftest ai-code-task-evidence-test-implementation-needs-verification ()
  "Implementation references without verification remain open questions."
  (ai-code-task-evidence-test-with-org
    (ai-code-task-evidence-record 'source
                                  "https://example.org/r/1" "Original")
    (ai-code-task-evidence-record 'implements
                                  "https://github.com/org/repo/commit/abc"
                                  "Code version")
    (should (seq-some (lambda (s) (string-match-p "verification" s))
                       (ai-code-task-evidence-audit-report)))))

(provide 'test_ai-code-task-evidence)
;;; test_ai-code-task-evidence.el ends here
