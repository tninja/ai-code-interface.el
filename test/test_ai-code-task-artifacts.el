;;; test_ai-code-task-artifacts.el --- Artifact tracking tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: Apache-2.0
;;; Commentary:
;; Task artifacts preserve original requirements and pin review targets.
;;; Code:
(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'ai-code-task-artifacts)

(defconst ai-code-task-artifacts-test-sha "0123456789abcdef0123456789abcdef01234567")

(defmacro ai-code-task-artifacts-test-with-org (&rest body)
  "Run BODY with an existing task and unrelated task."
  (declare (indent 0))
  (list 'with-temp-buffer
        '(setq buffer-file-name "/tmp/task-artifacts.org")
        '(insert "* Fix config merge\nHuman requirement remains.\n* Unrelated\nKeep this.\n")
        '(org-mode)
        '(goto-char (point-min))
        (cons 'progn body)))

(ert-deftest ai-code-task-artifacts-test-snapshot-is-additive ()
  "Recording twice must not overwrite a requirement or duplicate the section."
  (ai-code-task-artifacts-test-with-org
    (cl-letf (((symbol-function 'ai-code-task-artifacts--current-root)
               (lambda () "/tmp/repo/"))
              ((symbol-function 'ai-code-task-artifacts--git-state)
               (lambda (_root)
                 (list :head ai-code-task-artifacts-test-sha
                       :branch "feature" :change-hash "digest"
                       :tracked t :untracked t))))
      (ai-code-task-artifacts-record-snapshot)
      (ai-code-task-artifacts-record-snapshot))
    (should (string-match-p "Human requirement remains" (buffer-string)))
    (should (string-match-p "Unrelated\nKeep this" (buffer-string)))
    (should (= 1 (how-many "^\\*\\* AI Artifacts" (point-min) (point-max))))
    (should (= 2 (how-many "^- Snapshot" (point-min) (point-max))))
    (should (equal (plist-get (ai-code-task-artifacts-latest) :head)
                   ai-code-task-artifacts-test-sha))
    (should (plist-get (ai-code-task-artifacts-latest) :untracked))))

(ert-deftest ai-code-task-artifacts-test-pr-and-verification ()
  "Store only explicitly provided artifacts."
  (ai-code-task-artifacts-test-with-org
    (ai-code-task-artifacts-record-pr "https://github.example.com/org/repo/pull/42")
    (ai-code-task-artifacts-record-verification "/tmp/verify.org")
    (should (equal (plist-get (ai-code-task-artifacts-latest) :pr)
                   "https://github.example.com/org/repo/pull/42"))
    (should (equal (plist-get (ai-code-task-artifacts-latest) :verification)
                   "verify.org"))
    (should (string-match-p "file:verify.org" (buffer-string)))))

(ert-deftest ai-code-task-artifacts-test-invalid-pr-no-write ()
  "Reject incorrect PR URLs before changing the task."
  (ai-code-task-artifacts-test-with-org
    (let ((before (buffer-string)))
      (should-error (ai-code-task-artifacts-record-pr
                     "https://github.com/org/repo/issues/4") :type 'user-error)
      (should (equal before (buffer-string))))))

(ert-deftest ai-code-task-artifacts-test-dirty-snapshot-not-reproducible ()
  "Warn about untracked code and revision changes."
  (ai-code-task-artifacts-test-with-org
    (cl-letf (((symbol-function 'ai-code-task-artifacts--current-root)
               (lambda () "/tmp/repo/"))
              ((symbol-function 'ai-code-task-artifacts--git-state)
               (lambda (_root)
                 (list :head ai-code-task-artifacts-test-sha
                       :branch "feature" :change-hash "old"
                       :tracked t :untracked t))))
      (ai-code-task-artifacts-record-snapshot)
      (should (string-match-p "not a reproducible"
                              (ai-code-task-artifacts-snapshot-scope))))
    (cl-letf (((symbol-function 'ai-code-task-artifacts--current-root)
               (lambda () "/tmp/repo/"))
              ((symbol-function 'ai-code-task-artifacts--git-state)
               (lambda (_root)
                 (list :head "ffffffffffffffffffffffffffffffffffffffff"
                       :branch "feature" :change-hash "new"
                       :tracked t :untracked nil))))
      (should (string-match-p "DRIFT"
                              (ai-code-task-artifacts-snapshot-scope))))))

(ert-deftest ai-code-task-artifacts-test-pr-target-keeps-pr-head ()
  "Saved PR uses its actual head rather than the local branch."
  (ai-code-task-artifacts-test-with-org
    (ai-code-task-artifacts-record-pr "https://github.com/org/repo/pull/8")
    (should (string-match-p "actual PR head"
                            (ai-code-task-artifacts-pr-scope)))))

(provide 'test_ai-code-task-artifacts)
;;; test_ai-code-task-artifacts.el ends here
