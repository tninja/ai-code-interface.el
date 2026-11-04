;;; test_ai-code-task-evidence.el --- Automatic provenance summary tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Original sources should accumulate within an existing Org result summary
;; without asking the user to maintain a separate evidence database.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'ai-code-prompt-mode)

(defun ai-code-test-summary-evidence--send (accept)
  "Return dispatched prompt with Org result-summary choice ACCEPT."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/task-evidence.org")
    (insert "* TODO Fix config merge\n"
            "Human reference: https://example.org/issue/123\n")
    (org-mode)
    (goto-char (point-min))
    (let ((ai-code--prompt-already-edited t)
          (ai-code-use-compose-buffer nil)
          (ai-code-prompt-preprocess-filepaths nil)
          (ai-code--suppress-org-summary-prompt nil)
          (ai-code--org-summary-anchor nil)
          sent)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (question)
                   (should (equal question "Append result summary to current section? "))
                   accept))
                ((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text &optional _transport) (setq sent text))))
        (ai-code--insert-prompt "Implement the requirement and check it."))
      (should-not (buffer-modified-p))
      sent)))

(ert-deftest ai-code-test-summary-evidence-original-sources-are-automatic ()
  "Adding a result summary includes original human sources and derived facts."
  (let ((prompt (ai-code-test-summary-evidence--send t)))
    (should (string-match-p "After completing, append a concise result summary" prompt))
    (should (string-match-p "original human" prompt))
    (should (string-match-p "Slack" prompt))
    (should (string-match-p "Radar" prompt))
    (should (string-match-p "Splunk" prompt))
    (should (string-match-p "AI-generated" prompt))
    (should (string-match-p "raw" prompt))
    (should (string-match-p "unavailable" prompt))
    (should (string-match-p "same result-summary" prompt))
    (should (string-match-p "do not invent" prompt))))

(ert-deftest ai-code-test-summary-evidence-declined-no-writeback ()
  "A declined summary never adds evidence capture instructions."
  (should (equal (ai-code-test-summary-evidence--send nil)
                 "Implement the requirement and check it.")))

(ert-deftest ai-code-test-summary-evidence-disabled-for-other-buffers ()
  "Evidence writeback should not silently affect non-Org buffers."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((ai-code--prompt-already-edited t)
          (ai-code-prompt-preprocess-filepaths nil)
          sent)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "No Org summary question")))
                ((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text &optional _transport) (setq sent text))))
        (ai-code--insert-prompt "Explain this code."))
      (should (equal sent "Explain this code.")))))

(provide 'test_ai-code-task-evidence)
;;; test_ai-code-task-evidence.el ends here
