;;; test_ai-code-task-artifacts.el --- Automatic artifact summary tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Artifacts are gathered at task completion by the existing opt-in
;; Org result-summary workflow, not entered into a separate UI.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'ai-code-prompt-mode)

(defun ai-code-test-summary-artifacts--send (accept &optional anchor)
  "Return dispatched prompt when the Org summary choice is ACCEPT.
ANCHOR, when non-nil, is bound as `ai-code--org-summary-anchor'."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example-task.org")
    (insert "* TODO Fix config merge\nHuman-authored source: rdar://12345\n")
    (org-mode)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (let ((ai-code--prompt-already-edited t)
          (ai-code-prompt-preprocess-filepaths nil)
          (ai-code--suppress-org-summary-prompt nil)
          (ai-code--org-summary-anchor anchor)
          sent)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (question)
                   (should (equal question "Append result summary to current section? "))
                   accept))
                ((symbol-function 'ai-code--git-root)
                 (lambda (&rest _) (ert-fail "No Git snapshot at prompt-send time")))
                ((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text &optional _transport) (setq sent text))))
        (ai-code--insert-prompt "Implement the requested fix."))
      (should-not (buffer-modified-p))
      sent)))

(ert-deftest ai-code-test-summary-artifacts-automatic-when-accepted ()
  "Append artifact collection instructions as part of the same summary."
  (let ((prompt (ai-code-test-summary-artifacts--send t)))
    (should (string-match-p "After completing, append a concise result summary" prompt))
    (should (string-suffix-p ai-code--org-summary-artifacts-instruction prompt))))

(ert-deftest ai-code-test-summary-artifacts-guards-git-and-pr-head ()
  "Git reads are conditional and a remote PR head is never local HEAD."
  (let ((case-fold-search nil))
    (should (string-match-p "If this task changed code, read Git"
                            ai-code--org-summary-artifacts-instruction))
    (should (string-match-p "not local HEAD"
                            ai-code--org-summary-artifacts-instruction))
    (should-not (string-match-p "completing the implementation"
                                ai-code--org-summary-artifacts-instruction))))

(ert-deftest ai-code-test-summary-artifacts-with-section-anchor ()
  "An anchored summary, as used by Verify, also gets artifact guidance."
  (let ((prompt (ai-code-test-summary-artifacts--send
                 t '(:name "Fix config merge" :end-line 2))))
    (should (string-match-p "end of section \"Fix config merge\"" prompt))
    (should (string-suffix-p ai-code--org-summary-artifacts-instruction prompt))))

(ert-deftest ai-code-test-summary-artifacts-not-requested ()
  "Declining summary means no artifact writeback and no extra menu."
  (let ((prompt (ai-code-test-summary-artifacts--send nil)))
    (should (equal prompt "Implement the requested fix."))))

(ert-deftest ai-code-test-summary-artifacts-preserves-non-org-workflow ()
  "Plain buffers are not asked to append a summary."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((ai-code--prompt-already-edited t)
          (ai-code-prompt-preprocess-filepaths nil)
          sent)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "No Org summary question")))
                ((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text &optional _transport) (setq sent text))))
        (ai-code--insert-prompt "Review this function."))
      (should (equal sent "Review this function.")))))

(ert-deftest ai-code-test-summary-artifacts-timestamp-records-writeback-time ()
  "Opted-in results request a timestamp below a specific Org headline."
  (let ((prompt (ai-code-test-summary-artifacts--send t)))
    (should (string-search
             (concat "** AI Fixed config merge precedence :CODEX:\n"
                     "[YYYY-MM-DD Day HH:MM]") prompt))
    (should (string-search "first content line directly" prompt))
    (should (string-search "at summary writeback time" prompt))
    (should (string-search "not at prompt-send time" prompt))
    (should (string-search "timestamp is unavailable" prompt))))

(ert-deftest ai-code-test-summary-artifacts-timestamp-with-anchored-section ()
  "An anchored summary gets the same headline and timestamp guidance."
  (let ((prompt (ai-code-test-summary-artifacts--send
                 t '(:name "Fix config merge" :end-line 2))))
    (should (string-search "end of section \"Fix config merge\"" prompt))
    (should (string-search
             (concat "** AI Fixed config merge precedence :CODEX:\n"
                     "[YYYY-MM-DD Day HH:MM]") prompt))))

(ert-deftest ai-code-test-summary-artifacts-ai-authorship-is-marked ()
  "Result subheadings use AI as keyword, agent as tag, and specific titles."
  (let ((prompt (ai-code-test-summary-artifacts--send t)))
    (should (string-search "task-specific title" prompt))
    (should (string-search "actual outcome" prompt))
    (should (string-search "Do not use generic" prompt))
    (should (string-search "do not claim a fix" prompt))
    (should (string-search "org-todo-keywords" prompt))
    (should (string-search ":CLAUDE_CODE:" prompt))
    (should (string-search ":GEMINI:" prompt))
    (should (string-search ":UNKNOWN:" prompt))
    (should-not (string-search ":AI_AGENT:" prompt))
    (should (string-search "Do not add a property drawer" prompt))))

(ert-deftest ai-code-test-summary-artifacts-anchored-notes-mark-ai-authorship ()
  "Explicit section anchors retain agent tags and informative headings."
  (let ((prompt (ai-code-test-summary-artifacts--send
                 t '(:name "Investigate regression" :end-line 9))))
    (should (string-search "Investigate regression" prompt))
    (should (string-search "task-specific title" prompt))
    (should (string-search ":CODEX:" prompt))))

(ert-deftest ai-code-test-summary-artifacts-org-parses-ai-keyword-and-agent-tag ()
  "Org recognizes the AI keyword and CODEX tag with timestamp below."
  (let ((org-todo-keywords '((sequence "TODO" "AI" "|" "DONE"))))
    (with-temp-buffer
      (org-mode)
      (insert "** AI Fixed config merge precedence :CODEX:\n"
              "[2026-11-10 Tue 06:41]\n"
              "Investigated and fixed ordering.\n")
      (goto-char (point-min))
      (should (equal (org-get-todo-state) "AI"))
      (should (equal (org-get-heading t t t t)
                     "Fixed config merge precedence"))
      (should (member "CODEX" (org-get-tags)))
      (forward-line 1)
      (should (looking-at-p "^\\[2026-11-10 Tue 06:41\\]$")))))

(provide 'test_ai-code-task-artifacts)
;;; test_ai-code-task-artifacts.el ends here
