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

(defun ai-code-test-summary-artifacts--guidance ()
  "Read the bundled Org result-summary guidance used by the prompt."
  (with-temp-buffer
    (insert-file-contents (ai-code--org-summary-guidance-path))
    (buffer-string)))

(ert-deftest ai-code-test-summary-artifacts-automatic-when-accepted ()
  "Append artifact collection instructions as part of the same summary."
  (let ((prompt (ai-code-test-summary-artifacts--send t)))
    (should (string-match-p "After completing, append a concise result summary" prompt))
    (should (string-suffix-p (ai-code--org-summary-reference-instruction) prompt))
    (should (string-match-p "org-result-summary\\.v1\\.md" prompt))
    (should-not (string-search "When writing that same result-summary" prompt))
    (should (< (- (length prompt)
                  (length (ai-code-test-summary-artifacts--send nil)))
               900))))

(ert-deftest ai-code-test-summary-artifacts-guards-git-and-pr-head ()
  "Git reads are conditional and a remote PR head is never local HEAD."
  (let ((case-fold-search nil))
    (should (string-match-p "If this task changed code, read Git"
                            (ai-code-test-summary-artifacts--guidance)))
    (should (string-match-p "not local HEAD"
                            (ai-code-test-summary-artifacts--guidance)))
    (should-not (string-match-p "completing the implementation"
                                (ai-code-test-summary-artifacts--guidance)))))

(ert-deftest ai-code-test-summary-artifacts-with-section-anchor ()
  "An anchored summary, as used by Verify, also gets artifact guidance."
  (let ((prompt (ai-code-test-summary-artifacts--send
                 t '(:name "Fix config merge" :end-line 2))))
    (should (string-match-p "end of section \"Fix config merge\"" prompt))
    (should (string-suffix-p (ai-code--org-summary-reference-instruction) prompt))))

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
  (let ((prompt (ai-code-test-summary-artifacts--send t))
        (guidance (ai-code-test-summary-artifacts--guidance)))
    (should (string-match-p "org-result-summary\\.v1\\.md" prompt))
    (should (string-search
             (concat "** AI User config now overrides project defaults :CODEX:\n"
                     "[YYYY-MM-DD Day HH:MM]") guidance))
    (should (string-search "first content line directly" guidance))
    (should (string-match-p "at summary[[:space:]]+writeback[[:space:]]+time" guidance))
    (should (string-search "not at prompt-send time" guidance))
    (should (string-search "timestamp is unavailable" guidance))))

(ert-deftest ai-code-test-summary-artifacts-timestamp-with-anchored-section ()
  "An anchored summary gets the same headline and timestamp guidance."
  (let ((prompt (ai-code-test-summary-artifacts--send
                 t '(:name "Fix config merge" :end-line 2)))
        (guidance (ai-code-test-summary-artifacts--guidance)))
    (should (string-search "end of section \"Fix config merge\"" prompt))
    (should (string-search
             (concat "** AI User config now overrides project defaults :CODEX:\n"
                     "[YYYY-MM-DD Day HH:MM]") guidance))))

(ert-deftest ai-code-test-summary-artifacts-ai-authorship-is-marked ()
  "Result subheadings use AI as keyword, agent as tag, and specific titles."
  (let ((guidance (ai-code-test-summary-artifacts--guidance)))
    (should (string-search "task-specific title" guidance))
    (should (string-search "actual outcome" guidance))
    (should (string-search "Do not use generic" guidance))
    (should (string-search "do not claim a fix" guidance))
    (should (string-search "org-todo-keywords" guidance))
    (should (string-search ":CLAUDE_CODE:" guidance))
    (should (string-search ":GEMINI:" guidance))
    (should (string-search ":UNKNOWN:" guidance))
    (should-not (string-search ":AI_AGENT:" guidance))
    (should (string-match-p "Do not add a property[[:space:]]+drawer" guidance))))

(ert-deftest ai-code-test-summary-artifacts-anchored-notes-mark-ai-authorship ()
  "Explicit section anchors retain agent tags and informative headings."
  (let ((prompt (ai-code-test-summary-artifacts--send
                 t '(:name "Investigate regression" :end-line 9)))
        (guidance (ai-code-test-summary-artifacts--guidance)))
    (should (string-search "Investigate regression" prompt))
    (should (string-search "task-specific title" guidance))
    (should (string-search ":CODEX:" guidance))))

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

(ert-deftest ai-code-test-summary-artifacts-unreadable-guidance-fails-closed ()
  "Do not silently send a partial summary prompt if guidance is missing."
  (cl-letf (((symbol-function 'locate-library)
             (lambda (&rest _) "/nonexistent-ai-code-package/ai-code-prompt-mode.el")))
    (should-error (ai-code-test-summary-artifacts--send t)
                  :type 'user-error)))

(ert-deftest ai-code-test-summary-artifacts-decline-does-not-load-guidance ()
  "Declining the opt-in must not depend on the bundled guidance file."
  (cl-letf (((symbol-function 'ai-code--org-summary-guidance-path)
             (lambda () (ert-fail "No file read when summary declined"))))
    (should (equal (ai-code-test-summary-artifacts--send nil)
                   "Implement the requested fix."))))

(provide 'test_ai-code-task-artifacts)
;;; test_ai-code-task-artifacts.el ends here
