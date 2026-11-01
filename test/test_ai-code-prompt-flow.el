;;; test_ai-code-prompt-flow.el --- Complete prompt editing and sending -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Verify complete-context editing, cancellation, and alternate transports.

;;; Code:

(require 'ert)
(require 'ai-code)

(defun ai-code-flow-test--context-action (action accept)
  "Run ACTION with ACCEPT controlling complete-prompt editing.
Return the editor preview, final prompt, edit count and suffix count."
  (with-temp-buffer
    (if (eq action 'ai-code-implement-todo)
        (progn (org-mode) (insert "* TODO Task\nOriginal human requirement.\n"))
      (emacs-lisp-mode)
      (insert "(defun example ()\n  (+ 1 2))\n"))
    (setq buffer-file-name "/tmp/ai-code-flow.el")
    (goto-char (point-min))
    (let* ((ai-code-use-compose-buffer t)
           (ai-code-prompt-preprocess-filepaths nil)
           (edits 0) (suffixes 0) preview sent
           (ai-code-prompt-suffix-functions
            (list (lambda (context)
                    (should (string-match-p "Edited goal"
                                            (ai-code-prompt-context-prompt-text context)))
                    (cl-incf suffixes)
                    "FLOW SUFFIX"))))
      (cl-letf (((symbol-function 'ai-code-read-string) (lambda (&rest _) "User goal"))
                ((symbol-function 'completing-read) (lambda (&rest _) "Code change"))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () "\nVisible files: @support.el"))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () "Stored raw source"))
                ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
                 (lambda () (unless accept (ert-fail "Cancelled prompt wrote history")) nil))
                ((symbol-function 'ai-code--send-prompt)
                 (lambda (text) (setq sent text)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   (unless accept (ert-fail "Cancelled prompt asked about notes")) nil))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (_label initial &rest _)
                   (cl-incf edits)
                   (setq preview initial)
                   (when accept "Edited goal\nEdited scope\nEdited instruction"))))
        (funcall action nil))
      (list preview sent edits suffixes))))

(ert-deftest ai-code-flow-test-context-actions-edit-the-complete-prompt ()
  "Change, question and TODO expose assembled context to the same editor."
  (dolist (action '(ai-code-code-change ai-code-ask-question ai-code-implement-todo))
    (pcase-let ((`(,preview ,sent ,edits ,suffixes)
                 (ai-code-flow-test--context-action action t)))
      (should preview)
      (should (string-match-p "User goal" preview))
      (should (string-match-p "Stored raw source" preview))
      (should (string-match-p "support.el" preview))
      (when (eq action 'ai-code-implement-todo)
        (should (string-match-p "Original human requirement" preview)))
      (should (equal sent "Edited goal\nEdited scope\nEdited instruction\nFLOW SUFFIX\n"))
      (should (= edits 1))
      (should (= suffixes 1)))))

(ert-deftest ai-code-flow-test-context-action-cancel-has-no-send-side-effects ()
  "Cancelling the full prompt skips history, suffixes, notes and sending."
  (dolist (action '(ai-code-code-change ai-code-ask-question ai-code-implement-todo))
    (pcase-let ((`(,preview ,sent ,edits ,suffixes)
                 (ai-code-flow-test--context-action action nil)))
      (should preview)
      (should-not sent)
      (should (= edits 1))
      (should (= suffixes 0)))))

(ert-deftest ai-code-flow-test-confirmed-prompt-is-not-edited-twice ()
  "An existing confirmation step remains the single full-prompt edit."
  (let ((ai-code-use-compose-buffer t)
        (ai-code-prompt-preprocess-filepaths nil)
        (ai-code-prompt-suffix-functions nil)
        (edits 0) sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _) (cl-incf edits) "Edited\ncomplete\nprompt"))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
              ((symbol-function 'ai-code--send-prompt) (lambda (text) (setq sent text))))
      (with-temp-buffer
        (ai-code--confirm-and-send "Edit: " "Original\ncomplete\nprompt")))
    (should (= edits 1))
    (should (equal sent "Edited\ncomplete\nprompt\n"))))

(ert-deftest ai-code-flow-test-blank-confirmed-prompt-sends-nothing ()
  "Clearing a confirmed prompt cancels the send."
  (let ((ai-code-use-compose-buffer t))
    (cl-letf (((symbol-function 'ai-code-compose-read) (lambda (&rest _) "  \n "))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (&rest _) (ert-fail "Blank prompt was sent"))))
      (should-not (ai-code--confirm-and-send "Edit: " "Original\ncomplete\nprompt")))))

(ert-deftest ai-code-flow-test-verify-keeps-rules-after-one-edit ()
  "Verify's mandatory rules survive editing without another editable pass."
  (let ((ai-code-use-compose-buffer t)
        (ai-code-prompt-suffix-functions nil)
        (edits 0) sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _) (cl-incf edits) "Edited\nreview\nfocus"))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
              ((symbol-function 'ai-code--send-prompt) (lambda (text) (setq sent text))))
      (with-temp-buffer
        (ai-code-verify--confirm-and-send "Original\nreview\nrequest")))
    (should (= edits 1))
    (should (string-match-p "VERIFY MODE SAFETY RULE" sent))
    (should (string-match-p "VERIFY SOURCE-OF-TRUTH RULE" sent))))

(ert-deftest ai-code-flow-test-checkpoint-uses-shared-editor ()
  "Checkpoint honors the existing compose setting through shared confirmation."
  (let ((ai-code-use-compose-buffer t)
        (ai-code-prompt-suffix-functions nil)
        (edits 0) sent)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (ert-fail "Checkpoint should use compose")))
              ((symbol-function 'ai-code-compose-read)
               (lambda (_label initial &rest _)
                 (should (string-match-p "CHECKPOINT" initial))
                 (cl-incf edits)
                 "Edited checkpoint"))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
              ((symbol-function 'ai-code--send-prompt) (lambda (text) (setq sent text))))
      (with-temp-buffer (ai-code-session-checkpoint)))
    (should (= edits 1))
    (should (equal sent "Edited checkpoint\n"))))

(ert-deftest ai-code-flow-test-slash-command-bypasses-editor-and-history ()
  "Direct slash commands retain their command path with compose enabled."
  (let ((ai-code-use-compose-buffer t) executed)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _) (ert-fail "Slash command must not open compose")))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
               (lambda () (ert-fail "Slash command must not enter history")))
              ((symbol-function 'ai-code--execute-command)
               (lambda (text) (setq executed text))))
      (with-temp-buffer (ai-code--insert-prompt "/clear")))
    (should (equal executed "/clear"))))

(ert-deftest ai-code-flow-test-disabled-compose-keeps-direct-send ()
  "Disabling compose preserves the existing direct generated-prompt path."
  (let ((ai-code-use-compose-buffer nil)
        (ai-code-prompt-suffix-functions nil) sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _) (ert-fail "Disabled compose must not open")))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
              ((symbol-function 'ai-code--send-prompt) (lambda (text) (setq sent text))))
      (with-temp-buffer (ai-code--insert-prompt "Goal\nScope\nContext")))
    (should (equal sent "Goal\nScope\nContext\n"))))

(ert-deftest ai-code-flow-test-alternate-transport-records-the-final-text-once ()
  "History and an alternate transport receive the same suffixed prompt."
  (let* ((file (make-temp-file "ai-code-flow-history-" nil ".org"))
         (ai-code-use-compose-buffer nil)
         (ai-code-use-gptel-headline nil)
         (suffixes 0) (sends 0) sent
         (ai-code-prompt-suffix-functions
          (list (lambda (_context) (cl-incf suffixes) "Shared suffix"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ai-code--get-ai-code-prompt-file-path)
                     (lambda () file))
                    ((symbol-function 'ai-code--send-prompt)
                     (lambda (_) (ert-fail "Alternate transport used the AI session"))))
            (with-temp-buffer
              (ai-code--insert-prompt
               "Transport request"
               (lambda (text) (cl-incf sends) (setq sent text)))))
          (should (= suffixes 1))
          (should (= sends 1))
          (should (equal sent "Transport request\nShared suffix\n"))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p (regexp-quote sent) (buffer-string)))))
      (when-let ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest ai-code-flow-test-pipe-reuses-providers-and-streams-literal-text ()
  "The file transport uses all providers once and safely streams multiline text."
  (let* ((directory (make-temp-file "ai-code-pipe-" t))
         (file (expand-file-name "a ' quoted.txt" directory))
         (ai-code-use-compose-buffer nil)
         (ai-code-selected-backend 'gemini)
         (ai-code-cli "cat")
         (calls 0) command
         (ai-code-prompt-suffix-functions
          (list (lambda (_context) (cl-incf calls) "Extension\nsuffix"))))
    (unwind-protect
        (progn
          (with-temp-file file (insert "Original file\n"))
          (with-temp-buffer
            (setq buffer-file-name file)
            (cl-letf (((symbol-function 'ai-code-read-string)
                       (lambda (&rest _) "Keep literal $() and `backticks`"))
                      ((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
                      ((symbol-function 'compilation-start)
                       (lambda (text &rest _) (setq command text))))
              (ai-code-apply-prompt-on-current-file)))
          (should (= calls 1))
          (with-temp-buffer
            (should (zerop (call-process-shell-command command nil t)))
            (should (string-match-p (regexp-quote "Keep literal $() and `backticks`")
                                    (buffer-string)))
            (should (string-match-p "Extension\nsuffix" (buffer-string)))
            (should (string-suffix-p "Original file\n" (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest ai-code-flow-test-pipe-cancel-skips-process-history-and-suffixes ()
  "Cancelling a piped prompt stops before history, providers or process launch."
  (let ((ai-code-use-compose-buffer t)
        (ai-code-selected-backend 'gemini)
        (ai-code-prompt-suffix-functions
         (list (lambda (_) (ert-fail "Cancelled pipe ran suffix providers")))))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/ai-code-input.txt")
      (cl-letf (((symbol-function 'ai-code-read-string) (lambda (&rest _) "Summarize"))
                ((symbol-function 'ai-code-compose-read) (lambda (&rest _) nil))
                ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
                 (lambda () (ert-fail "Cancelled pipe wrote history")))
                ((symbol-function 'compilation-start)
                 (lambda (&rest _) (ert-fail "Cancelled pipe launched CLI"))))
        (should-not (ai-code-apply-prompt-on-current-file))))))

(defvar flycheck-current-errors)

(defun ai-code-flow-test--flycheck-fix ()
  "Run the Flycheck fix command against stubbed diagnostics."
  (setq buffer-file-name "/tmp/project/test.el")
  (setq-local flycheck-mode t)
  (let ((flycheck-current-errors '(fake-error))
        (original-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (or (eq feature 'flycheck)
                     (funcall original-featurep feature subfeature))))
              ((symbol-function 'ai-code--git-root) (lambda () "/tmp/project"))
              ((symbol-function 'ai-code--choose-flycheck-scope)
               (lambda () (list (point-min) (point-max) "whole file")))
              ((symbol-function 'ai-code-flycheck--get-errors-in-scope)
               (lambda (_start _end) '(fake-error)))
              ((symbol-function 'ai-code-flycheck--format-error-list)
               (lambda (_errors _file) "test.el:1:1 fake diagnostic"))
              ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
              ((symbol-function 'ai-code--format-repo-context-info) (lambda () "")))
      (ai-code-flycheck-fix-errors-in-scope))))

(defun ai-code-flow-test--send-region ()
  "Send a multi-line region from a buffer that is not the prompt file."
  (insert "First line\nSecond line\nThird line")
  (setq buffer-file-name "/tmp/notes.txt")
  (cl-letf (((symbol-function 'use-region-p) (lambda () t))
            ((symbol-function 'region-beginning) (lambda () (point-min)))
            ((symbol-function 'region-end) (lambda () (point-max))))
    (ai-code-prompt-send-block)))

(defun ai-code-flow-test--search-notes ()
  "Run note search with stubbed scopes and generated prompt."
  (cl-letf (((symbol-function 'ai-code--ensure-files-directory) (lambda () "/tmp"))
            ((symbol-function 'ai-code--read-note-search-scopes) (lambda (_dir) nil))
            ((symbol-function 'ai-code--build-note-search-prompt)
             (lambda (_scopes _description) "Search\nnotes\nprompt")))
    (ai-code-search-notes-with-ai)))

(defun ai-code-flow-test--search-tasks ()
  "Run task search with a stubbed directory and generated prompt."
  (cl-letf (((symbol-function 'ai-code--read-task-search-directory) (lambda (_dir) "/tmp"))
            ((symbol-function 'ai-code--build-task-search-prompt)
             (lambda (_dir _description) "Search\ntask\nprompt")))
    (ai-code--search-task-files-with-ai "/tmp")))

(defun ai-code-flow-test--speech ()
  "Send a multi-line speech transcription."
  (ai-code--speech-to-text-send-prompt (current-buffer) "Spoken\nmulti-line\nrequest"))

(ert-deftest ai-code-flow-test-self-confirmed-prompts-open-one-editor ()
  "Commands that confirm their complete prompt open compose exactly once."
  (dolist (flow '(ai-code-flow-test--flycheck-fix
                  ai-code-flow-test--send-region
                  ai-code-flow-test--search-notes
                  ai-code-flow-test--search-tasks
                  ai-code-flow-test--speech))
    (let ((ai-code-use-compose-buffer t)
          (ai-code-prompt-preprocess-filepaths nil)
          (ai-code-prompt-suffix-functions nil)
          (ai-code--suppress-org-summary-prompt t)
          (edits 0) sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (label &optional initial _candidates)
                   (when (ai-code--long-prompt-p initial)
                     (ert-fail (format "%s edited the full prompt in the minibuffer: %s"
                                       flow label)))
                   "short input"))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (&rest _) (cl-incf edits) "Edited\ncomplete\nprompt"))
                ((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
                ((symbol-function 'ai-code--send-prompt) (lambda (text) (setq sent text))))
        (with-temp-buffer (funcall flow)))
      (should (equal (list flow edits) (list flow 1)))
      (should (equal (list flow sent) (list flow "Edited\ncomplete\nprompt\n"))))))

(provide 'test_ai-code-prompt-flow)

;;; test_ai-code-prompt-flow.el ends here
