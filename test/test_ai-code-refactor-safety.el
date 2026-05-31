;;; test_ai-code-refactor-safety.el --- Refactor safety tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests that pin behavior around helper-extraction refactors.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ai-code-backends-infra)
(require 'ai-code-change)

(defun ai-code-refactor-safety-test--in-session-buffer (body-fn)
  "Run BODY-FN in a buffer whose name is recognized as an AI session."
  (let ((buffer (generate-new-buffer "*codex[refactor-safety]*")))
    (unwind-protect
        (with-current-buffer buffer
          (funcall body-fn))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-refactor-safety-strip-alternate-screen-transitions-and-scrollback-clear ()
  "Alternate-screen filtering should still strip 1049h/1049l and ED3."
  (ai-code-refactor-safety-test--in-session-buffer
   (lambda ()
     (let ((ai-code-backends-infra-strip-alternate-screen t)
           (ai-code-backends-infra-strip-alternate-screen-debug nil))
       (should (equal (ai-code-backends-infra--strip-alternate-screen-sequences
                       (concat "\033[?1049h" "hello" "\033[?1049l" "\033[3J"))
                      "hello"))))))

(ert-deftest ai-code-refactor-safety-screen-clear-injects-scrollback-sequence ()
  "ED2 screen clear should still be converted to scrollback preservation."
  (ai-code-refactor-safety-test--in-session-buffer
   (lambda ()
     (let ((ai-code-backends-infra-strip-alternate-screen t)
           (ai-code-backends-infra-scrollback-inject-interval 0)
           (ai-code-backends-infra--last-scrollback-inject-time 0))
       (cl-letf (((symbol-function 'ai-code-backends-infra--scroll-to-scrollback-sequence)
                  (lambda () "<SCROLL>")))
         (should (equal (ai-code-backends-infra--strip-alternate-screen-sequences
                         (concat "before" "\033[2J" "after"))
                        "before<SCROLL>after")))))))

(ert-deftest ai-code-refactor-safety-home-erase-injects-scrollback-and-restores-home ()
  "Home+erase redraws should still preserve scrollback and keep cursor home."
  (ai-code-refactor-safety-test--in-session-buffer
   (lambda ()
     (let ((ai-code-backends-infra-strip-alternate-screen t)
           (ai-code-backends-infra-scrollback-inject-interval 0)
           (ai-code-backends-infra--last-scrollback-inject-time 0))
       (cl-letf (((symbol-function 'ai-code-backends-infra--scroll-to-scrollback-sequence)
                  (lambda () "<SCROLL>")))
         (should (equal (ai-code-backends-infra--strip-alternate-screen-sequences
                         (concat "\033[H" "\033[J" "frame"))
                        (concat "<SCROLL>" "\033[H" "frame"))))))))

(ert-deftest ai-code-refactor-safety-sync-redraw-injects-before-frame ()
  "Synchronized redraws should still inject scrollback before the redraw frame."
  (ai-code-refactor-safety-test--in-session-buffer
   (lambda ()
     (let ((ai-code-backends-infra-strip-alternate-screen t)
           (ai-code-backends-infra-scrollback-inject-interval 0)
           (ai-code-backends-infra--last-scrollback-inject-time 0)
           (ai-code-backends-infra--sync-redraw-scrollback t)
           (prefix (make-string 501 ?x)))
       (cl-letf (((symbol-function 'ai-code-backends-infra--scroll-to-scrollback-sequence)
                  (lambda () "<SCROLL>")))
         (should (equal (ai-code-backends-infra--strip-alternate-screen-sequences
                         (concat prefix "\033[?2026h" "\033[1;1H" "frame"))
                        (concat prefix "<SCROLL>" "\033[?2026h" "\033[1;1H" "frame"))))))))

(ert-deftest ai-code-refactor-safety-implement-todo-comment-ask-question-with-clipboard ()
  "TODO comment ask-question path should preserve labels, context, and suffixes."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: explain this behavior\n")
    (goto-char (point-min))
    (let (captured-label
          captured-initial-input
          captured-final-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (should (member "Ask question" candidates))
                   "Ask question"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (label initial-input)
                   (setq captured-label label
                         captured-initial-input initial-input)
                   initial-input))
                ((symbol-function 'ai-code--get-clipboard-text)
                 (lambda () "clipboard details"))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () "\nContext files:\n- ai-code-change.el"))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () "\nRepo context here"))
                ((symbol-function 'ai-code--get-function-name-for-comment)
                 (lambda () nil))
                ((symbol-function 'which-function)
                 (lambda () nil))
                ((symbol-function 'region-active-p)
                 (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt)
                   (setq captured-final-prompt prompt))))
        (ai-code--implement-todo--build-and-send-prompt t)
        (should (equal captured-label
                       "Question about TODO comment (clipboard context): "))
        (should (string-match-p "Please answer my question about this TODO comment"
                                captured-initial-input))
        (should-not (string-match-p "Please implement code" captured-initial-input))
        (should (string-match-p "Context files" captured-final-prompt))
        (should (string-match-p "Clipboard context:\nclipboard details" captured-final-prompt))
        (should (string-match-p "Repo context here" captured-final-prompt))
        (should (string-match-p "do not make any code change" captured-final-prompt))))))

(ert-deftest ai-code-refactor-safety-implement-todo-org-code-change-with-context ()
  "Org headline code-change path should preserve headline/body/files/repo context."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "todo.org")
    (insert "* TODO Refactor CLI helpers\n")
    (insert "Keep existing user-visible behavior.\n")
    (org-mode)
    (goto-char (point-min))
    (let (captured-label
          captured-final-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (should (member "Code change" candidates))
                   "Code change"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (label initial-input)
                   (setq captured-label label)
                   initial-input))
                ((symbol-function 'ai-code--get-clipboard-text)
                 (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () "\nContext files:\n- ai-code-backends-infra.el"))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () "\nRepo context here"))
                ((symbol-function 'which-function)
                 (lambda () nil))
                ((symbol-function 'region-active-p)
                 (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt)
                   (setq captured-final-prompt prompt))))
        (ai-code--implement-todo--build-and-send-prompt nil)
        (should (equal captured-label
                       "Implementation instruction for Org headline: "))
        (should (string-match-p "Please implement code for this Org headline" captured-final-prompt))
        (should (string-match-p "TODO Refactor CLI helpers" captured-final-prompt))
        (should (string-match-p "Keep existing user-visible behavior" captured-final-prompt))
        (should (string-match-p "Context files" captured-final-prompt))
        (should (string-match-p "Repo context here" captured-final-prompt))
        (should-not (string-match-p "do not make any code change" captured-final-prompt))))))

(provide 'test_ai-code-refactor-safety)

;;; test_ai-code-refactor-safety.el ends here
