;;; test_ai-code-grow.el --- Tests for grow-next-step workflow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'ai-code-grow)

(ert-deftest ai-code-grow-test-heading-context-uses-containing-headline ()
  "Point in a headline body resolves to that existing headline."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name "/tmp/task.org")
    (insert "* TODO Build assistant\nDescribe the idea here.\n** DONE Existing child\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((context (ai-code-grow--heading-context)))
      (should (equal (plist-get context :file) "/tmp/task.org"))
      (should (= (plist-get context :line) 1))
      (should (equal (plist-get context :title) "Build assistant")))))

(ert-deftest ai-code-grow-test-heading-context-requires-headline ()
  "Grow Next Step requires point to be inside an Org headline subtree."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name "/tmp/task.org")
    (insert "No headline yet.\n")
    (should-error (ai-code-grow--heading-context) :type 'user-error)))

(ert-deftest ai-code-grow-test-build-prompt-generates-only-next-step ()
  "Prompt requests one next direct child and explicitly rejects a roadmap."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _dir) nil)))
    (let ((prompt (ai-code-grow--build-prompt
                   '(:file "/tmp/task.org" :line 7 :title "Build assistant"))))
      (should (string-match-p "growing-design\\.v1\\.md" prompt))
      (should (string-match-p "Build assistant" prompt))
      (should (string-match-p "line 7" prompt))
      (should (string-match-p "exactly one next value-growing direct child TODO step" prompt))
      (should (string-match-p "no child step exists yet, this is simply the first step" prompt))
      (should (string-match-p "Do not generate a roadmap or later steps" prompt))
      (should (string-match-p "Do not modify program code" prompt)))))

(ert-deftest ai-code-grow-test-command-saves-current-org-file-before-sending ()
  "The command saves current Org edits before asking for the next step."
  (let* ((file (make-temp-file "ai-code-grow-" nil ".org"
                               "* TODO Build something useful\nInitial idea.\n"))
         (buffer (find-file-noselect file))
         captured-prompt)
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (goto-char (point-max))
          (insert "More context.\n")
          (goto-char (point-min))
          (forward-line 1)
          (cl-letf (((symbol-function 'ai-code--insert-prompt)
                     (lambda (prompt) (setq captured-prompt prompt)))
                    ((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) nil)))
            (ai-code-grow-next-step))
          (should captured-prompt)
          (should-not (buffer-modified-p))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "More context" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest ai-code-grow-test-harness-requires-one-value-step ()
  "Harness requests one valuable step rather than a full breakdown."
  (with-temp-buffer
    (insert-file-contents (ai-code-grow--harness-file))
    (let ((text (buffer-string)))
      (should (string-match-p "Choose exactly one next growth step" text))
      (should (string-match-p "not to produce a roadmap" text))
      (should (string-match-p "unicycle -> bicycle -> motorcycle -> car" text))
      (should (string-match-p "wheel -> chassis -> engine -> car" text))
      (should (string-match-p "Generate or refine exactly one direct child `TODO`" text))
      (should (string-match-p "unfinished direct child" text))
      (should (string-match-p "independently verifiable" text))
      (should (string-match-p "Do not speculate about later steps" text)))))

(provide 'test-ai-code-grow)

;;; test_ai-code-grow.el ends here
