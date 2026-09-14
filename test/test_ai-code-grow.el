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

(ert-deftest ai-code-grow-test-build-prompt-starts-discussion-only ()
  "Prompt discusses one next step before any Org write-back."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _dir) nil)))
    (let ((prompt (ai-code-grow--build-prompt
                   '(:file "/tmp/task.org" :line 7 :title "Build assistant"))))
      (should (string-match-p "growing-design\\.v1\\.md" prompt))
      (should (string-match-p "Build assistant" prompt))
      (should (string-match-p "line 7" prompt))
      (should (string-match-p "Discuss exactly one recommended next value-growing step" prompt))
      (should (string-match-p "no child step exists yet, this is simply the first step" prompt))
      (should (string-match-p "Do not generate a roadmap or later steps" prompt))
      (should (string-match-p "do not edit any file yet" prompt))
      (should (string-match-p "discuss the step further or write it back" prompt)))))

(ert-deftest ai-code-grow-test-command-saves-current-org-file-before-discussion ()
  "The command saves current Org edits before starting the discussion."
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
            (should (string-match-p "More context" (buffer-string)))
            (should-not (string-match-p "^\\*\\* TODO" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest ai-code-grow-test-harness-requires-discussion-before-writeback ()
  "Harness discusses one valuable step and requires approval before write-back."
  (with-temp-buffer
    (insert-file-contents (ai-code-grow--harness-file))
    (let ((text (buffer-string)))
      (should (string-match-p "Discuss exactly one next growth step" text))
      (should (string-match-p "not to produce a roadmap" text))
      (should (string-match-p "unicycle -> bicycle -> motorcycle -> car" text))
      (should (string-match-p "wheel -> chassis -> engine -> car" text))
      (should (string-match-p "do not modify the Org file or any other file" text))
      (should (string-match-p "discuss the proposal further or write it back" text))
      (should (string-match-p "Only after the user explicitly asks to write it back" text))
      (should (string-match-p "independently verifiable" text)))))

(provide 'test-ai-code-grow)

;;; test_ai-code-grow.el ends here
