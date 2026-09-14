;;; test_ai-code-grow.el --- Tests for growing design workflow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'ai-code-grow)

(ert-deftest ai-code-grow-test-ensure-design-heading-before-code-change ()
  "Growing Design is inserted once, before Code Change."
  (with-temp-buffer
    (org-mode)
    (insert "* Task Description\n\nA task.\n\n")
    (insert "* Investigation\n\nNotes.\n\n")
    (insert "* Code Change\n\n")
    (should (ai-code-grow--ensure-design-heading))
    (should (string-match-p
             (concat "\\* Growing Design\\n\\n"
                     "\\* Code Change")
             (buffer-string)))
    (should-not (ai-code-grow--ensure-design-heading))
    (goto-char (point-min))
    (should (= 1 (how-many "^\\* Growing Design$" (point-min) (point-max))))))

(ert-deftest ai-code-grow-test-ensure-design-heading-appends-without-code-change ()
  "Growing Design is appended when the task file has no Code Change heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Task Description\n\nA task.\n")
    (should (ai-code-grow--ensure-design-heading))
    (should (string-match-p "\\* Growing Design\\n\\'" (buffer-string)))))

(ert-deftest ai-code-grow-test-build-prompt-is-design-only ()
  "Grow prompt references the task and enforces the design-only boundary."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _dir) nil)))
    (let ((prompt (ai-code-grow--build-prompt "/tmp/example-task.org")))
      (should (string-match-p "growing-design\\.v1\\.md" prompt))
      (should (string-match-p "@/tmp/example-task\\.org" prompt))
      (should (string-match-p "Update only the task file's top-level \\* Growing Design section" prompt))
      (should (string-match-p "Do not modify program code" prompt))
      (should (string-match-p "only one next growth increment" prompt)))))

(ert-deftest ai-code-grow-test-command-updates-task-document-before-sending ()
  "The interactive command saves Growing Design and sends one design prompt."
  (let* ((file (make-temp-file "ai-code-grow-" nil ".org"
                               "* Task Description\n\nBuild something useful.\n\n* Code Change\n\n"))
         (buffer (find-file-noselect file))
         captured-prompt)
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (cl-letf (((symbol-function 'ai-code--insert-prompt)
                     (lambda (prompt) (setq captured-prompt prompt)))
                    ((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) nil)))
            (ai-code-grow-design))
          (should captured-prompt)
          (should-not (buffer-modified-p))
          (goto-char (point-min))
          (should (re-search-forward "^\\* Growing Design$" nil t))
          (with-temp-buffer
            (insert-file-contents file)
            (should (re-search-forward "^\\* Growing Design$" nil t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest ai-code-grow-test-harness-requires-value-and-single-frontier ()
  "The harness protects value delivery and a single active increment."
  (with-temp-buffer
    (insert-file-contents (ai-code-grow--harness-file))
    (let ((text (buffer-string)))
      (should (string-match-p "Every increment must deliver observable user value" text))
      (should (string-match-p "At most one `TODO Increment` may exist at a time" text))
      (should (string-match-p "If development stopped after this increment" text))
      (should (string-match-p "After updating the design document, stop" text)))))

(provide 'test-ai-code-grow)

;;; test_ai-code-grow.el ends here
