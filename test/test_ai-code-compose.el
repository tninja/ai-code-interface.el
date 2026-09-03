;;; test_ai-code-compose.el --- Tests for prompt compose buffers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ai-code-input)

(ert-deftest ai-code-compose-test-disabled-keeps-existing-reader ()
  "Disabling compose keeps the existing minibuffer/Helm reader path."
  (let ((ai-code-use-compose-buffer nil)
        (ai-code--read-string-fn (lambda (&rest _args) "minibuffer"))
        (this-command 'ai-code-code-change))
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _args)
                 (ert-fail "compose reader should not be called"))))
      (should (equal (ai-code-read-string "Change: " "start")
                     "minibuffer")))))

(ert-deftest ai-code-compose-test-supported-command-routes-to-compose ()
  "Enabled supported commands use the compose reader."
  (let ((ai-code-use-compose-buffer t)
        (ai-code-compose-buffer-commands '(ai-code-code-change))
        (ai-code--read-string-fn
         (lambda (&rest _args)
           (ert-fail "minibuffer reader should not be called")))
        (this-command 'ai-code-code-change)
        captured)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (prompt initial-input candidate-list)
                 (setq captured (list prompt initial-input candidate-list))
                 "edited prompt")))
      (should (equal (ai-code-read-string "Change: " "start" '("one"))
                     "edited prompt"))
      (should (equal captured '("Change: " "start" ("one")))))))

(ert-deftest ai-code-compose-test-unsupported-command-keeps-existing-reader ()
  "Compose does not affect short inputs from commands outside the allow-list."
  (let ((ai-code-use-compose-buffer t)
        (ai-code-compose-buffer-commands '(ai-code-code-change))
        (ai-code--read-string-fn (lambda (&rest _args) "commit message"))
        (this-command 'ai-code-commit-current-change))
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _args)
                 (ert-fail "compose reader should not be called"))))
      (should (equal (ai-code-read-string "Commit message: ")
                     "commit message")))))

(ert-deftest ai-code-compose-test-long-confirm-prompt-uses-compose-when-enabled ()
  "Long confirmation prompts use compose without changing the send pipeline."
  (let ((ai-code-use-compose-buffer t)
        (ai-code-compose-buffer-commands '(ai-code-send-quick-prompt))
        (this-command 'ai-code-send-quick-prompt)
        (long-prompt "one\ntwo\nthree\nfour\nfive\nsix")
        sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (_prompt initial-input _candidate-list)
                 (should (equal initial-input long-prompt))
                 "edited long prompt"))
              ((symbol-function 'read-string)
               (lambda (&rest _args)
                 (ert-fail "read-string should not be called")))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq sent prompt)
                 t)))
      (should (ai-code--confirm-and-send "Edit: " long-prompt))
      (should (equal sent "edited long prompt")))))

(ert-deftest ai-code-compose-test-long-confirm-prompt-preserves-old-path-when-disabled ()
  "The old direct read-string behavior for long defaults remains when disabled."
  (let ((ai-code-use-compose-buffer nil)
        (this-command 'ai-code-send-quick-prompt)
        (long-prompt "one\ntwo\nthree\nfour\nfive\nsix")
        sent)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt initial-input &rest _args)
                 (should (equal initial-input long-prompt))
                 "minibuffer edit"))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq sent prompt)
                 t)))
      (should (ai-code--confirm-and-send "Edit: " long-prompt))
      (should (equal sent "minibuffer edit")))))

(ert-deftest ai-code-compose-test-accept-keeps-leading-and-trims-trailing-space ()
  "Accept returns edited text while removing accidental trailing whitespace."
  (with-temp-buffer
    (ai-code-compose-mode)
    (insert "  keep leading\nsecond line\n\n")
    (let (exited)
      (cl-letf (((symbol-function 'exit-recursive-edit)
                 (lambda () (setq exited t))))
        (ai-code-compose-accept))
      (should exited)
      (should (equal ai-code-compose--result
                     "  keep leading\nsecond line"))
      (should-not ai-code-compose--cancelled))))

(provide 'test-ai-code-compose)

;;; test_ai-code-compose.el ends here
