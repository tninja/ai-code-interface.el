;;; test_ai-code-compose.el --- Tests for prompt compose buffers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ai-code-input)

(ert-deftest ai-code-compose-test-read-string-stays-on-existing-reader ()
  "Direct `ai-code-read-string' calls do not use compose by command name."
  (let ((ai-code-use-compose-buffer t)
        (ai-code--read-string-fn (lambda (&rest _args) "existing reader"))
        (this-command 'ai-code-code-change))
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _args)
                 (ert-fail "compose reader should not be called"))))
      (should (equal (ai-code-read-string "Change: " "one\ntwo\nthree\nfour\nfive\nsix")
                     "existing reader")))))

(ert-deftest ai-code-compose-test-five-line-confirm-keeps-existing-reader ()
  "Five-line prompts stay on the existing reader even when compose is enabled."
  (let ((ai-code-use-compose-buffer t)
        (ai-code--read-string-fn (lambda (&rest _args) "edited short prompt"))
        (this-command 'ai-code-send-quick-prompt)
        (prompt "one\ntwo\nthree\nfour\nfive")
        sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _args)
                 (ert-fail "compose reader should not be called")))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (text)
                 (setq sent text)
                 t)))
      (should (ai-code--confirm-and-send "Edit: " prompt))
      (should (equal sent "edited short prompt")))))

(ert-deftest ai-code-compose-test-long-confirm-uses-compose-when-enabled ()
  "Prompts over five lines use compose regardless of the originating command."
  (let ((ai-code-use-compose-buffer t)
        ;; Deliberately use a command unrelated to the original allow-list to
        ;; prove command identity is not part of compose eligibility.
        (this-command 'ai-code-commit-current-change)
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

(ert-deftest ai-code-compose-test-long-confirm-preserves-old-path-when-disabled ()
  "Long prompts keep the previous direct `read-string' behavior when disabled."
  (let ((ai-code-use-compose-buffer nil)
        (this-command 'ai-code-send-quick-prompt)
        (long-prompt "one\ntwo\nthree\nfour\nfive\nsix")
        sent)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt initial-input &rest _args)
                 (should (equal initial-input long-prompt))
                 "minibuffer edit"))
              ((symbol-function 'ai-code-compose-read)
               (lambda (&rest _args)
                 (ert-fail "compose reader should not be called")))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq sent prompt)
                 t)))
      (should (ai-code--confirm-and-send "Edit: " long-prompt))
      (should (equal sent "minibuffer edit")))))

(ert-deftest ai-code-compose-test-mode-reuses-prompt-path-completion ()
  "Compose mode installs the same path-completion hooks as prompt mode."
  (with-temp-buffer
    (ai-code-compose-mode)
    (should (memq #'ai-code--prompt-filepath-capf
                  completion-at-point-functions))
    (should (memq #'ai-code--prompt-auto-trigger-filepath-completion
                  post-self-insert-hook))))

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
