;;; test_ai-code-goose-cli.el --- Tests for Goose CLI -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Verify Goose registration, command construction, and shared session routing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-goose-cli)

(ert-deftest ai-code-test-goose-cli-registration ()
  "Goose should expose the standard backend entry points."
  (let ((spec (cdr (assq 'goose ai-code-backends))))
    (should (equal (plist-get spec :cli) "goose"))
    (should (eq (plist-get spec :require) 'ai-code-goose-cli))
    (should (equal (plist-get spec :agent-file) "AGENTS.md"))
    (dolist (key '(:start :switch :send :resume))
      (should (fboundp (plist-get spec key))))))

(ert-deftest ai-code-test-goose-cli-start-and-resume ()
  "Use Goose's session subcommand and preserve customized options."
  (let ((ai-code-goose-cli-program "custom-goose")
        (ai-code-goose-cli-program-switches '("--name" "example"))
        (ai-code-goose-cli-multiline-input-sequence "\C-k")
        calls)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg) (push (list options arg) calls))))
      (ai-code-goose-cli)
      (ai-code-goose-cli-resume '(4)))
    (let ((resume (caar calls))
          (start (caadr calls)))
      (should-not (cadadr calls))
      (should (equal (cadar calls) '(4)))
      (should (equal (plist-get start :program) "custom-goose"))
      (should (equal (plist-get start :switches)
                     '("session" "--name" "example")))
      (should (equal (plist-get resume :switches)
                     '("session" "--resume" "--name" "example")))
      (should (equal (plist-get start :session-prefix) "goose"))
      (should (eq (plist-get start :process-table)
                  ai-code-goose-cli--processes))
      (should (equal (plist-get start :multiline-input-sequence) "\C-k"))
      (should (equal ai-code-goose-cli-program-switches
                     '("--name" "example"))))))

(ert-deftest ai-code-test-goose-cli-switch-and-send ()
  "Delegate switching and sending to the Goose session group."
  (let (calls)
    (cl-letf (((symbol-function 'ai-code-backends-infra--cli-switch-to-buffer)
               (lambda (&rest args) (push args calls)))
              ((symbol-function 'ai-code-backends-infra--cli-send-command)
               (lambda (&rest args) (push args calls))))
      (ai-code-goose-cli-switch-to-buffer '(4))
      (ai-code-goose-cli-send-command "first\nsecond"))
    (should (equal (nreverse calls)
                   '(("Goose" "goose" (4))
                     ("Goose" "goose" "first\nsecond"))))))

(ert-deftest ai-code-test-goose-cli-multiline-paste-default ()
  "The default shared submission policy should paste Goose prompts."
  (should (member "goose"
                  (default-value 'ai-code-backends-infra-use-paste-backends))))

(provide 'test_ai-code-goose-cli)

;;; test_ai-code-goose-cli.el ends here
