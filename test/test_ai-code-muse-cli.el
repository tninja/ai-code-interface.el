;;; test_ai-code-muse-cli.el --- Tests for ai-code-muse-cli.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-muse-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-muse-cli)

(ert-deftest ai-code-test-muse-cli-backend-registered ()
  "Muse backend should be registered in `ai-code-backends'."
  (let ((spec (assq 'muse ai-code-backends)))
    (should spec)
    (should (equal (plist-get (cdr spec) :label) "Muse Code"))
    (should (eq (plist-get (cdr spec) :require) 'ai-code-muse-cli))
    (should (equal (plist-get (cdr spec) :cli) "muse"))))

(ert-deftest ai-code-test-muse-cli-start-uses-generic-helper ()
  "Muse startup should delegate generic session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "muse --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-muse-cli-program "muse-test")
            (ai-code-muse-cli-program-switches '("--debug")))
        (ai-code-muse-cli 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "muse-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Muse Code"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-muse-cli--processes))
    (should (equal (plist-get captured-options :session-prefix) "muse"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-muse-cli-send-escape))))

(ert-deftest ai-code-test-muse-cli-resume-appends-resume-without-mutating-global ()
  "Resume should append \"resume\" via let-binding and not mutate global switches."
  (let ((ai-code-muse-cli-program-switches '("--foo"))
        (captured-switches nil))
    (cl-letf (((symbol-function 'ai-code-muse-cli)
               (lambda (&optional _arg)
                 (setq captured-switches ai-code-muse-cli-program-switches)))
              ((symbol-function 'ai-code-backends-infra--cli-show-resume-picker)
               (lambda (_prefix) nil)))
      (ai-code-muse-cli-resume nil)
      (should (equal captured-switches '("--foo" "resume")))
      (should (equal ai-code-muse-cli-program-switches '("--foo"))))))

(ert-deftest ai-code-test-muse-cli-resume-shows-picker ()
  "Resume should invoke the resume picker for the muse prefix."
  (let ((picker-called nil)
        (picker-prefix nil))
    (cl-letf (((symbol-function 'ai-code-muse-cli)
               (lambda (&optional _arg) nil))
              ((symbol-function 'ai-code-backends-infra--cli-show-resume-picker)
               (lambda (prefix)
                 (setq picker-called t
                       picker-prefix prefix))))
      (ai-code-muse-cli-resume 'some-arg)
      (should picker-called)
      (should (equal picker-prefix "muse")))))

(ert-deftest ai-code-test-muse-cli-resume-passes-arg-through ()
  "Resume should forward ARG to the start command."
  (let ((forwarded-arg 'not-set)
        (picker-called nil))
    (cl-letf (((symbol-function 'ai-code-muse-cli)
               (lambda (&optional arg)
                 (setq forwarded-arg arg)))
              ((symbol-function 'ai-code-backends-infra--cli-show-resume-picker)
               (lambda (_prefix) (setq picker-called t))))
      (ai-code-muse-cli-resume 'my-arg)
      (should (eq forwarded-arg 'my-arg))
      (should picker-called))))

(provide 'test_ai-code-muse-cli)

;;; test_ai-code-muse-cli.el ends here
