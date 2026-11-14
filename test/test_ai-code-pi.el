;;; test_ai-code-pi.el --- Tests for ai-code-pi.el -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the Pi coding agent backend.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-pi)

(ert-deftest ai-code-test-pi-start-uses-generic-helper ()
  "Pi startup should delegate session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg))))
      (let ((ai-code-pi-program "pi-test")
            (ai-code-pi-program-switches '("--offline"))
            (ai-code-pi-tui-mode 'regular)
            (ai-code-pi-multiline-input-sequence "\e[13;2u"))
        (ai-code-pi-start 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "pi-test"))
    (should (equal (plist-get captured-options :switches)
                   '("--offline" "--tui-mode" "regular")))
    (should (equal (plist-get captured-options :label) "Pi"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-pi--processes))
    (should (equal (plist-get captured-options :session-prefix) "pi"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-pi-send-escape))
    (should (equal (plist-get captured-options :multiline-input-sequence)
                   "\e[13;2u"))))

(ert-deftest ai-code-test-pi-start-defaults-to-regular-tui-mode ()
  "Pi should default to its normal-screen TUI so Emacs keeps scrollback."
  (should (eq (default-value 'ai-code-pi-tui-mode) 'regular)))

(ert-deftest ai-code-test-pi-startup-switches-respect-nil-tui-mode ()
  "A nil TUI mode should leave Pi's configured mode untouched."
  (let ((ai-code-pi-tui-mode nil))
    (should (equal (ai-code-pi--startup-switches '("--offline"))
                   '("--offline")))))

(ert-deftest ai-code-test-pi-startup-switches-keep-explicit-tui-mode ()
  "An explicit --tui-mode switch should take precedence over the option."
  (let ((ai-code-pi-tui-mode 'regular))
    (should (equal (ai-code-pi--startup-switches
                    '("--tui-mode" "fullscreen"))
                   '("--tui-mode" "fullscreen")))
    (should (equal (ai-code-pi--startup-switches
                    '("--tui-mode=fullscreen"))
                   '("--tui-mode=fullscreen")))
    (should (equal (ai-code-pi--startup-switches '("--tui-modes"))
                   '("--tui-modes" "--tui-mode" "regular")))))

(ert-deftest ai-code-test-pi-resume-passes-tui-mode-after-resume ()
  "Pi resume should keep --resume and still apply the TUI mode."
  (let (captured-options)
    (cl-letf (((symbol-function 'ai-code-backends-infra--selected-session-id)
               (lambda () nil))
              ((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options _arg)
                 (setq captured-options options)))
              ((symbol-function 'ai-code-backends-infra--cli-show-resume-picker)
               #'ignore))
      (let ((ai-code-pi-program-switches nil)
            (ai-code-pi-tui-mode 'regular))
        (ai-code-pi-resume)))
    (should (equal (plist-get captured-options :switches)
                   '("--resume" "--tui-mode" "regular")))))

(ert-deftest ai-code-test-pi-resume-opens-session-picker ()
  "Pi resume should launch with --resume and display its picker."
  (let (captured-switches
        captured-arg
        captured-prefix)
    (cl-letf (((symbol-function 'ai-code-backends-infra--selected-session-id)
               (lambda () nil))
              ((symbol-function 'ai-code-pi-start)
               (lambda (&optional arg)
                 (setq captured-switches ai-code-pi-program-switches
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--cli-show-resume-picker)
               (lambda (prefix)
                 (setq captured-prefix prefix))))
      (let ((ai-code-pi-program-switches '("--offline")))
        (ai-code-pi-resume)))
    (should (equal captured-switches '("--offline" "--resume")))
    (should-not captured-arg)
    (should (equal captured-prefix "pi"))))

(ert-deftest ai-code-test-pi-resume-selected-session-uses-session-flag ()
  "Pi resume should pass a selected UUID through --session."
  (let ((session-id "12345678-1234-1234-1234-123456789abc")
        captured-switches
        captured-arg
        picker-shown)
    (cl-letf (((symbol-function 'ai-code-backends-infra--selected-session-id)
               (lambda () session-id))
              ((symbol-function 'ai-code-pi-start)
               (lambda (&optional arg)
                 (setq captured-switches ai-code-pi-program-switches
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--cli-show-resume-picker)
               (lambda (_prefix)
                 (setq picker-shown t))))
      (let ((ai-code-pi-program-switches '("--offline")))
        (ai-code-pi-resume)))
    (should (equal captured-switches
                   (list "--offline" "--session" session-id)))
    (should-not captured-arg)
    (should-not picker-shown)))

(ert-deftest ai-code-test-pi-start-wires-mcp-prepare-launch ()
  "Pi startup should delegate MCP launch preparation for the pi backend."
  (let (captured-options captured-call)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options _arg) (setq captured-options options)))
              ((symbol-function 'ai-code-mcp-agent-prepare-launch)
               (lambda (backend working-dir argv)
                 (setq captured-call (list backend working-dir argv))
                 '(:argv ("pi" "--extension" "x.js")))))
      (ai-code-pi-start)
      (let ((prepare (plist-get captured-options :prepare-launch)))
        (should (functionp prepare))
        (should (equal (funcall prepare "/tmp/" '("pi"))
                       '(:argv ("pi" "--extension" "x.js"))))
        (should (equal captured-call '(pi "/tmp/" ("pi"))))))))

(provide 'test_ai-code-pi)

;;; test_ai-code-pi.el ends here
