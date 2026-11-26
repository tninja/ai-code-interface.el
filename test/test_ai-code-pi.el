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

(ert-deftest ai-code-test-pi-startup-switches-stay-model-and-trust-neutral ()
  "Default startup switches must not auto-inject Gemini or trust flags."
  (let ((process-environment (cons "GEMINI_API_KEY=test-key"
                                   process-environment))
        (ai-code-pi-tui-mode 'regular)
        (ai-code-pi-project-trust nil))
    (should (equal (ai-code-pi--startup-switches nil)
                   '("--tui-mode" "regular")))))

(ert-deftest ai-code-test-pi-startup-switches-project-trust-and-precedence ()
  "Project trust option should append flags unless switches already set trust."
  (let ((ai-code-pi-tui-mode 'regular))
    (let ((ai-code-pi-project-trust 'approve))
      (should (equal (ai-code-pi--startup-switches '("--offline"))
                     '("--offline" "--tui-mode" "regular" "--approve")))
      (dolist (explicit '("-a" "--approve" "-na" "--no-approve"))
        (should (equal (ai-code-pi--startup-switches (list explicit))
                       (list explicit "--tui-mode" "regular")))))
    (let ((ai-code-pi-project-trust 'no-approve))
      (should (equal (ai-code-pi--startup-switches nil)
                     '("--tui-mode" "regular" "--no-approve"))))
    (let ((ai-code-pi-project-trust 'invalid))
      (should-error (ai-code-pi--startup-switches nil)))))

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

(ert-deftest ai-code-test-pi-default-follow-up-sequence-matches-pi-platform-keys ()
  "Follow-up should use Ctrl+Q where Pi uses Windows keybindings."
  (let ((process-environment
         (cl-remove-if (lambda (entry)
                         (string-match-p "\\`WSL_\\(?:DISTRO_NAME\\|INTEROP\\)="
                                         entry))
                       process-environment)))
    (should (equal (ai-code-pi--default-follow-up-sequence 'darwin)
                   "\e[13;3u"))
    (should (equal (ai-code-pi--default-follow-up-sequence 'gnu/linux)
                   "\e[13;3u"))
    (should (equal (ai-code-pi--default-follow-up-sequence 'windows-nt)
                   "\x11"))
    (dolist (var '("WSL_DISTRO_NAME" "WSL_INTEROP"))
      (let ((process-environment (cons (concat var "=x") process-environment)))
        (should (equal (ai-code-pi--default-follow-up-sequence 'gnu/linux)
                       "\x11"))
        (should (equal (ai-code-pi--default-follow-up-sequence 'darwin)
                       "\e[13;3u"))))))

(ert-deftest ai-code-test-pi-send-command-respects-busy-policy-and-follow-up ()
  "Send commands should use Return for steer and Alt+Enter for followup."
  ;; Loading `ai-code-pi' must not intern bare `follow-up' into `obarray',
  ;; which would make `ai-code-session-link' linkify English prose "follow-up".
  (should-not (intern-soft "follow-up"))
  (let ((session-buffer (generate-new-buffer " *pi[test-send]*"))
        sent-events
        touched-buffers)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
                     (lambda (&optional _prompt) "/tmp/proj/"))
                    ((symbol-function 'ai-code-backends-infra--resolve-session-buffer)
                     (lambda (&rest _args) session-buffer))
                    ((symbol-function 'ai-code-backends-infra--remember-session-buffer)
                     #'ignore)
                    ((symbol-function 'ai-code-mcp-agent-refresh-source-context)
                     #'ignore)
                    ((symbol-function 'ai-code-mcp-agent-touch-session)
                     (lambda (buf) (push buf touched-buffers)))
                    ((symbol-function 'ai-code-session-note-prompt)
                     #'ignore)
                    ((symbol-function 'sit-for)
                     #'ignore)
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (str &optional _paste)
                       (push (cons 'string str) sent-events)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda ()
                       (push '(return) sent-events))))
            (let ((ai-code-pi-send-while-busy 'steer)
                  (ai-code-pi-follow-up-sequence "\e[13;3u"))
              (ai-code-pi-send-command "fix bug")
              (should (equal (nreverse sent-events)
                             '((string . "fix bug") (return))))
              (setq sent-events nil
                    touched-buffers nil)
              (ai-code-pi-send-follow-up "queued item")
              (should (equal (nreverse sent-events)
                             '((string . "queued item")
                               (string . "\e[13;3u"))))
              (should (equal touched-buffers (list session-buffer))))
            (setq sent-events nil
                  touched-buffers nil)
            (let ((ai-code-pi-send-while-busy 'followup)
                  (ai-code-pi-follow-up-sequence "\e[13;3u"))
              (ai-code-pi-send-command "next step")
              (should (equal (nreverse sent-events)
                             '((string . "next step")
                               (string . "\e[13;3u"))))
              (should (equal touched-buffers (list session-buffer))))
            (let ((ai-code-pi-send-while-busy 'bogus))
              (should-error (ai-code-pi-send-command "bad")
                            :type 'user-error))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest ai-code-test-pi-post-start-chains-mcp-and-binds-follow-up-key ()
  "Pi startup should chain MCP post-start, bind M-RET, and wire viewport submit."
  (let ((session-buffer (generate-new-buffer " *pi[test-post-start]*"))
        captured-post-start
        mcp-post-start-args
        sent-events)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ai-code-backends-infra--resolve-start-command)
                     (lambda (program switches _arg _label)
                       (list :argv (cons program switches))))
                    ((symbol-function 'ai-code-backends-infra--session-working-directory)
                     (lambda (&optional _prompt) "/tmp/proj/"))
                    ((symbol-function 'ai-code-mcp-agent-prepare-launch)
                     (lambda (_backend _dir argv)
                       (list :argv argv
                             :post-start-fn
                             (lambda (buf proc inst)
                               (setq mcp-post-start-args
                                     (list buf proc inst))))))
                    ((symbol-function 'ai-code-backends-infra--create-new-session)
                     (lambda (_buffer-name _working-dir _command _env-vars
                                           _session-key _process-table
                                           _resolved-instance _prefix
                                           _escape-fn _cleanup-fn
                                           _multiline-input-sequence
                                           post-start-fn
                                           &rest _rest)
                       (setq captured-post-start post-start-fn)
                       nil))
                    ((symbol-function 'ai-code-mcp-agent-touch-session)
                     #'ignore)
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (str &optional _paste)
                       (push (cons 'string str) sent-events)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda ()
                       (push '(return) sent-events))))
            (let ((ai-code-pi--processes (make-hash-table :test 'equal)))
              (ai-code-pi-start))
            (should (functionp captured-post-start))
            (ai-code-backends-infra--configure-session-buffer
             session-buffer #'ai-code-pi-send-escape "\e[13;2u")
            (funcall captured-post-start session-buffer 'fake-proc "main")
            (should (equal mcp-post-start-args
                           (list session-buffer 'fake-proc "main")))
            (with-current-buffer session-buffer
              (should (eq (lookup-key (ai-code-backends-infra--session-keymap)
                                      (kbd "M-<return>"))
                          #'ai-code-pi-terminal-send-follow-up))
              (let ((ai-code-pi-send-while-busy 'steer))
                (funcall ai-code-editor-viewport--submit-function)
                (should (equal sent-events '((return)))))
              (setq sent-events nil)
              (let ((ai-code-pi-send-while-busy 'followup)
                    (ai-code-pi-follow-up-sequence "\e[13;3u"))
                (funcall ai-code-editor-viewport--submit-function)
                (should (equal sent-events '((string . "\e[13;3u"))))))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest ai-code-test-pi-compose-sends-ctrl-g-and-refreshes-context ()
  "Compose command should send Ctrl+G, refresh source context, and fail cleanly."
  (let ((session-buffer (generate-new-buffer "*pi[proj]*"))
        refreshed
        touched
        sent-in-buffer)
    (unwind-protect
        (progn
          (with-current-buffer session-buffer
            (setq-local ai-code-backends-infra--session-prefix "pi"))
          (with-temp-buffer
            (let ((source-buffer (current-buffer))
                  (ai-code-pi-compose-sequence "\x07"))
              (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
                         (lambda (&optional _prompt) "/tmp/proj/"))
                        ((symbol-function 'ai-code-backends-infra--resolve-session-buffer)
                         (lambda (_buf-name missing-msg &rest _rest)
                           (or session-buffer (user-error "%s" missing-msg))))
                        ((symbol-function 'ai-code-backends-infra--remember-session-buffer)
                         #'ignore)
                        ((symbol-function 'ai-code-mcp-agent-refresh-source-context)
                         (lambda (agent-buf src-buf)
                           (push (list agent-buf src-buf) refreshed)))
                        ((symbol-function 'ai-code-mcp-agent-touch-session)
                         (lambda (buf) (push buf touched)))
                        ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                         (lambda (str &optional _paste)
                           (push (list (current-buffer) str) sent-in-buffer))))
                (ai-code-pi-compose)
                (should (equal refreshed
                               (list (list session-buffer source-buffer))))
                (should (equal touched (list session-buffer)))
                (should (equal sent-in-buffer
                               (list (list session-buffer "\x07"))))
                ;; Invoking directly inside the Pi session buffer should not
                ;; overwrite source context with the terminal buffer itself.
                (setq refreshed nil
                      touched nil
                      sent-in-buffer nil)
                (with-current-buffer session-buffer
                  (ai-code-pi-compose))
                (should-not refreshed)
                (should (equal touched (list session-buffer)))
                (should (equal sent-in-buffer
                               (list (list session-buffer "\x07"))))
                ;; Missing session should signal user-error.
                (setq session-buffer nil)
                (should-error (ai-code-pi-compose) :type 'user-error)))))
      (when (and session-buffer (buffer-live-p session-buffer))
        (kill-buffer session-buffer)))))

(ert-deftest ai-code-test-pi-slash-command-normalizes-and-uses-standard-return ()
  "Slash commands should prefix `/', reject blanks, and always use Return."
  (let ((session-buffer (generate-new-buffer " *pi[test-slash]*"))
        sent-events)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
                     (lambda (&optional _prompt) "/tmp/proj/"))
                    ((symbol-function 'ai-code-backends-infra--resolve-session-buffer)
                     (lambda (&rest _args) session-buffer))
                    ((symbol-function 'ai-code-backends-infra--remember-session-buffer)
                     #'ignore)
                    ((symbol-function 'ai-code-mcp-agent-refresh-source-context)
                     #'ignore)
                    ((symbol-function 'ai-code-session-note-prompt)
                     #'ignore)
                    ((symbol-function 'sit-for)
                     #'ignore)
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (str &optional _paste)
                       (push (cons 'string str) sent-events)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda ()
                       (push '(return) sent-events))))
            (let ((ai-code-pi-send-while-busy 'followup))
              (ai-code-pi-slash-command "model")
              (should (equal (nreverse sent-events)
                             '((string . "/model") (return))))
              (setq sent-events nil)
              (ai-code-pi-slash-command "  /compact focus  ")
              (should (equal (nreverse sent-events)
                             '((string . "/compact focus") (return))))
              (setq sent-events nil)
              (should-error (ai-code-pi-slash-command "   ")
                            :type 'user-error)
              (should-not sent-events))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(provide 'test_ai-code-pi)

;;; test_ai-code-pi.el ends here
