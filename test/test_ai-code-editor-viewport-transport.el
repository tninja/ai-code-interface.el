;;; test_ai-code-editor-viewport-transport.el --- Viewport transport tests  -*- lexical-binding: t; -*-

;; Author: realazy
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for authenticated PTY transport and editor helper integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-editor-viewport)

(cl-defmacro ai-code-editor-viewport-transport-test--with-buffer
    ((buffer name) &rest body)
  "Bind BUFFER to a temporary buffer named from NAME while running BODY."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(with-temp-buffer
     (rename-buffer (generate-new-buffer-name ,name))
     (let ((,buffer (current-buffer)))
       ,@body)))

(ert-deftest test-ai-code-editor-viewport-transport--environment-overrides-editor-commands ()
  "CLI environment should route EDITOR and VISUAL through the viewport helper."
  (let ((ai-code-editor-viewport-enabled t)
        (ai-code-editor-viewport--protocol-token "test-token"))
    (cl-letf (((symbol-function 'ai-code-editor-viewport--ensure-helper)
               (lambda () "/tmp/ai-code-editor-helper"))
              ((symbol-function 'ai-code-editor-viewport--supported-p)
               (lambda () t)))
      (should
       (equal
        (ai-code-editor-viewport-environment
         '("TERM_PROGRAM=emacs" "EDITOR=vim" "VISUAL=nano"
           "GIT_EDITOR=vi" "GIT_SEQUENCE_EDITOR=vim"))
        '("EDITOR=/tmp/ai-code-editor-helper --ai-code-staging --ai-code-submit"
          "VISUAL=/tmp/ai-code-editor-helper --ai-code-staging --ai-code-submit"
          "GIT_EDITOR=/tmp/ai-code-editor-helper"
          "GIT_SEQUENCE_EDITOR=/tmp/ai-code-editor-helper"
          "AI_CODE_EDITOR_VIEWPORT_FRAME_PREFIX=\e]6973;ai-code-editor;test-token;"
          "TERM_PROGRAM=emacs"))))))

(ert-deftest test-ai-code-editor-viewport-transport--environment-can-disable-auto-submit ()
  "General editor requests should save only when auto-submit is disabled."
  (let ((ai-code-editor-viewport-enabled t)
        (ai-code-editor-viewport-auto-submit nil)
        (ai-code-editor-viewport--protocol-token "test-token"))
    (cl-letf (((symbol-function 'ai-code-editor-viewport--ensure-helper)
               (lambda () "/tmp/ai-code-editor-helper"))
              ((symbol-function 'ai-code-editor-viewport--supported-p)
               (lambda () t)))
      (should
       (equal
        (seq-take (ai-code-editor-viewport-environment nil) 4)
        '("EDITOR=/tmp/ai-code-editor-helper --ai-code-staging"
          "VISUAL=/tmp/ai-code-editor-helper --ai-code-staging"
          "GIT_EDITOR=/tmp/ai-code-editor-helper"
          "GIT_SEQUENCE_EDITOR=/tmp/ai-code-editor-helper"))))))

(ert-deftest test-ai-code-editor-viewport-transport--environment-preserves-unsupported-host ()
  "Unsupported hosts should retain the CLI's existing editor environment."
  (let ((environment '("EDITOR=vim" "TERM_PROGRAM=emacs")))
    (cl-letf (((symbol-function 'ai-code-editor-viewport--supported-p)
               (lambda () nil))
              ((symbol-function 'ai-code-editor-viewport--ensure-helper)
               (lambda () (ert-fail "Helper should not be generated"))))
      (should (eq (ai-code-editor-viewport-environment environment)
                  environment)))))

(ert-deftest test-ai-code-editor-viewport-transport--helper-content-uses-frame-prefix ()
  "The helper should emit the adapter-provided terminal frame prefix."
  (let ((content (ai-code-editor-viewport--helper-content)))
    (should
     (string-match-p
      (regexp-quote
       "[ \"${1-}\" = \"--ai-code-staging\" ] && request_kind=staging && shift")
      content))
    (should
     (string-match-p
      (regexp-quote
       "[ \"${1-}\" = \"--ai-code-submit\" ] && submit=1 && shift")
      content))
    (should
     (string-match-p
      (regexp-quote "printf '%s\\0' \"$submit\"")
      content))
    (should
     (string-match-p
      (regexp-quote
       (format "printf '%%s\\0' \"%s\""
               ai-code-editor-viewport--request-version))
      content))
    (should
     (string-match-p
      (regexp-quote "printf '%s\\0' \"$request_kind\"")
      content))
    (should
     (string-match-p
      (regexp-quote
       "read -r result submit_result submit_token < \"$status_file\"")
      content))
    (should
     (string-match-p
      (regexp-quote "submit-ready:$submit_token")
      content))
    (should (string-match-p (regexp-quote "parent_pid=$PPID") content))
    (should
     (string-match-p
      (regexp-quote "timeout=${AI_CODE_EDITOR_VIEWPORT_TIMEOUT:-3600}")
      content))
    (should
     (string-match-p
      (regexp-quote "kill -0 \"$parent_pid\" 2>/dev/null || exit 1")
      content))
    (should
     (string-match-p
      (regexp-quote "[ \"$attempts\" -lt \"$timeout\" ] || exit 1")
      content))
    (should (string-match-p (regexp-quote "sleep 1") content))
    (should
     (string-match-p
      (regexp-quote "[ \"$notify_attempts\" -lt 10 ] || exit 1")
      content))
    (should-not
     (string-match-p
      "sleep [[:digit:]][[:digit:]]*\\.[[:digit:]][[:digit:]]*"
      content))
    (should
     (string-match-p
      (regexp-quote
       "frame_prefix=${AI_CODE_EDITOR_VIEWPORT_FRAME_PREFIX-}")
      content))
    (should
     (string-match-p
      (regexp-quote
       "printf '%s%s\\007' \"$frame_prefix\" \"$payload\"")
      content))
    (should-not (string-match-p "ghostel" content))))

(ert-deftest test-ai-code-editor-viewport-transport--handle-request-authenticates-and-schedules ()
  "Adapter requests should require the session token before opening a viewport."
  (with-temp-buffer
    (let ((source-buffer (current-buffer))
          (origin-frame (selected-frame))
          (ai-code-editor-viewport--protocol-token "test-token")
          (ai-code-editor-viewport--pending-submit-token "old-submit-token")
          scheduled)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (push (cons function arguments) scheduled))))
        (should-not
        (ai-code-editor-viewport-handle-request
          source-buffer "wrong-token" "PAYLOAD"))
        (should-not scheduled)
        (should
         (equal ai-code-editor-viewport--pending-submit-token
                "old-submit-token"))
        (should
         (ai-code-editor-viewport-handle-request
          source-buffer "test-token" "PAYLOAD"))
        (should-not ai-code-editor-viewport--pending-submit-token)
        (should
         (equal scheduled
                `((ai-code-editor-viewport--open-request
                   ,source-buffer "PAYLOAD" ,origin-frame))))))))

(ert-deftest test-ai-code-editor-viewport-transport--handle-request-rejects-oversized-payload ()
  "Adapter requests over the protocol limit should not be scheduled."
  (with-temp-buffer
    (let ((source-buffer (current-buffer))
          (ai-code-editor-viewport--protocol-token "test-token")
          (ai-code-editor-viewport-max-request-size 3)
          scheduled)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest arguments)
                   (push arguments scheduled))))
        (should-not
         (ai-code-editor-viewport-handle-request
          source-buffer "test-token" "LONG"))
        (should-not scheduled)))))

(ert-deftest test-ai-code-editor-viewport-transport--submit-ready-token-is-one-time ()
  "Only the current one-time completion token should schedule submission."
  (with-temp-buffer
    (let ((source-buffer (current-buffer))
          (ai-code-editor-viewport--pending-submit-token "current-token")
          scheduled)
      (cl-letf (((symbol-function 'ai-code-editor-viewport--schedule-submit)
                 (lambda (buffer)
                   (push buffer scheduled))))
        (should-not
         (ai-code-editor-viewport--dispatch-payload
          source-buffer "submit-ready:stale-token"))
        (should
         (equal ai-code-editor-viewport--pending-submit-token
                "current-token"))
        (should-not scheduled)
        (should
         (ai-code-editor-viewport--dispatch-payload
          source-buffer "submit-ready:current-token"))
        (should-not ai-code-editor-viewport--pending-submit-token)
        (should (equal scheduled (list source-buffer)))
        (should-not
         (ai-code-editor-viewport--dispatch-payload
          source-buffer "submit-ready:current-token"))
        (should (equal scheduled (list source-buffer)))))))


(ert-deftest test-ai-code-editor-viewport-transport--ensure-helper-creates-tty-request-script ()
  "The injected EDITOR should request a viewport through its controlling TTY."
  (let* ((directory (make-temp-file "ai-code-editor-helper-" t))
         (temporary-file-directory directory)
         (ai-code-editor-viewport-enabled t)
         (ai-code-editor-viewport--helper-file nil)
         (ai-code-editor-viewport--helper-status-directory nil)
         environment
         editor)
    (unwind-protect
        (progn
          (setq environment (ai-code-editor-viewport-environment nil))
          (setq editor
                (car
                 (split-string-shell-command
                  (string-remove-prefix
                   "EDITOR="
                   (car environment)))))
          (should (file-executable-p editor))
          (should
           (seq-some
            (lambda (entry)
              (string-prefix-p
               "AI_CODE_EDITOR_VIEWPORT_FRAME_PREFIX=\e]6973;ai-code-editor;"
               entry))
            environment))
          (with-temp-buffer
            (insert-file-contents editor)
            (let ((contents (buffer-string)))
              (should (string-match-p "ai-code-editor-status" contents))
              (should
               (string-match-p "AI_CODE_EDITOR_VIEWPORT_FRAME_PREFIX"
                               contents))
              (should (string-match-p "> /dev/tty" contents))
              (should-not (string-match-p "emacsclient" contents))
              (should-not (string-match-p "socket-name" contents)))))
      (when (and editor (file-exists-p editor))
        (delete-file editor))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-transport--ensure-helper-round-trips-through-pty ()
  "The helper should request a viewport and receive its status through a PTY."
  (ai-code-editor-viewport-transport-test--with-buffer
      (source-buffer " *ai-code-editor-pty*")
    (let* ((directory (make-temp-file "ai-code-editor-pty-" t))
           (temporary-file-directory directory)
           (ai-code-editor-viewport--helper-file nil)
           (ai-code-editor-viewport--helper-status-directory nil)
           (environment (ai-code-editor-viewport-environment nil))
           (editor-command
            (split-string-shell-command
             (string-remove-prefix "EDITOR=" (car environment))))
           (process-environment (append environment process-environment))
           process
           request
           visible-output)
      (unwind-protect
          (cl-letf (((symbol-function 'ai-code-editor-viewport--open-request)
                     (lambda (buffer payload &optional origin-frame)
                       (setq request
                             (list buffer
                                   (ai-code-editor-viewport--decode-request
                                    payload)
                                   origin-frame))
                       (ai-code-editor-viewport--write-status
                        (plist-get (cadr request) :status-file) 0)
                       t)))
            (setq process
                  (make-process
                   :name "ai-code-editor-pty"
                   :buffer source-buffer
                   :command (append editor-command (list "draft prompt.md"))
                   :connection-type 'pty
                   :coding 'no-conversion
                   :noquery t
                   :filter
                   (lambda (proc output)
                     (setq visible-output
                           (concat visible-output
                                   (ai-code-editor-viewport-filter-output
                                    proc output))))))
            (let ((attempts 0))
              (while (and (process-live-p process) (< attempts 100))
                (accept-process-output process 0.05)
                (setq attempts (1+ attempts))))
            (should (eq (process-status process) 'exit))
            (should (= (process-exit-status process) 0))
            (should (eq (car request) source-buffer))
            (should (eq (nth 2 request) (selected-frame)))
            (should (eq (plist-get (cadr request) :submit-p) t))
            (should (eq (plist-get (cadr request) :staging-request-p) t))
            (should
             (equal
              (list (plist-get (cadr request) :directory)
                    (car (plist-get (cadr request) :arguments)))
              (list (directory-file-name
                     (expand-file-name default-directory))
                    "draft prompt.md")))
            (should-not
             (string-match-p "6973;ai-code-editor" visible-output)))
        (when (process-live-p process)
          (delete-process process))
        (ai-code-editor-viewport--cleanup-helper)
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport-transport--auto-submit-waits-for-helper-release ()
  "Auto-submit should run only after the external editor helper exits."
  (ai-code-editor-viewport-transport-test--with-buffer
      (source-buffer " *ai-code-editor-submit-pty*")
    (let* ((directory (make-temp-file "ai-code-editor-submit-pty-" t))
           (temporary-file-directory directory)
           (file (expand-file-name "draft.md" directory))
           (pid-file (expand-file-name "helper.pid" directory))
           (ai-code-editor-viewport--helper-file nil)
           (ai-code-editor-viewport--helper-status-directory nil)
           (original-helper-content
            (symbol-function 'ai-code-editor-viewport--helper-content))
           environment
           editor-command
           (deadline (+ (float-time) 5))
           process
           helper-pid
           submitted
           helper-live-at-submit)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (cl-letf (((symbol-function
                        'ai-code-editor-viewport--helper-content)
                       (lambda (&optional status-directory)
                         (string-replace
                          "fi\nexit 0\n"
                          "fi\nsleep 1\nexit 0\n"
                          (funcall original-helper-content
                                   status-directory)))))
              (setq environment (ai-code-editor-viewport-environment nil)))
            (setq editor-command
                  (split-string-shell-command
                   (string-remove-prefix "EDITOR=" (car environment))))
            (with-current-buffer source-buffer
              (setq-local default-directory directory)
              (setq-local
               ai-code-editor-viewport--submit-function
               (lambda ()
                 (setq submitted t
                       helper-live-at-submit
                       (process-attributes helper-pid)))))
            (let ((process-environment
                   (append environment process-environment)))
              (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                         (lambda (&rest _arguments) nil))
                        ((symbol-function 'recursive-edit)
                         (lambda ()
                           (ai-code-editor-viewport-finish)))
                        ((symbol-function 'exit-recursive-edit)
                         (lambda () nil)))
                (setq process
                      (make-process
                       :name "ai-code-editor-submit-pty"
                       :buffer source-buffer
                       :command
                       (append
                        (list
                         "/bin/sh" "-c"
                         (concat
                          "pid_file=$1; shift; \"$@\" & helper_pid=$!; "
                          "printf '%s\\n' \"$helper_pid\" > \"$pid_file\"; "
                          "wait \"$helper_pid\"; sleep 2")
                         "ai-code-editor-wrapper"
                         pid-file)
                        editor-command
                        (list file))
                       :connection-type 'pty
                       :coding 'no-conversion
                       :noquery t
                       :filter
                       (lambda (proc output)
                         (ai-code-editor-viewport-filter-output proc output))))
                (while (and (not (file-exists-p pid-file))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.02))
                (should (file-exists-p pid-file))
                (with-temp-buffer
                  (insert-file-contents pid-file)
                  (setq helper-pid (string-to-number (buffer-string))))
                (while (and (not submitted) (< (float-time) deadline))
                  (accept-process-output nil 0.02))))
            (should submitted)
            (should-not helper-live-at-submit))
        (when (process-live-p process)
          (delete-process process))
        (ai-code-editor-viewport--cleanup-helper)
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport-transport--filter-output-intercepts-split-requests ()
  "Terminal output should hide and reassemble split editor request frames."
  (with-temp-buffer
    (let ((ai-code-editor-viewport--protocol-token "test-token")
          (source-buffer (current-buffer))
          (origin-frame (selected-frame))
          scheduled)
      (cl-letf (((symbol-function 'process-buffer)
                 (lambda (_process) source-buffer))
                ((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (when (eq function
                             'ai-code-editor-viewport--open-request)
                     (push (cons function arguments) scheduled)))))
        (should
         (equal
          (ai-code-editor-viewport-filter-output
           'process "\e]6973;ai-code-editor;wrong-token;IGNORED\a")
          "\e]6973;ai-code-editor;wrong-token;IGNORED\a"))
        (should-not scheduled)
        (should
         (equal
          (ai-code-editor-viewport-filter-output
           'process "before\e]6973;ai-code-editor;test-")
          "before"))
        (should
         (equal
          (ai-code-editor-viewport-filter-output
           'process
           (concat "token;PAYLOAD\amiddle"
                   "\e]6973;ai-code-editor;test-token;TWO\aafter"))
          "middleafter"))
        (should
         (equal
          (nreverse scheduled)
          `((ai-code-editor-viewport--open-request
             ,source-buffer "PAYLOAD" ,origin-frame)
            (ai-code-editor-viewport--open-request
             ,source-buffer "TWO" ,origin-frame))))))))

(ert-deftest test-ai-code-editor-viewport-transport--filter-output-discards-oversized-request ()
  "A complete editor request over the configured limit should not be opened."
  (with-temp-buffer
    (let ((ai-code-editor-viewport--protocol-token "test-token")
          (ai-code-editor-viewport-max-request-size 4)
          (source-buffer (current-buffer))
          scheduled)
      (cl-letf (((symbol-function 'process-buffer)
                 (lambda (_process) source-buffer))
                ((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (when (eq function
                             'ai-code-editor-viewport--open-request)
                     (push (cons function arguments) scheduled)))))
        (should
         (equal
          (ai-code-editor-viewport-filter-output
           'process
           "before\e]6973;ai-code-editor;test-token;TOO-LONG\aafter")
          "beforeafter"))
        (should-not scheduled)
        (should (string-empty-p
                 ai-code-editor-viewport--protocol-pending))))))


(provide 'test_ai-code-editor-viewport-transport)
;;; test_ai-code-editor-viewport-transport.el ends here
