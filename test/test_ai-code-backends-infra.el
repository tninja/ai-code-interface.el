;;; test_ai-code-backends-infra.el --- Tests for ai-code-backends-infra.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-backends-infra.el behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)
(require 'ai-code-notifications)

(declare-function ghostel--window-adjust-process-window-size "ghostel" (process windows))

(defvar vterm-copy-mode-hook)
(defvar ghostel-enable-title-tracking)
(defvar ghostel--copy-mode-active)
(defvar ghostel--process)

(ert-deftest test-ai-code-backends-infra-output-meaningful-p-noise ()
  "Ensure terminal noise is not considered meaningful output."
  (should-not (ai-code-backends-infra--output-meaningful-p nil))
  (should-not (ai-code-backends-infra--output-meaningful-p "\x1b[31m\x1b[0m"))
  (should-not (ai-code-backends-infra--output-meaningful-p "\x1b]0;title\x07"))
  (should-not (ai-code-backends-infra--output-meaningful-p "\x1b]0;title\x1b\\"))
  (should-not (ai-code-backends-infra--output-meaningful-p " \t\n\r")))

(ert-deftest test-ai-code-backends-infra-output-meaningful-p-content ()
  "Ensure printable content is still detected after stripping noise."
  (should (ai-code-backends-infra--output-meaningful-p "\x1b[31mhello\x1b[0m")))

(ert-deftest test-ai-code-backends-infra-buffer-user-visible-p ()
  "Return non-nil only when buffer has a visible window."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _args) nil)))
        (should-not (ai-code-backends-infra--buffer-user-visible-p buf)))
      (cl-letf (((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _args) (list (selected-window)))))
         (should (ai-code-backends-infra--buffer-user-visible-p buf))))))

(ert-deftest test-ai-code-backends-infra-vterm-notification-tracker-relinks-after-redraw ()
  "Re-linkify vterm output when a later redraw strips custom properties."
  (let* ((root (make-temp-file "ai-code-vterm-redraw-links-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir))
         (buffer (generate-new-buffer "*codex[session-links]*"))
         (process 'fake-process)
         (output "src/FileABC.java:42\nhttps://example.com/path\n"))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (cl-letf (((symbol-function 'process-buffer)
                       (lambda (_process) buffer)))
              (ai-code-backends-infra--vterm-notification-tracker
               (lambda (_process _input)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert output)
                   ;; Simulate a later vterm redraw that rewrites the rendered text.
                   (run-at-time
                    0 nil
                    (lambda (buf text)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (let ((inhibit-read-only t))
                            (erase-buffer)
                            (insert text)))))
                    buffer output)))
               process
               output))
            (sleep-for 0.02)
            (goto-char (point-min))
            (search-forward-regexp "src/FileABC\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42"))
            (should-not (get-text-property (match-beginning 0) 'ai-code-session-link-type))
            (should-not (get-text-property (match-beginning 0) 'ai-code-session-link-data))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (goto-char (point-min))
            (search-forward-regexp "https://example\\.com/path")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/path"))
            (should-not (get-text-property (match-beginning 0) 'ai-code-session-link-type))
            (should-not (get-text-property (match-beginning 0) 'ai-code-session-link-data))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-backends-infra-response-seen-visible ()
  "Mark responses as seen without notifying when visible."
  (let ((notification-count 0))
    (cl-letf (((symbol-function 'ai-code-backends-infra--buffer-user-visible-p)
               (lambda (_buffer) t))
              ((symbol-function 'ai-code-notifications-response-ready)
               (lambda (&rest _args)
                 (setq notification-count (1+ notification-count)))))
      (with-temp-buffer
        (rename-buffer "*testbackend[test-dir]*" t)
        (setq ai-code-backends-infra--response-seen nil)
        (ai-code-backends-infra--check-response-complete (current-buffer))
        (should ai-code-backends-infra--response-seen)
        (should (= notification-count 0))))))

(ert-deftest test-ai-code-backends-infra-response-seen-notify-once ()
  "Notify once when responses complete while not visible."
  (let ((notification-count 0))
    (cl-letf (((symbol-function 'ai-code-backends-infra--buffer-user-visible-p)
               (lambda (_buffer) nil))
              ((symbol-function 'ai-code-notifications-response-ready)
               (lambda (&rest _args)
                 (setq notification-count (1+ notification-count)))))
      (with-temp-buffer
        (rename-buffer "*testbackend[test-dir]*" t)
        (setq ai-code-backends-infra--response-seen nil)
        (ai-code-backends-infra--check-response-complete (current-buffer))
        (should ai-code-backends-infra--response-seen)
        (should (= notification-count 1))
        (ai-code-backends-infra--check-response-complete (current-buffer))
        (should (= notification-count 1))))))

(ert-deftest test-ai-code-backends-infra-response-not-idle-reschedules ()
  "Reschedule idle checks when meaningful output is too recent."
  (let ((scheduled nil)
        (ai-code-backends-infra-idle-delay 10.0))
    (cl-letf (((symbol-function 'ai-code-backends-infra--buffer-user-visible-p)
               (lambda (_buffer) nil))
              ((symbol-function 'ai-code-backends-infra--schedule-idle-check)
               (lambda () (setq scheduled t)))
              ((symbol-function 'ai-code-notifications-response-ready)
               (lambda (&rest _args)
                 (error "Should not notify"))))
      (with-temp-buffer
        (rename-buffer "*testbackend[test-dir]*" t)
        (setq ai-code-backends-infra--response-seen nil)
        (setq ai-code-backends-infra--last-meaningful-output-time (float-time))
        (ai-code-backends-infra--check-response-complete (current-buffer))
        (should-not ai-code-backends-infra--response-seen)
        (should scheduled)))))

(ert-deftest test-ai-code-backends-infra-sync-reflow-filter-advice-vterm ()
  "Enable and disable reflow advice for vterm according to toggle."
  (let ((handler 'ai-code-backends-infra--test-resize-vterm))
    (fset handler (lambda (&rest args) args))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-resize-handler)
                   (lambda () handler)))
          (let ((ai-code-backends-infra-terminal-backend 'vterm)
                (ai-code-backends-infra-prevent-reflow-glitch t))
            (ai-code-backends-infra--sync-reflow-filter-advice)
            (should (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                     handler)))
          (let ((ai-code-backends-infra-terminal-backend 'vterm)
                (ai-code-backends-infra-prevent-reflow-glitch nil))
            (ai-code-backends-infra--sync-reflow-filter-advice)
            (should-not (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                         handler))))
      (when (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter handler)
        (advice-remove handler #'ai-code-backends-infra--terminal-reflow-filter))
      (fmakunbound handler))))

(ert-deftest test-ai-code-backends-infra-sync-reflow-filter-advice-eat ()
  "Install reflow advice for eat when glitch prevention is enabled."
  (let ((handler 'ai-code-backends-infra--test-resize-eat)
        (ai-code-backends-infra--reflow-advised-handlers nil))
    (fset handler (lambda (&rest args) args))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-resize-handler)
                   (lambda () handler)))
          (let ((ai-code-backends-infra-terminal-backend 'eat)
                (ai-code-backends-infra-prevent-reflow-glitch t)
                (ai-code-backends-infra-eat-preserve-position nil))
            (ai-code-backends-infra--sync-reflow-filter-advice)
            (should (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                     handler)))
          (when (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                 handler)
            (advice-remove handler #'ai-code-backends-infra--terminal-reflow-filter))
          (setq ai-code-backends-infra--reflow-advised-handlers nil)
          (let ((ai-code-backends-infra-terminal-backend 'eat)
                (ai-code-backends-infra-prevent-reflow-glitch t)
                (ai-code-backends-infra-eat-preserve-position t))
            (ai-code-backends-infra--sync-reflow-filter-advice)
            (should (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                     handler))))
      (when (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter handler)
        (advice-remove handler #'ai-code-backends-infra--terminal-reflow-filter))
      (fmakunbound handler))))

(ert-deftest test-ai-code-backends-infra-sync-reflow-filter-advice-clears-stale-handler ()
  "Switching backend should remove stale reflow advice from old handler."
  (let ((vterm-handler 'ai-code-backends-infra--test-resize-vterm-stale)
        (eat-handler 'ai-code-backends-infra--test-resize-eat-stale)
        (ai-code-backends-infra--reflow-advised-handlers nil))
    (fset vterm-handler (lambda (&rest args) args))
    (fset eat-handler (lambda (&rest args) args))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-resize-handler)
                   (lambda ()
                     (pcase ai-code-backends-infra-terminal-backend
                       ('vterm vterm-handler)
                       ('eat eat-handler)
                       (_ (error "Unexpected backend"))))))
          (let ((ai-code-backends-infra-terminal-backend 'vterm)
                (ai-code-backends-infra-prevent-reflow-glitch t))
            (ai-code-backends-infra--sync-reflow-filter-advice)
            (should (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                     vterm-handler)))
          (let ((ai-code-backends-infra-terminal-backend 'eat)
                (ai-code-backends-infra-prevent-reflow-glitch t)
                (ai-code-backends-infra-eat-preserve-position nil))
            (ai-code-backends-infra--sync-reflow-filter-advice))
          (should-not (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                       vterm-handler)))
      (dolist (handler (list vterm-handler eat-handler))
        (when (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter handler)
          (advice-remove handler #'ai-code-backends-infra--terminal-reflow-filter))
        (fmakunbound handler)))))

(ert-deftest test-ai-code-backends-infra-display-buffer-in-side-window-uses-body-width ()
  "Horizontal side windows should size to the configured body width."
  (with-temp-buffer
    (let ((ai-code-backends-infra-use-side-window t)
          (ai-code-backends-infra-window-side 'right)
          (ai-code-backends-infra-window-width 100)
          (ai-code-backends-infra-focus-on-open nil)
          captured-entry
          resize-call)
      (rename-buffer " *ai-code-side-width*" t)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (_buffer &optional _action)
                   (setq captured-entry (car display-buffer-alist))
                   'fake-window))
                ((symbol-function 'window-body-width)
                 (lambda (_window) 96))
                ((symbol-function 'window-resize)
                 (lambda (window delta horizontal)
                   (setq resize-call (list window delta horizontal)))))
        (ai-code-backends-infra--display-buffer-in-side-window (current-buffer))
        (should (functionp (cdr (assq 'window-width captured-entry))))
        (funcall (cdr (assq 'window-width captured-entry)) 'fake-window)
        (should (equal resize-call '(fake-window 4 t)))))))

(ert-deftest test-ai-code-backends-infra-sync-terminal-cursor-vterm-copy-mode ()
  "Show an Emacs cursor in vterm copy mode and restore terminal cursor on exit."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
    (setq-local cursor-type nil)
    (setq-local vterm-copy-mode t)
    (ai-code-backends-infra--sync-terminal-cursor)
    (should (eq cursor-type t))
    (should ai-code-backends-infra--navigation-cursor-active)
    (should (null ai-code-backends-infra--terminal-active-cursor-type))
    (setq-local vterm-copy-mode nil)
    (ai-code-backends-infra--sync-terminal-cursor)
    (should-not ai-code-backends-infra--navigation-cursor-active)
    (should (null cursor-type))))

(ert-deftest test-ai-code-backends-infra-sync-terminal-cursor-eat-emacs-mode ()
  "Show an Emacs cursor in Eat navigation mode and restore terminal cursor on exit."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'eat)
    (setq-local eat-terminal t)
    (setq-local eat--semi-char-mode t)
    (setq-local buffer-read-only nil)
    (setq-local cursor-type 'bar)
    (setq-local buffer-read-only t)
    (setq-local eat--semi-char-mode nil)
    (ai-code-backends-infra--sync-terminal-cursor)
    (should (eq cursor-type t))
    (should ai-code-backends-infra--navigation-cursor-active)
    (should (eq ai-code-backends-infra--terminal-active-cursor-type 'bar))
    (setq-local buffer-read-only nil)
    (setq-local eat--semi-char-mode t)
    (ai-code-backends-infra--sync-terminal-cursor)
    (should-not ai-code-backends-infra--navigation-cursor-active)
    (should (eq cursor-type 'bar))))

(ert-deftest test-ai-code-backends-infra-terminal-navigation-mode-p-ghostel-copy-mode ()
  "Ghostel copy mode should count as terminal navigation mode."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (setq-local ghostel--copy-mode-active t)
    (should (ai-code-backends-infra--terminal-navigation-mode-p))
    (setq-local ghostel--copy-mode-active nil)
    (should-not (ai-code-backends-infra--terminal-navigation-mode-p))))

(ert-deftest test-ai-code-backends-infra-configure-vterm-buffer-installs-cursor-sync-hook ()
  "Configuring a vterm buffer should install copy-mode cursor synchronization."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
    (let ((ai-code-backends-infra--vterm-advices-installed t))
      (ai-code-backends-infra--configure-vterm-buffer))
    (should (memq #'ai-code-backends-infra--sync-terminal-cursor
                  vterm-copy-mode-hook))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-adds-eat-cursor-sync-hook ()
  "Eat sessions should track navigation-mode cursor handoff locally."
  (let* ((buffer-name "*test-ai-code-eat-cursor-sync*")
         (buffer (get-buffer-create buffer-name))
         (ai-code-backends-infra-terminal-backend 'eat))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                   (lambda () nil))
                  ((symbol-function 'eat-mode)
                   (lambda () nil))
                  ((symbol-function 'eat-exec)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'get-buffer-process)
                   (lambda (_buffer) nil)))
          (ai-code-backends-infra--create-terminal-session
           buffer-name
           default-directory
           "echo hi"
           nil)
          (with-current-buffer buffer
            (should (memq #'ai-code-backends-infra--sync-terminal-cursor
                          post-command-hook))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-terminal-send-string-prefers-session-backend ()
  "Send should use session-local backend even after global backend changes."
  (let ((ai-code-backends-infra-terminal-backend 'eat)
        (calls nil)
        (buffer (generate-new-buffer " *ai-code-terminal-dispatch*")))
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (_str) (push 'vterm calls)))
                  ((symbol-function 'eat-term-send-string)
                   (lambda (&rest _args) (push 'eat calls))))
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
            (ai-code-backends-infra--terminal-send-string "hello"))
          (should (equal calls '(vterm))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-terminal-send-string-ghostel-sends-to-process ()
  "Ghostel sessions should send input through the terminal process."
  (let ((calls nil))
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (_process) t))
              ((symbol-function 'process-send-string)
               (lambda (process string)
                 (push (list process string) calls))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (setq-local ghostel--process 'ghostel-proc)
        (ai-code-backends-infra--terminal-send-string "hello"))
      (should (equal calls '((ghostel-proc "hello")))))))

(ert-deftest test-ai-code-backends-infra-terminal-resize-handler-supports-ghostel ()
  "Ghostel backend should expose its resize handler."
  (let ((ai-code-backends-infra-terminal-backend 'ghostel))
    (should (eq (ai-code-backends-infra--terminal-resize-handler)
                #'ghostel--window-adjust-process-window-size))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel ()
  "Ghostel backend should start sessions via `ghostel-exec'."
  (let* ((buffer-name "*test-ai-code-ghostel*")
         (buffer (get-buffer-create buffer-name))
         (process 'ghostel-proc)
         (ghostel-exec-call nil)
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                   (lambda () nil))
                  ((symbol-function 'ghostel-exec)
                   (lambda (target-buffer program &optional args)
                     (setq ghostel-exec-call (list target-buffer program args))
                     (with-current-buffer target-buffer
                       (setq-local ghostel--process process))
                     process))
                  ((symbol-function 'get-buffer-process)
                   (lambda (target-buffer)
                     (with-current-buffer target-buffer
                       ghostel--process))))
          (ai-code-backends-infra--create-terminal-session
           buffer-name
           default-directory
           "echo \"hello world\" --flag"
           '("FOO=1"))
          (with-current-buffer buffer
            (should (eq ai-code-backends-infra--session-terminal-backend 'ghostel))
            (should (equal ai-code-backends-infra--session-directory
                           (file-name-as-directory
                            (expand-file-name default-directory))))
            (should (eq ghostel--process process)))
          (should (equal ghostel-exec-call
                         (list buffer "echo" '("hello world" "--flag")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel-errors-without-exec ()
  "Ghostel startup should raise a clear error when `ghostel-exec' is unavailable."
  (let* ((buffer-name "*test-ai-code-ghostel-missing-exec*")
         (buffer (get-buffer-create buffer-name))
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                   (lambda () nil)))
          (should-error
           (ai-code-backends-infra--create-terminal-session
            buffer-name
            default-directory
            "echo hi"
            nil)
           :type 'user-error))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel-disables-title-tracking-before-start ()
  "Ghostel startup should disable title tracking before spawning the process."
  (let* ((buffer-name "*test-ai-code-ghostel-title-tracking*")
         (buffer (get-buffer-create buffer-name))
         (process 'ghostel-proc)
         (title-tracking-before-start :unset)
         (saved-default (default-value 'ghostel-enable-title-tracking))
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (progn
          (setq-default ghostel-enable-title-tracking t)
          (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                     (lambda () nil))
                    ((symbol-function 'ghostel-exec)
                     (lambda (target-buffer _program &optional _args)
                       (with-current-buffer target-buffer
                         (setq title-tracking-before-start ghostel-enable-title-tracking)
                         (setq-local ghostel--process process))
                       process))
                    ((symbol-function 'get-buffer-process)
                     (lambda (target-buffer)
                       (with-current-buffer target-buffer
                         ghostel--process))))
            (ai-code-backends-infra--create-terminal-session
             buffer-name
             default-directory
             "echo hi"
             nil)
            (should (eq title-tracking-before-start nil))))
      (setq-default ghostel-enable-title-tracking saved-default)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-configure-ghostel-buffer-installs-cursor-sync-hook ()
  "Ghostel session configuration should only add AI Code local behavior."
  (let ((hook-calls nil))
    (cl-letf (((symbol-function 'add-hook)
               (lambda (hook function &optional append local)
                 (push (list hook function append local) hook-calls))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (ai-code-backends-infra--configure-ghostel-buffer)))
    (should (member '(post-command-hook
                      ai-code-backends-infra--sync-terminal-cursor
                      nil t)
                    hook-calls))
    (should-not (member '(window-configuration-change-hook
                          ai-code-backends-infra--initialize-ghostel-when-displayed
                          nil t)
                        hook-calls))))

(ert-deftest test-ai-code-backends-infra-configure-ghostel-buffer-disables-title-tracking ()
  "Ghostel AI session buffers should keep their original buffer names."
  (let ((saved-default (default-value 'ghostel-enable-title-tracking)))
    (unwind-protect
        (progn
          (setq-default ghostel-enable-title-tracking t)
          (cl-letf (((symbol-function 'ghostel-mode)
                     (lambda () nil))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'add-hook)
                     (lambda (&rest _args) nil)))
            (with-temp-buffer
              (ai-code-backends-infra--configure-ghostel-buffer)
              (should-not ghostel-enable-title-tracking)
              (should (eq (default-value 'ghostel-enable-title-tracking) t)))))
      (setq-default ghostel-enable-title-tracking saved-default))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel-wraps-output-filter ()
  "Ghostel session creation should track meaningful output and linkify output."
  (let* ((buffer-name "*test-ai-code-ghostel-output*")
         (buffer (get-buffer-create buffer-name))
         (proc (make-process :name "ai-code-ghostel-output"
                             :buffer buffer
                             :command '("sleep" "10")
                             :noquery t))
         (orig-outputs nil)
         (meaningful-outputs nil)
         (linkify-outputs nil)
         (ai-code-backends-infra-terminal-backend 'ghostel)
         (note-advice (lambda (&rest _args)
                        (push 'noted meaningful-outputs)))
         (linkify-advice (lambda (orig-fun output)
                           (push output linkify-outputs)
                           (funcall orig-fun output))))
    (unwind-protect
        (progn
          (set-process-filter
           proc
           (lambda (_process output)
             (push output orig-outputs)))
          (advice-add 'ai-code-backends-infra--note-meaningful-output
                      :before note-advice)
          (advice-add 'ai-code-session-link--linkify-recent-output
                      :around linkify-advice)
          (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                     (lambda () nil))
                    ((symbol-function 'ghostel-exec)
                     (lambda (target-buffer _program &optional _args)
                       (with-current-buffer target-buffer
                         (setq-local ghostel--process proc))
                       proc)))
            (ai-code-backends-infra--create-terminal-session
             buffer-name
             default-directory
             "echo hi"
             nil))
          (funcall (process-filter proc) proc "src/foo.el:12\n")
          (should (equal orig-outputs '("src/foo.el:12\n")))
          (should (equal meaningful-outputs '(noted)))
          (should (equal linkify-outputs '("src/foo.el:12\n"))))
      (advice-remove 'ai-code-backends-infra--note-meaningful-output note-advice)
      (advice-remove 'ai-code-session-link--linkify-recent-output linkify-advice)
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-source-comment-uses-repo-stable-rationale ()
  "Source comments should avoid local paths and chat transcripts."
  (with-temp-buffer
    (insert-file-contents "ai-code-backends-infra.el")
    (goto-char (point-min))
    (should (search-forward "Prefer `ghostel-exec' for Ghostel backend startup" nil t))
    (should-not (search-forward "/home/tninja/" nil t))
    (goto-char (point-min))
    (should-not (search-forward "Background:" nil t))
    (goto-char (point-min))
    (should-not (search-forward "@tninja" nil t))))

(ert-deftest test-ai-code-backends-infra-normalize-file-path-stable-across-existence ()
  "Normalization should stay stable when file existence changes."
  (let* ((root (make-temp-file "ai-code-normalize-file-path-" t))
         (target-dir (expand-file-name "target" root))
         (target-file (expand-file-name "main.el" target-dir))
         (link-dir (expand-file-name "link" root))
         (link-file (expand-file-name "main.el" link-dir))
         before
         after)
    (unwind-protect
        (progn
          (make-directory target-dir t)
          (make-directory link-dir t)
          (condition-case err
              (make-symbolic-link target-file link-file t)
            (file-error
             (ert-skip (format "Symlink unavailable for this environment: %S" err))))
          (setq before (ai-code-backends-infra--normalize-file-path link-file))
          (with-temp-file target-file
            (insert "(message \"x\")\n"))
          (setq after (ai-code-backends-infra--normalize-file-path link-file))
          (should (equal before after)))
      (ignore-errors
        (delete-directory root t)))))

(ert-deftest test-ai-code-backends-infra-toggle-or-create-session-default-process-table ()
  "Fallback to global process table when PROCESS-TABLE is nil."
  (let* ((ai-code-backends-infra--processes (make-hash-table :test 'equal))
         (working-dir "/tmp/ai-code-default-table/")
         (buffer-name "*ai-code-default-table*")
         (buffer (get-buffer-create buffer-name))
         (captured-table nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--cleanup-dead-processes)
                   (lambda (table) (setq captured-table table)))
                  ((symbol-function 'ai-code-backends-infra--create-terminal-session)
                   (lambda (&rest _args) (cons buffer 'mock-process)))
                  ((symbol-function 'sleep-for)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'process-live-p)
                   (lambda (&rest _args) t))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                   (lambda (&rest _args) nil)))
          (ai-code-backends-infra--toggle-or-create-session
           working-dir
           buffer-name
           nil
           "echo hi")
          (should (eq captured-table ai-code-backends-infra--processes))
          (should (eq (gethash (cons working-dir "default")
                               ai-code-backends-infra--processes)
                      'mock-process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-reuse-session-window-refreshes-hidden-buffer ()
  "Reusing a hidden session should refresh its state and display it."
  (let* ((working-dir "/tmp/ai-code-reuse-hidden/")
         (prefix "codex")
         (buffer (get-buffer-create "*codex[reuse-hidden]*"))
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--set-session-directory)
                   (lambda (target-buffer directory)
                     (push (list :set-directory target-buffer directory) calls)))
                  ((symbol-function 'ai-code-backends-infra--configure-session-buffer)
                   (lambda (target-buffer escape-fn multiline-input-sequence)
                     (push (list :configure target-buffer escape-fn multiline-input-sequence) calls)))
                  ((symbol-function 'ai-code-backends-infra--remember-session-buffer)
                   (lambda (target-prefix directory target-buffer)
                     (push (list :remember target-prefix directory target-buffer) calls)))
                  ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                   (lambda (target-buffer)
                     (push (list :display target-buffer) calls)
                     nil)))
          (ai-code-backends-infra--reuse-session-window
           buffer
           working-dir
           prefix
           "\\\r\n")
          (should (equal (nreverse calls)
                         (list (list :set-directory buffer working-dir)
                               (list :configure buffer nil "\\\r\n")
                               (list :remember prefix working-dir buffer)
                               (list :display buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-resolve-session-target-prefers-explicit-instance ()
  "Explicit INSTANCE-NAME should bypass prompting and produce stable target info."
  (let* ((working-dir "/tmp/ai-code-session-target/")
         (prefix "codex")
         (context nil)
         (prompt-called nil))
    (cl-letf (((symbol-function 'ai-code-backends-infra--prompt-for-instance-name)
               (lambda (&rest _args)
                 (setq prompt-called t)
                 "prompted-instance")))
      (setq context
            (ai-code-backends-infra--resolve-session-target
             working-dir
             nil
             prefix
             "review"
             nil))
      (should (equal (plist-get context :instance-name) "review"))
      (should (equal (plist-get context :buffer-name)
                     "*codex[ai-code-session-target:review]*"))
      (should (equal (plist-get context :session-key)
                     (cons working-dir "review")))
      (should-not prompt-called))))

(ert-deftest test-ai-code-backends-infra-resolve-session-context-includes-runtime-state ()
  "Resolved session context should include target data plus buffer and process."
  (let* ((working-dir "/tmp/ai-code-session-context/")
         (buffer-name "*ai-code-session-context*")
         (process-table (make-hash-table :test 'equal))
         (buffer (get-buffer-create buffer-name))
         (process 'mock-process)
         (context nil))
    (unwind-protect
        (progn
          (puthash (cons working-dir "default") process process-table)
          (setq context
                (ai-code-backends-infra--resolve-session-context
                 working-dir
                 buffer-name
                 process-table
                 nil
                 nil
                 nil))
          (should (equal (plist-get context :instance-name) "default"))
          (should (equal (plist-get context :buffer-name) buffer-name))
          (should (equal (plist-get context :session-key)
                         (cons working-dir "default")))
          (should (eq (plist-get context :buffer) buffer))
          (should (eq (plist-get context :existing-process) process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-cleanup-session-kills-buffer-on-normal-exit ()
  "Buffer is killed when the process exits normally (event starts with \"finished\")."
  (let* ((table (make-hash-table :test 'equal))
         (dir "/tmp/test-cleanup/")
         (buf-name "*test-cleanup-normal*")
         (buf (get-buffer-create buf-name)))
    (puthash (cons dir "default") t table)
    (ai-code-backends-infra--cleanup-session dir buf-name table nil nil "finished\n")
    (should-not (get-buffer buf-name))
    (ignore buf)))

(ert-deftest test-ai-code-backends-infra-cleanup-session-preserves-buffer-on-abnormal-exit ()
  "Buffer is preserved when the process exits abnormally."
  (let* ((table (make-hash-table :test 'equal))
         (dir "/tmp/test-cleanup/")
         (buf-name "*test-cleanup-abnormal*")
         (buf (get-buffer-create buf-name)))
    (puthash (cons dir "default") t table)
    (ai-code-backends-infra--cleanup-session dir buf-name table nil nil "exited abnormally with code 1\n")
    (should (get-buffer buf-name))
    ;; Clean up
    (when (get-buffer buf-name) (kill-buffer buf-name))
    (ignore buf)))

(ert-deftest test-ai-code-backends-infra-cleanup-session-kills-buffer-on-nil-event ()
  "Buffer is killed when event is nil (legacy / direct call behavior)."
  (let* ((table (make-hash-table :test 'equal))
         (dir "/tmp/test-cleanup/")
         (buf-name "*test-cleanup-nil-event*")
         (buf (get-buffer-create buf-name)))
    (puthash (cons dir "default") t table)
    (ai-code-backends-infra--cleanup-session dir buf-name table nil nil nil)
    (should-not (get-buffer buf-name))
    (ignore buf)))

(ert-deftest test-ai-code-backends-infra-find-session-buffers-uses-full-directory ()
  "Find sessions by exact directory even when project base names collide."
  (let* ((prefix "codex")
         (base (format "ai-code-collision-%d" (random 1000000)))
         (dir-a (format "/tmp/a/%s/" base))
         (dir-b (format "/tmp/b/%s/" base))
         (buf-name (format "*%s[%s]*" prefix base))
         (buf (get-buffer-create buf-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local ai-code-backends-infra--session-directory dir-a))
          (should (memq buf (ai-code-backends-infra--find-session-buffers prefix dir-a)))
          (should-not (memq buf (ai-code-backends-infra--find-session-buffers prefix dir-b))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-ai-code-backends-infra-find-session-buffers-legacy-default-directory-fallback ()
  "Use buffer `default-directory' when explicit session metadata is absent."
  (let* ((prefix "codex")
         (base (format "ai-code-legacy-%d" (random 1000000)))
         (dir-a (format "/tmp/a/%s/" base))
         (dir-b (format "/tmp/b/%s/" base))
         (buf-name (format "*%s[%s]*" prefix base))
         (buf (get-buffer-create buf-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local ai-code-backends-infra--session-directory nil)
            (setq default-directory dir-a))
          (should (memq buf (ai-code-backends-infra--find-session-buffers prefix dir-a)))
          (should-not (memq buf (ai-code-backends-infra--find-session-buffers prefix dir-b))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-ai-code-backends-infra-send-line-attaches-session-per-file ()
  "Sending from different files should keep independent attached sessions."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-session/")
         (source-a (generate-new-buffer " *ai-code-source-a*"))
         (source-b (generate-new-buffer " *ai-code-source-b*"))
         (session-a (get-buffer-create "*codex[file-session:a]*"))
         (session-b (get-buffer-create "*codex[file-session:b]*"))
         (selection-order (list session-a session-b))
         (send-targets nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source-a
            (setq buffer-file-name "/tmp/ai-code-file-session/file-a.el")
            (setq default-directory working-dir))
          (with-current-buffer source-b
            (setq buffer-file-name "/tmp/ai-code-file-session/file-b.el")
            (setq default-directory working-dir))
          (with-current-buffer session-a
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer session-b
            (setq-local ai-code-backends-infra--session-directory working-dir))

          (cl-letf (((symbol-function 'ai-code-backends-infra--select-session-buffer)
                     (lambda (&rest _args)
                       (if selection-order
                           (pop selection-order)
                         (ert-fail "Selection should not run again for an attached file."))))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (&rest _args)
                       (push (buffer-name (current-buffer)) send-targets)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda () nil))
                    ((symbol-function 'sit-for)
                     (lambda (&rest _args) nil)))
            (with-current-buffer source-a
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-a1" prefix working-dir))
            (with-current-buffer source-b
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-b1" prefix working-dir))
            (with-current-buffer source-a
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-a2" prefix working-dir)))
          (should (equal (nreverse send-targets)
                         (list "*codex[file-session:a]*"
                               "*codex[file-session:b]*"
                               "*codex[file-session:a]*"))))
      (dolist (buf (list source-a source-b session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-send-line-unassociated-file-prompts-before-binding ()
  "Unassociated file should prompt before it is bound to a repo session."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-new-association/")
         (source (generate-new-buffer " *ai-code-source-new-association*"))
         (session-a (get-buffer-create "*codex[file-new-association:a]*"))
         (session-b (get-buffer-create "*codex[file-new-association:b]*"))
         (selection-count 0)
         (force-prompts nil)
         (send-targets nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-new-association/new-file.el")
            (setq default-directory working-dir))
          (with-current-buffer session-a
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer session-b
            (setq-local ai-code-backends-infra--session-directory working-dir))
          ;; Simulate the current repo-level active/remembered session.
          (ai-code-backends-infra--remember-session-buffer prefix working-dir session-b)

          (cl-letf (((symbol-function 'ai-code-backends-infra--select-session-buffer)
                     (lambda (_prefix _dir &optional force-prompt)
                       (setq selection-count (1+ selection-count))
                       (push force-prompt force-prompts)
                       session-b))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (&rest _args)
                       (push (buffer-name (current-buffer)) send-targets)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda () nil))
                    ((symbol-function 'sit-for)
                     (lambda (&rest _args) nil)))
            (with-current-buffer source
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-1" prefix working-dir)
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-2" prefix working-dir)))

          (should (= selection-count 1))
          (should (equal (nreverse force-prompts) (list t)))
          (should (equal (nreverse send-targets)
                         (list "*codex[file-new-association:b]*"
                               "*codex[file-new-association:b]*")))
          (should (eq (gethash
                       (ai-code-backends-infra--file-session-map-key prefix source)
                       ai-code-backends-infra--file-session-map)
                      session-b)))
      (dolist (buf (list source session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-send-line-uses-remembered-renamed-session ()
  "Sending should reuse a remembered session even if Ghostel renamed its buffer."
  (let* ((prefix "opencode")
         (working-dir "/tmp/ai-code-ghostel-renamed/")
         (source (generate-new-buffer " *ai-code-source-renamed-session*"))
         (session (get-buffer-create "*opencode[ghostel-renamed]*"))
         (send-targets nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-ghostel-renamed/main.el")
            (setq default-directory working-dir))
          (with-current-buffer session
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (ai-code-backends-infra--remember-session-buffer prefix working-dir session)
          (with-current-buffer session
            (rename-buffer "*ghostel: opencode*" t))

          (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (&rest _args)
                       (push (buffer-name (current-buffer)) send-targets)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda () nil))
                    ((symbol-function 'sit-for)
                     (lambda (&rest _args) nil)))
            (with-current-buffer source
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-1" prefix working-dir)))

          (should (equal (nreverse send-targets)
                         (list "*ghostel: opencode*"))))
      (dolist (buf (list source session))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-select-session-buffer-skips-renamed-remembered-on-force-prompt ()
  "Force prompt should not offer renamed remembered buffers as completion candidates."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-force-prompt-renamed/")
         (remembered (get-buffer-create "*ghostel: codex*"))
         (session-a (get-buffer-create "*codex[force-prompt-renamed:a]*"))
         (session-b (get-buffer-create "*codex[force-prompt-renamed:b]*"))
         (captured-collection nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (dolist (buf (list remembered session-a session-b))
            (with-current-buffer buf
              (setq-local ai-code-backends-infra--session-directory working-dir)))
          (ai-code-backends-infra--remember-session-buffer prefix working-dir remembered)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection _predicate _require-match
                              &optional _initial-input _hist _def &rest _)
                       (setq captured-collection collection)
                       "b")))
            (should (eq (ai-code-backends-infra--select-session-buffer
                         prefix working-dir t)
                        session-b)))
          (should (equal captured-collection '("a" "b"))))
      (dolist (buf (list remembered session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-readme-dependencies-mention-ghostel-as-backend-option ()
  "README should list Ghostel in the native terminal backend dependency note."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "README.org" default-directory))
    (should (re-search-forward
             "One of vterm (default), eat, or .*ghostel.* needs to be installed"
             nil
             t))))

(ert-deftest test-ai-code-backends-infra-switch-force-prompt-rebinds-file-session ()
  "Force switching should rebind the current file to the newly selected session."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-rebind/")
         (source (generate-new-buffer " *ai-code-source-rebind*"))
         (session-a (get-buffer-create "*codex[file-rebind:a]*"))
         (session-b (get-buffer-create "*codex[file-rebind:b]*"))
         (selection-order (list session-a session-b))
         (force-prompts nil)
         (display-targets nil)
         (send-targets nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-rebind/main.el")
            (setq default-directory working-dir))
          (with-current-buffer session-a
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer session-b
            (setq-local ai-code-backends-infra--session-directory working-dir))

          (cl-letf (((symbol-function 'ai-code-backends-infra--select-session-buffer)
                     (lambda (_prefix _dir &optional force-prompt)
                       (push force-prompt force-prompts)
                       (if selection-order
                           (pop selection-order)
                         (ert-fail "Selection should not run after file session is rebound."))))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (buffer)
                       (push (buffer-name buffer) display-targets)
                       nil))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (&rest _args)
                       (push (buffer-name (current-buffer)) send-targets)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda () nil))
                    ((symbol-function 'sit-for)
                     (lambda (&rest _args) nil)))
            (with-current-buffer source
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-1" prefix working-dir)
              (ai-code-backends-infra--switch-to-session-buffer
               nil "missing" prefix working-dir t)
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-2" prefix working-dir)))

          (should (equal (nreverse force-prompts) (list t t)))
          (should (equal (nreverse send-targets)
                         (list "*codex[file-rebind:a]*"
                               "*codex[file-rebind:b]*")))
          (should (equal (nreverse display-targets)
                         (list "*codex[file-rebind:b]*"))))
      (dolist (buf (list source session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-switch-new-file-prompts-when-multiple-sessions-active ()
  "A newly opened file should prompt when multiple repo sessions are active."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-multi-active/")
         (source (generate-new-buffer " *ai-code-source-multi-active*"))
         (session-a (get-buffer-create "*codex[file-multi-active:a]*"))
         (session-b (get-buffer-create "*codex[file-multi-active:b]*"))
         (captured-collection nil)
         (captured-default nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-multi-active/main.el")
            (setq default-directory working-dir))
          (with-current-buffer session-a
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer session-b
            (setq-local ai-code-backends-infra--session-directory working-dir))

          (cl-letf (((symbol-function 'ai-code-backends-infra--find-session-buffers)
                     (lambda (_prefix _dir)
                       (list session-a session-b)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection _predicate _require-match
                              &optional _initial-input _hist def &rest _)
                       (setq captured-collection collection)
                       (setq captured-default def)
                       "b"))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (_buffer) nil)))
            (with-current-buffer source
              (ai-code-backends-infra--switch-to-session-buffer
               nil
               "missing"
               prefix
               working-dir
               nil)))

          (should (equal captured-collection '("a" "b")))
          (should (equal captured-default "a"))
          (should (eq (gethash
                       (ai-code-backends-infra--file-session-map-key prefix source)
                       ai-code-backends-infra--file-session-map)
                      session-b)))
      (dolist (buf (list source session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-switch-new-file-prompts-when-remembered-session-exists ()
  "A newly opened file should still prompt when multiple repo sessions are active."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-multi-remembered/")
         (source (generate-new-buffer " *ai-code-source-multi-remembered*"))
         (session-a (get-buffer-create "*codex[file-multi-remembered:a]*"))
         (session-b (get-buffer-create "*codex[file-multi-remembered:b]*"))
         (captured-collection nil)
         (captured-default nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-multi-remembered/main.el")
            (setq default-directory working-dir))
          (with-current-buffer session-a
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer session-b
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (ai-code-backends-infra--remember-session-buffer prefix working-dir session-b)

          (cl-letf (((symbol-function 'ai-code-backends-infra--find-session-buffers)
                     (lambda (_prefix _dir)
                       (list session-a session-b)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection _predicate _require-match
                              &optional _initial-input _hist def &rest _)
                       (setq captured-collection collection)
                       (setq captured-default def)
                       "a"))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (_buffer) nil)))
            (with-current-buffer source
              (ai-code-backends-infra--switch-to-session-buffer
               nil
               "missing"
               prefix
               working-dir
               nil)))

          (should (equal captured-collection '("b" "a")))
          (should (equal captured-default "b"))
          (should (eq (gethash
                       (ai-code-backends-infra--file-session-map-key prefix source)
                       ai-code-backends-infra--file-session-map)
                      session-a)))
      (dolist (buf (list source session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-switch-force-prompt-prioritizes-attached-session ()
  "Force prompt should place attached file session at the top and as default."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-preselect/")
         (source (generate-new-buffer " *ai-code-source-preselect*"))
         (session-a (get-buffer-create "*codex[file-preselect:a]*"))
         (session-b (get-buffer-create "*codex[file-preselect:b]*"))
         (captured-collection nil)
         (captured-default nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-preselect/main.el")
            (setq default-directory working-dir))
          (with-current-buffer session-a
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer session-b
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (ai-code-backends-infra--remember-file-session-buffer
           prefix
           source
           session-b)

          (cl-letf (((symbol-function 'ai-code-backends-infra--find-session-buffers)
                     (lambda (_prefix _dir)
                       (list session-a session-b)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection _predicate _require-match
                              &optional _initial-input _hist def &rest _)
                       (setq captured-collection collection)
                       (setq captured-default def)
                       "a"))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (_buffer) nil)))
            (with-current-buffer source
              (ai-code-backends-infra--switch-to-session-buffer
               nil
               "missing"
               prefix
               working-dir
               t)))

          (should (equal captured-collection '("b" "a")))
          (should (equal captured-default "b"))
          (should (eq (gethash
                       (ai-code-backends-infra--file-session-map-key prefix source)
                       ai-code-backends-infra--file-session-map)
                      session-a)))
      (dolist (buf (list source session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-resolve-session-buffer-no-message-with-explicit-buffer-name ()
  "Do not show attached-missing warning when explicit BUFFER-NAME is provided."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-explicit/")
         (source (generate-new-buffer " *ai-code-source-explicit*"))
         (attached (get-buffer-create "*codex[file-explicit:attached]*"))
         (target (get-buffer-create "*codex[file-explicit:target]*"))
         (messages nil)
         result)
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-explicit/main.el")
            (setq default-directory working-dir))
          (with-current-buffer attached
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer target
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (ai-code-backends-infra--remember-file-session-buffer prefix source attached)
          (kill-buffer attached)

          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)
                       nil))
                    ((symbol-function 'ai-code-backends-infra--select-session-buffer)
                     (lambda (&rest _args)
                       (ert-fail "Should not prompt when explicit buffer-name exists."))))
            (with-current-buffer source
              (setq result
                    (ai-code-backends-infra--resolve-session-buffer
                     (buffer-name target)
                     "missing"
                     prefix
                     working-dir
                     nil
                     source))))
          (should (eq result target))
          (should (null messages)))
      (dolist (buf (list source attached target))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-send-line-reselects-when-attached-session-missing ()
  "When an attached session buffer is killed, notify and force re-selection."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-file-missing/")
         (source (generate-new-buffer " *ai-code-source-missing*"))
         (session-a (get-buffer-create "*codex[file-missing:a]*"))
         (session-b (get-buffer-create "*codex[file-missing:b]*"))
         (selection-order (list session-a session-b))
         (force-prompts nil)
         (messages nil)
         (send-targets nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-missing/main.el")
            (setq default-directory working-dir))
          (with-current-buffer session-a
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (with-current-buffer session-b
            (setq-local ai-code-backends-infra--session-directory working-dir))

          (cl-letf (((symbol-function 'ai-code-backends-infra--select-session-buffer)
                     (lambda (_prefix _dir &optional force-prompt)
                       (push force-prompt force-prompts)
                       (if selection-order
                           (pop selection-order)
                         (ert-fail "Selection should only happen twice in this scenario."))))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (&rest _args)
                       (push (buffer-name (current-buffer)) send-targets)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda () nil))
                    ((symbol-function 'sit-for)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)
                       nil)))
            (with-current-buffer source
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-1" prefix working-dir))
            (when (buffer-live-p session-a)
              (kill-buffer session-a))
            (with-current-buffer source
              (ai-code-backends-infra--send-line-to-session
               nil "missing" "line-2" prefix working-dir)))

          (should (equal (nreverse force-prompts) (list t t)))
          (should (equal (nreverse send-targets)
                         (list "*codex[file-missing:a]*"
                               "*codex[file-missing:b]*")))
          (should (= (length messages) 1))
          (should (string-match-p
                   "Attached AI session .* no longer exists"
                   (car messages))))
      (dolist (buf (list source session-a session-b))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-switch-reuses-live-attached-session-despite-working-dir-mismatch ()
  "Reuse a live attached session even when WORKING-DIR no longer matches it."
  (let* ((prefix "codex")
         (session-dir "/tmp/ai-code-file-attached-root/")
         (working-dir "/tmp/ai-code-file-attached-root/subdir/")
         (source (generate-new-buffer " *ai-code-source-attached-live*"))
         (attached (get-buffer-create "*codex[file-attached-root:attached]*"))
         (displayed nil))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-file-attached-root/main.el")
            (setq default-directory working-dir))
          (with-current-buffer attached
            (setq-local ai-code-backends-infra--session-directory session-dir))
          (ai-code-backends-infra--remember-file-session-buffer prefix source attached)

          (cl-letf (((symbol-function 'ai-code-backends-infra--find-session-buffers)
                     (lambda (_prefix _dir) nil))
                    ((symbol-function 'ai-code-backends-infra--select-session-buffer)
                     (lambda (&rest _args)
                       (ert-fail "Should reuse the live attached session without prompting.")))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (buffer)
                       (setq displayed buffer)
                       nil)))
            (with-current-buffer source
              (ai-code-backends-infra--switch-to-session-buffer
               nil
               "missing"
               prefix
               working-dir
               nil)))

          (should (eq displayed attached))
          (should (eq (gethash
                       (ai-code-backends-infra--file-session-map-key prefix source)
                       ai-code-backends-infra--file-session-map)
                      attached)))
      (dolist (buf (list source attached))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-ai-code-backends-infra-toggle-or-create-session-passes-env-vars ()
  "ENV-VARS are forwarded to `ai-code-backends-infra--create-terminal-session'."
  (let* ((ai-code-backends-infra--processes (make-hash-table :test 'equal))
         (working-dir "/tmp/ai-code-env-vars/")
         (buffer-name "*ai-code-env-vars*")
         (buffer (get-buffer-create buffer-name))
         (captured-env-vars :not-set))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--cleanup-dead-processes)
                   (lambda (_table) nil))
                  ((symbol-function 'ai-code-backends-infra--create-terminal-session)
                   (lambda (_buf _dir _cmd env-vars)
                     (setq captured-env-vars env-vars)
                     (cons buffer 'mock-process)))
                  ((symbol-function 'sleep-for)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'process-live-p)
                   (lambda (&rest _args) t))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                   (lambda (&rest _args) nil)))
          (ai-code-backends-infra--toggle-or-create-session
           working-dir
           buffer-name
           nil
           "echo hi"
           nil nil nil nil nil
           '("TERM_PROGRAM=vscode" "MY_VAR=1"))
          (should (equal captured-env-vars '("TERM_PROGRAM=vscode" "MY_VAR=1"))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-toggle-or-create-session-binds-multiline-input ()
  "MULTILINE-INPUT-SEQUENCE binds Shift+Enter and Ctrl+Enter in session buffers."
  (let* ((ai-code-backends-infra--processes (make-hash-table :test 'equal))
         (working-dir "/tmp/ai-code-multiline/")
         (buffer-name "*ai-code-multiline*")
         (buffer (get-buffer-create buffer-name))
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--cleanup-dead-processes)
                   (lambda (_table) nil))
                  ((symbol-function 'ai-code-backends-infra--create-terminal-session)
                   (lambda (_buf _dir _cmd _env-vars)
                     (cons buffer 'mock-process)))
                  ((symbol-function 'sleep-for)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'process-live-p)
                   (lambda (&rest _args) t))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                   (lambda (string)
                     (push string calls))))
          (ai-code-backends-infra--toggle-or-create-session
           working-dir
           buffer-name
           nil
           "echo hi"
           nil nil nil nil nil
           nil
           "\\\r\n")
          (with-current-buffer buffer
            (call-interactively (key-binding (kbd "S-<return>")))
            (call-interactively (key-binding (kbd "C-<return>"))))
          (should (equal (nreverse calls) '("\\\r\n" "\\\r\n"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-toggle-or-create-session-calls-post-start-hook ()
  "POST-START-FN should receive the created buffer, process, and instance."
  (let* ((working-dir "/tmp/ai-code-post-start/")
         (buffer-name "*ai-code-post-start*")
         (buffer (get-buffer-create buffer-name))
         (process 'mock-process)
         (called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--cleanup-dead-processes)
                   (lambda (_table) nil))
                  ((symbol-function 'ai-code-backends-infra--create-terminal-session)
                   (lambda (_buf _dir _cmd _env-vars)
                     (cons buffer process)))
                  ((symbol-function 'sleep-for)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'process-live-p)
                   (lambda (&rest _args) t))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--configure-session-buffer)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--remember-session-buffer)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                   (lambda (&rest _args) nil)))
          (ai-code-backends-infra--toggle-or-create-session
           working-dir
           buffer-name
           (make-hash-table :test 'equal)
           "echo ok"
           nil nil nil nil nil nil nil
           (lambda (created-buffer created-process created-instance)
             (setq called (list created-buffer created-process created-instance))))
          (should (equal (list buffer process "default")
                         called)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-vterm-smart-renderer-renders-in-copy-mode ()
  "Incoming vterm data should still render while `vterm-copy-mode' is active."
  (with-temp-buffer
    (rename-buffer "*testclaude[test-dir]*" t)
    (setq-local ai-code-backends-infra--vterm-render-queue nil)
    (setq-local ai-code-backends-infra--vterm-render-timer nil)
    (setq-local vterm-copy-mode t)
    (insert "before")
    (goto-char (point-min))
    (let* ((original-point (point))
           (rendered nil)
           (orig-fun (lambda (_process input)
                       ;; Mimic vterm rendering moving point to the live terminal end.
                       (goto-char (point-max))
                       (insert input)
                       (push input rendered)))
           (mock-process 'mock-proc))
      (cl-letf (((symbol-function 'process-buffer)
                 (lambda (_proc) (current-buffer)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args) 'mock-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (&rest _args) nil)))
        (ai-code-backends-infra--vterm-smart-renderer
         orig-fun mock-process "hello")
        (should (equal rendered '("hello")))
        (should (equal (buffer-string) "beforehello"))
        (should (= (point) original-point))
        (should-not ai-code-backends-infra--vterm-render-queue)))))

(ert-deftest test-ai-code-backends-infra-vterm-render-preserving-copy-mode-view-restores-window-state ()
  "Copy-mode rendering should restore the visible window viewport."
  (let ((buffer (generate-new-buffer " *ai-code-vterm-copy-mode-window*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (setq-local vterm-copy-mode t)
            (dotimes (line 80)
              (insert (format "line %02d\n" line)))
            (goto-char (point-min))
            (forward-line 25)
            (set-window-start (selected-window) (point))
            (forward-line 4)
            (set-window-point (selected-window) (point))
            (let ((original-start (window-start))
                  (original-window-point (window-point))
                  (orig-fun (lambda ()
                              (goto-char (point-max))
                              (insert "tail\n")
                              (goto-char (point-max)))))
              (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
               orig-fun)
              (should (= (window-start) original-start))
              (should (= (window-point) original-window-point)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-vterm-render-preserving-copy-mode-view-tracks-head-deletions ()
  "Copy-mode rendering should preserve viewport content after head deletions."
  (let ((buffer (generate-new-buffer " *ai-code-vterm-copy-mode-trim*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (setq-local vterm-copy-mode t)
            (dotimes (line 80)
              (insert (format "line %02d\n" line)))
            (cl-labels ((line-at (position)
                          (save-excursion
                            (goto-char position)
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))))
              (goto-char (point-min))
              (forward-line 25)
              (set-window-start (selected-window) (point))
              (forward-line 4)
              (set-window-point (selected-window) (point))
              (let ((original-start-line (line-at (window-start)))
                    (original-window-point-line (line-at (window-point))))
                (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
                 (lambda ()
                   (goto-char (point-min))
                   (forward-line 10)
                   (delete-region (point-min) (point))))
                (should (equal (line-at (window-start))
                               original-start-line))
                (should (equal (line-at (window-point))
                               original-window-point-line))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-vterm-render-queued-output-skips-dead-process ()
  "Queued output should be dropped when the buffer has no live process."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--vterm-render-queue "queued-output")
    (setq-local ai-code-backends-infra--vterm-render-timer 'mock-timer)
    (let ((orig-called nil))
      (cl-letf (((symbol-function 'get-buffer-process)
                 (lambda (_buf) nil)))
        (ai-code-backends-infra--vterm-render-queued-output
         (lambda (process _input)
           (setq orig-called t)
           (unless (process-live-p process)
             (error "Missing live process")))
         (current-buffer))
        (should-not orig-called)
        (should (null ai-code-backends-infra--vterm-render-queue))
        (should (null ai-code-backends-infra--vterm-render-timer))))))

(ert-deftest test-ai-code-backends-infra-vterm-smart-renderer-timer-renders-in-copy-mode ()
  "Render timer should flush queued redraws while `vterm-copy-mode' is active."
  (with-temp-buffer
    (rename-buffer "*testclaude[test-dir2]*" t)
    (insert "before")
    (goto-char (point-min))
    (setq-local ai-code-backends-infra--vterm-render-queue nil)
    (setq-local ai-code-backends-infra--vterm-render-timer nil)
    (setq-local vterm-copy-mode t)
    (let* ((original-point (point))
           (rendered nil)
           (orig-fun (lambda (_process input)
                       ;; Mimic vterm rendering moving point to the live terminal end.
                       (goto-char (point-max))
                       (insert input)
                       (push input rendered)))
           (mock-process 'mock-proc)
           (captured-timer-fn nil))
      (cl-letf (((symbol-function 'process-buffer)
                 (lambda (_proc) (current-buffer)))
                ((symbol-function 'get-buffer-process)
                 (lambda (_buf) mock-process))
                ((symbol-function 'process-live-p)
                 (lambda (process) (eq process mock-process)))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (setq captured-timer-fn (cons fn args))
                   'mock-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (&rest _args) nil)))
        (ai-code-backends-infra--vterm-smart-renderer
         orig-fun mock-process "\r\rqueued-data")
        (when captured-timer-fn
          (apply (car captured-timer-fn) (cdr captured-timer-fn)))
        (should (equal rendered '("\r\rqueued-data")))
        (should (equal (buffer-string) "before\r\rqueued-data"))
        (should (= (point) original-point))
        (should (null ai-code-backends-infra--vterm-render-timer))
        (should-not ai-code-backends-infra--vterm-render-queue)))))

(ert-deftest test-ai-code-backends-infra-vterm-flush-on-copy-mode-exit ()
  "Pending render queue is flushed when exiting vterm-copy-mode."
  (with-temp-buffer
    (rename-buffer "*testclaude[test-dir3]*" t)
    (setq-local ai-code-backends-infra--vterm-render-queue "queued-output")
    (setq-local vterm-copy-mode nil)   ; copy mode is now OFF (just exited)
    (let* ((flushed-data nil)
           (mock-process 'mock-proc))
      (cl-letf (((symbol-function 'get-buffer-process)
                 (lambda (_buf) mock-process))
                ((symbol-function 'vterm--filter)
                 (lambda (_proc data) (setq flushed-data data))))
        (ai-code-backends-infra--vterm-flush-on-copy-mode-exit)
        ;; Queue should have been flushed.
        (should (equal flushed-data "queued-output"))
        (should (null ai-code-backends-infra--vterm-render-queue))))))

(ert-deftest test-ai-code-backends-infra-vterm-flush-on-copy-mode-exit-noop-when-active ()
  "Flush function does nothing when vterm-copy-mode is still active."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--vterm-render-queue "queued-output")
    (setq-local vterm-copy-mode t)   ; copy mode is still ON
    (let* ((flush-called nil))
      (cl-letf (((symbol-function 'vterm--filter)
                 (lambda (&rest _args) (setq flush-called t))))
        (ai-code-backends-infra--vterm-flush-on-copy-mode-exit)
        ;; Still in copy mode: flush should be a no-op.
        (should-not flush-called)
        (should (equal ai-code-backends-infra--vterm-render-queue "queued-output"))))))

(ert-deftest test-ai-code-backends-infra-finalize-started-session-configures-and-displays ()
  "Successful startup finalization should wire buffer state and UI updates."
  (let* ((working-dir "/tmp/ai-code-finalize-start/")
         (prefix "codex")
         (buffer-name "*codex[finalize-start]*")
         (buffer (get-buffer-create buffer-name))
         (process 'mock-process)
         (sentinel nil)
         (post-start-args nil)
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'set-process-sentinel)
                   (lambda (_process fn)
                     (setq sentinel fn)
                     (push :sentinel calls)))
                  ((symbol-function 'ai-code-backends-infra--configure-session-buffer)
                   (lambda (target-buffer escape-fn multiline-input-sequence)
                     (push (list :configure target-buffer escape-fn multiline-input-sequence) calls)))
                  ((symbol-function 'ai-code-backends-infra--remember-session-buffer)
                   (lambda (target-prefix directory target-buffer)
                     (push (list :remember target-prefix directory target-buffer) calls)))
                  ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                   (lambda (target-buffer)
                     (push (list :display target-buffer) calls)
                     nil)))
          (ai-code-backends-infra--finalize-started-session
           buffer
           process
           working-dir
           buffer-name
           (make-hash-table :test 'equal)
           "default"
           prefix
           'mock-escape
           'mock-cleanup
           "\\\r\n"
           (lambda (created-buffer created-process created-instance)
             (setq post-start-args
                   (list created-buffer created-process created-instance))
             (push :post-start calls)))
          (should sentinel)
          (should (equal post-start-args (list buffer process "default")))
          (should (equal (nreverse calls)
                         (list :sentinel
                               (list :configure buffer 'mock-escape "\\\r\n")
                               :post-start
                               (list :remember prefix working-dir buffer)
                               (list :display buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-handle-session-start-failure-shows-live-buffer ()
  "Startup failure should preserve and show a live buffer with an error message."
  (let* ((session-key '("/tmp/ai-code-start-failure/" . "default"))
         (process-table (make-hash-table :test 'equal))
         (buffer (get-buffer-create "*ai-code-start-failure*"))
         (calls nil))
    (unwind-protect
        (progn
          (puthash session-key 'mock-process process-table)
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (target-buffer &rest _args)
                       (push (list :pop target-buffer) calls)
                       nil))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) calls)
                       nil)))
            (ai-code-backends-infra--handle-session-start-failure
             buffer
             session-key
             process-table)
            (should-not (gethash session-key process-table))
            (should (equal (nreverse calls)
                           (list (list :pop buffer)
                                 "CLI failed to start - see buffer for error details")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-configure-session-buffer-does-not-bind-manual-navigation ()
  "Configuring a session buffer should not add a manual `C-c g' navigation feature."
  (let ((buffer (generate-new-buffer "*ai-code-session-config*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-link--linkify-session-region)
                   (lambda (&rest _args) nil)))
          (ai-code-backends-infra--configure-session-buffer buffer)
          (with-current-buffer buffer
            (should-not (key-binding (kbd "C-c g")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-vterm-smart-renderer-queues-on-carriage-return ()
  "Incoming vterm data is queued when it contains multiple carriage returns."
  (with-temp-buffer
    (rename-buffer "*testgemini[test-dir]*" t)
    (setq-local ai-code-backends-infra--vterm-render-queue nil)
    (setq-local ai-code-backends-infra--vterm-render-timer nil)
    (setq-local vterm-copy-mode nil)
    (let* ((rendered nil)
           (orig-fun (lambda (_process input) (push input rendered)))
           (mock-process 'mock-proc))
      (cl-letf (((symbol-function 'process-buffer)
                 (lambda (_proc) (current-buffer)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args) 'mock-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (&rest _args) nil)))
        ;; Send input with multiple \r (common in TUI progress bars/updates).
        (ai-code-backends-infra--vterm-smart-renderer
         orig-fun mock-process "Loading... 10%\rLoading... 20%\r")
        ;; It should NOT be rendered immediately.
        (should (null rendered))
        ;; It should be in the queue.
        (should (equal ai-code-backends-infra--vterm-render-queue "Loading... 10%\rLoading... 20%\r"))))))

(ert-deftest test-ai-code-backends-infra-vterm-smart-renderer-allows-crlf-pass-through ()
  "Simple CRLF output should render immediately instead of being queued."
  (with-temp-buffer
    (rename-buffer "*testgemini[test-crlf]*" t)
    (setq-local ai-code-backends-infra--vterm-render-queue nil)
    (setq-local ai-code-backends-infra--vterm-render-timer nil)
    (setq-local vterm-copy-mode nil)
    (let* ((rendered nil)
           (timer-scheduled nil)
           (orig-fun (lambda (_process input) (push input rendered)))
           (mock-process 'mock-proc))
      (cl-letf (((symbol-function 'process-buffer)
                 (lambda (_proc) (current-buffer)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (setq timer-scheduled t)
                   'mock-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (&rest _args) nil)))
        (ai-code-backends-infra--vterm-smart-renderer
         orig-fun mock-process "hello\r\n")
        (should (equal rendered '("hello\r\n")))
        (should-not timer-scheduled)
        (should-not ai-code-backends-infra--vterm-render-queue)
        (should-not ai-code-backends-infra--vterm-render-timer)))))

(provide 'test_ai-code-backends-infra)

;;; test_ai-code-backends-infra.el ends here
