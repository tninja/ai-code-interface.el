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
(defvar eat-term-name)
(defvar ghostel-set-title-function)
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel--copy-mode-active)
(defvar ghostel--input-mode)
(defvar ghostel--process)

(defconst test-ai-code-backends-infra-valid-uuid
  "123e4567-e89b-12d3-a456-426614174000"
  "UUID fixture used by resume command resolution tests.")

(defun test-ai-code-backends-infra--capture-default-binding (symbol)
  "Return SYMBOL's default binding state.
The result is a cons of whether SYMBOL is bound and its default value."
  (if (boundp symbol)
      (cons t (default-value symbol))
    (cons nil nil)))

(defun test-ai-code-backends-infra--restore-default-binding (symbol state)
  "Restore SYMBOL's default binding STATE from `...--capture-default-binding'."
  (if (car state)
      (set-default symbol (cdr state))
    (makunbound symbol)))

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

(ert-deftest test-ai-code-backends-infra-main-file-keeps-backend-defcustoms-out ()
  "Backend-specific defcustoms should no longer be defined in the main infra file."
  (with-temp-buffer
    (insert-file-contents "ai-code-backends-infra.el")
    (goto-char (point-min))
    (should-not (re-search-forward
                 "^(defcustom ai-code-backends-infra-vterm-anti-flicker\\_>" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward
                 "^(defcustom ai-code-backends-infra-vterm-render-delay\\_>" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward
                 "^(defcustom ai-code-backends-infra-eat-preserve-position\\_>" nil t))))

(ert-deftest test-ai-code-backends-infra-backend-files-own-backend-defcustoms ()
  "Backend-specific defcustoms should live in their backend modules."
  (with-temp-buffer
    (insert-file-contents "ai-code-backends-infra-vterm.el")
    (goto-char (point-min))
    (should (re-search-forward
             "^(defcustom ai-code-backends-infra-vterm-anti-flicker\\_>" nil t))
    (goto-char (point-min))
    (should (re-search-forward
             "^(defcustom ai-code-backends-infra-vterm-render-delay\\_>" nil t)))
  (with-temp-buffer
    (insert-file-contents "ai-code-backends-infra-eat.el")
    (goto-char (point-min))
    (should (re-search-forward
             "^(defcustom ai-code-backends-infra-eat-preserve-position\\_>" nil t))))

(ert-deftest test-ai-code-backends-infra-ghostel-forward-declarations-do-not-set-defaults ()
  "Ghostel forward declarations should not override Ghostel defaults."
  (with-temp-buffer
    (insert-file-contents "ai-code-backends-infra-ghostel.el")
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file nil))
      (setq forms (nreverse forms))
      (dolist (symbol '(ghostel-kill-buffer-on-exit
                        ghostel-set-title-function))
        (let ((form (seq-find
                     (lambda (sexp)
                       (and (listp sexp)
                            (eq (car sexp) 'defvar)
                            (eq (cadr sexp) symbol)))
                     forms)))
          (should form)
          (should (= (length form) 2)))))))

(ert-deftest test-ai-code-backends-infra--resume-double-dash-prefills-uuid ()
  "A selected UUID should make `--resume' prompt with that id appended."
  (let ((uuid test-ai-code-backends-infra-valid-uuid)
        seen-prompt
        seen-initial
        seen-history
        result)
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert uuid)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (activate-mark)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial-input history &rest _args)
                   (setq seen-prompt prompt
                         seen-initial initial-input
                         seen-history history)
                   initial-input)))
        (setq result
              (ai-code-backends-infra--resolve-start-command
               "claude" '("--resume") nil "Claude"))))
    (should (equal seen-prompt "Claude args: "))
    (should (equal seen-initial (format "--resume %s" uuid)))
    (should (eq seen-history 'ai-code-cli-args-history))
    (should (equal (plist-get result :args) `("--resume" ,uuid)))
    (should (equal (plist-get result :command)
                   (format "claude --resume %s" uuid)))))

(ert-deftest test-ai-code-backends-infra--resume-subcommand-prefills-uuid ()
  "A selected UUID should make `resume' prompt with that id appended."
  (let ((uuid test-ai-code-backends-infra-valid-uuid)
        seen-initial
        result)
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert uuid)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (activate-mark)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial-input _history &rest _args)
                   (setq seen-initial initial-input)
                   initial-input)))
        (setq result
              (ai-code-backends-infra--resolve-start-command
               "codex" '("resume") nil "Codex"))))
    (should (equal seen-initial (format "resume %s" uuid)))
    (should (equal (plist-get result :args) `("resume" ,uuid)))
    (should (equal (plist-get result :command)
                   (format "codex resume %s" uuid)))))

(ert-deftest test-ai-code-backends-infra--resume-ignores-non-uuid ()
  "A non-UUID region should not trigger resume prompting."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "not-a-session-id")
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args)
                 (ert-fail "non-UUID selection should not prompt"))))
      (let ((result (ai-code-backends-infra--resolve-start-command
                     "claude" '("--resume") nil "Claude")))
        (should (equal (plist-get result :args) '("--resume")))
        (should (equal (plist-get result :command) "claude --resume"))))))

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
  "Do not install height-only reflow advice for vterm."
  (let ((handler 'ai-code-backends-infra--test-resize-vterm)
        (ai-code-backends-infra--reflow-advised-handlers nil))
    (fset handler (lambda (&rest args) args))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-resize-handler)
                   (lambda () handler)))
          (let ((ai-code-backends-infra-terminal-backend 'vterm)
                (ai-code-backends-infra-prevent-reflow-glitch t))
            (ai-code-backends-infra--sync-reflow-filter-advice)
            (should-not (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                         handler)))
          (advice-add handler :around #'ai-code-backends-infra--terminal-reflow-filter)
          (setq ai-code-backends-infra--reflow-advised-handlers nil)
          (let ((ai-code-backends-infra-terminal-backend 'vterm)
                (ai-code-backends-infra-prevent-reflow-glitch t))
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
          (advice-add vterm-handler :around #'ai-code-backends-infra--terminal-reflow-filter)
          (setq ai-code-backends-infra--reflow-advised-handlers (list vterm-handler))
          (let ((ai-code-backends-infra-terminal-backend 'eat)
                (ai-code-backends-infra-prevent-reflow-glitch t)
                (ai-code-backends-infra-eat-preserve-position nil))
            (ai-code-backends-infra--sync-reflow-filter-advice))
          (should-not (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                       vterm-handler))
          (should (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter
                                   eat-handler)))
      (dolist (handler (list vterm-handler eat-handler))
        (when (advice-member-p #'ai-code-backends-infra--terminal-reflow-filter handler)
          (advice-remove handler #'ai-code-backends-infra--terminal-reflow-filter))
        (fmakunbound handler)))))

(ert-deftest test-ai-code-backends-infra-sync-terminal-dimensions-uses-ghostel-handler ()
  "Ghostel dimension sync should update Ghostel's terminal model before PTY size."
  (let ((adjust-calls nil)
        (set-size-calls nil))
    (cl-letf (((symbol-function 'get-buffer-process)
               (lambda (_buffer) 'ghostel-proc))
              ((symbol-function 'window-live-p)
               (lambda (_window) t))
              ((symbol-function 'ghostel--window-adjust-process-window-size)
               (lambda (process windows)
                 (push (list process windows) adjust-calls)
                 '(90 . 24)))
              ((symbol-function 'set-process-window-size)
               (lambda (process height width)
                 (push (list process height width) set-size-calls))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (ai-code-backends-infra--sync-terminal-dimensions
         (current-buffer)
         'mock-window)))
    (should (equal adjust-calls '((ghostel-proc (mock-window)))))
    (should (equal set-size-calls '((ghostel-proc 24 90))))))

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
                 (lambda (&rest _args) 96))
                ((symbol-function 'window-resize)
                 (lambda (window delta horizontal)
                   (setq resize-call (list window delta horizontal)))))
        (ai-code-backends-infra--display-buffer-in-side-window (current-buffer))
        (should (functionp (cdr (assq 'window-width captured-entry))))
        (funcall (cdr (assq 'window-width captured-entry)) 'fake-window)
        (should (equal resize-call '(fake-window 4 t)))))))

(ert-deftest test-ai-code-backends-infra-terminal-reflow-filter-ignores-non-ai-vterm-buffer ()
  "The reflow filter should pass through non-session vterm buffers."
  (with-temp-buffer
    (rename-buffer "*vterm*" t)
    (let ((ai-code-backends-infra-terminal-backend 'vterm)
          (ai-code-backends-infra-prevent-reflow-glitch t)
          (original-called nil))
      (cl-letf (((symbol-function 'window-list)
                 (lambda (&rest _args) (list 'fake-window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) (current-buffer))))
        (should (eq (ai-code-backends-infra--terminal-reflow-filter
                     (lambda (&rest _args)
                       (setq original-called t)
                       'native-result)
                     'fake-arg)
                    'native-result))
        (should original-called)))))

(ert-deftest test-ai-code-backends-infra-sync-terminal-dimensions-vterm-height-change ()
  "Vterm height changes should go through the native resize handler."
  (let* ((buffer (get-buffer-create "*codex[vterm-height-resize]*"))
         (process (start-process "ai-code-vterm-resize-test"
                                 buffer
                                 "sleep"
                                 "5"))
         (window 'fake-window)
         (height 24)
         (width 80)
         resize-calls
         rendered
         cancelled-timer)
    (unwind-protect
        (cl-letf (((symbol-function 'window-live-p)
                   (lambda (candidate) (eq candidate window)))
                  ((symbol-function 'get-buffer-window-list)
                   (lambda (_buffer &rest _args) (list window)))
                  ((symbol-function 'window-body-height)
                   (lambda (_window) height))
                  ((symbol-function 'window-body-width)
                   (lambda (_window) width))
                  ((symbol-function 'set-process-window-size)
                   (lambda (&rest _args)
                     (ert-fail "vterm sync should use the native resize handler")))
                  ((symbol-function 'ai-code-backends-infra--terminal-resize-handler)
                   (lambda (&optional backend)
                     (should (eq backend 'vterm))
                     (lambda (proc windows)
                       (push (list proc windows height width) resize-calls)
                       (cons width height))))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (setq cancelled-timer timer)))
                  ((symbol-function 'vterm--filter)
                   (lambda (proc data)
                     (push (list proc data ai-code-backends-infra-vterm-anti-flicker)
                           rendered))))
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
            (setq-local ai-code-backends-infra--vterm-render-timer 'mock-timer)
            (setq-local ai-code-backends-infra--vterm-render-queue "old-redraw-1")
            (ai-code-backends-infra--sync-terminal-dimensions buffer window)
            (setq height 18)
            (setq-local ai-code-backends-infra--vterm-render-queue "old-redraw-2")
            (ai-code-backends-infra--sync-terminal-dimensions buffer window)
            (should (null ai-code-backends-infra--vterm-render-timer))
            (should-not ai-code-backends-infra--vterm-render-queue))
          (should (eq cancelled-timer 'mock-timer))
          (should (equal (nreverse resize-calls)
                         `((,process (,window) 24 80)
                           (,process (,window) 18 80)))))
          (should (equal (nreverse rendered)
                         `((,process "old-redraw-1" nil)
                           (,process "old-redraw-2" nil))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-sync-terminal-dimensions-vterm-width-change ()
  "Vterm width changes should go through the native resize handler."
  (let* ((buffer (get-buffer-create "*codex[vterm-width-resize]*"))
         (process (start-process "ai-code-vterm-width-resize-test"
                                 buffer
                                 "sleep"
                                 "5"))
         (window 'fake-window)
         (height 24)
         (width 80)
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'window-live-p)
                   (lambda (candidate) (eq candidate window)))
                  ((symbol-function 'get-buffer-window-list)
                   (lambda (_buffer &rest _args) (list window)))
                  ((symbol-function 'window-body-height)
                   (lambda (_window) height))
                  ((symbol-function 'window-body-width)
                   (lambda (_window) width))
                  ((symbol-function 'set-process-window-size)
                   (lambda (&rest _args)
                     (ert-fail "vterm sync should not call generic process sizing")))
                  ((symbol-function 'ai-code-backends-infra--terminal-resize-handler)
                   (lambda (&optional backend)
                     (should (eq backend 'vterm))
                     (lambda (proc windows)
                       (push (list proc windows height width) calls)
                       (cons width height)))))
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
            (ai-code-backends-infra--sync-terminal-dimensions buffer window)
            (setq width 100)
            (ai-code-backends-infra--sync-terminal-dimensions buffer window))
          (should (equal (nreverse calls)
                         `((,process (,window) 24 80)
                           (,process (,window) 24 100)))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
    (setq-local ghostel--input-mode 'copy)
    (should (ai-code-backends-infra--terminal-navigation-mode-p))
    (setq-local ghostel--input-mode 'semi-char)
    (should-not (ai-code-backends-infra--terminal-navigation-mode-p))))

(ert-deftest test-ai-code-backends-infra-configure-vterm-buffer-installs-cursor-sync-hook ()
  "Configuring a vterm buffer should install copy-mode cursor synchronization."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
    (let ((ai-code-backends-infra--vterm-advices-installed t))
      (ai-code-backends-infra--configure-vterm-buffer))
    (should (memq #'ai-code-backends-infra--sync-terminal-cursor
                  vterm-copy-mode-hook))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-delegates-to-vterm-module ()
  "Session creation should delegate vterm specifics to the vterm module."
  (let* ((ai-code-backends-infra-terminal-backend 'vterm)
         (buffer-name "*test-ai-code-vterm-delegate*")
         (expected (cons 'delegated-buffer 'delegated-process))
         delegated-call)
    (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
               (lambda () nil))
              ((symbol-function 'ai-code-backends-infra-vterm-create-session)
               (lambda (target-buffer working-dir command env-vars)
                 (setq delegated-call
                       (list target-buffer working-dir command env-vars))
                 expected))
              ((symbol-function 'vterm)
               (lambda (target-buffer)
                 (get-buffer-create target-buffer)))
              ((symbol-function 'get-buffer-process)
               (lambda (_buffer) 'legacy-process))
              ((symbol-function 'ai-code-backends-infra--set-session-directory)
               (lambda (&rest _args) nil))
              ((symbol-function 'ai-code-backends-infra--configure-vterm-buffer)
               (lambda () nil)))
      (should (equal (ai-code-backends-infra--create-terminal-session
                      buffer-name
                      default-directory
                      "echo hi"
                      '("FOO=1"))
                     expected))
      (should (equal delegated-call
                     (list buffer-name
                           default-directory
                           "echo hi"
                           '("FOO=1")))))))

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

(ert-deftest test-ai-code-backends-infra-create-terminal-session-eat-uses-portable-term-name ()
  "Eat startup should preserve the portable TERM value used before the refactor."
  (let* ((buffer-name "*test-ai-code-eat-term-name*")
         (buffer (get-buffer-create buffer-name))
         (ai-code-backends-infra-terminal-backend 'eat)
         captured-term-name)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                   (lambda () nil))
                  ((symbol-function 'eat-mode)
                   (lambda () nil))
                  ((symbol-function 'eat-exec)
                   (lambda (&rest _args)
                     (setq captured-term-name eat-term-name)))
                  ((symbol-function 'get-buffer-process)
                   (lambda (_buffer) nil)))
          (ai-code-backends-infra--create-terminal-session
           buffer-name
           default-directory
           "echo hi"
           nil)
          (should (equal captured-term-name "xterm-256color")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-eat-uses-working-directory ()
  "Eat session buffers should keep WORKING-DIR as `default-directory'."
  (let* ((buffer-name "*test-ai-code-eat-working-dir*")
         (buffer (get-buffer-create buffer-name))
         (working-dir (make-temp-file "ai-code-eat-working-dir-" t))
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
          (let ((default-directory "/tmp/"))
            (ai-code-backends-infra--create-terminal-session
             buffer-name
             working-dir
             "echo hi"
             nil))
          (with-current-buffer buffer
            (should (equal default-directory
                           (file-name-as-directory
                            (expand-file-name working-dir))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-directory-p working-dir)
        (delete-directory working-dir t)))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-eat-filter-ignores-dead-buffer ()
  "Eat filter wrapper should skip bookkeeping when session buffer is dead."
  (let* ((buffer-name "*test-ai-code-eat-dead-buffer*")
         (buffer (get-buffer-create buffer-name))
         (ai-code-backends-infra-terminal-backend 'eat)
         (wrapped-filter nil)
         (orig-filter-called nil)
         (strip-called nil)
         (note-called nil)
         (linkify-called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                     (lambda () nil))
                    ((symbol-function 'eat-mode)
                     (lambda () nil))
                    ((symbol-function 'eat-exec)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'eat-proc))
                    ((symbol-function 'process-filter)
                     (lambda (_process)
                       (lambda (_process _output)
                         (setq orig-filter-called t))))
                    ((symbol-function 'process-sentinel)
                     (lambda (_process) nil))
                    ((symbol-function 'set-process-filter)
                     (lambda (_process filter)
                       (setq wrapped-filter filter)))
                    ((symbol-function 'process-buffer)
                     (lambda (_process) buffer))
                    ((symbol-function 'ai-code-backends-infra--strip-alternate-screen-sequences)
                     (lambda (output)
                       (setq strip-called t)
                       output))
                    ((symbol-function 'ai-code-backends-infra--note-meaningful-output)
                     (lambda (&rest _args)
                       (setq note-called t)))
                    ((symbol-function 'ai-code-session-link--linkify-recent-output)
                     (lambda (&rest _args)
                       (setq linkify-called t))))
            (ai-code-backends-infra--create-terminal-session
             buffer-name
             default-directory
             "echo hi"
             nil))
          (should wrapped-filter)
          (kill-buffer buffer)
          (should-not
           (condition-case nil
               (progn (funcall wrapped-filter 'eat-proc "src/foo.el:12\n")
                      nil)
             (error t)))
          (should-not orig-filter-called)
          (should-not strip-called)
          (should-not note-called)
          (should-not linkify-called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-terminal-send-string-delegates-to-vterm-module ()
  "Terminal send should delegate vterm specifics to the vterm module."
  (let ((buffer (generate-new-buffer " *ai-code-terminal-send-delegate*"))
        delegated-string)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra-vterm-send-string)
                   (lambda (string)
                     (setq delegated-string string)))
                  ((symbol-function 'vterm-send-string)
                   (lambda (&rest _args) nil)))
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
            (ai-code-backends-infra--terminal-send-string "hello"))
          (should (equal delegated-string "hello")))
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

(ert-deftest test-ai-code-backends-infra-terminal-send-string-ghostel-uses-public-api ()
  "Ghostel sessions should send input through `ghostel-send-string'."
  (let ((calls nil))
    (cl-letf (((symbol-function 'ghostel-send-string)
               (lambda (string)
                 (push string calls))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (ai-code-backends-infra--terminal-send-string "hello"))
      (should (equal calls '("hello"))))))

(ert-deftest test-ai-code-backends-infra-terminal-send-special-keys-ghostel-uses-public-api ()
  "Ghostel sessions should send special keys through `ghostel-send-key'."
  (let ((calls nil))
    (cl-letf (((symbol-function 'ghostel-send-key)
               (lambda (key-name &optional mods)
                 (push (list key-name mods) calls))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (ai-code-backends-infra--terminal-send-escape)
        (ai-code-backends-infra--terminal-send-return)
        (ai-code-backends-infra--terminal-send-backspace))
      (should (equal calls
                     '(("backspace" nil)
                       ("return" nil)
                       ("escape" nil)))))))

(ert-deftest test-ai-code-backends-infra-terminal-navigation-mode-delegates-to-ghostel-module ()
  "Navigation-mode detection should delegate ghostel specifics to the ghostel module."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (setq-local ghostel--copy-mode-active nil)
    (cl-letf (((symbol-function 'ai-code-backends-infra-ghostel-navigation-mode-p)
               (lambda () t)))
      (should (ai-code-backends-infra--terminal-navigation-mode-p)))))

(ert-deftest test-ai-code-backends-infra-terminal-resize-handler-supports-ghostel ()
  "Ghostel backend should expose its resize handler."
  (let ((ai-code-backends-infra-terminal-backend 'ghostel))
    (should (eq (ai-code-backends-infra--terminal-resize-handler)
                #'ghostel--window-adjust-process-window-size))))

(ert-deftest test-ai-code-backends-infra-terminal-resize-handler-delegates-to-eat-module ()
  "Resize handler lookup should delegate eat specifics to the eat module."
  (let ((ai-code-backends-infra-terminal-backend 'eat)
        (expected 'ai-code-backends-infra--test-eat-resize-handler))
    (fset expected (lambda (&rest _args) nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra-eat-resize-handler)
                   (lambda () expected)))
          (should (eq (ai-code-backends-infra--terminal-resize-handler)
                      expected)))
      (fmakunbound expected))))

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
         (saved-default
          (test-ai-code-backends-infra--capture-default-binding
           'ghostel-set-title-function))
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (progn
          (setq-default ghostel-set-title-function #'ignore)
          (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                     (lambda () nil))
                    ((symbol-function 'ghostel-exec)
                     (lambda (target-buffer _program &optional _args)
                       (with-current-buffer target-buffer
                         (setq title-tracking-before-start ghostel-set-title-function)
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
      (test-ai-code-backends-infra--restore-default-binding
       'ghostel-set-title-function
       saved-default)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel-disables-title-tracking-after-exec ()
  "Ghostel startup should keep title tracking disabled after `ghostel-exec'."
  (let* ((buffer-name "*test-ai-code-ghostel-title-tracking-after-exec*")
         (buffer (get-buffer-create buffer-name))
         (process 'ghostel-proc)
         (saved-default
          (test-ai-code-backends-infra--capture-default-binding
           'ghostel-set-title-function))
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (progn
          (setq-default ghostel-set-title-function #'ignore)
          (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                     (lambda () nil))
                    ((symbol-function 'ghostel-exec)
                     (lambda (target-buffer _program &optional _args)
                       (with-current-buffer target-buffer
                         ;; `ghostel-exec' enters `ghostel-mode', which resets
                         ;; buffer-local title-tracking state.
                         (kill-local-variable 'ghostel-set-title-function)
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
            (with-current-buffer buffer
              (should-not ghostel-set-title-function))))
      (test-ai-code-backends-infra--restore-default-binding
       'ghostel-set-title-function
       saved-default)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel-uses-working-directory ()
  "Ghostel startup should spawn the process from WORKING-DIR."
  (let* ((buffer-name "*test-ai-code-ghostel-working-dir*")
         (buffer (get-buffer-create buffer-name))
         (process 'ghostel-proc)
         (working-dir (make-temp-file "ai-code-ghostel-working-dir-" t))
         observed-default-directory
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                   (lambda () nil))
                  ((symbol-function 'ghostel-exec)
                   (lambda (target-buffer _program &optional _args)
                     (setq observed-default-directory default-directory)
                     (with-current-buffer target-buffer
                       (setq-local ghostel--process process))
                     process))
                  ((symbol-function 'get-buffer-process)
                   (lambda (target-buffer)
                     (with-current-buffer target-buffer
                       ghostel--process))))
          (let ((default-directory "/tmp/"))
            (ai-code-backends-infra--create-terminal-session
             buffer-name
             working-dir
             "echo hi"
             nil))
          (should (equal observed-default-directory
                         (file-name-as-directory
                          (expand-file-name working-dir)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-directory-p working-dir)
        (delete-directory working-dir t)))))

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel-preserves-native-sentinel ()
  "Ghostel startup should preserve Ghostel's process sentinel for chaining."
  (let* ((buffer-name "*test-ai-code-ghostel-native-sentinel*")
         (buffer (get-buffer-create buffer-name))
         (proc (make-process :name "ai-code-ghostel-native-sentinel"
                             :buffer buffer
                             :command '("sleep" "10")
                             :noquery t))
         (native-sentinel (lambda (&rest _args) nil))
         (saved-kill-default
          (test-ai-code-backends-infra--capture-default-binding
           'ghostel-kill-buffer-on-exit))
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (progn
          (setq-default ghostel-kill-buffer-on-exit t)
          (set-process-sentinel proc native-sentinel)
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
             nil)
            (should (eq (process-get proc 'ai-code-backends-infra--ghostel-sentinel)
                        native-sentinel))
            (with-current-buffer buffer
              (should-not ghostel-kill-buffer-on-exit))))
      (test-ai-code-backends-infra--restore-default-binding
       'ghostel-kill-buffer-on-exit
       saved-kill-default)
      (when (process-live-p proc)
        (delete-process proc))
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
  (let ((saved-default
         (test-ai-code-backends-infra--capture-default-binding
          'ghostel-set-title-function)))
    (unwind-protect
        (progn
          (setq-default ghostel-set-title-function #'ignore)
          (cl-letf (((symbol-function 'ghostel-mode)
                     (lambda () nil))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'add-hook)
                     (lambda (&rest _args) nil)))
            (with-temp-buffer
              (ai-code-backends-infra--configure-ghostel-buffer)
              (should-not ghostel-set-title-function)
              (should (eq (default-value 'ghostel-set-title-function) #'ignore)))))
      (test-ai-code-backends-infra--restore-default-binding
       'ghostel-set-title-function
       saved-default))))

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

(ert-deftest test-ai-code-backends-infra-create-terminal-session-ghostel-filter-ignores-dead-buffer ()
  "Ghostel filter wrapper should skip bookkeeping when session buffer is dead."
  (let* ((buffer-name "*test-ai-code-ghostel-dead-buffer*")
         (buffer (get-buffer-create buffer-name))
         (orig-filter-called nil)
         (note-called nil)
         (linkify-called nil)
         (wrapped-filter nil)
         (ai-code-backends-infra-terminal-backend 'ghostel))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-ensure-backend)
                     (lambda () nil))
                    ((symbol-function 'ghostel-exec)
                     (lambda (target-buffer _program &optional _args)
                       (with-current-buffer target-buffer
                         (setq-local ghostel--process 'ghostel-proc))
                       'ghostel-proc))
                    ((symbol-function 'get-buffer-process)
                     (lambda (target-buffer)
                       (with-current-buffer target-buffer
                         ghostel--process)))
                    ((symbol-function 'process-filter)
                     (lambda (_process)
                       (lambda (_process _output)
                         (setq orig-filter-called t))))
                    ((symbol-function 'set-process-filter)
                     (lambda (_process filter)
                       (setq wrapped-filter filter)))
                    ((symbol-function 'processp)
                     (lambda (proc)
                       (eq proc 'ghostel-proc)))
                    ((symbol-function 'process-buffer)
                     (lambda (_process) buffer))
                    ((symbol-function 'ai-code-backends-infra--note-meaningful-output)
                     (lambda (&rest _args)
                       (setq note-called t)))
                    ((symbol-function 'ai-code-session-link--linkify-recent-output)
                     (lambda (&rest _args)
                       (setq linkify-called t))))
            (ai-code-backends-infra--create-terminal-session
             buffer-name
             default-directory
             "echo hi"
             nil))
          (should wrapped-filter)
          (kill-buffer buffer)
          (should-not
           (condition-case nil
               (progn (funcall wrapped-filter 'ghostel-proc "src/foo.el:12\n")
                      nil)
             (error t)))
          (should-not orig-filter-called)
          (should-not note-called)
          (should-not linkify-called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-source-comment-uses-repo-stable-rationale ()
  "Source comments should avoid local paths and chat transcripts."
  (let ((comment-found nil))
    (dolist (file '("ai-code-backends-infra.el"
                    "ai-code-backends-infra-ghostel.el"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (search-forward "Prefer `ghostel-exec' for Ghostel backend startup" nil t)
          (setq comment-found t))
        (goto-char (point-min))
        (should-not (search-forward "/home/tninja/" nil t))
        (goto-char (point-min))
        (should-not (search-forward "Background:" nil t))
        (goto-char (point-min))
        (should-not (search-forward "@tninja" nil t))))
    (should comment-found)))

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

(ert-deftest test-ai-code-backends-infra-toggle-or-create-session-rebinds-source-file ()
  "Starting a new session from a file buffer should reattach that file."
  (let* ((prefix "codex")
         (working-dir "/tmp/ai-code-start-rebind/")
         (source (generate-new-buffer " *ai-code-source-start-rebind*"))
         (old-session (get-buffer-create "*codex[ai-code-start-rebind:a]*"))
         (new-session (get-buffer-create "*codex[ai-code-start-rebind:b]*"))
         (process-table (make-hash-table :test 'equal))
         (process 'mock-process))
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (when (boundp 'ai-code-backends-infra--file-session-map)
            (clrhash ai-code-backends-infra--file-session-map))

          (with-current-buffer source
            (setq buffer-file-name "/tmp/ai-code-start-rebind/main.el")
            (setq default-directory working-dir))
          (with-current-buffer old-session
            (setq-local ai-code-backends-infra--session-directory working-dir))
          (ai-code-backends-infra--remember-file-session-buffer prefix source old-session)

          (cl-letf (((symbol-function 'ai-code-backends-infra--cleanup-dead-processes)
                     (lambda (_table) nil))
                    ((symbol-function 'ai-code-backends-infra--create-terminal-session)
                     (lambda (&rest _args)
                       (cons new-session process)))
                    ((symbol-function 'sleep-for)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'process-live-p)
                     (lambda (&rest _args) t))
                    ((symbol-function 'set-process-sentinel)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--configure-session-buffer)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (&rest _args) nil)))
            (with-current-buffer source
              (ai-code-backends-infra--toggle-or-create-session
               working-dir
               nil
               process-table
               "echo hi"
               nil
               nil
               "b"
               prefix)))

          (should (eq (gethash
                       (ai-code-backends-infra--file-session-map-key prefix source)
                       ai-code-backends-infra--file-session-map)
                      new-session)))
      (dolist (buf (list source old-session new-session))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

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

(ert-deftest test-ai-code-backends-infra-resolve-session-target-prefills-prompt-buffer-filename ()
  "Prompt-mode buffers should seed new instance names from the file name."
  (let* ((working-dir "/tmp/ai-code-session-target/")
         (prefix "codex")
         (existing-buffer (get-buffer-create "*codex[ai-code-session-target]*"))
         seen-prompt
         seen-default
         context)
    (unwind-protect
        (with-temp-buffer
          (setq-local major-mode 'ai-code-prompt-mode)
          (setq-local buffer-file-name "/tmp/project/.ai.code.files/review-notes.org")
          (cl-letf (((symbol-function 'ai-code-backends-infra--find-session-buffers)
                    (lambda (&rest _args)
                       (list existing-buffer)))
                    ((symbol-function 'read-string)
                     (lambda (prompt &optional _initial-input _history default-value &rest _args)
                       (setq seen-prompt prompt
                             seen-default default-value)
                       default-value)))
            (setq context
                  (ai-code-backends-infra--resolve-session-target
                   working-dir
                   nil
                   prefix
                   nil
                   nil))))
      (when (buffer-live-p existing-buffer)
        (kill-buffer existing-buffer)))
    (should (equal seen-prompt "Instance name (existing: default): "))
    (should (equal seen-default "review-notes.org"))
    (should (equal (plist-get context :instance-name) "review-notes.org"))
    (should (equal (plist-get context :buffer-name)
                   "*codex[ai-code-session-target:review-notes.org]*"))
    (should (equal (plist-get context :session-key)
                   (cons working-dir "review-notes.org")))))

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

(ert-deftest test-ai-code-backends-infra-finalize-started-session-chains-ghostel-sentinel ()
  "Successful startup finalization should run Ghostel's native sentinel first."
  (let* ((working-dir "/tmp/ai-code-finalize-ghostel-sentinel/")
         (prefix "claude")
         (buffer-name "*claude[finalize-ghostel-sentinel]*")
         (buffer (get-buffer-create buffer-name))
         (process 'mock-process)
         (process-table (make-hash-table :test 'equal))
         (installed-sentinel nil)
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-get)
                   (lambda (_process prop)
                     (and (eq prop 'ai-code-backends-infra--ghostel-sentinel)
                          (lambda (proc event)
                            (push (list :ghostel proc event) calls)))))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (_process fn)
                     (setq installed-sentinel fn)))
                  ((symbol-function 'ai-code-backends-infra--cleanup-session)
                   (lambda (&rest _args)
                     (push :cleanup calls)))
                  ((symbol-function 'ai-code-backends-infra--configure-session-buffer)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--remember-session-buffer)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                   (lambda (&rest _args) nil)))
          (ai-code-backends-infra--finalize-started-session
           buffer
           process
           working-dir
           buffer-name
           process-table
           "default"
           prefix
           nil
           nil
           nil
           nil)
          (should installed-sentinel)
          (funcall installed-sentinel process "finished\n")
          (should (equal (nreverse calls)
                         (list (list :ghostel process "finished\n")
                               :cleanup))))
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
