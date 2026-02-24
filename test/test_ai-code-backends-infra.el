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
  "Use buffer default-directory when explicit session directory metadata is absent."
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

          (should (equal (nreverse force-prompts) (list nil t)))
          (should (equal (nreverse send-targets)
                         (list "*codex[file-rebind:a]*"
                               "*codex[file-rebind:b]*")))
          (should (equal (nreverse display-targets)
                         (list "*codex[file-rebind:b]*"))))
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

          (should (equal (nreverse force-prompts) (list nil t)))
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

(provide 'test_ai-code-backends-infra)

;;; test_ai-code-backends-infra.el ends here
