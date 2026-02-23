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

(provide 'test_ai-code-backends-infra)

;;; test_ai-code-backends-infra.el ends here
