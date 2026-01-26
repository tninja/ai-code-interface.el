;;; test_ai-code-notifications.el --- Tests for ai-code-notifications.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-notifications module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-notifications)

(ert-deftest test-ai-code-notifications-toggle ()
  "Test toggling notifications on/off."
  (let ((initial-state ai-code-notifications-enabled))
    (unwind-protect
        (progn
          ;; Set to known state
          (setq ai-code-notifications-enabled t)
          (should (eq ai-code-notifications-enabled t))
          
          ;; Toggle off
          (ai-code-notifications-toggle)
          (should (eq ai-code-notifications-enabled nil))
          
          ;; Toggle on
          (ai-code-notifications-toggle)
          (should (eq ai-code-notifications-enabled t)))
      ;; Restore initial state
      (setq ai-code-notifications-enabled initial-state))))

(ert-deftest test-ai-code-notifications-can-notify-p ()
  "Test notification throttling logic."
  (let ((ai-code-notifications--last-notification-time nil)
        (ai-code-notifications--min-interval 2.0))
    ;; First notification should be allowed
    (should (ai-code-notifications--can-notify-p))
    
    ;; Simulate a recent notification
    (setq ai-code-notifications--last-notification-time (current-time))
    
    ;; Should not allow immediate notification
    (should-not (ai-code-notifications--can-notify-p))
    
    ;; Simulate waiting long enough
    (setq ai-code-notifications--last-notification-time
          (time-subtract (current-time) (seconds-to-time 3.0)))
    
    ;; Should allow notification after interval
    (should (ai-code-notifications--can-notify-p))))

(ert-deftest test-ai-code-notifications-notify-disabled ()
  "Test that notifications are not sent when disabled."
  (let ((ai-code-notifications-enabled nil)
        (ai-code-notifications--last-notification-time nil)
        (notification-called nil))
    ;; Mock the notifications-notify function
    (cl-letf (((symbol-function 'notifications-notify)
               (lambda (&rest _args)
                 (setq notification-called t))))
      (ai-code-notifications-notify "Test" "Message")
      (should-not notification-called))))

(ert-deftest test-ai-code-notifications-response-ready ()
  "Test response ready notification."
  (let ((ai-code-notifications-enabled t)
        (ai-code-notifications-show-on-response t)
        (ai-code-notifications--last-notification-time nil)
        (notification-title nil)
        (notification-body nil))
    ;; Mock the notifications-notify function and force D-Bus availability
    (cl-letf (((symbol-function 'ai-code-notifications--dbus-available-p)
               (lambda () t))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args)
                 (setq notification-title (plist-get args :title))
                 (setq notification-body (plist-get args :body)))))
      (with-temp-buffer
        ;; Use buffer name matching expected format: *<backend>[<dir>]*
        (rename-buffer "*testbackend[test-project]*" t)
        (ai-code-notifications-response-ready "testbackend")
        ;; Notification should have been sent
        (should (string-match-p "testbackend" (or notification-title "")))
        (should (string-match-p "test-project" (or notification-body "")))))))

(provide 'test_ai-code-notifications)

;;; test_ai-code-notifications.el ends here
