;;; ai-code-notifications.el --- Desktop notifications for AI Code Interface  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This library provides desktop notification support for AI Code Interface.
;; It notifies users when AI responses are ready, which is useful when
;; working with multiple AI sessions.

;;; Code:

;;; Customization

(defgroup ai-code-notifications nil
  "Desktop notifications for AI Code Interface."
  :group 'tools)

(defcustom ai-code-notifications-enabled t
  "Whether to enable desktop notifications for AI responses."
  :type 'boolean
  :group 'ai-code-notifications)

(defcustom ai-code-notifications-show-on-response t
  "Whether to show a notification when AI completes a response."
  :type 'boolean
  :group 'ai-code-notifications)

(defcustom ai-code-notifications-timeout 5000
  "Timeout in milliseconds for notifications.
Set to 0 for no timeout (notification stays until dismissed)."
  :type 'integer
  :group 'ai-code-notifications)

;;; Variables

(defvar ai-code-notifications--last-notification-time nil
  "Time of the last notification, to avoid spamming.")

(defvar ai-code-notifications--min-interval 2.0
  "Minimum interval in seconds between notifications.")

;;; Functions

(defun ai-code-notifications--dbus-available-p ()
  "Check if D-Bus notifications are available."
  (and (require 'notifications nil t)
       (fboundp 'notifications-notify)))

(defun ai-code-notifications--can-notify-p ()
  "Check if we can send a notification now.
Returns non-nil if enough time has passed since the last notification."
  (or (null ai-code-notifications--last-notification-time)
      (> (float-time (time-subtract (current-time)
                                    ai-code-notifications--last-notification-time))
         ai-code-notifications--min-interval)))

(defun ai-code-notifications-notify (title message)
  "Send a desktop notification with TITLE and MESSAGE.
Respects the notification interval to avoid spamming."
  (when (and ai-code-notifications-enabled
             (ai-code-notifications--can-notify-p))
    (setq ai-code-notifications--last-notification-time (current-time))
    (if (ai-code-notifications--dbus-available-p)
        ;; Use D-Bus notifications if available
        (notifications-notify
         :title title
         :body message
         :timeout ai-code-notifications-timeout
         :app-name "Emacs AI Code")
      ;; Fallback to message in minibuffer
      (message "[AI Code] %s: %s" title message))))

(defun ai-code-notifications-response-ready (&optional backend-name)
  "Notify that an AI response is ready.
BACKEND-NAME is the name of the backend that completed the response."
  (when ai-code-notifications-show-on-response
    (let ((backend (or backend-name "AI"))
          (buffer (current-buffer)))
      (ai-code-notifications-notify
       (format "%s Response Ready" backend)
       (format "Response ready in %s" (buffer-name buffer))))))

;;;###autoload
(defun ai-code-notifications-toggle ()
  "Toggle desktop notifications on/off."
  (interactive)
  (setq ai-code-notifications-enabled (not ai-code-notifications-enabled))
  (message "AI Code notifications %s"
           (if ai-code-notifications-enabled "enabled" "disabled")))

(provide 'ai-code-notifications)

;;; ai-code-notifications.el ends here
