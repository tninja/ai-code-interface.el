;;; eca-chat.el --- Minimal ECA chat test stub -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Lightweight ECA chat stub for batch tests.

;;; Code:

(defun eca-chat-open (&optional _session)
  "Return nil in the lightweight test stub."
  nil)

(defun eca-chat-send-prompt (_message)
  "Return nil in the lightweight test stub."
  nil)

(defun eca-chat--get-last-buffer (_session)
  "Return the current buffer in the lightweight test stub."
  (current-buffer))

(defun eca-chat-add-workspace-root ()
  "Return nil in the lightweight test stub."
  (interactive)
  nil)

(defun eca-chat-add-file-context (&rest _args)
  "Return nil in the lightweight test stub."
  nil)

(defun eca-chat-add-repo-map-context (&rest _args)
  "Return nil in the lightweight test stub."
  nil)

(provide 'eca-chat)

;;; eca-chat.el ends here
