;;; ai-code-backends-infra-platform.el --- Platform terminal compatibility -*- lexical-binding: t; -*-

;; Author: AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Platform-specific compatibility helpers for AI Code terminal sessions.
;; Native Windows Emacs cannot rely on vterm in the same way as Unix-like
;; systems, so failed vterm startup can fall back to Ghostel's ConPTY support.

;;; Code:

(defvar ai-code-backends-infra-terminal-backend)

(declare-function ai-code-backends-infra-ghostel-ensure-backend
                  "ai-code-backends-infra-ghostel" ())
(declare-function ai-code-backends-infra-vterm-ensure-backend
                  "ai-code-backends-infra-vterm" ())

(defcustom ai-code-backends-infra-windows-vterm-fallback t
  "Fall back to Ghostel when vterm cannot start on native Windows.
This only applies when `ai-code-backends-infra-terminal-backend' is `vterm'.
An explicitly selected non-vterm backend is never changed."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defun ai-code-backends-infra-platform--native-windows-p ()
  "Return non-nil when Emacs is running natively on Windows."
  (eq system-type 'windows-nt))

(defun ai-code-backends-infra-platform--vterm-ensure-advice
    (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, adding a native Windows Ghostel fallback.
If vterm startup fails on native Windows, retry using Ghostel.  Keep Ghostel as
the selected backend only after the retry succeeds."
  (if (not (and ai-code-backends-infra-windows-vterm-fallback
                (ai-code-backends-infra-platform--native-windows-p)
                (eq ai-code-backends-infra-terminal-backend 'vterm)))
      (apply orig-fun args)
    (condition-case vterm-error
        (apply orig-fun args)
      (error
       (let ((previous-backend ai-code-backends-infra-terminal-backend))
         (setq ai-code-backends-infra-terminal-backend 'ghostel)
         (condition-case ghostel-error
             (prog1 (ai-code-backends-infra-ghostel-ensure-backend)
               (message
                "AI Code: vterm unavailable on native Windows; using Ghostel"))
           (error
            (setq ai-code-backends-infra-terminal-backend previous-backend)
            (user-error
             (concat
              "Native Windows terminal startup failed. Install Ghostel 0.45+ "
              "from MELPA and run M-x ghostel once to install its native module. "
              "vterm: %s; Ghostel: %s")
             (error-message-string vterm-error)
             (error-message-string ghostel-error)))))))))

(defun ai-code-backends-infra-platform-activate ()
  "Activate platform-specific terminal compatibility behavior."
  (unless
      (advice-member-p
       #'ai-code-backends-infra-platform--vterm-ensure-advice
       #'ai-code-backends-infra-vterm-ensure-backend)
    (advice-add
     #'ai-code-backends-infra-vterm-ensure-backend
     :around
     #'ai-code-backends-infra-platform--vterm-ensure-advice)))

(provide 'ai-code-backends-infra-platform)

;;; ai-code-backends-infra-platform.el ends here
