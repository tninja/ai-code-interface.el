;;; ai-code-backends-infra-startup.el --- Startup helpers for AI Code terminals -*- lexical-binding: t; -*-

;; Author: Kang Tu, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused helpers for making terminal process startup predictable across
;; backend implementations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function ai-code-backends-infra--handle-session-start-failure
                  "ai-code-backends-infra" (buffer session-key process-table))
(declare-function ai-code-backends-infra--start-ghostel-process
                  "ai-code-backends-infra-ghostel" (buffer command))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))

(defvar ai-code-backends-infra--session-prefix)

(defconst ai-code-backends-infra-ghostel--ai-cli-programs
  '("aider" "claude" "codebuddy" "codex" "copilot" "cursor-agent" "eca"
    "gemini" "goose" "grok" "interpreter" "kiro-cli" "kilo" "opencode" "pi")
  "Bare AI CLI executable names that should be resolved before Ghostel startup.
Generic commands are intentionally excluded so the terminal abstraction keeps
Ghostel's normal command semantics outside AI Code backends.")

(defun ai-code-backends-infra-ghostel--resolve-program (program)
  "Resolve a supported bare AI CLI PROGRAM to an executable path when possible.
Explicit paths and non-AI commands are preserved.  An unresolved AI CLI name
falls back to PROGRAM so Ghostel can report its normal startup error."
  (if (and (stringp program)
           (member program ai-code-backends-infra-ghostel--ai-cli-programs)
           (not (file-name-directory program)))
      (or (executable-find program) program)
    program))

(defun ai-code-backends-infra-startup--resolve-ghostel-exec
    (orig-fun buffer command)
  "Run ORIG-FUN for BUFFER and COMMAND with resolved Ghostel AI executables."
  (if (not (fboundp 'ghostel-exec))
      (funcall orig-fun buffer command)
    (let ((ghostel-exec-function (symbol-function 'ghostel-exec)))
      (cl-letf (((symbol-function 'ghostel-exec)
                 (lambda (target-buffer program &optional args)
                   (funcall ghostel-exec-function
                            target-buffer
                            (ai-code-backends-infra-ghostel--resolve-program program)
                            args))))
        (funcall orig-fun buffer command)))))

(defun ai-code-backends-infra-startup--last-buffer-output (buffer)
  "Return a concise tail of BUFFER output for startup diagnostics."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((end (point-max))
             (start (max (point-min) (- end 500)))
             (text (string-trim (buffer-substring-no-properties start end))))
        (unless (string-empty-p text)
          (replace-regexp-in-string "[\r\n]+" " | " text))))))

(defun ai-code-backends-infra-startup--failure-details (buffer process)
  "Return diagnostic details for failed PROCESS associated with BUFFER."
  (let ((backend
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (boundp 'ai-code-backends-infra--session-prefix)
               ai-code-backends-infra--session-prefix))))
        (cwd
         (when (buffer-live-p buffer)
           (with-current-buffer buffer default-directory)))
        (command (when (and process (processp process))
                   (ignore-errors (process-command process))))
        (status (when (and process (processp process))
                  (ignore-errors (process-status process))))
        (exit-status (when (and process (processp process))
                       (ignore-errors (process-exit-status process)))))
    (list :backend backend
          :executable (car-safe command)
          :command command
          :cwd cwd
          :status status
          :exit-status exit-status
          :last-output (ai-code-backends-infra-startup--last-buffer-output buffer))))

(defun ai-code-backends-infra-startup--format-failure-details (details)
  "Format startup diagnostic DETAILS as a compact user-facing message."
  (format
   "CLI failed to start [backend=%s executable=%s cwd=%s status=%s exit=%s]%s"
   (or (plist-get details :backend) "unknown")
   (or (plist-get details :executable) "unknown")
   (or (plist-get details :cwd) "unknown")
   (or (plist-get details :status) "unknown")
   (or (plist-get details :exit-status) "unknown")
   (if-let* ((output (plist-get details :last-output)))
       (format " output: %s" output)
     "")))

(defun ai-code-backends-infra-startup--diagnose-start-failure
    (orig-fun buffer session-key process-table)
  "Run ORIG-FUN and report actionable startup context for failed sessions."
  (let* ((process (and process-table (gethash session-key process-table)))
         (details (ai-code-backends-infra-startup--failure-details buffer process)))
    (funcall orig-fun buffer session-key process-table)
    (message "%s"
             (ai-code-backends-infra-startup--format-failure-details details))))

(defun ai-code-backends-infra-startup-activate ()
  "Activate shared terminal startup helpers."
  (when (and (fboundp 'ai-code-backends-infra--start-ghostel-process)
             (not (advice-member-p
                   #'ai-code-backends-infra-startup--resolve-ghostel-exec
                   #'ai-code-backends-infra--start-ghostel-process)))
    (advice-add #'ai-code-backends-infra--start-ghostel-process
                :around
                #'ai-code-backends-infra-startup--resolve-ghostel-exec))
  (when (and (fboundp 'ai-code-backends-infra--handle-session-start-failure)
             (not (advice-member-p
                   #'ai-code-backends-infra-startup--diagnose-start-failure
                   #'ai-code-backends-infra--handle-session-start-failure)))
    (advice-add #'ai-code-backends-infra--handle-session-start-failure
                :around
                #'ai-code-backends-infra-startup--diagnose-start-failure)))

(provide 'ai-code-backends-infra-startup)

;;; ai-code-backends-infra-startup.el ends here
