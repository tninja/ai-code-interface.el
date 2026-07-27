;;; ai-code-startup-diagnostics.el --- Safe CLI startup diagnostics -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This library reports actionable CLI startup failures without exposing
;; sensitive command arguments or raw terminal output.

;;; Code:

(require 'subr-x)

(defconst ai-code-startup-diagnostics--sensitive-command-options
  '("--access-key"
    "--access-key-id"
    "--access-token"
    "--api-key"
    "--apikey"
    "--auth-token"
    "--authorization"
    "--client-secret"
    "--credential"
    "--credentials"
    "--password"
    "--passwd"
    "--private-key"
    "--secret"
    "--token")
  "CLI options whose values must be redacted in startup diagnostics.")

(defconst ai-code-startup-diagnostics--child-exit-status-property
  'ai-code-startup-diagnostics--child-exit-status
  "Process property storing a terminal child's reported exit status.")

(defun ai-code-startup-diagnostics--sensitive-command-name-p (name)
  "Return non-nil when NAME identifies a sensitive command value."
  (when (stringp name)
    (let ((normalized
           (replace-regexp-in-string "_" "-" (downcase name))))
      (or (string= name "-H")
          (string= normalized "--header")
          (member normalized
                  ai-code-startup-diagnostics--sensitive-command-options)
          (string-match-p
           "\\(?:api-?key\\|access-key\\(?:-id\\)?\\|token\\|secret\\|password\\|passwd\\|credentials?\\|authorization\\|private-key\\)\\'"
           normalized)))))

(defun ai-code-startup-diagnostics--sensitive-environment-name-p (name)
  "Return non-nil when NAME identifies a sensitive environment value."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name)
       (let ((case-fold-search t))
         (string-match-p
          "\\(?:API[_-]?KEY\\|ACCESS[_-]?KEY\\|TOKEN\\|SECRET\\|PASSWORD\\|PASSWD\\|CREDENTIALS?\\|AUTHORIZATION\\|PRIVATE[_-]?KEY\\)"
          name))))

(defun ai-code-startup-diagnostics--redact-command (command)
  "Return a copy of COMMAND with sensitive argument values redacted."
  (let ((redact-next nil))
    (mapcar
     (lambda (argument)
       (cond
        ((not (stringp argument)) argument)
        ((and (> (length argument) 2)
              (string-prefix-p "-H" argument))
         (setq redact-next nil)
         "-H<redacted>")
        ((string-match
          "\\`\\(\\(?:--env=\\|-e=?\\)\\)\\([A-Za-z_][A-Za-z0-9_]*\\)="
          argument)
         (let ((option (match-string 1 argument))
               (name (match-string 2 argument)))
           (cond
            ((ai-code-startup-diagnostics--sensitive-environment-name-p name)
             (setq redact-next nil)
             (concat option name "=<redacted>"))
            (redact-next
             (setq redact-next nil)
             "<redacted>")
            (t argument))))
        ((string-match "\\`\\([^=]+\\)=" argument)
         (let* ((name (match-string 1 argument))
                (sensitive
                 (or (ai-code-startup-diagnostics--sensitive-command-name-p
                      name)
                     (ai-code-startup-diagnostics--sensitive-environment-name-p
                      name))))
           (cond
            (sensitive
             (setq redact-next nil)
             (concat name "=<redacted>"))
            (redact-next
             (setq redact-next nil)
             "<redacted>")
            (t argument))))
        ((and (string-prefix-p "-" argument)
              (ai-code-startup-diagnostics--sensitive-command-name-p
               argument))
         (setq redact-next t)
         argument)
        (redact-next
         (setq redact-next nil)
         "<redacted>")
        (t argument)))
     command)))

(defun ai-code-startup-diagnostics--shell-command-balanced-p (command)
  "Return non-nil when shell COMMAND has balanced quotes and escapes."
  (let (quote escaped)
    (dotimes (index (length command))
      (let ((character (aref command index)))
        (cond
         ((eq quote ?\')
          (when (eq character ?\')
            (setq quote nil)))
         (escaped
          (setq escaped nil))
         ((eq character ?\\)
          (setq escaped t))
         ((and quote (eq character quote))
          (setq quote nil))
         ((and (null quote) (memq character '(?\' ?\")))
          (setq quote character)))))
    (and (null quote) (not escaped))))

(defun ai-code-startup-diagnostics--command-argv (command)
  "Return the argv represented by startup COMMAND without shell wrappers."
  (cond
   ((stringp command)
    (if (ai-code-startup-diagnostics--shell-command-balanced-p command)
        (condition-case nil
            (split-string-shell-command command)
          (error '("<unparseable-command>")))
      '("<unparseable-command>")))
   ((listp command) (copy-sequence command))))

(defun ai-code-startup-diagnostics--environment-assignment-p (argument)
  "Return non-nil when ARGUMENT is a shell environment assignment."
  (and (stringp argument)
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" argument)))

(defun ai-code-startup-diagnostics--command-executable (command)
  "Return the executable token from parsed startup COMMAND argv."
  (let ((arguments command))
    (while (and arguments
                (ai-code-startup-diagnostics--environment-assignment-p
                 (car arguments)))
      (setq arguments (cdr arguments)))
    (car-safe arguments)))

(defun ai-code-startup-diagnostics--format-command (command)
  "Return COMMAND as complete, redacted, and safely escaped argv text."
  (if command
      (let ((print-escape-control-characters t)
            (print-escape-newlines t)
            (print-length nil)
            (print-level nil))
        (prin1-to-string
         (ai-code-startup-diagnostics--redact-command command)))
    "unknown"))

(defun ai-code-startup-diagnostics--failure-details
    (buffer process backend command)
  "Return safe diagnostic details for a failed CLI startup.
BUFFER, PROCESS, BACKEND, and COMMAND identify the startup attempt."
  (let* ((command-argv
          (ai-code-startup-diagnostics--command-argv command))
         (executable
          (ai-code-startup-diagnostics--command-executable command-argv))
         (safe-executable
          (car-safe
           (ai-code-startup-diagnostics--redact-command
            (list executable))))
         (working-directory
          (when (buffer-live-p buffer)
            (with-current-buffer buffer default-directory)))
         (status
          (when (and process (processp process))
            (ignore-errors (process-status process))))
         (exit-status
          (when (and process (processp process))
            (or
             (ignore-errors
               (process-get
                process
                ai-code-startup-diagnostics--child-exit-status-property))
             (ignore-errors (process-exit-status process))))))
    (list :backend backend
          :executable safe-executable
          :command command-argv
          :cwd working-directory
          :status status
          :exit-status exit-status)))

(defun ai-code-startup-diagnostics--append (buffer details)
  "Append safely formatted startup failure DETAILS to live BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((buffer-undo-list t)
              (inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert
           "\nAI Code startup diagnostics:\n"
           (format "  Backend: %S\n"
                   (or (plist-get details :backend) 'unknown))
           (format "  Executable: %S\n"
                   (or (plist-get details :executable) 'unknown))
           (format "  Command argv: %s\n"
                   (ai-code-startup-diagnostics--format-command
                    (plist-get details :command)))
           (format "  Working directory: %S\n"
                   (or (plist-get details :cwd) 'unknown))
           (format "  Process status: %S\n"
                   (or (plist-get details :status) 'unknown))
           (format "  Exit status: %S\n"
                   (or (plist-get details :exit-status) 'unknown))))))))

(defun ai-code-startup-diagnostics--format-field (value)
  "Return VALUE as escaped single-line startup failure text."
  (let* ((print-escape-control-characters t)
         (print-escape-newlines t)
         (printed (prin1-to-string (format "%s" (or value "unknown")))))
    (substring printed 1 -1)))

(defun ai-code-startup-diagnostics--format-summary (details)
  "Format startup failure DETAILS as a compact user-facing message."
  (format
   "CLI failed to start [backend=%s executable=%s cwd=%s status=%s exit=%s]"
   (ai-code-startup-diagnostics--format-field
    (plist-get details :backend))
   (ai-code-startup-diagnostics--format-field
    (plist-get details :executable))
   (ai-code-startup-diagnostics--format-field
    (plist-get details :cwd))
   (ai-code-startup-diagnostics--format-field
    (plist-get details :status))
   (ai-code-startup-diagnostics--format-field
    (plist-get details :exit-status))))

(defun ai-code-startup-diagnostics-record-child-exit-status
    (process exit-status)
  "Record EXIT-STATUS reported by the native child behind PROCESS.
Return non-nil when PROCESS and EXIT-STATUS are valid."
  (when (and (processp process) (integerp exit-status))
    (process-put
     process
     ai-code-startup-diagnostics--child-exit-status-property
     exit-status)
    t))

(defun ai-code-startup-diagnostics-report (buffer process backend command)
  "Report a safe startup failure for BUFFER and PROCESS.
BACKEND identifies the CLI backend and COMMAND is its raw launch command.
The displayed diagnostics never include raw terminal output or unredacted
sensitive command values."
  (let ((details
         (ai-code-startup-diagnostics--failure-details
          buffer process backend command)))
    (when (buffer-live-p buffer)
      (ignore-errors
        (ai-code-startup-diagnostics--append buffer details))
      (pop-to-buffer buffer))
    (message "%s"
             (ai-code-startup-diagnostics--format-summary details))))

(provide 'ai-code-startup-diagnostics)

;;; ai-code-startup-diagnostics.el ends here
