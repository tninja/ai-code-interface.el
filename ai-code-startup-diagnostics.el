;;; ai-code-startup-diagnostics.el --- CLI startup metadata -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Report failed CLI startup using launch metadata only.  Arguments,
;; environment values, and terminal output are deliberately omitted.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun ai-code-startup-diagnostics--shell-command-balanced-p (command)
  "Return non-nil when shell COMMAND has balanced quotes and escapes."
  (let (quote escaped)
    (dotimes (index (length command))
      (let ((character (aref command index)))
        (cond
         ((eq quote ?\')
          (when (eq character ?\') (setq quote nil)))
         (escaped (setq escaped nil))
         ((eq character ?\\) (setq escaped t))
         ((and quote (eq character quote)) (setq quote nil))
         ((and (null quote) (memq character '(?\' ?\")))
          (setq quote character)))))
    (and (null quote) (not escaped))))

(defun ai-code-startup-diagnostics--executable (command)
  "Return only the executable from argv or legacy shell COMMAND.
Skip leading environment assignments.  Unparseable commands yield nil."
  (let ((argv (condition-case nil
                  (if (stringp command)
                      (when (ai-code-startup-diagnostics--shell-command-balanced-p command)
                        (split-string-shell-command command))
                    command)
                (error nil))))
    (while (and (consp argv) (stringp (car argv))
                (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car argv)))
      (setq argv (cdr argv)))
    (when (and (consp argv) (stringp (car argv))) (car argv))))

(defun ai-code-startup-diagnostics--format-field (value)
  "Return VALUE as escaped single-line startup failure text."
  (let* ((print-escape-control-characters t)
         (print-escape-newlines t)
         (printed (prin1-to-string (format "%s" (or value "unknown")))))
    (substring printed 1 -1)))

(defun ai-code-startup-diagnostics--append (buffer fields)
  "Append startup metadata FIELDS to live BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let ((buffer-undo-list t) (inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\nAI Code startup diagnostics:\n")
            (cl-mapc (lambda (label value)
                       (insert "  " label ": " value "\n"))
                     '("Backend" "Executable" "Working directory"
                       "Process status" "Exit status")
                     fields)))))))

(defun ai-code-startup-diagnostics-report
    (buffer process backend command &optional working-directory)
  "Report failed startup for BUFFER, PROCESS, BACKEND, and COMMAND.
WORKING-DIRECTORY is the selected session directory, even if BUFFER died.
Only metadata is appended to BUFFER and logged in `message'.  Command
arguments, environment values, and terminal output are never copied.
Exit status describes the tracked process when it has exited or received
a signal; an open native terminal event pipe has no known child exit code."
  (let* ((status (when (processp process)
                   (ignore-errors (process-status process))))
         (exit-status (when (memq status '(exit signal))
                        (ignore-errors (process-exit-status process))))
         (fields (mapcar
                  #'ai-code-startup-diagnostics--format-field
                  (list backend
                        (ai-code-startup-diagnostics--executable command)
                        (or working-directory
                            (when (buffer-live-p buffer)
                              (buffer-local-value 'default-directory buffer)))
                        status exit-status))))
    (when (buffer-live-p buffer)
      (ignore-errors (ai-code-startup-diagnostics--append buffer fields))
      (pop-to-buffer buffer))
    (message "%s"
             (apply #'format
                    (concat "CLI failed to start [backend=%s executable=%s "
                            "cwd=%s status=%s exit=%s]")
                    fields))))

(provide 'ai-code-startup-diagnostics)
;;; ai-code-startup-diagnostics.el ends here
