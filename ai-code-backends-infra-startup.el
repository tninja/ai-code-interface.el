;;; ai-code-backends-infra-startup.el --- Startup helpers for AI Code terminals -*- lexical-binding: t; -*-

;; Author: Kang Tu, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused helpers for making terminal process startup predictable across
;; backend implementations.

;;; Code:

(require 'cl-lib)

(declare-function ai-code-backends-infra--start-ghostel-process
                  "ai-code-backends-infra-ghostel" (buffer command))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))

(defun ai-code-backends-infra-ghostel--resolve-program (program)
  "Resolve bare Ghostel PROGRAM names to executable paths when possible.
Explicit paths are preserved, and unresolved bare names fall back to PROGRAM so
Ghostel can report the startup failure in its normal way."
  (if (and (stringp program)
           (not (file-name-directory program)))
      (or (executable-find program) program)
    program))

(defun ai-code-backends-infra-startup--resolve-ghostel-exec
    (orig-fun buffer command)
  "Run ORIG-FUN for BUFFER and COMMAND with resolved Ghostel executables."
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

(defun ai-code-backends-infra-startup-activate ()
  "Activate shared terminal startup helpers."
  (when (and (fboundp 'ai-code-backends-infra--start-ghostel-process)
             (not (advice-member-p
                   #'ai-code-backends-infra-startup--resolve-ghostel-exec
                   #'ai-code-backends-infra--start-ghostel-process)))
    (advice-add #'ai-code-backends-infra--start-ghostel-process
                :around
                #'ai-code-backends-infra-startup--resolve-ghostel-exec)))

(provide 'ai-code-backends-infra-startup)

;;; ai-code-backends-infra-startup.el ends here
