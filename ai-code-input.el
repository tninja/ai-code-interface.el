;;; ai-code-input.el --- Helm completion for ai-code-interface.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/ai-code-interface.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional Helm completion interface for ai-code-interface.el
;; To use this, ensure both ai-code-interface.el and helm are installed.

;;; Code:

(require 'cl-lib)  ; For `cl-subseq`
(require 'imenu)

(declare-function helm-comp-read "helm-mode" (prompt collection &rest args))

;;;###autoload
(defun ai-code-plain-read-string (prompt &optional initial-input candidate-list)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
CANDIDATE-LIST provides additional completion options if provided.
This function combines candidate-list with history for better completion."
  ;; Combine candidate-list with history, removing duplicates
  (let ((completion-candidates
         (delete-dups (append candidate-list
                              (when (boundp 'ai-code-read-string-history)
                                ai-code-read-string-history)))))
    ;; Use completing-read with the combined candidates
    (completing-read prompt
                     completion-candidates
                     nil nil initial-input
                     'ai-code-read-string-history)))

;;;###autoload
(defalias 'ai-code-read-string #'ai-code-plain-read-string)

(defun ai-code-helm-read-string-with-history (prompt history-file-name &optional initial-input candidate-list)
  "Read a string with Helm completion using specified history file.
PROMPT is the prompt string.
HISTORY-FILE-NAME is the base name for history file.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  ;; Load history from file
  (let* ((helm-history-file (expand-file-name history-file-name user-emacs-directory))
         (helm-history (if (file-exists-p helm-history-file)
                           (with-temp-buffer
                             (insert-file-contents helm-history-file)
                             (read (buffer-string))) ; Assumed newest first
                         '()))
         ;; Use only Helm history, no CLI history
         (history helm-history)
         ;; Extract the most recent item from history (if exists)
         (most-recent (when history
                        (car history)))
         ;; Remove the first item to add it back later
         (rest-history (when history
                         (cl-remove-duplicates (cdr history) :test #'equal)))
         ;; Combine completion list: most recent + candidates + separator + rest of history
         (completion-list
          (append
           ;; If most recent item exists, put it at the top
           (when most-recent
             (list most-recent))
           ;; Add candidate list
           (or candidate-list '())
           ;; Add separator and rest of history
           (when rest-history
             (cons "==================== HISTORY ========================================" rest-history))))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 completion-list
                 :must-match nil
                 :name "Helm Read String, Use C-c C-y to edit selected command. C-b and C-f to move cursor during editing"
                 :fuzzy nil
                 :initial-input initial-input)))
    ;; Add to history if non-empty, single-line and save
    (unless (or (string-empty-p input) (string-match "\n" input))
      (push input history)
      ;; (setq history (mapcar #'substring-no-properties history))
      (with-temp-file helm-history-file ; Save to the Helm-specific history file
        (let ((history-entries (cl-subseq history
                                          0 (min (length history)
                                                 1000))))  ; Keep last 1000 entries
          (insert (let ((print-circle nil))
                    (prin1-to-string history-entries))))))
    input))

(defun ai-code-helm-read-string (prompt &optional initial-input candidate-list)
  "Read a string with Helm completion for ai-code, showing historical inputs.
PROMPT is the prompt string.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  (ai-code-helm-read-string-with-history prompt "ai-code-helm-read-string-history.el" initial-input candidate-list))

;;;###autoload
(if (featurep 'helm)
    (defalias 'ai-code-read-string #'ai-code-helm-read-string))

(defun ai-code--get-window-files ()
  "Get a list of unique file paths from all visible windows."
  (let ((files nil))
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (and buffer (buffer-file-name buffer))
          (cl-pushnew (buffer-file-name buffer) files :test #'string=))))
    files))

(defun ai-code--get-context-files-string ()
  "Get a string of files in the current window for context.
The current buffer's file is always first."
  (if (not buffer-file-name)
      ""
    (let* ((current-buffer-file-name buffer-file-name)
           (all-buffer-files (ai-code--get-window-files))
           (other-buffer-files (remove current-buffer-file-name all-buffer-files))
           (sorted-files (cons current-buffer-file-name other-buffer-files)))
      (if sorted-files
          (concat "\nFiles:\n" (mapconcat #'identity sorted-files "\n"))
        ""))))

(defun ai-code--flatten-imenu-index (index)
  "Flatten imenu INDEX alist into a list of strings."
  (let (result)
    (dolist (item index)
      (when (consp item)
        (let ((name (car item))
              (payload (cdr item)))
          (cond
           ((and (listp payload) (consp (car payload)))
            ;; Nested list (category or sub-index)
            (setq result (append result (ai-code--flatten-imenu-index payload))))
           ((stringp name)
            (push name result))))))
    result))

(defun ai-code--get-functions-from-buffer (buffer)
  "Get a list of function/symbol names from BUFFER using imenu."
  (with-current-buffer buffer
    (when (derived-mode-p 'prog-mode)
      (condition-case nil
          (let ((imenu-auto-rescan t)
                (index (imenu--make-index-alist t)))
            (ai-code--flatten-imenu-index index))
        (error nil)))))

;;;###autoload
(defun ai-code-insert-function-at-point ()
  "Insert a function name selected from current windows' prog-mode buffers."
  (interactive)
  (let ((functions nil))
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (setq functions (append (ai-code--get-functions-from-buffer buffer) functions))))
    (setq functions (sort (delete-dups (cl-remove-if-not #'stringp functions)) #'string<))
    (let ((selected (completing-read "Insert function: " functions nil nil)))
      (when (and selected (not (string-empty-p selected)))
        (insert selected)))))

(provide 'ai-code-input)
;;; ai-code-input.el ends here
