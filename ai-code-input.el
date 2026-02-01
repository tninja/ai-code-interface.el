;;; ai-code-input.el --- Helm completion for ai-code.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/ai-code-interface.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional Helm completion interface for ai-code.el
;; To use this, ensure both ai-code.el and helm are installed.

;;; Code:

(require 'cl-lib)  ; For `cl-subseq`
(require 'imenu)
(require 'magit)

(declare-function helm-comp-read "helm-mode" (prompt collection &rest args))
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra" (buffer))
(declare-function ai-code-backends-infra--terminal-send-string "ai-code-backends-infra" (string))
(declare-function ai-code-backends-infra--terminal-send-backspace "ai-code-backends-infra" ())
(declare-function ai-code--prompt-filepath-candidates "ai-code-prompt-mode" ())

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

(defvar ai-code-prompt-filepath-completion-enabled nil
  "Non-nil enables @ file completion inside comments and AI sessions.")

(defun ai-code--any-ai-session-active-p ()
  "Return non-nil when any AI session buffer is active."
  (when (fboundp 'ai-code-backends-infra--session-buffer-p)
    (let ((active nil))
      (dolist (buf (buffer-list))
        (when (and (not active)
                   (ai-code-backends-infra--session-buffer-p buf))
          (setq active t)))
      active)))

(defun ai-code--comment-context-p ()
  "Return non-nil when point is inside a comment."
  (nth 4 (syntax-ppss)))

(defun ai-code--comment-filepath-capf ()
  "Provide completion candidates for @file paths inside comments."
  (when (and ai-code-prompt-filepath-completion-enabled
             (ai-code--comment-context-p)
             (buffer-file-name)
             (not (minibufferp))
             (magit-toplevel))
    (let ((end (point))
          (start (save-excursion
                   (skip-chars-backward "A-Za-z0-9_./-")
                   (when (eq (char-before) ?@)
                     (1- (point))))))
      (when start
        (let ((candidates (ai-code--prompt-filepath-candidates)))
          (when candidates
            (list start end candidates :exclusive 'no)))))))

(defun ai-code--comment-auto-trigger-filepath-completion ()
  "Auto trigger file path completion in comments when '@' is inserted."
  (when (and ai-code-prompt-filepath-completion-enabled
             (ai-code--comment-context-p)
             (buffer-file-name)
             (not (minibufferp))
             (eq (char-before) ?@))
    (let ((candidates (ai-code--prompt-filepath-candidates)))
      (when candidates
        (let ((choice (completing-read "File: " candidates nil nil)))
          (when (and choice (not (string-empty-p choice)))
            (delete-char -1)  ; Remove the '@' we just typed
            (insert choice)))))))

(defun ai-code--session-auto-trigger-filepath-completion ()
  "Auto trigger file path completion in AI session buffers when '@' is inserted."
  (when (and ai-code-prompt-filepath-completion-enabled
             (fboundp 'ai-code-backends-infra--session-buffer-p)
             (ai-code-backends-infra--session-buffer-p (current-buffer))
             (not (minibufferp))
             (eq (char-before) ?@)
             (magit-toplevel))
    (let ((candidates (ai-code--prompt-filepath-candidates)))
      (when candidates
        (let ((choice (completing-read "File: " candidates nil nil)))
          (when (and choice (not (string-empty-p choice)))
            (ai-code-backends-infra--terminal-send-backspace)
            (ai-code-backends-infra--terminal-send-string choice)))))))

(defun ai-code--session-handle-at-input ()
  "Handle '@' input in AI session buffers with optional filepath completion."
  (interactive)
  (let ((should-complete
         (and ai-code-prompt-filepath-completion-enabled
              (fboundp 'ai-code-backends-infra--session-buffer-p)
              (ai-code-backends-infra--session-buffer-p (current-buffer))
              (not (minibufferp))
              (magit-toplevel))))
    (ai-code-backends-infra--terminal-send-string "@")
    (when should-complete
      (let ((candidates (ai-code--prompt-filepath-candidates)))
        (when candidates
          (let ((choice (condition-case nil
                            (completing-read "File: " candidates nil nil)
                          (quit nil))))
            (when (and choice (not (string-empty-p choice)))
              (ai-code-backends-infra--terminal-send-backspace)
              (ai-code-backends-infra--terminal-send-string choice))))))))

(defun ai-code--comment-filepath-setup ()
  "Ensure comment @ completion is available in the current buffer."
  (add-hook 'completion-at-point-functions #'ai-code--comment-filepath-capf nil t))

;;;###autoload
(define-minor-mode ai-code-prompt-filepath-completion-mode
  "Toggle @ file completion in comments and AI sessions across all buffers."
  :global t
  (setq ai-code-prompt-filepath-completion-enabled
        ai-code-prompt-filepath-completion-mode)
  (if ai-code-prompt-filepath-completion-mode
      (progn
        (add-hook 'post-self-insert-hook
                  #'ai-code--comment-auto-trigger-filepath-completion)
        (add-hook 'post-self-insert-hook
                  #'ai-code--session-auto-trigger-filepath-completion)
        (add-hook 'after-change-major-mode-hook #'ai-code--comment-filepath-setup)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (ai-code--comment-filepath-setup))))
    (remove-hook 'post-self-insert-hook
                 #'ai-code--comment-auto-trigger-filepath-completion)
    (remove-hook 'post-self-insert-hook
                 #'ai-code--session-auto-trigger-filepath-completion)
    (remove-hook 'after-change-major-mode-hook #'ai-code--comment-filepath-setup)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (remove-hook 'completion-at-point-functions
                     #'ai-code--comment-filepath-capf t)))))

;;;###autoload
(defun ai-code-toggle-filepath-completion ()
  "Toggle @ file completion in comments and AI sessions across all buffers."
  (interactive)
  (if ai-code-prompt-filepath-completion-mode
      (ai-code-prompt-filepath-completion-mode -1)
    (ai-code-prompt-filepath-completion-mode 1))
  (message "Filepath @ completion is %s"
           (if ai-code-prompt-filepath-completion-mode "enabled" "disabled")))

(provide 'ai-code-input)
;;; ai-code-input.el ends here
