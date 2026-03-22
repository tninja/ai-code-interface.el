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
(require 'project)
(require 'ai-code-session-link)
(require 'subr-x)

(declare-function browse-url "browse-url" (url &optional new-window))
(declare-function helm-comp-read "helm-mode" (prompt collection &rest args))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-files "project" (project &optional dirs))
(declare-function project-root "project" (project))
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra" (buffer))
(declare-function ai-code-backends-infra--linkify-session-region "ai-code-backends-infra" (start end))
(declare-function ai-code-backends-infra--terminal-send-string "ai-code-backends-infra" (string))
(declare-function ai-code-backends-infra--terminal-send-backspace "ai-code-backends-infra" ())
(declare-function ai-code--prompt-filepath-candidates "ai-code-prompt-mode" ())
(declare-function ai-code--git-root "ai-code-file" (&optional dir))

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

(defvar ai-code--read-string-fn #'ai-code-plain-read-string
  "Function used by `ai-code-read-string' to read user input.")

;;;###autoload
(defun ai-code-read-string (prompt &optional initial-input candidate-list)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
CANDIDATE-LIST provides additional completion options if provided."
  (funcall ai-code--read-string-fn prompt initial-input candidate-list))

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
(when (featurep 'helm)
  (setq ai-code--read-string-fn #'ai-code-helm-read-string))

(with-eval-after-load 'helm
  (setq ai-code--read-string-fn #'ai-code-helm-read-string))

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

(defun ai-code--imenu-subalist-p (payload)
  "Return non-nil when PAYLOAD looks like an imenu sub-alist."
  (and (listp payload)
       (cl-some (lambda (entry)
                  (and (consp entry) (stringp (car entry))))
                payload)))

(defun ai-code--imenu-item-position (payload)
  "Extract buffer position from imenu PAYLOAD."
  (cond
   ((or (integerp payload) (markerp payload)) payload)
   ((overlayp payload) (overlay-start payload))
   ((and (consp payload)
         (or (integerp (car payload))
             (markerp (car payload))))
    (car payload))
   (t nil)))

(defun ai-code--extract-symbol-from-line (line)
  "Extract a likely symbol identifier from LINE."
  (let ((patterns
         '("^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|class\\|function\\|func\\|fn\\|sub\\|proc\\|method\\|interface\\|struct\\|enum\\|type\\|trait\\|module\\|namespace\\)[ \t]+\\([[:word:]_.$:\\-]+\\)"
           "^[ \t]*\\([[:word:]_.$:\\-]+\\)[ \t]*("
           "^[ \t]*\\([[:word:]_.$:\\-]+\\)[ \t]*[{:]")))
    (catch 'found
      (dolist (pattern patterns)
        (when (string-match pattern line)
          (let ((name (match-string 1 line)))
            (throw 'found (replace-regexp-in-string ":+\\'" "" name)))))
      nil)))

(defun ai-code--imenu-symbol-from-position (payload)
  "Extract a symbol name from PAYLOAD position as fallback."
  (when-let ((pos (ai-code--imenu-item-position payload)))
    (save-excursion
      (goto-char pos)
      (ai-code--extract-symbol-from-line
       (buffer-substring-no-properties
        (line-beginning-position)
        (line-end-position))))))

(defun ai-code--imenu-noise-name-p (name)
  "Return non-nil when NAME looks like an imenu group/template label."
  (or (not (stringp name))
      (string-empty-p (string-trim name))
      (string-match-p "\\`\\*.*\\*\\'" name)
      (string-match-p "\\`[0-9]+\\'" name)))

(defun ai-code--normalize-imenu-symbol-name (name payload)
  "Normalize imenu NAME using PAYLOAD as fallback source."
  (let ((trimmed (and (stringp name) (string-trim name))))
    (if (and trimmed (not (ai-code--imenu-noise-name-p trimmed)))
        trimmed
      (ai-code--imenu-symbol-from-position payload))))

(defun ai-code--flatten-imenu-index (index)
  "Flatten imenu INDEX into a list of useful symbol names."
  (let (result)
    (dolist (item index)
      (when (consp item)
        (let ((name (car item))
              (payload (cdr item)))
          (if (ai-code--imenu-subalist-p payload)
              (setq result (append result (ai-code--flatten-imenu-index payload)))
            (when-let ((symbol (ai-code--normalize-imenu-symbol-name name payload)))
              (push symbol result))))))
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

(defun ai-code--hash-completion-target-file (&optional end-pos)
  "Return an absolute file path for @relative path ending at END-POS.
END-POS defaults to the current '#' position."
  (when-let ((git-root (ai-code--git-root)))
    (let* ((end (or end-pos (1- (point))))
           (start (save-excursion
                    (goto-char end)
                    (skip-chars-backward "A-Za-z0-9_./-")
                    (point)))
           (has-at (save-excursion
                     (goto-char start)
                     (eq (char-before) ?@)))
           (relative-path (and has-at
                               (< start end)
                               (buffer-substring-no-properties start end))))
      (when relative-path
        (let ((file (expand-file-name relative-path git-root)))
          (when (and (file-regular-p file)
                     (string-prefix-p git-root (file-truename file)))
            file))))))

(defun ai-code--file-symbol-candidates (file)
  "Return sorted function/class symbol candidates from FILE."
  (let (symbols)
    (with-current-buffer (find-file-noselect file t)
      (condition-case nil
          (let ((imenu-auto-rescan t)
                (index (imenu--make-index-alist t)))
            (setq symbols (ai-code--flatten-imenu-index index)))
        (error nil)))
    (sort (delete-dups (cl-remove-if-not #'stringp symbols)) #'string<)))

(defun ai-code--choose-symbol-from-file (file)
  "Prompt user to select a symbol from FILE and return it."
  (let ((candidates (ai-code--file-symbol-candidates file)))
    (when candidates
      (condition-case nil
          (completing-read "Symbol: " candidates nil nil)
        (quit nil)))))

(defun ai-code--comment-filepath-capf ()
  "Provide completion candidates for @file paths inside comments."
  (when (and ai-code-prompt-filepath-completion-enabled
             (ai-code--comment-context-p)
             (buffer-file-name)
             (not (minibufferp))
             (ai-code--git-root))
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
  "Auto trigger file path/symbol completion in comments."
  (when (and ai-code-prompt-filepath-completion-enabled
             (ai-code--comment-context-p)
             (buffer-file-name)
             (not (minibufferp)))
    (pcase (char-before)
      (?@
       (let ((candidates (ai-code--prompt-filepath-candidates)))
         (when candidates
           (let ((choice (completing-read "File: " candidates nil nil)))
             (when (and choice (not (string-empty-p choice)))
               (delete-char -1)  ; Remove the '@' we just typed
               (insert choice))))))
      (?#
       (when-let* ((file (ai-code--hash-completion-target-file (1- (point))))
                   (symbol (ai-code--choose-symbol-from-file file)))
         (when (not (string-empty-p symbol))
           (delete-char -1)  ; Remove the '#' we just typed
           (insert (concat "#" symbol))))))))

(defun ai-code--session-auto-trigger-filepath-completion ()
  "Auto trigger file path/symbol completion in AI session buffers."
  (when (and ai-code-prompt-filepath-completion-enabled
             (fboundp 'ai-code-backends-infra--session-buffer-p)
             (ai-code-backends-infra--session-buffer-p (current-buffer))
             (not (minibufferp))
             (ai-code--git-root))
    (pcase (char-before)
      (?@
       (let ((candidates (ai-code--prompt-filepath-candidates)))
         (when candidates
           (let ((choice (completing-read "File: " candidates nil nil)))
             (when (and choice (not (string-empty-p choice)))
               (ai-code-backends-infra--terminal-send-backspace)
               (ai-code-backends-infra--terminal-send-string choice))))))
      (?#
       (when-let* ((file (ai-code--hash-completion-target-file (1- (point))))
                   (symbol (ai-code--choose-symbol-from-file file)))
         (when (not (string-empty-p symbol))
           (ai-code-backends-infra--terminal-send-backspace)
           (ai-code-backends-infra--terminal-send-string (concat "#" symbol))))))))

(defun ai-code--session-handle-at-input ()
  "Handle '@' input in AI session buffers with optional filepath completion."
  (interactive)
  (let ((should-complete
         (and ai-code-prompt-filepath-completion-enabled
              (fboundp 'ai-code-backends-infra--session-buffer-p)
              (ai-code-backends-infra--session-buffer-p (current-buffer))
              (not (minibufferp))
              (ai-code--git-root))))
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

(defun ai-code--session-handle-hash-input ()
  "Handle '#' input in AI session buffers with optional symbol completion."
  (interactive)
  (let* ((should-complete
          (and ai-code-prompt-filepath-completion-enabled
               (fboundp 'ai-code-backends-infra--session-buffer-p)
               (ai-code-backends-infra--session-buffer-p (current-buffer))
               (not (minibufferp))
               (ai-code--git-root)))
         (file (and should-complete
                    (ai-code--hash-completion-target-file (point)))))
    (ai-code-backends-infra--terminal-send-string "#")
    (when-let ((symbol (and file (ai-code--choose-symbol-from-file file))))
      (when (not (string-empty-p symbol))
        (ai-code-backends-infra--terminal-send-backspace)
        (ai-code-backends-infra--terminal-send-string (concat "#" symbol))))))

(defun ai-code--comment-filepath-setup ()
  "Ensure comment @ completion is available in the current buffer."
  (add-hook 'completion-at-point-functions #'ai-code--comment-filepath-capf nil t))

;;;###autoload
(define-minor-mode ai-code-prompt-filepath-completion-mode
  "Toggle @ file completion in comments and AI sessions across all buffers."
  :global t
  :group 'ai-code
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

;;; Code Link Navigation

(defconst ai-code--session-link-file-base-regexp
  "@?[[:alnum:]_./~-]*[./][[:alnum:]_./~-]+"
  "Regexp matching the base path portion of a session file link.")

(defconst ai-code--session-link-file-regexp
  (concat ai-code--session-link-file-base-regexp
          "\\(?:"
          ":[0-9]+:[0-9]+"
          "\\|:[0-9]+"
          "\\|:L[0-9]+\\(?:-[0-9]+\\)?"
          "\\|#[Ll][0-9]+\\(?:-[Ll]?[0-9]+\\)?"
          "\\|([0-9]+\\(?:,[0-9]+\\)?)"
          "\\)?")
  "Regexp matching clickable file-like session links.")

(defconst ai-code--session-link-url-regexp
  "https?://[^][\"'`()<>[:space:]]+"
  "Regexp matching clickable http/https session links.")

(defconst ai-code--session-clickable-link-regexp
  (concat "\\(?:" ai-code--session-link-url-regexp
          "\\)\\|\\(?:" ai-code--session-link-file-regexp
          "\\)")
  "Regexp matching file and URL links in session buffers.")

(defun ai-code--session-link-property-at-point ()
  "Return the clickable session link text at point, or nil."
  (or (get-text-property (point) 'ai-code-session-link)
      (when (> (point) (point-min))
        (get-text-property (1- (point)) 'ai-code-session-link))))

(defun ai-code--session-link-bounds-at-point ()
  "Return bounds of the clickable session link at point, or nil."
  (if-let ((text (ai-code--session-link-property-at-point)))
      (let* ((prop-pos (if (get-text-property (point) 'ai-code-session-link)
                           (point)
                         (1- (point))))
             (start (or (previous-single-property-change
                         (1+ prop-pos) 'ai-code-session-link nil (point-min))
                        (point-min)))
             (end (or (next-single-property-change
                       prop-pos 'ai-code-session-link nil (point-max))
                      (point-max))))
        (when (equal (buffer-substring-no-properties start end) text)
          (cons start end)))
    (let ((point (point))
          (line-start (line-beginning-position))
          (line-end (line-end-position))
          bounds)
      (save-excursion
        (goto-char line-start)
        (while (and (not bounds)
                    (re-search-forward ai-code--session-clickable-link-regexp line-end t))
          (when (and (<= (match-beginning 0) point)
                     (<= point (match-end 0)))
            (setq bounds (cons (match-beginning 0) (match-end 0))))))
      bounds)))

(defun ai-code--session-link-text-at-point ()
  "Return the clickable session link text at point, or nil."
  (or (ai-code--session-link-property-at-point)
      (when-let ((bounds (ai-code--session-link-bounds-at-point)))
        (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun ai-code--parse-session-link (text)
  "Parse TEXT as a session link and return a plist."
  (when (and text (not (string-empty-p (string-trim text))))
    (cond
     ((string-match-p "\\`https?://" text)
      (list :url text))
     ((string-match "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :column-start (string-to-number (match-string 3 text))))
     ((string-match "^\\(.*\\)#[Ll]\\([0-9]+\\)-[Ll]?\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))))
     ((string-match "^\\(.*\\)#[Ll]\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))))
     ((string-match "^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\))$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :column-start (string-to-number (match-string 3 text))))
     ((string-match "^\\(.*\\)(\\([0-9]+\\))$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))))
     ((string-match "^\\(.*\\):L\\([0-9]+\\)-\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))))
     ((string-match "^\\(.*\\):L\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))))
     ((string-match "^\\(.*?\\):\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))))
     ((string-match-p "[./]" text)
      (list :file text))
     (t nil))))

(defun ai-code--session-project-root ()
  "Return the best available project root for the current session."
  (or (when-let ((project (ignore-errors (project-current nil default-directory))))
        (expand-file-name (project-root project)))
      (ai-code--git-root)
      (expand-file-name default-directory)))

(defun ai-code--project-file-candidates (filename)
  "Return possible project file matches for FILENAME."
  (when-let* ((raw filename)
              (filename (ai-code-session-link--normalize-file raw))
              ((not (string-empty-p filename))))
    (let* ((root (ai-code--session-project-root))
           (project-files (ai-code-session-link--project-files root))
           (exact-root (expand-file-name filename root))
           (exact-default (expand-file-name filename default-directory))
           (matches
            (append
             (when (and (file-name-absolute-p filename) (file-exists-p filename))
               (list filename))
             (when (file-exists-p exact-root)
               (list exact-root))
             (when (file-exists-p exact-default)
               (list exact-default))
             (ai-code-session-link--matching-project-files
              filename root project-files))))
      (delete-dups matches))))

(defun ai-code--read-session-link-candidate (prompt candidates)
  "Read one item from CANDIDATES using PROMPT."
  (let* ((choices (delete-dups (copy-sequence candidates)))
         (default (car choices)))
    (when choices
      (completing-read prompt choices nil t nil nil default))))

(defun ai-code--existing-absolute-session-path (filename)
  "Return an existing absolute local path for FILENAME, or nil."
  (when-let ((normalized (ai-code-session-link--normalize-file filename)))
    (when (file-name-absolute-p normalized)
      (ai-code-session-link--resolve-existing-local-path normalized nil))))

(defun ai-code--find-project-file (filename)
  "Locate FILENAME within the current project."
  (or (ai-code--existing-absolute-session-path filename)
        (when-let ((candidates (ai-code--project-file-candidates filename)))
          (if (= (length candidates) 1)
              (car candidates)
            (ai-code--read-session-link-candidate
             (format "Choose file for %s: " filename)
             candidates)))))

(defun ai-code-session-navigate-link-at-mouse (event)
  "Navigate to the session link clicked by mouse EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (position (posn-point start)))
    (when (window-live-p window)
      (select-window window)
      (when (integer-or-marker-p position)
        (goto-char position)
        (ai-code-session-navigate-link-at-point)))))

(defun ai-code--open-session-link-file (text link)
  "Open the file described by LINK for session link TEXT."
  (when-let* ((file (plist-get link :file))
              (abs-file (ai-code--find-project-file file)))
    (find-file-other-window abs-file)
    (when-let ((line-start (plist-get link :line-start)))
      (goto-char (point-min))
      (forward-line (1- line-start))
      (when-let ((column-start (plist-get link :column-start)))
        (when (> column-start 0)
          (move-to-column (1- column-start)))))
    (message "Navigated to %s" text)
    t))

;;;###autoload
(defun ai-code-session-navigate-link-at-point ()
  "Navigate to the file or URL session link at point."
  (interactive)
  (let* ((text (ai-code--session-link-text-at-point))
         (link (and text (ai-code--parse-session-link text))))
    (cond
     ((not link)
      (message "No code link found at point"))
     ((when-let ((url (plist-get link :url)))
        (browse-url url)
        (message "Opened URL: %s" url)
        t))
     ((ai-code--open-session-link-file text link))
     (t
      (message "No code link found at point")))))

(provide 'ai-code-input)

;;; ai-code-input.el ends here
