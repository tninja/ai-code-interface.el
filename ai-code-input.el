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
(require 'subr-x)

(declare-function helm-comp-read "helm-mode" (prompt collection &rest args))
(declare-function helm-gtags-dwim "helm-gtags" ())
(declare-function projectile-project-root "projectile" ())
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-files "project" (project &optional dirs))
(declare-function project-root "project" (project))
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra" (buffer))
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
  "Regexp matching the base path portion of a session file link.
This intentionally requires either a directory separator or a dot in
the filename so ordinary prose like \"output\" does not become clickable.")

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

(defconst ai-code--session-link-symbol-regexp
  "\\(?:\\_<\\(?:[A-Z][A-Za-z0-9_$]*\\|[[:lower:]_][A-Za-z0-9_$]*[A-Z][A-Za-z0-9_$]*\\)\\_>\\|[A-Za-z_][A-Za-z0-9_$]*\\(?:-+[A-Za-z0-9_$]+\\)+\\)"
  "Regexp matching clickable symbol-like session links.")

(defconst ai-code--session-clickable-link-regexp
  (concat "\\(?:" ai-code--session-link-file-regexp
          "\\)\\|\\(?:" ai-code--session-link-symbol-regexp "\\)")
  "Regexp matching session text that should become mouse-clickable.")

(defvar-local ai-code--session-link-refresh-timer nil
  "Idle timer used to refresh clickable links in the current session buffer.")

(defvar-local ai-code--session-link-pending-bounds nil
  "Pending (START . END) region to rescan for clickable links.")

(defconst ai-code--session-link-symbol-search-max-bytes (* 1024 1024)
  "Maximum file size, in bytes, to scan when validating symbol links.")

(defvar ai-code--session-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ai-code-session-navigate-link-at-mouse)
    (define-key map [mouse-2] #'ai-code-session-navigate-link-at-mouse)
    (define-key map (kbd "RET") #'ai-code-session-navigate-link-at-point)
    map)
  "Keymap used for clickable code links in AI session buffers.")

(defun ai-code--session-link-bounds-at-point ()
  "Return bounds of the clickable session link at point, or nil."
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
    bounds))

(defun ai-code--session-link-text-at-point ()
  "Extract potential code link text at point.
Includes file references and supported location suffixes at point.
Returns the extracted string, or nil if no meaningful text is found."
  (when-let ((bounds (ai-code--session-link-bounds-at-point)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun ai-code--parse-session-link (text)
  "Parse TEXT as a code link and return a plist describing it.
Recognized formats:
  filename:line:column => (:file FILE :line-start N :column-start C)
  filename#Lstart-end  => (:file FILE :line-start N :line-end M)
  filename#Lline       => (:file FILE :line-start N :line-end nil)
  filename(line,column) => (:file FILE :line-start N :column-start C)
  filename(line)       => (:file FILE :line-start N :line-end nil)
  filename:Lstart-end  => (:file FILE :line-start N :line-end M)
  filename:Lline       => (:file FILE :line-start N :line-end nil)
  filename:line        => (:file FILE :line-start N :line-end nil)
  filename (with . or /) => (:file FILE :line-start nil :line-end nil)
  symbol               => (:symbol SYM)
Returns nil when TEXT does not match any recognized format."
  (when (and text (not (string-empty-p (string-trim text))))
    (cond
     ;; filename:42:8 (line + column)
     ((string-match "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end nil
            :column-start (string-to-number (match-string 3 text))))
     ;; filename#L42-60 (GitHub anchor range)
     ((string-match "^\\(.*\\)#[Ll]\\([0-9]+\\)-[Ll]?\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end (string-to-number (match-string 3 text))))
     ;; filename#L42 (GitHub anchor single line)
     ((string-match "^\\(.*\\)#[Ll]\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end nil))
     ;; filename(42,8) (editor-style line + column)
     ((string-match "^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\))$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end nil
            :column-start (string-to-number (match-string 3 text))))
     ;; filename(42) (editor-style line)
     ((string-match "^\\(.*\\)(\\([0-9]+\\))$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end nil))
     ;; filename:L42-60 (GitHub line range)
     ((string-match "^\\(.*\\):L\\([0-9]+\\)-\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end (string-to-number (match-string 3 text))))
     ;; filename:L42 (GitHub single line)
     ((string-match "^\\(.*\\):L\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end nil))
     ;; filename:42 (standard file:line)
     ((string-match "^\\(.*?\\):\\([0-9]+\\)$" text)
      (list :file (match-string 1 text)
            :line-start (string-to-number (match-string 2 text))
            :line-end nil))
     ;; Path or filename containing . or /
      ((string-match-p "[./]" text)
       (list :file text :line-start nil :line-end nil))
      ;; Plain or hyphenated symbol
      ((string-match-p "^[A-Za-z_][A-Za-z0-9_$-]*$" text)
       (list :symbol text))
      (t nil))))

(defun ai-code--session-project-root ()
  "Return the best available project root for the current session."
  (or (when-let ((project (ignore-errors (project-current nil default-directory))))
        (expand-file-name (project-root project)))
      (when (fboundp 'projectile-project-root)
        (ignore-errors
          (let ((root (projectile-project-root)))
            (when root
              (expand-file-name root)))))
      (ai-code--git-root)
      (expand-file-name default-directory)))

(defun ai-code--normalize-session-link-file (filename)
  "Normalize session link FILENAME for project lookup."
  (when (stringp filename)
    (string-remove-prefix "@" filename)))

(defun ai-code--search-file-in-project (filename project-root)
  "Search for FILENAME by its basename under PROJECT-ROOT.
Returns a de-duplicated list of absolute matches."
  (let ((basename (file-name-nondirectory filename)))
    (when (and (stringp basename) (not (string-empty-p basename)))
      (condition-case nil
          (delete-dups
           (directory-files-recursively
            project-root
            (concat "\\`" (regexp-quote basename) "\\'")
            nil t))
        (error nil)))))

(defun ai-code--project-file-candidates (filename)
  "Return possible project file matches for FILENAME."
  (when-let* ((raw filename)
              (filename (ai-code--normalize-session-link-file raw))
              ((not (string-empty-p filename))))
    (let* ((root (ai-code--session-project-root))
           (project (ignore-errors (project-current nil root)))
           (project-files
            (when project
              (mapcar (lambda (file)
                        (if (file-name-absolute-p file)
                            file
                          (expand-file-name file (project-root project))))
                      (project-files project))))
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
             (cl-remove-if-not
              (lambda (file)
                (or (string= (file-relative-name file root) filename)
                    (string= (file-name-nondirectory file)
                             (file-name-nondirectory filename))))
              project-files)
             (ai-code--search-file-in-project filename root))))
      (delete-dups matches))))

(defun ai-code--read-session-link-candidate (prompt candidates)
  "Read one item from CANDIDATES using PROMPT."
  (let* ((choices (delete-dups (copy-sequence candidates)))
         (default (car choices)))
    (if (and (fboundp 'helm-comp-read)
             (or (featurep 'helm)
                 (require 'helm nil t)))
        (helm-comp-read prompt choices :must-match t :initial-input default)
      (completing-read prompt choices nil t nil nil default))))

(defun ai-code--find-project-file (filename)
  "Locate FILENAME within the current project.
Tries the following locations in order:
  1. Absolute path (if FILENAME is absolute and exists).
  2. Relative to the current project root.
  3. Relative to `default-directory'.
  4. Project file matches and basename search inside the project root.
Returns the absolute path on success, or nil when the file cannot be found."
  (when-let ((candidates (ai-code--project-file-candidates filename)))
    (if (= (length candidates) 1)
        (car candidates)
      (ai-code--read-session-link-candidate
       (format "Choose file for %s: " filename)
       candidates))))

(defun ai-code--project-files-for-symbol-search (root)
  "Return project files suitable for symbol search under ROOT."
  (let ((project (ignore-errors (project-current nil root))))
    (when project
      (mapcar (lambda (file)
                (if (file-name-absolute-p file)
                    file
                  (expand-file-name file (project-root project))))
              (project-files project)))))

(defun ai-code--project-symbol-exists-p (symbol)
  "Return non-nil when SYMBOL can be resolved in the current project."
  (when (and (stringp symbol) (not (string-empty-p symbol)))
    (or (let ((sym (intern-soft symbol)))
          (and sym
               (or (fboundp sym)
                   (boundp sym)
                   (facep sym)
                   (featurep sym)
                   (custom-variable-p sym))))
        (when-let ((root (ai-code--session-project-root)))
          (let ((regexp (concat "\\(?:^\\|[^[:alnum:]_$-]\\)"
                                (regexp-quote symbol)
                                "\\(?:$\\|[^[:alnum:]_$-]\\)")))
            (cl-some
             (lambda (file)
               (when (and (file-regular-p file)
                          (let ((attrs (file-attributes file)))
                            (and attrs
                                 (numberp (file-attribute-size attrs))
                                 (<= (file-attribute-size attrs)
                                     ai-code--session-link-symbol-search-max-bytes))))
                 (with-temp-buffer
                   (condition-case nil
                       (progn
                         (insert-file-contents file)
                         (re-search-forward regexp nil t))
                     (error nil)))))
             (ai-code--project-files-for-symbol-search root)))))))

(defun ai-code--session-link-valid-p (text link cache)
  "Return non-nil when LINK parsed from TEXT resolves in the current project."
  (let ((cached (gethash text cache :missing)))
    (if (not (eq cached :missing))
        cached
      (puthash text
               (cond
                ((plist-get link :file)
                 (and (ai-code--project-file-candidates (plist-get link :file)) t))
                ((plist-get link :symbol)
                 (and (ai-code--project-symbol-exists-p (plist-get link :symbol)) t))
                (t nil))
               cache))))

(defun ai-code--session-navigate-symbol (symbol)
  "Navigate to SYMBOL using the best available project-aware backend."
  (let ((default-directory (ai-code--session-project-root)))
    (cond
     ((and (fboundp 'helm-gtags-dwim)
           (or (featurep 'helm-gtags)
               (require 'helm-gtags nil t)))
      (helm-gtags-dwim))
     ((fboundp 'xref-find-definitions)
      (xref-find-definitions symbol))
     (t
      (message "Symbol not found: %s (no navigation backend available)" symbol)))))

(defun ai-code--session-link-help-echo (text)
  "Return help text for clickable session link TEXT."
  (format "mouse-1: navigate to %s" text))

(defun ai-code--session-link-put-properties (start end text)
  "Make TEXT between START and END clickable in the current buffer."
  (add-text-properties
   start end
   `(ai-code-session-link ,text
                           mouse-face highlight
                           font-lock-face link
                           help-echo ,(ai-code--session-link-help-echo text)
                           follow-link t
                           keymap ,ai-code--session-link-keymap)))

(defun ai-code--session-link-clear-properties (start end)
  "Remove clickable-link properties previously applied between START and END."
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'ai-code-session-link nil end)
                      end)))
        (when (get-text-property pos 'ai-code-session-link)
           (remove-list-of-text-properties
            pos next
            '(ai-code-session-link mouse-face font-lock-face help-echo
                                   follow-link keymap)))
         (setq pos next)))))

(defun ai-code--session-link-refresh-region (start end)
  "Refresh clickable session links between START and END."
  (when (< start end)
    (with-silent-modifications
      (let ((inhibit-read-only t)
            (link-cache (make-hash-table :test 'equal))
            (case-fold-search nil))
        (ai-code--session-link-clear-properties start end)
        (save-excursion
          (goto-char start)
           (while (re-search-forward ai-code--session-clickable-link-regexp end t)
             (let* ((match-start (match-beginning 0))
                    (match-end (match-end 0))
                    (text (match-string-no-properties 0))
                    (link (ai-code--parse-session-link text)))
               (when (and link
                          (ai-code--session-link-valid-p text link link-cache))
                 (ai-code--session-link-put-properties
                  match-start
                  match-end
                  text)))))))))

(defun ai-code--session-link-schedule-refresh (start end _len)
  "Schedule clickable-link refresh for changed text between START and END."
  (when (and (ai-code-backends-infra--session-buffer-p (current-buffer))
             (<= start end))
    (save-excursion
      (let ((region (cons (progn (goto-char start) (line-beginning-position))
                          (progn (goto-char end) (line-end-position)))))
        (setq ai-code--session-link-pending-bounds
              (let ((pending ai-code--session-link-pending-bounds))
                (if pending
                    (cons (min (car pending) (car region))
                          (max (cdr pending) (cdr region)))
                  region)))))
    (when (timerp ai-code--session-link-refresh-timer)
      (cancel-timer ai-code--session-link-refresh-timer))
    (let ((buffer (current-buffer)))
      (setq ai-code--session-link-refresh-timer
            (run-with-idle-timer
             0.1 nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when-let ((bounds ai-code--session-link-pending-bounds))
                     (setq ai-code--session-link-pending-bounds nil
                           ai-code--session-link-refresh-timer nil)
                     (ai-code--session-link-refresh-region
                      (car bounds)
                      (cdr bounds))))))
             buffer)))))

(defun ai-code-setup-session-link-navigation ()
  "Enable clickable code links in the current AI session buffer."
  (remove-hook 'after-change-functions #'ai-code--session-link-schedule-refresh t)
  (add-hook 'after-change-functions #'ai-code--session-link-schedule-refresh nil t)
  (setq-local ai-code--session-link-pending-bounds
              (cons (point-min) (point-max)))
  (ai-code--session-link-refresh-region (point-min) (point-max)))

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

;;;###autoload
(defun ai-code-session-navigate-link-at-point ()
  "Navigate to the code link at point in AI session windows.
Handles the following link formats:
  - filename            open the file
  - filename:42         open the file and go to line 42
  - filename:L42-60     open the file and go to line 42
  - SymbolName          jump to the symbol definition via xref
Binds to \\[ai-code-session-navigate-link-at-point] inside AI session buffers."
  (interactive)
  (let* ((text (ai-code--session-link-text-at-point))
         (link (and text (ai-code--parse-session-link text))))
    (if link
        (let ((file (plist-get link :file))
              (line-start (plist-get link :line-start))
              (column-start (plist-get link :column-start))
              (symbol (plist-get link :symbol)))
          (cond
           (file
            (let ((abs-file (ai-code--find-project-file file)))
              (if abs-file
                  (progn
                    (find-file-other-window abs-file)
                     (when line-start
                       ;; line-start is 1-indexed; forward-line uses 0-indexed offset
                       (goto-char (point-min))
                       (forward-line (1- line-start))
                       (when (and column-start (> column-start 0))
                         (move-to-column (1- column-start))))
                     (message "Navigated to %s%s" file
                              (if line-start
                                  (if column-start
                                      (format ":%d:%d" line-start column-start)
                                    (format ":%d" line-start))
                                "")))
                 (message "File not found: %s" file))))
            (symbol
             (ai-code--session-navigate-symbol symbol))))
      (message "No code link found at point"))))

(provide 'ai-code-input)

;;; ai-code-input.el ends here
