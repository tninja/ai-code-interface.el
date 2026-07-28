;;; ai-code-send.el --- Insert files and selections into ai-code sessions -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Commands for inserting files, images, regions, diagnostics, and current lines
;; into a session managed by ai-code.  An open viewport editor takes precedence;
;; otherwise the selection is inserted into the TUI session attached to the
;; current buffer without submitting it.

;;; Code:

(require 'dired)
(require 'seq)
(require 'subr-x)
(require 'flymake)
(require 'ai-code-backends-infra)
(require 'ai-code-editor-viewport-attachments)
(require 'ai-code-editor-viewport)
(require 'ai-code-prompt-mode)
(require 'ai-code-utils)

(defvar flycheck-mode)

(declare-function ai-code--prompt-filepath-candidates "ai-code-prompt-mode" ())
(declare-function ai-code--session-project-root "ai-code-utils" ())
(declare-function ai-code-backends-infra-current-buffer-session
                  "ai-code-backends-infra" (&optional buffer))
(declare-function ai-code-backends-infra-session-buffers
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra-session-directory
                  "ai-code-backends-infra" (buffer))
(declare-function ai-code-backends-infra-insert-string
                  "ai-code-backends-infra" (string buffer))
(declare-function ai-code-editor-viewport-save-clipboard-image
                  "ai-code-editor-viewport-attachments" ())
(declare-function ai-code-editor-viewport-ensure-attachment-directory
                  "ai-code-editor-viewport-attachments" ())
(declare-function ai-code-editor-viewport-insert-files
                  "ai-code-editor-viewport-attachments" (files))
(declare-function ai-code-editor-viewport-source-buffer
                  "ai-code-editor-viewport" (&optional viewport))
(declare-function ai-code-editor-viewport-source-directory
                  "ai-code-editor-viewport" (&optional viewport))
(declare-function ai-code-editor-viewport-for-session
                  "ai-code-editor-viewport" (session))
(declare-function flycheck-error-message "flycheck" (error))
(declare-function flycheck-overlay-errors-at "flycheck" (position))

;;;###autoload
(defcustom ai-code-send-screenshot-command
  (cond
   ((eq system-type 'darwin) '("/usr/sbin/screencapture" "-i"))
   ((and (eq system-type 'gnu/linux) (getenv "WAYLAND_DISPLAY")) '("grim"))
   ((eq system-type 'windows-nt) nil)
   (t '("import")))
  "Command used by `ai-code-send-screenshot' to capture an image.
macOS uses the preinstalled screencapture program.  GNU/Linux uses grim under
Wayland and ImageMagick's import program under X11.  Other Unix-like systems
use import by default.  Windows does not bundle a compatible capture program,
so customize this option there.

The destination file name is appended to this list."
  :type '(repeat string)
  :group 'ai-code)

(defun ai-code-send--file-reference (file &optional root)
  "Return an @-prefixed reference for FILE relative to ROOT.
When ROOT is nil, use the source buffer's project root."
  (let* ((absolute (expand-file-name file))
         (root (or root (ai-code--session-project-root))))
    (concat "@"
            (if (file-in-directory-p absolute root)
                (file-relative-name absolute root)
              absolute))))

(defun ai-code-send--candidate-file (candidate)
  "Resolve completion CANDIDATE to a local file path."
  (let ((root (ai-code--session-project-root)))
    (expand-file-name
     (if (string-prefix-p "@" candidate)
         (substring candidate 1)
       candidate)
     root)))

(defun ai-code-send--read-file ()
  "Prompt for a project file and return its local path."
  (let ((candidates (ai-code--prompt-filepath-candidates)))
    (if candidates
        (ai-code-send--candidate-file
         (completing-read "Insert file: " candidates nil t))
      (read-file-name "Insert file: "
                      (ai-code--session-project-root)
                      nil t))))

(defun ai-code-send--buffer-files ()
  "Return files selected by the current buffer, if any."
  (cond
   ((derived-mode-p 'dired-mode)
    (let ((files (or (ai-code-send--dired-region-files)
                     (dired-get-marked-files nil nil nil nil nil))))
      (and files (seq-filter #'file-exists-p files))))
   ((buffer-file-name) (list (buffer-file-name)))
   (t nil)))

(defun ai-code-send--dired-region-files ()
  "Return files represented by the active Dired region, if any."
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end))
          files)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (when-let* ((file (dired-get-filename nil t)))
            (push file files))
          (forward-line 1)))
      (delete-dups (nreverse files)))))

(defun ai-code-send--files (&optional prompt-for-file)
  "Return files to insert, prompting when PROMPT-FOR-FILE is non-nil."
  (or (and (not prompt-for-file)
           (ai-code-send--buffer-files))
      (list (ai-code-send--read-file))))

(defun ai-code-send--files-text (files &optional root)
  "Return prompt text representing FILES relative to ROOT."
  (mapconcat (lambda (file) (ai-code-send--file-reference file root))
             files "\n\n"))

(defun ai-code-send--language ()
  "Return a short language name for the current buffer."
  (let ((mode (symbol-name major-mode)))
    (replace-regexp-in-string
     "-mode\\'" ""
     (replace-regexp-in-string "-ts\\'" "" mode))))

(defun ai-code-send--region-available-p ()
  "Return non-nil when the active region contains non-whitespace text."
  (and (use-region-p)
       (< (region-beginning) (region-end))
       (string-match-p
        "\\S-"
        (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun ai-code-send--region-text (&optional root)
  "Return the active region with its location relative to ROOT."
  (unless (ai-code-send--region-available-p)
    (user-error "No region selected"))
  (let* ((file (buffer-file-name))
         (start (region-beginning))
         (end (region-end))
         (line-start (line-number-at-pos start))
         (line-end
          (line-number-at-pos
           (if (and (> end start)
                    (save-excursion
                      (goto-char end)
                      (bolp)))
               (1- end)
             end)))
         (content (buffer-substring-no-properties start end))
         (location (if file
                       (format "%s#L%d-L%d"
                               (ai-code-send--file-reference file root)
                               line-start line-end)
                     (format "region L%d-L%d" line-start line-end))))
    (format "%s\n\n```%s\n%s\n```"
            location (ai-code-send--language) (string-trim-right content))))

(defun ai-code-send--point-text (&optional root)
  "Return the current line with its file location relative to ROOT."
  (let* ((file (buffer-file-name))
         (start (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties start end))
         (location (if file
                       (format "%s#L%d"
                               (ai-code-send--file-reference file root)
                               (line-number-at-pos))
                     (format "current line L%d" (line-number-at-pos)))))
    (format "%s\n\n```%s\n%s\n```"
            location (ai-code-send--language) line)))

(defun ai-code-send--viewport-buffer (&optional session)
  "Return the AI editor viewport associated with SESSION.
When SESSION is nil, return the current viewport only when its source session
is still running."
  (if session
      (ai-code-editor-viewport-for-session session)
    (when (bound-and-true-p ai-code-editor-viewport-mode)
      (let ((source (ai-code-editor-viewport-source-buffer)))
        (and (memq source (ai-code-backends-infra-session-buffers))
             (current-buffer))))))

(defun ai-code-send--default-destination-p ()
  "Return non-nil when this buffer has an implicit insertion destination."
  (or (ai-code-send--viewport-buffer)
      (ai-code-backends-infra-current-buffer-session)))

(defconst ai-code-send--explicit-target-commands
  '(ai-code-send-file-to
    ai-code-send-region-to
    ai-code-send-dwim-to
    ai-code-send-screenshot-to
    ai-code-send-clipboard-image-to)
  "Insert commands that always ask for a destination session.")

(defconst ai-code-send--region-commands
  '(ai-code-send-region ai-code-send-region-to)
  "Insert commands that require a non-empty region.")

(defun ai-code-send--menu-child-command (child)
  "Return the command represented by Transient CHILD."
  (plist-get (cdr-safe child) :command))

(defun ai-code-send-setup-menu-children (children)
  "Return available Insert menu CHILDREN for the current buffer.
Without an implicit destination, remove commands that do not ask the user to
choose a session.  Also remove region commands when no non-empty region is
active.  Filtering the children before Transient initializes them keeps the
hidden keys out of its active keymap."
  (let ((default-destination (ai-code-send--default-destination-p))
        (region-available (ai-code-send--region-available-p)))
    (seq-filter
     (lambda (child)
       (let ((command (ai-code-send--menu-child-command child)))
         (and (or default-destination
                  (memq command ai-code-send--explicit-target-commands))
              (or region-available
                  (not (memq command ai-code-send--region-commands))))))
     children)))

(defun ai-code-send--prepare-viewport-insertion ()
  "Move to the insertion point and separate existing viewport content."
  (goto-char (point-max))
  (unless (zerop (buffer-size))
    (insert "\n\n")))

(defun ai-code-send--insert-into-viewport (text buffer)
  "Append TEXT to viewport BUFFER without submitting it."
  (with-current-buffer buffer
    (ai-code-send--prepare-viewport-insertion)
    (insert text "\n\n")))

(defun ai-code-send--diagnostic-messages ()
  "Return diagnostic messages at point, or nil."
  (or
   (when (and (bound-and-true-p flymake-mode)
              (fboundp 'flymake-diagnostics))
     (mapcar #'flymake-diagnostic-text
             (flymake-diagnostics (point) (point))))
   (when (and (bound-and-true-p flycheck-mode)
              (fboundp 'flycheck-overlay-errors-at))
     (mapcar #'flycheck-error-message
             (flycheck-overlay-errors-at (point))))))

(defun ai-code-send--diagnostic-text (&optional root)
  "Return diagnostics and the current line relative to ROOT, or nil."
  (when-let* ((messages
               (seq-filter
                (lambda (message)
                  (and (stringp message) (not (string-empty-p message))))
                (ai-code-send--diagnostic-messages))))
    (format "Diagnostics at %s:\n%s\n\n%s"
            (if (buffer-file-name)
                (format "%s#L%d"
                        (ai-code-send--file-reference
                         (buffer-file-name) root)
                        (line-number-at-pos))
              (format "current line L%d" (line-number-at-pos)))
            (mapconcat (lambda (message) (concat "- " message)) messages "\n")
            (ai-code-send--point-text root))))

(defun ai-code-send-prepare-menu ()
  "Check session availability before displaying the Insert menu."
  (cond
   ((ai-code-send--default-destination-p) t)
   ((ai-code-backends-infra-session-buffers)
    (message "No AI session for this project")
    t)
   (t
    (user-error "No AI sessions are running; start one first"))))

(defun ai-code-send--default-session ()
  "Return the session associated with the current buffer or viewport."
  (or (and (ai-code-send--viewport-buffer)
           (ai-code-editor-viewport-source-buffer))
      (ai-code-backends-infra-current-buffer-session)
      (if (ai-code-backends-infra-session-buffers)
          (user-error "No AI session for this project")
        (user-error "No AI sessions are running; start one first"))))

(defun ai-code-send--select-session ()
  "Prompt for any live AI session and return its buffer."
  (let* ((buffers (ai-code-backends-infra-session-buffers))
         (names (mapcar #'buffer-name buffers))
         selected)
    (unless names
      (user-error "No AI sessions are running; start one first"))
    (setq selected
          (get-buffer
           (completing-read "Insert into session: " names nil t)))
    (unless (memq selected (ai-code-backends-infra-session-buffers))
      (user-error "AI session is no longer available"))
    selected))

(defun ai-code-send--content-text (content root)
  "Return CONTENT rendered for ROOT.
CONTENT may be a string or a function accepting ROOT."
  (if (functionp content)
      (funcall content root)
    content))

(defun ai-code-send--tui-insertion-text (text)
  "Return TEXT with two leading and trailing newlines for TUI insertion."
  (concat "\n\n" text "\n\n"))

(defun ai-code-send--destination-for-viewport (viewport &optional root)
  "Return an insertion destination for VIEWPORT using ROOT when provided."
  (list :buffer viewport
        :viewport viewport
        :root (or root
                  (ai-code-editor-viewport-source-directory viewport))))

(defun ai-code-send--destination-for-session (session)
  "Return an insertion destination for SESSION."
  (let ((viewport (ai-code-send--viewport-buffer session)))
    (if viewport
        (ai-code-send--destination-for-viewport
         viewport (ai-code-backends-infra-session-directory session))
      (list :buffer session
            :viewport nil
            :root (ai-code-backends-infra-session-directory session)))))

(defun ai-code-send--resolve-destination (&optional pick-session)
  "Return the implicit or explicitly selected insertion destination.
When PICK-SESSION is non-nil, always select a live session first."
  (if pick-session
      (ai-code-send--destination-for-session
       (ai-code-send--select-session))
    (if-let* ((viewport (ai-code-send--viewport-buffer)))
        (ai-code-send--destination-for-viewport viewport)
      (ai-code-send--destination-for-session
       (ai-code-send--default-session)))))

(defun ai-code-send--insert-at-destination (destination content &optional files)
  "Insert CONTENT or FILES at DESTINATION without submitting it."
  (let ((buffer (plist-get destination :buffer))
        (viewport (plist-get destination :viewport))
        (root (plist-get destination :root)))
    (if viewport
        (if files
            (with-current-buffer buffer
              (ai-code-send--prepare-viewport-insertion)
              (ai-code-editor-viewport-insert-files files)
              (goto-char (point-max))
              (insert "\n\n"))
          (ai-code-send--insert-into-viewport
           (ai-code-send--content-text content root)
           buffer))
      (ai-code-backends-infra-insert-string
       (ai-code-send--tui-insertion-text
        (ai-code-send--content-text content root))
       buffer))))

(defun ai-code-send--dispatch-files (files &optional pick-session)
  "Insert FILES into an ai-code viewport or TUI input.
When PICK-SESSION is non-nil, always choose the destination session first."
  (ai-code-send--insert-at-destination
   (ai-code-send--resolve-destination pick-session)
   (lambda (root) (ai-code-send--files-text files root))
   files))

(defun ai-code-send--dispatch (content &optional pick-session)
  "Insert CONTENT into an ai-code viewport or TUI input without submitting.
CONTENT may be a string or a function accepting the destination session root.
When PICK-SESSION is non-nil, always choose an existing session first."
  (ai-code-send--insert-at-destination
   (ai-code-send--resolve-destination pick-session)
   content))

(defun ai-code-send--dispatch-generated-file (producer &optional pick-session)
  "Insert the file returned by PRODUCER, cleaning it up on failure.
When PICK-SESSION is non-nil, select the destination before calling PRODUCER."
  (let ((destination (ai-code-send--resolve-destination pick-session))
        file
        inserted)
    (unwind-protect
        (progn
          (let ((buffer (plist-get destination :buffer)))
            (unless (buffer-live-p buffer)
              (user-error "AI session is no longer available"))
            (setq file
                  (with-current-buffer buffer
                    (funcall producer))))
          (ai-code-send--insert-at-destination
           destination
           (lambda (root) (ai-code-send--files-text (list file) root))
           (list file))
          (setq inserted t))
      (unless inserted
        (when (and file (file-exists-p file))
          (delete-file file))))))

(defun ai-code-send--capture-screenshot ()
  "Capture a screenshot and return its saved file path."
  (let* ((directory
          (ai-code-editor-viewport-ensure-attachment-directory))
         (command (car ai-code-send-screenshot-command)))
    (unless (and command (executable-find command))
      (user-error "Screenshot command is not available: %s" command))
    (let ((local-file (make-temp-file "ai-code-screenshot-" nil ".png"))
          file
          completed)
      (unwind-protect
          (progn
            (let ((default-directory temporary-file-directory)
                  (arguments
                   (append (cdr ai-code-send-screenshot-command)
                           (list local-file))))
              (unless (zerop
                       (apply #'call-process command nil nil nil arguments))
                (user-error "Screenshot command failed: %s" command)))
            (unless (and (file-exists-p local-file)
                         (> (file-attribute-size
                             (file-attributes local-file))
                            0))
              (user-error "Screenshot command did not create an image"))
            (setq file
                  (make-temp-file
                   (expand-file-name "screenshot-" directory) nil ".png"))
            (copy-file local-file file t)
            (setq completed t)
            file)
        (when (file-exists-p local-file)
          (delete-file local-file))
        (unless completed
          (when (and file (file-exists-p file))
            (delete-file file)))))))

;;;###autoload
(defun ai-code-send-file (&optional prompt-for-file pick-session)
  "Insert a file or selected Dired files into an ai-code session.
When PROMPT-FOR-FILE is non-nil, always prompt for a file.
When PICK-SESSION is non-nil, choose the destination session."
  (interactive "P")
  (ai-code-send--dispatch-files (ai-code-send--files prompt-for-file)
                                pick-session))

;;;###autoload
(defun ai-code-send-file-to (&optional prompt-for-file)
  "Insert a file into a selected ai-code session.
With PROMPT-FOR-FILE, always prompt for a file."
  (interactive "P")
  (ai-code-send-file prompt-for-file t))

;;;###autoload
(defun ai-code-send-current-file ()
  "Insert the current file into an ai-code session."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (ai-code-send--dispatch-files (list file))
    (user-error "Current buffer is not visiting a file")))

;;;###autoload
(defun ai-code-send-other-file ()
  "Prompt for and insert another file into an ai-code session."
  (interactive)
  (ai-code-send--dispatch-files (list (ai-code-send--read-file))))

;;;###autoload
(defun ai-code-send-screenshot (&optional pick-session)
  "Capture and insert a screenshot into an ai-code session.
When PICK-SESSION is non-nil, choose the destination session."
  (interactive)
  (ai-code-send--dispatch-generated-file
   #'ai-code-send--capture-screenshot pick-session))

;;;###autoload
(defun ai-code-send-screenshot-to ()
  "Capture and insert a screenshot into a selected ai-code session."
  (interactive)
  (ai-code-send-screenshot t))

;;;###autoload
(defun ai-code-send-clipboard-image (&optional pick-session)
  "Save and insert the clipboard image into an ai-code session.
When PICK-SESSION is non-nil, choose the destination session."
  (interactive)
  (ai-code-send--dispatch-generated-file
   #'ai-code-editor-viewport-save-clipboard-image pick-session))

;;;###autoload
(defun ai-code-send-clipboard-image-to ()
  "Save and insert the clipboard image into a selected ai-code session."
  (interactive)
  (ai-code-send-clipboard-image t))

;;;###autoload
(defun ai-code-send-region (&optional pick-session)
  "Insert the active region into an ai-code session.
When PICK-SESSION is non-nil, choose the destination session."
  (interactive)
  (ai-code-send--dispatch
   (lambda (root) (ai-code-send--region-text root))
   pick-session)
  (deactivate-mark))

;;;###autoload
(defun ai-code-send-region-to ()
  "Insert the active region into a selected ai-code session."
  (interactive)
  (ai-code-send-region t))

(defun ai-code-send--dwim (pick-session)
  "Insert relevant editor input, selecting a session when PICK-SESSION is set."
  (let ((files (and (derived-mode-p 'dired-mode)
                    (ai-code-send--buffer-files))))
    (if files
        (ai-code-send--dispatch-files files pick-session)
      (ai-code-send--dispatch
       (lambda (root)
         (or (and (ai-code-send--region-available-p)
                  (ai-code-send--region-text root))
             (ai-code-send--diagnostic-text root)
             (ai-code-send--point-text root)))
       pick-session))
    (when (use-region-p)
      (deactivate-mark))))

;;;###autoload
(defun ai-code-send-dwim ()
  "Insert the most relevant editor input into an ai-code session.
Prefer Dired files, the active region, diagnostics at point, then the current
line."
  (interactive)
  (ai-code-send--dwim nil))

;;;###autoload
(defun ai-code-send-dwim-to ()
  "Insert relevant editor input into a selected ai-code session."
  (interactive)
  (ai-code-send--dwim t))

(provide 'ai-code-send)

;;; ai-code-send.el ends here
