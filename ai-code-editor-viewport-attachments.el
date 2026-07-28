;;; ai-code-editor-viewport-attachments.el --- Viewport attachments  -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;; Keywords: tools, convenience

;;; Commentary:
;; Cross-platform clipboard, file drag-and-drop, and image preview support for
;; AI CLI editor viewports.

;;; Code:

(require 'cl-lib)
(require 'dnd)
(require 'image)
(require 'seq)
(require 'subr-x)

(declare-function ai-code-editor-viewport-source-directory
                  "ai-code-editor-viewport" (&optional viewport))

(defvar ai-code-editor-viewport-mode)

(defcustom ai-code-editor-viewport-image-preview-max-width 480
  "Maximum width in pixels for an image inserted into a viewport."
  :type '(choice (const :tag "Unbounded" nil) integer)
  :group 'ai-code)

(defcustom ai-code-editor-viewport-image-preview-max-height 320
  "Maximum height in pixels for an image inserted into a viewport."
  :type '(choice (const :tag "Unbounded" nil) integer)
  :group 'ai-code)

(defcustom ai-code-editor-viewport-attachment-directory
  ".ai.code.files/attachments"
  "Directory where clipboard images pasted into a viewport are saved.
Relative values are resolved from the CLI session's working directory."
  :type 'directory
  :group 'ai-code)

(defcustom ai-code-editor-viewport-clipboard-image-handlers
  '(("pngpaste" file "%s")
    ("wl-paste" stdout "--type" "image/png")
    ("xclip" stdout "-selection" "clipboard" "-target" "image/png" "-out"))
  "External programs that can save a clipboard image as PNG.
Each entry is (PROGRAM MODE ARGS...).  MODE is `file' when PROGRAM writes
to the path substituted for %s in ARGS, or `stdout' when it emits PNG data."
  :type '(repeat
          (list (string :tag "Program")
                (choice (const file) (const stdout))
                (repeat :inline t (string :tag "Argument"))))
  :group 'ai-code)

(defvar-local ai-code-editor-viewport-attachments--previous-dnd-state nil
  "Locality and value of `dnd-protocol-alist' before attachment support.")

(defun ai-code-editor-viewport-attachments-enable ()
  "Enable file drag-and-drop in the current viewport buffer."
  (setq ai-code-editor-viewport-attachments--previous-dnd-state
        (list (local-variable-p 'dnd-protocol-alist)
              dnd-protocol-alist))
  (setq-local
   dnd-protocol-alist
   (cons (cons "\\`file:"
               (if (>= emacs-major-version 30)
                   #'ai-code-editor-viewport-handle-drops
                 #'ai-code-editor-viewport-handle-drop))
         (cl-remove-if
          (lambda (entry)
            (memq (cdr entry)
                  '(ai-code-editor-viewport-handle-drop
                    ai-code-editor-viewport-handle-drops)))
          dnd-protocol-alist))))

(defun ai-code-editor-viewport-attachments-disable ()
  "Restore drag-and-drop configuration in the current viewport buffer."
  (pcase ai-code-editor-viewport-attachments--previous-dnd-state
    (`(t ,value) (setq-local dnd-protocol-alist value))
    (`(nil ,_) (kill-local-variable 'dnd-protocol-alist)))
  (setq ai-code-editor-viewport-attachments--previous-dnd-state nil))

;;;; File and clipboard attachments

(defun ai-code-editor-viewport--selection-items (selection)
  "Return the individual values encoded in clipboard SELECTION."
  (cond
   ((stringp selection)
    (split-string selection "[\r\n]+" t))
   ((vectorp selection) (append selection nil))
   ((listp selection) selection)))

(defun ai-code-editor-viewport--selection-file (item)
  "Return an existing local file named by clipboard ITEM."
  (when (stringp item)
    (let ((file
           (if (string-prefix-p "file:" item)
               (dnd-get-local-file-name item t)
             (and (file-name-absolute-p item)
                  (file-exists-p item)
                  item))))
      (and file (expand-file-name file)))))

(defconst ai-code-editor-viewport--clipboard-file-targets
  '(text/uri-list FILE_NAME FILE NSFilenamesPboardType
    public.file-url text/plain)
  "Known selection targets that can contain copied file paths.")

(defun ai-code-editor-viewport--clipboard-file-target-p (target)
  "Return non-nil when selection TARGET can contain copied files."
  (let ((case-fold-search t))
    (string-match-p
     (concat
      "\\`\\(?:text/uri-list"
      "\\|x-special/\\(?:gnome\\|kde\\|mate\\)-copied-files"
      "\\|application/x-kde4-urilist"
      "\\|public\\.file-url"
      "\\|nsfilenamespboardtype"
      "\\|file_name\\|file\\)\\'")
     (format "%s" target))))

(defun ai-code-editor-viewport--selection-files (target)
  "Return existing files represented by clipboard selection TARGET."
  (let ((selection
         (ignore-errors
           (gui-get-selection 'CLIPBOARD target))))
    (delete-dups
     (delq nil
           (mapcar #'ai-code-editor-viewport--selection-file
                   (ai-code-editor-viewport--selection-items selection))))))

(defun ai-code-editor-viewport--generic-clipboard-files ()
  "Return copied files through Emacs's cross-platform selection API."
  (when (fboundp 'gui-get-selection)
    (let* ((targets
            (ai-code-editor-viewport--selection-items
             (ignore-errors
               (gui-get-selection 'CLIPBOARD 'TARGETS))))
           (candidates
            (delete-dups
             (append
              (seq-filter
               #'ai-code-editor-viewport--clipboard-file-target-p
               targets)
              ai-code-editor-viewport--clipboard-file-targets))))
      (seq-some #'ai-code-editor-viewport--selection-files candidates))))

(defconst ai-code-editor-viewport--macos-clipboard-files-script
  (string-join
   '("ObjC.import('AppKit');"
     "var pb = $.NSPasteboard.generalPasteboard;"
     "var names = pb.propertyListForType('NSFilenamesPboardType');"
     "if (names.isNil()) { '' } else {"
     "  var result = '';"
     "  for (var i = 0; i < names.count; i++) {"
     "    result += ObjC.unwrap(names.objectAtIndex(i)) + '\\n';"
     "  }"
     "  result;"
     "}")
   "\n")
  "JavaScript for Automation that reads Finder file paths.")

(defun ai-code-editor-viewport--macos-clipboard-files ()
  "Return Finder file paths when the generic selection API cannot."
  (when (eq window-system 'ns)
    (when-let* ((osascript (executable-find "osascript")))
      (condition-case nil
          (with-temp-buffer
            (when (zerop
                   (call-process
                    osascript nil t nil
                    "-l" "JavaScript" "-e"
                    ai-code-editor-viewport--macos-clipboard-files-script))
              (delete-dups
               (seq-filter
                #'file-exists-p
                (mapcar #'string-trim
                        (split-string (buffer-string) "\n" t))))))
        (error nil)))))

(defun ai-code-editor-viewport--clipboard-files ()
  "Return local files currently represented by the system clipboard."
  (or (ai-code-editor-viewport--generic-clipboard-files)
      (ai-code-editor-viewport--macos-clipboard-files)))

(defun ai-code-editor-viewport--image-clipboard-target-p (target)
  "Return non-nil when clipboard TARGET names a supported image format."
  (let ((name (downcase (format "%s" target))))
    (or (string-prefix-p "image/" name)
        (member name '("png" "tiff" "public.png" "public.tiff")))))

(defun ai-code-editor-viewport--clipboard-content-kind ()
  "Return `image', `text', or `unknown' for the current clipboard."
  (let* ((targets
          (and (fboundp 'gui-get-selection)
               (ignore-errors
                 (gui-get-selection 'CLIPBOARD 'TARGETS))))
         (target-items
          (ai-code-editor-viewport--selection-items targets)))
    (cond
     ((seq-some #'ai-code-editor-viewport--image-clipboard-target-p
                target-items)
      'image)
     (target-items 'text)
     ((and (fboundp 'gui-get-selection)
           (seq-some
            (lambda (data-type)
              (let ((text
                     (ignore-errors
                       (gui-get-selection 'CLIPBOARD data-type))))
                (and (stringp text) (not (string-empty-p text)))))
            '(UTF8_STRING STRING)))
      'text)
     (t 'unknown))))

(defconst ai-code-editor-viewport--clipboard-image-targets
  '(image/png image/jpeg image/webp image/gif image/tiff image/svg+xml
    image/bmp)
  "Clipboard image targets that can be persisted without conversion.")

(defun ai-code-editor-viewport--clipboard-image-extension (target)
  "Return a file extension suitable for clipboard image TARGET."
  (pcase (downcase (format "%s" target))
    ("image/png" ".png")
    ("image/jpeg" ".jpg")
    ("image/webp" ".webp")
    ("image/gif" ".gif")
    ("image/tiff" ".tiff")
    ("image/svg+xml" ".svg")
    ("image/bmp" ".bmp")))

(defun ai-code-editor-viewport--clipboard-image-target ()
  "Return the best directly readable image target on the clipboard."
  (when (fboundp 'gui-get-selection)
    (let ((available
           (ai-code-editor-viewport--selection-items
            (ignore-errors
              (gui-get-selection 'CLIPBOARD 'TARGETS)))))
      (seq-some
       (lambda (preferred)
         (seq-find
          (lambda (target)
            (string-equal-ignore-case
             (format "%s" target) (format "%s" preferred)))
          available))
       ai-code-editor-viewport--clipboard-image-targets))))

(defun ai-code-editor-viewport--session-directory ()
  "Return the CLI session directory used for viewport file references."
  (file-name-as-directory
   (expand-file-name
    (or (ai-code-editor-viewport-source-directory)
        default-directory))))

(defun ai-code-editor-viewport-ensure-attachment-directory ()
  "Return the attachment directory, creating it when needed."
  (let ((directory
         (if (file-name-absolute-p
              ai-code-editor-viewport-attachment-directory)
           ai-code-editor-viewport-attachment-directory
           (expand-file-name ai-code-editor-viewport-attachment-directory
                             (ai-code-editor-viewport--session-directory)))))
    (make-directory directory t)
    (file-name-as-directory directory)))

(defun ai-code-editor-viewport--save-generic-clipboard-image ()
  "Save clipboard image data through Emacs's selection API.
Return the attachment file, or nil when no supported image target is readable."
  (when-let* ((target (ai-code-editor-viewport--clipboard-image-target))
              (extension
               (ai-code-editor-viewport--clipboard-image-extension target))
              (data
               (ignore-errors
                 (gui-get-selection 'CLIPBOARD target)))
              ((stringp data))
              ((not (string-empty-p data))))
    (let ((file
           (make-temp-file
            (expand-file-name
             "clipboard-"
             (ai-code-editor-viewport-ensure-attachment-directory))
            nil extension))
          completed)
      (unwind-protect
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert data)
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) file nil 'silent))
            (setq completed t)
            file)
        (unless completed
          (when (file-exists-p file)
            (delete-file file)))))))

(defun ai-code-editor-viewport--handler-arguments (arguments file)
  "Substitute FILE for %s in clipboard handler ARGUMENTS."
  (mapcar
   (lambda (argument)
     (replace-regexp-in-string "%s" file argument t t))
   arguments))

(defun ai-code-editor-viewport--run-clipboard-image-handler
    (handler output-file)
  "Use HANDLER to write a clipboard image to OUTPUT-FILE."
  (pcase-let* ((`(,program ,mode . ,arguments) handler)
               (executable (executable-find program)))
    (when executable
      (condition-case nil
          (pcase mode
            ('file
             (and (zerop
                   (apply #'call-process executable nil nil nil
                          (ai-code-editor-viewport--handler-arguments
                           arguments output-file)))
                  (file-exists-p output-file)
                  (> (file-attribute-size (file-attributes output-file)) 0)))
            ('stdout
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (let ((coding-system-for-read 'binary)
                     (coding-system-for-write 'binary))
                 (when (and (zerop
                             (apply #'call-process executable nil t nil
                                    arguments))
                            (> (buffer-size) 0))
                   (write-region (point-min) (point-max)
                                 output-file nil 'silent)
                   t)))))
        (error nil)))))

(defun ai-code-editor-viewport--save-external-clipboard-image ()
  "Save a clipboard image with an external handler and return its file."
  (let ((staging-file (make-temp-file "ai-code-clipboard-" nil ".png"))
        attachment-file
        completed
        saved)
    (unwind-protect
        (progn
          (setq saved
                (seq-some
                 (lambda (handler)
                   (ai-code-editor-viewport--run-clipboard-image-handler
                    handler staging-file))
                 ai-code-editor-viewport-clipboard-image-handlers))
          (when saved
            (setq attachment-file
                  (make-temp-file
                   (expand-file-name
                    "clipboard-"
                    (ai-code-editor-viewport-ensure-attachment-directory))
                   nil ".png"))
            (rename-file staging-file attachment-file t)
            (setq completed t))
          (if saved
              attachment-file
            (user-error
             (concat
              "Clipboard image extraction requires pngpaste, wl-paste,"
              " xclip, or a custom handler"))))
      (when (file-exists-p staging-file)
        (delete-file staging-file))
      (unless completed
        (when (and attachment-file (file-exists-p attachment-file))
          (delete-file attachment-file))))))

(defun ai-code-editor-viewport-save-clipboard-image ()
  "Save a clipboard image and return its attachment file.
Prefer Emacs's cross-platform selection API, then try configured external
handlers for display servers that do not expose raw image selections."
  (or (ai-code-editor-viewport--save-generic-clipboard-image)
      (ai-code-editor-viewport--save-external-clipboard-image)))

(defun ai-code-editor-viewport--try-save-clipboard-image ()
  "Save and return a clipboard image, or nil when none can be extracted."
  (condition-case nil
      (ai-code-editor-viewport-save-clipboard-image)
    (user-error nil)))

(defun ai-code-editor-viewport--file-reference (file)
  "Return the prompt reference to FILE for the current viewport."
  (let* ((absolute-file (expand-file-name file))
         (session-directory
          (ai-code-editor-viewport--session-directory)))
    (concat "@"
            (if (and session-directory
                     (file-in-directory-p absolute-file session-directory))
                (file-relative-name absolute-file session-directory)
              absolute-file))))

(defun ai-code-editor-viewport--preview-image (file)
  "Return an Emacs image preview for FILE, or nil when unsupported."
  (when (and (display-images-p)
             (image-supported-file-p file))
    (condition-case nil
        (apply #'create-image
               file nil nil
               (append
                (when ai-code-editor-viewport-image-preview-max-width
                  (list :max-width
                        ai-code-editor-viewport-image-preview-max-width))
                (when ai-code-editor-viewport-image-preview-max-height
                  (list :max-height
                        ai-code-editor-viewport-image-preview-max-height))))
      (error nil))))

(defun ai-code-editor-viewport--delete-image-reference
    (edit-start edit-end &optional old-length)
  "Make an image reference atomic when modified from EDIT-START to EDIT-END.
OLD-LENGTH is non-nil for the after-change invocation of a text-property
modification hook."
  (unless old-length
    (when-let* (((get-text-property
                  edit-start 'ai-code-editor-viewport-file))
                (image-start
                 (or (previous-single-property-change
                      (1+ edit-start)
                      'ai-code-editor-viewport-file
                      nil (point-min))
                     (point-min)))
                (image-end
                 (or (next-single-property-change
                      edit-start
                      'ai-code-editor-viewport-file
                      nil (point-max))
                     (point-max)))
                (inhibit-modification-hooks t))
      (when (> image-end edit-end)
        (delete-region edit-end image-end))
      (when (< image-start edit-start)
        (delete-region image-start edit-start)))))

(defun ai-code-editor-viewport--insert-file (file)
  "Insert one prompt reference for FILE, previewing it when it is an image."
  (let* ((start (point))
         (reference (ai-code-editor-viewport--file-reference file))
         (preview (ai-code-editor-viewport--preview-image file))
         (image-p (or preview (image-supported-file-p file)))
         (image-reference-id
          (and image-p (make-symbol "image-reference"))))
    (insert reference)
    (add-text-properties
     start (point)
     (append
      (list 'ai-code-editor-viewport-file file
            'help-echo (format "File: %s" file)
            'mouse-face 'highlight
            'rear-nonsticky t)
      (when image-p
        (list 'ai-code-editor-viewport-image image-reference-id))
      (when preview
        (list 'display preview
              'modification-hooks
              (list #'ai-code-editor-viewport--delete-image-reference)))))))

(defun ai-code-editor-viewport-attachments--space-character-p (character)
  "Return non-nil when CHARACTER is whitespace."
  (and character
       (string-match-p "\\`[[:space:]]\\'" (char-to-string character))))

(defun ai-code-editor-viewport-attachments-serialize-buffer (buffer)
  "Return BUFFER text with spacing around adjacent image references."
  (let ((contents
         (with-current-buffer buffer
           (save-restriction
             (widen)
             (buffer-substring (point-min) (point-max))))))
    (with-temp-buffer
      (let ((buffer-undo-list t)
            (inhibit-modification-hooks t))
        (insert contents)
        (let ((position (point-min))
              spans)
          (while (< position (point-max))
            (let ((next
                   (or (next-single-property-change
                        position 'ai-code-editor-viewport-image
                        nil (point-max))
                       (point-max))))
              (when (get-text-property
                     position 'ai-code-editor-viewport-image)
                (push (cons position next) spans))
              (setq position next)))
          (dolist (span spans)
            (goto-char (cdr span))
            (unless (or (eobp)
                        (ai-code-editor-viewport-attachments--space-character-p
                         (char-after)))
              (insert " "))
            (goto-char (car span))
            (unless (or (bobp)
                        (ai-code-editor-viewport-attachments--space-character-p
                         (char-before)))
              (insert " "))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun ai-code-editor-viewport-insert-files (files)
  "Insert FILES as readable references in the current editor viewport."
  (unless ai-code-editor-viewport-mode
    (user-error "Not in an AI CLI editor viewport"))
  (when (and files (not (or (bolp) (looking-back "[[:space:]]" 1))))
    (insert "\n"))
  (let ((first t))
    (dolist (file files)
      (unless first
        (insert "\n\n"))
      (ai-code-editor-viewport--insert-file file)
      (setq first nil))))

(defun ai-code-editor-viewport-handle-drop (uri action)
  "Insert the local file represented by dropped URI and return ACTION."
  (when-let* ((file (dnd-get-local-file-name uri t)))
    (ai-code-editor-viewport-insert-files (list file))
    (or action 'copy)))

(defun ai-code-editor-viewport-handle-drops (uris action)
  "Insert local files represented by dropped URIS and return ACTION."
  (let ((files
         (delq nil
               (mapcar
                (lambda (uri)
                  (dnd-get-local-file-name uri t))
                uris))))
    (when files
      (ai-code-editor-viewport-insert-files files)
      (or action 'copy))))

(put 'ai-code-editor-viewport-handle-drops 'dnd-multiple-handler t)

(put 'ai-code-editor-viewport-yank 'delete-selection 'yank)

(defun ai-code-editor-viewport-yank (&optional arg)
  "Insert copied files or images into the viewport, or `yank' with ARG."
  (interactive "*P")
  (unless ai-code-editor-viewport-mode
    (user-error "Not in an AI CLI editor viewport"))
  (if-let* ((files (ai-code-editor-viewport--clipboard-files)))
      (ai-code-editor-viewport-insert-files files)
    (pcase (ai-code-editor-viewport--clipboard-content-kind)
      ('image
       (ai-code-editor-viewport-insert-files
        (list (ai-code-editor-viewport-save-clipboard-image))))
      ('text (yank arg))
      (_
       (if-let* ((image-file
                  (ai-code-editor-viewport--try-save-clipboard-image)))
           (ai-code-editor-viewport-insert-files (list image-file))
         (yank arg))))))


(provide 'ai-code-editor-viewport-attachments)
;;; ai-code-editor-viewport-attachments.el ends here
