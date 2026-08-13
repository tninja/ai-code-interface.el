;;; test_ai-code-editor-viewport-attachments.el --- Viewport attachment tests  -*- lexical-binding: t; -*-

;; Author: realazy
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for viewport clipboard, file drag-and-drop, and image previews.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-editor-viewport)

(ert-deftest test-ai-code-editor-viewport-attachments--yank-copied-image-file-previews-path ()
  "Yanking a copied image file should insert its project path with a preview."
  (let* ((directory (make-temp-file "ai-code-editor-copied-file-" t))
         (image-file (expand-file-name "assets/photo.png" directory))
         (preview '(image :type png :data "preview"))
         preview-source)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" directory))
          (make-directory (file-name-directory image-file))
          (with-temp-file image-file
            (insert "png"))
          (with-temp-buffer
            (setq-local ai-code-editor-viewport--source-directory directory)
            (ai-code-editor-viewport-mode 1)
            (cl-letf (((symbol-function 'gui-get-selection)
                       (lambda (_selection data-type)
                         (when (eq data-type 'text/uri-list)
                           (concat "file://" image-file))))
                      ((symbol-function 'display-images-p) (lambda (&rest _) t))
                      ((symbol-function 'image-supported-file-p)
                       (lambda (_file) t))
                      ((symbol-function 'create-image)
                       (lambda (file &rest _args)
                         (setq preview-source file)
                         preview)))
              (call-interactively (key-binding (kbd "C-y")))
              (should (equal (buffer-string) "@assets/photo.png"))
              (should (equal preview-source image-file))
              (should (equal (get-text-property (point-min) 'display)
                             preview))
              (should (equal
                       (get-text-property
                        (point-min) 'ai-code-editor-viewport-file)
                       image-file))
              (goto-char (point-max))
              (let ((delete-command (key-binding (kbd "DEL"))))
                (should (commandp delete-command))
                (call-interactively delete-command))
              (should (string-empty-p (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-attachments--clipboard-files-reads-finder-paths ()
  "A copied Finder file should win over its pasteboard thumbnail."
  (let* ((directory (make-temp-file "ai-code-editor-macos-file-" t))
         (image-file (expand-file-name "photo with spaces.png" directory)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "png"))
          (let ((window-system 'ns))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (program)
                         (and (string= program "osascript")
                              "/usr/bin/osascript")))
                      ((symbol-function 'call-process)
                       (lambda (program _infile destination _display
                                &rest arguments)
                         (should (equal program "/usr/bin/osascript"))
                         (should (eq destination t))
                         (should (equal (seq-take arguments 3)
                                        '("-l" "JavaScript" "-e")))
                         (insert image-file "\n")
                         0))
                      ((symbol-function 'gui-get-selection)
                       (lambda (_selection data-type)
                         (when (eq data-type 'TARGETS)
                           [TARGETS image/png]))))
              (should
               (equal (ai-code-editor-viewport--clipboard-files)
                      (list image-file))))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-attachments--clipboard-files-reads-desktop-target ()
  "Copied-file MIME targets should work without a platform helper."
  (let* ((directory (make-temp-file "ai-code-editor-desktop-file-" t))
         (file (expand-file-name "diagram.png" directory))
         (file-target 'x-special/gnome-copied-files))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "png"))
          (let ((window-system nil))
            (cl-letf (((symbol-function 'gui-get-selection)
                       (lambda (_selection data-type)
                         (cond
                          ((eq data-type 'TARGETS)
                           (vector 'TARGETS file-target 'image/png))
                          ((eq data-type file-target)
                           (concat "copy\nfile://" file))))))
              (should
               (equal (ai-code-editor-viewport--clipboard-files)
                      (list file))))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-attachments--handle-drops-inserts-every-file ()
  "Dropping multiple files should use the same insertion path as yank."
  (let* ((directory (make-temp-file "ai-code-editor-multi-drop-" t))
         (first (expand-file-name "one.txt" directory))
         (second (expand-file-name "two.txt" directory)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" directory))
          (dolist (file (list first second))
            (with-temp-file file
              (insert "file")))
          (with-temp-buffer
            (setq-local ai-code-editor-viewport--source-directory directory)
            (ai-code-editor-viewport-mode 1)
            (should
             (get 'ai-code-editor-viewport-handle-drops
                  'dnd-multiple-handler))
            (should
             (eq (ai-code-editor-viewport-handle-drops
                  (list (concat "file://" first)
                        (concat "file://" second))
                  'copy)
                 'copy))
            (should (equal (buffer-string) "@one.txt\n\n@two.txt"))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-attachments--handle-drop-inserts-readable-path ()
  "Dropping a file should insert a project-relative reference for the CLI."
  (let* ((directory (make-temp-file "ai-code-editor-drop-" t))
         (file (expand-file-name "notes/design.txt" directory)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" directory))
          (make-directory (file-name-directory file))
          (with-temp-file file
            (insert "design"))
          (with-temp-buffer
            (setq-local ai-code-editor-viewport--source-directory directory)
            (ai-code-editor-viewport-mode 1)
            (should (eq (ai-code-editor-viewport-handle-drop
                         (concat "file://" file) 'copy)
                        'copy))
            (should (equal (buffer-string) "@notes/design.txt"))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-attachments--uses-session-directory-for-reference ()
  "File references should be relative to the viewport's CLI session directory."
  (let* ((repository (make-temp-file "ai-code-editor-nested-project-" t))
         (session-directory (expand-file-name "packages/app/" repository))
         (file (expand-file-name "src/main.el" session-directory)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" repository))
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert ";; main"))
          (with-temp-buffer
            (setq-local ai-code-editor-viewport--source-directory
                        session-directory)
            (ai-code-editor-viewport-mode 1)
            (ai-code-editor-viewport-insert-files (list file))
            (should (equal (buffer-string) "@src/main.el"))))
      (delete-directory repository t))))

(ert-deftest test-ai-code-editor-viewport-attachments--yank-saves-image-preview ()
  "`C-y' should detect clipboard images without target metadata."
  (let* ((directory (make-temp-file "ai-code-editor-clipboard-image-" t))
         (ai-code-editor-viewport-clipboard-image-handlers
          '(("pngpaste" file "%s")))
         (preview '(image :type png :data "preview"))
         handler-output)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" directory))
          (with-temp-buffer
            (setq-local ai-code-editor-viewport--source-directory directory)
            (ai-code-editor-viewport-mode 1)
            (let ((window-system nil))
              (cl-letf (((symbol-function 'gui-get-selection)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'executable-find)
                         (lambda (program)
                           (when (string= program "pngpaste")
                             "/mock/pngpaste")))
                        ((symbol-function 'call-process)
                         (lambda (program _infile _destination _display
                                  &rest args)
                           (should (equal program "/mock/pngpaste"))
                           (setq handler-output (car args))
                           (with-temp-file handler-output
                             (insert "png"))
                           0))
                        ((symbol-function 'display-images-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'image-supported-file-p)
                         (lambda (_file) t))
                        ((symbol-function 'create-image)
                         (lambda (&rest _args) preview)))
                (call-interactively (key-binding (kbd "C-y")))
                (should
                 (string-match-p
                  "\\`@\\.ai\\.code\\.files/attachments/clipboard-.+\\.png\\'"
                  (buffer-string)))
                (let ((attachment-file
                       (get-text-property
                        (point-min) 'ai-code-editor-viewport-file)))
                  (should (file-exists-p attachment-file))
                  (should-not (equal handler-output attachment-file))
                  (should-not (file-exists-p handler-output)))
                (should (equal (get-text-property (point-min) 'display)
                               preview))))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-attachments--yank-saves-generic-image-data ()
  "`C-y' should persist raw image data through Emacs before external tools."
  (let* ((directory (make-temp-file "ai-code-editor-generic-image-" t))
         (image-data (unibyte-string #x89 #x50 #x4e #x47 0 #xff))
         external-handler-probed)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" directory))
          (with-temp-buffer
            (setq-local ai-code-editor-viewport--source-directory directory)
            (ai-code-editor-viewport-mode 1)
            (cl-letf (((symbol-function 'gui-get-selection)
                       (lambda (_selection data-type)
                         (pcase data-type
                           ('TARGETS [image/png UTF8_STRING])
                           ('image/png image-data))))
                      ((symbol-function 'executable-find)
                       (lambda (_program)
                         (setq external-handler-probed t)
                         nil))
                      ((symbol-function 'display-images-p)
                       (lambda (&rest _) nil)))
              (call-interactively (key-binding (kbd "C-y")))
              (let ((attachment-file
                     (get-text-property
                      (point-min) 'ai-code-editor-viewport-file)))
                (should (string-suffix-p ".png" attachment-file))
                (should (file-exists-p attachment-file))
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally attachment-file)
                  (should (equal (buffer-string) image-data))))
              (should-not external-handler-probed))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-editor-viewport-attachments--yank-text-skips-image-extraction ()
  "Yanking ordinary clipboard text should not create or probe attachments."
  (let* ((directory (make-temp-file "ai-code-editor-clipboard-text-" t))
         (attachment-directory
          (expand-file-name ".ai.code.files/attachments" directory))
         (ai-code-editor-viewport-clipboard-image-handlers
          '(("pngpaste" file "%s")))
         image-handler-called)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" directory))
          (with-temp-buffer
            (setq-local ai-code-editor-viewport--source-directory directory)
            (ai-code-editor-viewport-mode 1)
            (let ((window-system 'ns)
                  (kill-ring '("ordinary text"))
                  (kill-ring-yank-pointer nil))
              (setq kill-ring-yank-pointer kill-ring)
              (cl-letf (((symbol-function 'gui-get-selection)
                         (lambda (_selection data-type)
                           (pcase data-type
                             ('TARGETS [UTF8_STRING])
                             ((or 'UTF8_STRING 'STRING) "ordinary text"))))
                        ((symbol-function 'executable-find)
                         (lambda (program)
                           (and (string= program "pngpaste")
                                "/mock/pngpaste")))
                        ((symbol-function 'call-process)
                         (lambda (&rest _args)
                           (setq image-handler-called t)
                           1)))
                (call-interactively (key-binding (kbd "C-y")))
                (should (equal (buffer-string) "ordinary text"))
                (should-not image-handler-called)
                (should-not (file-exists-p attachment-directory))))))
      (delete-directory directory t))))


(provide 'test_ai-code-editor-viewport-attachments)
;;; test_ai-code-editor-viewport-attachments.el ends here
