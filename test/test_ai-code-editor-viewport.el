;;; test_ai-code-editor-viewport.el --- Tests for editor viewport  -*- lexical-binding: t; -*-

;; Author: realazy
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for editing files requested by native AI CLI sessions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'recentf)
(require 'ai-code-editor-viewport)

(declare-function window-layout-transpose "window-x" (&optional window))

(cl-defmacro ai-code-editor-viewport-test--with-buffer
    ((buffer name) &rest body)
  "Bind BUFFER to a temporary buffer named from NAME while running BODY."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(with-temp-buffer
     (rename-buffer (generate-new-buffer-name ,name))
     (let ((,buffer (current-buffer)))
       ,@body)))

(defun ai-code-editor-viewport-test--confirm-cancel ()
  "Cancel the current viewport and confirm discarding nonblank content."
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (_message) t)))
    (ai-code-editor-viewport-cancel)))

(defun ai-code-editor-viewport-test--display-from-lateral-side
    (side source-buffer viewport-buffer &optional width)
  "Display VIEWPORT-BUFFER from a SIDE SOURCE-BUFFER fixture.
Give the source side window WIDTH columns, defaulting to 40."
  (let* ((ai-code-editor-viewport-window-placement 'below)
         (window-sides-slots '(nil nil nil nil))
         (source-window
          (display-buffer-in-side-window
           source-buffer
           `((side . ,side)
             (slot . 0)
             (window-width . ,(or width 40))
             (preserve-size . (t . nil))
             (window-parameters
              . ((no-delete-other-windows . t)
                 (window-size-fixed . width))))))
         (source-height (window-total-height source-window))
         (source-width (window-total-width source-window))
         (main-window (window-main-window))
         (main-buffer (window-buffer main-window))
         (display-state
          (ai-code-editor-viewport--display viewport-buffer source-buffer)))
    (list :source-window source-window
          :source-height source-height
          :source-width source-width
          :main-window main-window
          :main-buffer main-buffer
          :display-state display-state
          :viewport-window (plist-get display-state :window))))

(defun ai-code-editor-viewport-test--transpose-parent-if-supported (window)
  "Transpose WINDOW's parent when the current Emacs provides `window-x'."
  (when (require 'window-x nil t)
    (let ((ignore-window-parameters t))
      (window-layout-transpose (window-parent window)))))

(ert-deftest test-ai-code-editor-viewport--mode-uses-one-yank-key ()
  "Viewport users should paste every supported clipboard type with `C-y'."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (should (eq (key-binding (kbd "C-y"))
                #'ai-code-editor-viewport-yank))
    (should-not (key-binding (kbd "C-c C-y")))))

(ert-deftest test-ai-code-editor-viewport--mode-binds-c-g-to-cancel ()
  "The standard quit key should cancel an active editor viewport cleanly."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (should (eq (key-binding (kbd "C-g"))
                #'ai-code-editor-viewport-cancel))))

(ert-deftest test-ai-code-editor-viewport--mode-remaps-recursive-exits-to-cancel ()
  "Standard `recursive-edit' exits should use viewport cancellation."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (dolist (command '(abort-recursive-edit
                       exit-recursive-edit
                       keyboard-escape-quit))
      (should (eq (command-remapping command)
                  #'ai-code-editor-viewport-cancel)))))

(ert-deftest test-ai-code-editor-viewport--cancel-keeps-nonblank-content-when-declined ()
  "Declining discard confirmation should keep editing nonblank content."
  (with-temp-buffer
    (insert "draft")
    (ai-code-editor-viewport-mode 1)
    (let (prompt exited)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (message)
                   (setq prompt message)
                   nil))
                ((symbol-function 'exit-recursive-edit)
                 (lambda ()
                   (setq exited t))))
        (ai-code-editor-viewport-cancel))
      (should (string-match-p "discard" (downcase prompt)))
      (should-not exited)
      (should (equal (buffer-string) "draft")))))

(ert-deftest test-ai-code-editor-viewport--cancel-discards-nonblank-content-when-confirmed ()
  "Confirming discard should cancel editing nonblank content."
  (with-temp-buffer
    (insert "draft")
    (ai-code-editor-viewport-mode 1)
    (let (prompt exited)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (message)
                   (setq prompt message)
                   t))
                ((symbol-function 'exit-recursive-edit)
                 (lambda ()
                   (setq exited t))))
        (ai-code-editor-viewport-cancel))
      (should (string-match-p "discard" (downcase prompt)))
      (should exited))))

(ert-deftest test-ai-code-editor-viewport--cancel-skips-confirmation-for-blank-content ()
  "Canceling whitespace-only content should exit without confirmation."
  (with-temp-buffer
    (insert " \n\t ")
    (ai-code-editor-viewport-mode 1)
    (let (exited)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_message)
                   (ert-fail "Blank viewport content should not prompt")))
                ((symbol-function 'exit-recursive-edit)
                 (lambda ()
                   (setq exited t))))
        (ai-code-editor-viewport-cancel))
      (should exited))))

(ert-deftest test-ai-code-editor-viewport--cancel-checks-content-outside-narrowing ()
  "Canceling should confirm when nonblank content is hidden by narrowing."
  (with-temp-buffer
    (insert "hidden draft")
    (narrow-to-region (point-max) (point-max))
    (ai-code-editor-viewport-mode 1)
    (let (prompt exited)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (message)
                   (setq prompt message)
                   nil))
                ((symbol-function 'exit-recursive-edit)
                 (lambda ()
                   (setq exited t))))
        (ai-code-editor-viewport-cancel))
      (should prompt)
      (should-not exited))))

(ert-deftest test-ai-code-editor-viewport--alternate-exit-decline-keeps-editing ()
  "Declining an alternate recursive exit should resume viewport editing."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-alternate-exit-source*")
    (let* ((directory (make-temp-file "ai-code-editor-alternate-exit-" t))
           (file (expand-file-name "prompt.md" directory))
           (answers '(nil t))
           (recursive-calls 0))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq recursive-calls (1+ recursive-calls))
                         (erase-buffer)
                         (insert "discard me")))
                      ((symbol-function 'y-or-n-p)
                       (lambda (_message)
                         (prog1 (car answers)
                           (setq answers (cdr answers))))))
              (should-not
               (ai-code-editor-viewport--edit-file
                file source-buffer)))
            (should (= recursive-calls 2))
            (should-not answers)
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--alternate-abort-decline-keeps-editing ()
  "Declining an alternate recursive abort should resume viewport editing."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-alternate-abort-source*")
    (let* ((directory (make-temp-file "ai-code-editor-alternate-abort-" t))
           (file (expand-file-name "prompt.md" directory))
           (answers '(nil t))
           (recursive-calls 0))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq recursive-calls (1+ recursive-calls))
                         (erase-buffer)
                         (insert "discard me")
                         (signal 'quit nil)))
                      ((symbol-function 'y-or-n-p)
                       (lambda (_message)
                         (prog1 (car answers)
                           (setq answers (cdr answers))))))
              (should-not
               (ai-code-editor-viewport--edit-file
                file source-buffer)))
            (should (= recursive-calls 2))
            (should-not answers)
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--cancel-refocuses-source-input ()
  "Canceling should focus the visible source session after closing the viewport."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-focus-source*")
    (ai-code-editor-viewport-test--with-buffer
        (main-buffer "*ai-code-editor-focus-main*")
      (let* ((directory (make-temp-file "ai-code-editor-focus-" t))
             (file (expand-file-name "prompt.md" directory)))
        (unwind-protect
            (progn
              (with-temp-file file
                (insert "draft"))
              (save-window-excursion
                (delete-other-windows)
                (switch-to-buffer main-buffer)
                (let* ((ai-code-editor-viewport-window-height 11)
                       (source-window
                        (display-buffer-in-side-window
                         source-buffer
                         '((side . right)
                           (slot . 0)
                           (window-width . 40))))
                       viewport-window)
                  (select-window source-window)
                  (cl-letf (((symbol-function 'recursive-edit)
                             (lambda ()
                               (setq viewport-window (selected-window))
                               (ai-code-editor-viewport-test--confirm-cancel)))
                            ((symbol-function 'exit-recursive-edit)
                             (lambda () nil)))
                    (should-not
                     (ai-code-editor-viewport--edit-file
                      file source-buffer)))
                  (should-not (window-live-p viewport-window))
                  (should (eq (selected-window) source-window))
                  (should (eq (window-buffer source-window)
                              source-buffer)))))
          (when-let* ((buffer (get-file-buffer file)))
            (kill-buffer buffer))
          (delete-directory directory t))))))

(ert-deftest test-ai-code-editor-viewport--mode-advertises-smart-yank ()
  "The viewport header should describe the single smart paste command."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (should
     (equal header-line-format
            (concat
             " C-c C-c: submit  C-g/C-c C-k: cancel"
             "  C-y: paste text, files, or images ")))))

(ert-deftest test-ai-code-editor-viewport--mode-styles-header-shortcuts ()
  "The viewport header should leave layout to Emacs and bold shortcut keys."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (let ((header header-line-format))
      (dolist (key '("C-c C-c" "C-g/C-c C-k" "C-y"))
        (let ((start (string-match (regexp-quote key) header)))
          (should start)
          (should
           (eq (get-text-property start 'face header)
               'ai-code-editor-viewport-header-key-face))))
      (dotimes (index (length header))
        (should-not (get-text-property index 'display header)))
      (should-not
       (get-text-property (string-match-p ": submit" header) 'face header))
      (should
       (eq (face-attribute
            'ai-code-editor-viewport-header-key-face :inherit nil t)
           'bold)))))

(ert-deftest test-ai-code-editor-viewport--mode-header-uses-current-bindings ()
  "The viewport header should derive key hints from the current mode map."
  (let ((ai-code-editor-viewport-mode-map
         (copy-keymap ai-code-editor-viewport-mode-map)))
    (define-key ai-code-editor-viewport-mode-map (kbd "C-c C-c") nil)
    (define-key ai-code-editor-viewport-mode-map
                (kbd "C-c s") #'ai-code-editor-viewport-finish)
    (with-temp-buffer
      (ai-code-editor-viewport-mode 1)
      (should (string-match-p "C-c s: submit" header-line-format))
      (should-not (string-match-p "C-c C-c: submit" header-line-format)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-finish-saves-file ()
  "Finishing a viewport edit should save the file and report success."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-source*")
    (let* ((directory (make-temp-file "ai-code-editor-viewport-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (goto-char (point-max))
                         (insert " changed")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should-not (get-file-buffer file))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original changed"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-preserves-existing-file-buffer ()
  "Editing should not kill a file buffer that was already visiting the file."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-existing-source*")
    (let* ((directory (make-temp-file "ai-code-editor-existing-buffer-" t))
           (file (expand-file-name "prompt.md" directory))
           existing-buffer)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (setq existing-buffer (find-file-noselect file))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should (buffer-live-p existing-buffer))
            (should (eq (get-file-buffer file) existing-buffer)))
        (when (buffer-live-p existing-buffer)
          (kill-buffer existing-buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-submit-separates-image-and-text ()
  "Submitting should separate an image reference from adjacent text."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[image spacing]*")
    (let* ((directory (make-temp-file "ai-code-editor-image-spacing-" t))
           (file (expand-file-name "prompt.md" directory))
           (image-file (expand-file-name "photo.png" directory))
           (second-image-file (expand-file-name "diagram.png" directory))
           (preview '(image :type png :data "preview")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" directory))
            (with-temp-file file)
            (with-temp-file image-file
              (insert "png"))
            (with-temp-file second-image-file
              (insert "png"))
            (with-current-buffer source-buffer
              (setq-local default-directory directory))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'gui-get-selection)
                       (lambda (_selection target)
                         (pcase target
                           ('TARGETS '(text/uri-list))
                           ('text/uri-list
                            (mapconcat
                             (lambda (path) (concat "file://" path))
                             (list image-file second-image-file)
                             "\n")))))
                      ((symbol-function 'display-images-p)
                       (lambda (&rest _args) t))
                      ((symbol-function 'image-supported-file-p)
                       (lambda (_file) t))
                      ((symbol-function 'create-image)
                       (lambda (&rest _args) preview))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (call-interactively (key-binding (kbd "C-y")))
                         (goto-char (point-min))
                         (search-forward "\n\n")
                         (delete-region (match-beginning 0) (match-end 0))
                         (goto-char (point-max))
                         (insert "after")
                         (goto-char (point-min))
                         (insert "before")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string)
                             "before @photo.png @diagram.png after"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-names-buffer-for-session ()
  "A viewport should identify its source session without a temporary filename."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[sloth:main]*")
    (let* ((directory (make-temp-file "ai-code-editor-buffer-name-" t))
           (file (expand-file-name ".tmpABC123.md" directory))
           viewport-name)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq viewport-name (buffer-name))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should (equal viewport-name "Edit: codex[sloth:main]")))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(defun ai-code-editor-viewport-test--temporary-file-recorded-in-recentf-p
    (finish-p staging-request-p &optional project-file-p)
  "Return non-nil when an editor file enters `recentf'.
When FINISH-P is non-nil, finish and save the edit; otherwise cancel it.
STAGING-REQUEST-P is the request's explicit staging marker.  When
PROJECT-FILE-P is non-nil, place the file outside the configured temporary
directory."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-recentf-source*")
    (let* ((root (make-temp-file "ai-code-editor-recentf-" t))
           (staging-directory (expand-file-name "staging/" root))
           (project-directory (expand-file-name "project/" root))
           (_ (make-directory staging-directory))
           (_ (make-directory project-directory))
           (temporary-file-directory
            (file-name-as-directory staging-directory))
           (directory (if project-file-p
                          project-directory
                        staging-directory))
           (file (expand-file-name ".tmpABC123.md" directory))
           (recentf-list nil)
           (find-file-hook
            (cons #'recentf-track-opened-file find-file-hook))
           (write-file-functions
            (if finish-p
                (cons #'recentf-track-opened-file write-file-functions)
              write-file-functions)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (if finish-p
                             (ai-code-editor-viewport-finish)
                           (ai-code-editor-viewport-test--confirm-cancel))))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (eq (not (null
                         (ai-code-editor-viewport--edit-files
                          source-buffer directory (list file)
                          nil staging-request-p)))
                   finish-p)))
            (member (file-truename file)
                    (mapcar #'file-truename recentf-list)))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory root t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-excludes-temporary-file-from-recentf ()
  "Canceling a temporary editor file should not record it in `recentf'."
  (should-not
   (ai-code-editor-viewport-test--temporary-file-recorded-in-recentf-p
    nil t)))

(ert-deftest test-ai-code-editor-viewport--edit-files-keeps-saved-temporary-file-out-of-recentf ()
  "Saving a temporary editor file should not record it in `recentf'."
  (should-not
   (ai-code-editor-viewport-test--temporary-file-recorded-in-recentf-p
    t t)))

(ert-deftest test-ai-code-editor-viewport--edit-files-records-normal-file-in-recentf ()
  "A normal file opened through the viewport should still enter `recentf'."
  (should
   (ai-code-editor-viewport-test--temporary-file-recorded-in-recentf-p
    nil nil)))

(ert-deftest test-ai-code-editor-viewport--staging-request-records-project-file ()
  "A project file should enter `recentf' even for a staging request."
  (should
   (ai-code-editor-viewport-test--temporary-file-recorded-in-recentf-p
    nil t t)))

(ert-deftest test-ai-code-editor-viewport--without-recentf-file-preserves-bindings ()
  "The recentf guard should preserve caller bindings and exclusions."
  (let* ((excluded-file 'caller-value)
         (original-exclusions (list "keep-this-exclusion"))
         (recentf-exclude original-exclusions)
         exclusions-inside)
    (should
     (eq (ai-code-editor-viewport--without-recentf-file "ignored-file"
           (setq exclusions-inside recentf-exclude)
           excluded-file)
         'caller-value))
    (should (eq (cdr exclusions-inside) original-exclusions))
    (should (eq recentf-exclude original-exclusions))))

(ert-deftest test-ai-code-editor-viewport--edit-files-disables-source-input ()
  "An active viewport should replace the source input only visually."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[disabled source]*")
    (let* ((directory (make-temp-file "ai-code-editor-disabled-" t))
           (file (expand-file-name "prompt.md" directory))
           input-position
           source-text)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (with-current-buffer source-buffer
              (insert "history\n› Summarize recent commits"
                      (make-string 80 ?\s)
                      "\n")
              (goto-char (point-min))
              (search-forward "Summarize")
              (setq input-position (match-beginning 0)
                    source-text (buffer-string))
              (goto-char input-position))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (with-current-buffer source-buffer
                           (let ((display
                                  (get-char-property
                                   input-position 'display)))
                             (should
                              (string-prefix-p
                               (concat
                                " Editing in viewport below —"
                                " C-c C-c: submit,"
                                " C-g/C-c C-k: cancel")
                               (substring-no-properties display)))
                             (should (= (string-width display) 105)))
                           (should (equal (buffer-string) source-text)))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (with-current-buffer source-buffer
              (should-not (get-char-property input-position 'display))
              (should (equal (buffer-string) source-text))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-uses-container-face ()
  "The disabled-input hint should derive any prompt's container background."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*eat[styled source]*")
    (let ((container-face '(:background "gray20"))
          (input-face '(:background "dark green")))
      (insert (propertize "λ" 'face container-face)
              (propertize " prompt" 'face input-face))
      (goto-char (+ (point-min) 2))
      (let* ((overlay
              (ai-code-editor-viewport--disable-source-input source-buffer))
             (display (get-char-property (point) 'display)))
        (unwind-protect
            (progn
              (should (= (overlay-start overlay) (1+ (point-min))))
              (should
               (equal (get-text-property 0 'face display)
                      (list 'ai-code-editor-viewport-source-hint-face
                            container-face))))
          (delete-overlay overlay))))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-fills-window ()
  "The source hint background should fill the visible input width."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ghostel[filled source hint]*")
    (let ((container-face '(:background "gray20")))
      (insert (propertize "›" 'face container-face) " prompt")
      (goto-char (+ (point-min) 2))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) (selected-window)))
                ((symbol-function 'window-body-width)
                 (lambda (&rest _args) 100)))
        (let* ((overlay
                (ai-code-editor-viewport--disable-source-input source-buffer))
               (display (get-char-property (point) 'display)))
          (unwind-protect
              (progn
                (should (= (string-width display) 99))
                (should
                 (equal (get-text-property (1- (length display))
                                           'face display)
                        (list 'ai-code-editor-viewport-source-hint-face
                              container-face))))
            (delete-overlay overlay)))))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-describes-replace-window ()
  "The source hint should describe a viewport replacing its source window."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[replace hint]*")
    (insert "› prompt")
    (goto-char (point-min))
    (forward-char 2)
    (let* ((ai-code-editor-viewport-window-placement 'replace)
           (overlay
            (ai-code-editor-viewport--disable-source-input source-buffer))
           (display (get-char-property (point) 'display)))
      (unwind-protect
          (should
           (equal (substring-no-properties display)
                  (concat
                   " Editing in current window —"
                   " C-c C-c: submit, C-g/C-c C-k: cancel")))
        (delete-overlay overlay)))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-describes-side-window ()
  "The source hint should describe a viewport beside its source window."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[side hint]*")
    (insert "› prompt")
    (goto-char (point-min))
    (forward-char 2)
    (let* ((overlay
            (ai-code-editor-viewport--disable-source-input
             source-buffer nil nil 'side))
           (display (get-char-property (point) 'display)))
      (unwind-protect
          (should
           (equal (substring-no-properties display)
                  (concat
                   " Editing in viewport beside —"
                   " C-c C-c: submit, C-g/C-c C-k: cancel")))
        (delete-overlay overlay)))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-uses-current-bindings ()
  "The source hint should reflect the viewport mode's current bindings."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[custom source hint]*")
    (insert "› prompt")
    (goto-char (point-min))
    (forward-char 2)
    (let ((ai-code-editor-viewport-mode-map
           (copy-keymap ai-code-editor-viewport-mode-map)))
      (define-key ai-code-editor-viewport-mode-map (kbd "C-c C-c") nil)
      (define-key ai-code-editor-viewport-mode-map
                  (kbd "C-c s")
                  #'ai-code-editor-viewport-finish)
      (let* ((overlay
              (ai-code-editor-viewport--disable-source-input source-buffer))
             (display (get-char-property (point) 'display)))
        (unwind-protect
            (progn
              (should
               (string-match-p "C-c s: submit"
                               (substring-no-properties display)))
              (should-not
               (string-match-p "C-c C-c: submit"
                               (substring-no-properties display))))
          (delete-overlay overlay))))))

(ert-deftest test-ai-code-editor-viewport--edit-files-cancel-rolls-back-file ()
  "Canceling a viewport edit should roll back changes and report cancellation."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-cancel-source*")
    (let* ((directory (make-temp-file "ai-code-editor-viewport-cancel-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (erase-buffer)
                         (insert "discard me")
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-preserves-arguments ()
  "A terminal request should decode its status, directory, and arguments."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[path with spaces]*")
    (let* ((status-file (make-temp-file "ai-code-editor-status-"))
           (directory "/tmp/project with spaces/")
           (arguments '("+12:3" "draft's prompt.md"))
           (fields
            (append
             (list status-file directory "1"
                   ai-code-editor-viewport--request-version "staging")
             arguments))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           (origin-frame (selected-frame))
           captured)
      (unwind-protect
          (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                     (lambda (source seen-directory seen-arguments
                              &optional seen-frame staging-request-p)
                       (setq captured
                             (list source seen-directory seen-arguments
                                   seen-frame staging-request-p))
                       t)))
            (should (ai-code-editor-viewport--open-request
                     source-buffer payload origin-frame))
            (should (equal captured
                           (list source-buffer directory arguments
                                 origin-frame t)))
            (with-temp-buffer
              (insert-file-contents status-file)
              (should (equal (buffer-string) "0\n"))))
        (when (file-exists-p status-file)
          (delete-file status-file))))))

(ert-deftest test-ai-code-editor-viewport--decode-request-accepts-legacy-staging-name ()
  "An old payload may use `staging' as an ordinary first file name."
  (let* ((fields '("/tmp/status" "/tmp/project" "0" "staging"))
         (payload
          (base64-encode-string
           (concat (mapconcat #'identity fields "\0") "\0") t))
         (request (ai-code-editor-viewport--decode-request payload)))
    (should-not (plist-get request :staging-request-p))
    (should (equal (plist-get request :arguments) '("staging")))))

(ert-deftest test-ai-code-editor-viewport--decode-request-preserves-legacy-staging ()
  "An old submitting helper should retain staging-file hygiene."
  (let* ((fields '("/tmp/status" "/tmp/project" "1" "prompt.md"))
         (payload
          (base64-encode-string
           (concat (mapconcat #'identity fields "\0") "\0") t))
         (request (ai-code-editor-viewport--decode-request payload)))
    (should (plist-get request :staging-request-p))
    (should (equal (plist-get request :arguments) '("prompt.md")))))

(ert-deftest test-ai-code-editor-viewport--decode-request-rejects-invalid-kind ()
  "A versioned payload should reject an unknown request kind."
  (let* ((fields
          (list "/tmp/status" "/tmp/project" "0"
                ai-code-editor-viewport--request-version "unknown"
                "prompt.md"))
         (payload
          (base64-encode-string
           (concat (mapconcat #'identity fields "\0") "\0") t)))
    (should-error
     (ai-code-editor-viewport--decode-request payload)
     :type 'error)))

(ert-deftest test-ai-code-editor-viewport--open-request-finish-requests-submit ()
  "Finishing should tell the helper to submit after releasing the terminal."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[submit editor]*")
    (let* ((directory (make-temp-file "ai-code-editor-submit-" t))
           (status-file (make-temp-file "ai-code-editor-status-"))
           (file (expand-file-name "prompt.md" directory))
           (fields (list status-file directory "1" file))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (goto-char (point-max))
                         (insert " ready")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--open-request
                source-buffer payload)))
            (with-temp-buffer
              (insert-file-contents status-file)
              (let ((fields (split-string (string-trim (buffer-string)))))
                (should (equal (seq-take fields 2) '("0" "1")))
                (should (= (length (nth 2 fields)) 64))
                (with-current-buffer source-buffer
                  (should
                   (equal (nth 2 fields)
                          ai-code-editor-viewport--pending-submit-token))))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (when (file-exists-p status-file)
          (delete-file status-file))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-save-only-skips-submit ()
  "A save-only editor request should not submit restored terminal input."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[git editor]*")
    (let* ((directory (make-temp-file "ai-code-editor-general-" t))
           (status-file (make-temp-file "ai-code-editor-status-"))
           (file (expand-file-name "COMMIT_EDITMSG" directory))
           (fields (list status-file directory "0" file))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           scheduled)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "Commit message"))
            (with-current-buffer source-buffer
              (setq-local ai-code-editor-viewport--submit-function #'ignore))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                       (lambda (&rest _args) t))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _arguments)
                         (setq scheduled t))))
              (should
               (ai-code-editor-viewport--open-request source-buffer payload))
              (should-not scheduled)))
        (when (file-exists-p status-file)
          (delete-file status-file))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-blank-skips-submit ()
  "Finishing with only whitespace should not submit the restored input."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[blank editor]*")
    (let* ((directory (make-temp-file "ai-code-editor-blank-" t))
           (status-file (make-temp-file "ai-code-editor-status-"))
           (file (expand-file-name "prompt.md" directory))
           (fields (list status-file directory "1" file))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           (ai-code-editor-viewport-submit-delay 0)
           scheduled
           submitted)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (with-current-buffer source-buffer
              (setq-local ai-code-editor-viewport--submit-function
                          (lambda () (setq submitted t))))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (erase-buffer)
                         (insert " \n\t ")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _arguments)
                         (setq scheduled t)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--open-request
                source-buffer payload)))
            (should-not scheduled)
            (should-not submitted)
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) " \n\t ")))
            (with-temp-buffer
              (insert-file-contents status-file)
              (should (equal (buffer-string) "0\n"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (when (file-exists-p status-file)
          (delete-file status-file))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-cancel-returns-cleanly ()
  "Canceling should restore the TUI without saving or submitting input."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[cancel editor]*")
    (let* ((status-file (make-temp-file "ai-code-editor-status-"))
           (fields (list status-file default-directory "1" "prompt.md"))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           (ai-code-editor-viewport-submit-delay 0)
           scheduled
           submitted)
      (unwind-protect
          (progn
            (with-current-buffer source-buffer
              (setq-local ai-code-editor-viewport--submit-function
                          (lambda () (setq submitted t))))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _arguments)
                         (setq scheduled t))))
              (should-not
               (ai-code-editor-viewport--open-request source-buffer payload))
              (should-not scheduled)
              (should-not submitted)
              (with-temp-buffer
                (insert-file-contents status-file)
                (should (equal (buffer-string) "0\n")))))
        (when (file-exists-p status-file)
          (delete-file status-file))))))

(ert-deftest test-ai-code-editor-viewport--open-request-error-reports-failure ()
  "A real viewport error should still make the editor helper fail."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[broken editor]*")
    (let* ((status-file (make-temp-file "ai-code-editor-status-"))
           (fields (list status-file default-directory "1" "prompt.md"))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t)))
      (unwind-protect
          (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                     (lambda (&rest _args)
                       (error "Broken editor"))))
            (should-not
             (ai-code-editor-viewport--open-request source-buffer payload))
            (with-temp-buffer
              (insert-file-contents status-file)
              (should (equal (buffer-string) "1\n"))))
        (when (file-exists-p status-file)
          (delete-file status-file))))))

(ert-deftest test-ai-code-editor-viewport--edit-files-positions-point ()
  "An editor-style +LINE:COLUMN argument should position the viewport."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-position-source*")
    (let* ((directory (make-temp-file "ai-code-editor-position-" t))
           (file (expand-file-name "prompt.md" directory))
           seen-position)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "first\nsecond line\n"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq seen-position
                               (list (line-number-at-pos) (current-column)))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list "+2:3" "prompt.md"))))
            (should (equal seen-position '(2 3))))
        (dolist (visited-file (list file (expand-file-name "+2:3" directory)))
          (when-let* ((buffer (get-file-buffer visited-file)))
            (set-buffer-modified-p nil)
            (kill-buffer buffer)))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--valid-status-file-p-is-scoped ()
  "Only regular files in the helper status directory should be valid."
  (let* ((directory (make-temp-file "ai-code-editor-status-dir-" t))
         (other-directory (make-temp-file "ai-code-editor-other-dir-" t))
         (temporary-file-directory directory)
         (ai-code-editor-viewport--helper-status-directory directory)
         (valid-file (make-temp-file "ai-code-editor-status-"))
         (wrong-name (make-temp-file "unrelated-status-"))
         (outside-file
          (make-temp-file
           (expand-file-name "ai-code-editor-status-" other-directory))))
    (unwind-protect
        (progn
          (should
           (ai-code-editor-viewport--valid-status-file-p valid-file))
          (should-not
           (ai-code-editor-viewport--valid-status-file-p wrong-name))
          (should-not
           (ai-code-editor-viewport--valid-status-file-p outside-file)))
      (delete-directory directory t)
      (delete-directory other-directory t))))

(ert-deftest test-ai-code-editor-viewport--status-file-survives-temp-dir-change ()
  "A helper status file should remain valid when the active temp dir changes."
  (let* ((helper-directory
          (make-temp-file "ai-code-editor-helper-dir-" t))
         (other-directory
          (make-temp-file "ai-code-editor-other-dir-" t))
         (ai-code-editor-viewport--helper-file nil)
         (ai-code-editor-viewport--helper-status-directory nil)
         helper-file
         status-file)
    (unwind-protect
        (progn
          (let ((temporary-file-directory helper-directory))
            (setq helper-file (ai-code-editor-viewport--ensure-helper))
            (setq status-file (make-temp-file "ai-code-editor-status-")))
          (let ((temporary-file-directory other-directory))
            (should (equal (ai-code-editor-viewport--ensure-helper)
                           helper-file))
            (ai-code-editor-viewport--write-status status-file 0))
          (with-temp-buffer
            (insert-file-contents status-file)
            (should (equal (buffer-string) "0\n"))))
      (ai-code-editor-viewport--cleanup-helper)
      (delete-directory helper-directory t)
      (delete-directory other-directory t))))

(ert-deftest test-ai-code-editor-viewport--try-display-rejects-reused-side-window ()
  "Side placement should create a window instead of reusing a neighbor."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-reuse-source*")
    (ai-code-editor-viewport-test--with-buffer
        (neighbor-buffer "*ai-code-editor-reuse-neighbor*")
      (ai-code-editor-viewport-test--with-buffer
          (viewport-buffer "*ai-code-editor-reuse-viewport*")
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source-buffer)
          (let* ((anchor-window (selected-window))
                 (neighbor-window (split-window-right)))
            (set-window-buffer neighbor-window neighbor-buffer)
            (should-not
             (ai-code-editor-viewport--try-display
              viewport-buffer anchor-window
              (lambda (buffer _anchor)
                (set-window-buffer neighbor-window buffer)
                neighbor-window)
              'side #'ai-code-editor-viewport--window-right-p))
            (should (window-live-p neighbor-window))
            (should (eq (window-buffer neighbor-window) neighbor-buffer))
            (should-not (get-buffer-window viewport-buffer t))))))))

(ert-deftest test-ai-code-editor-viewport--try-display-rejects-unbalanced-side-window ()
  "Side placement should reject a newly split but unbalanced layout."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-unbalanced-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-unbalanced-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let* ((anchor-window (selected-window))
               (minimum-width ai-code-editor-viewport-min-width))
          (should (>= (window-total-width anchor-window)
                      (+ (* 2 minimum-width) 2)))
          (should-not
           (ai-code-editor-viewport--try-display
            viewport-buffer anchor-window
            (lambda (buffer anchor)
              (ai-code-editor-viewport--split-normal-window
               buffer anchor minimum-width 'right))
            'side #'ai-code-editor-viewport--window-right-p))
          (should (= (length (window-list)) 1))
          (should (eq (window-buffer anchor-window) source-buffer))
          (should-not (get-buffer-window viewport-buffer t)))))))

(ert-deftest test-ai-code-editor-viewport--display-defaults-below-source-window ()
  "The default setup should prefer a viewport below the source window."
  (should (eq (default-value 'ai-code-editor-viewport-window-placement)
              'below))
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-below-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-below-viewport*")
      (ai-code-editor-viewport-test--with-buffer
          (other-buffer "*ai-code-editor-below-other*")
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source-buffer)
          (let* ((source-window (selected-window))
                 (other-window (split-window-right))
                 (source-height (window-total-height source-window)))
            (set-window-buffer other-window other-buffer)
            (let* ((ai-code-editor-viewport-window-height
                    (/ source-height 2))
                   (display-state
                    (ai-code-editor-viewport--display
                     viewport-buffer source-buffer))
                   (viewport-window (plist-get display-state :window)))
              (should (window-live-p viewport-window))
              (should-not (eq viewport-window source-window))
              (should (eq (plist-get display-state :placement) 'below))
              (should (eq (window-buffer source-window) source-buffer))
              (should (eq (window-buffer viewport-window) viewport-buffer))
              (should (>= (window-total-height source-window)
                          ai-code-editor-viewport-min-height))
              (should (>= (window-total-height viewport-window)
                          ai-code-editor-viewport-min-height))
              (should (= (window-total-height viewport-window)
                         ai-code-editor-viewport-window-height))
              (should (>= (nth 1 (window-edges viewport-window))
                          (nth 3 (window-edges source-window))))
              (should (= (length (window-list)) 3))
              (ai-code-editor-viewport--restore-window
               display-state viewport-buffer)
              (should (= (length (window-list)) 2))
              (should (eq (window-buffer source-window) source-buffer))
              (should (eq (window-buffer other-window) other-buffer))
              (should (eq (selected-window) source-window))
              (should-not (get-buffer-window viewport-buffer t)))))))))

(ert-deftest test-ai-code-editor-viewport--display-defaults-to-usable-dimensions ()
  "Viewport placement should default to usable minimum dimensions."
  (should (= (default-value 'ai-code-editor-viewport-window-height) 12))
  (should (= (* 2 (default-value 'ai-code-editor-viewport-window-height)) 24))
  (should (= (default-value 'ai-code-editor-viewport-min-height) 8))
  (should (= (default-value 'ai-code-editor-viewport-min-width) 24)))

(ert-deftest test-ai-code-editor-viewport--display-accepts-eight-line-minimum ()
  "Below placement should accept eight lines but reject seven."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-min-height-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-min-height-viewport*")
      (dolist (height '(7 8))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source-buffer)
          (let* ((source-height (window-total-height))
                 (ai-code-editor-viewport-window-height
                  (/ source-height 2))
                 (ai-code-editor-viewport-min-width 20)
                 display-state)
            (cl-letf
                (((symbol-function
                   'ai-code-editor-viewport--display-below-action)
                  (lambda (buffer anchor-window)
                    (let ((window
                           (split-window anchor-window (- height) 'below)))
                      (set-window-buffer window buffer)
                      window))))
              (setq display-state
                    (ai-code-editor-viewport--display
                     viewport-buffer source-buffer)))
            (if (= height 8)
                (should (eq (plist-get display-state :placement) 'below))
              (should-not (eq (plist-get display-state :placement) 'below)))
            (ai-code-editor-viewport--restore-window
             display-state viewport-buffer)))))))

(ert-deftest test-ai-code-editor-viewport--display-uses-side-for-short-window ()
  "A ten-line source should preserve its height with a side viewport."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-short-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-short-viewport*")
      (ai-code-editor-viewport-test--with-buffer
          (other-buffer "*ai-code-editor-short-other*")
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source-buffer)
          (let* ((source-window (selected-window))
                 (other-window (split-window source-window 10 'below)))
            (set-window-buffer other-window other-buffer)
            (should (= (window-total-height source-window) 10))
            (let* ((display-state
                    (ai-code-editor-viewport--display
                     viewport-buffer source-buffer))
                   (viewport-window (plist-get display-state :window)))
              (should (eq (plist-get display-state :placement) 'side))
              (should (>= (window-total-width source-window) 24))
              (should (>= (window-total-width viewport-window) 24))
              (should (<= (abs (- (window-total-width source-window)
                                  (window-total-width viewport-window)))
                          1))
              (should (= (window-total-height source-window) 10))
              (should (= (window-total-height viewport-window) 10))
              (ai-code-editor-viewport--restore-window
               display-state viewport-buffer)
              (should (window-live-p source-window))
              (should (window-live-p other-window))
              (should-not (get-buffer-window viewport-buffer t)))))))))

(ert-deftest test-ai-code-editor-viewport--display-replaces-narrow-source ()
  "Side placement should not squeeze a narrow source to make the viewport fit."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-narrow-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-narrow-viewport*")
      (ai-code-editor-viewport-test--with-buffer
          (other-buffer "*ai-code-editor-narrow-other*")
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source-buffer)
          (let* ((source-window (selected-window))
                 (right-window (split-window source-window 33 'right))
                 (bottom-window (split-window source-window 10 'below)))
            (set-window-buffer right-window other-buffer)
            (set-window-buffer bottom-window other-buffer)
            (let* ((display-state
                    (ai-code-editor-viewport--display
                     viewport-buffer source-buffer))
                   (viewport-window (plist-get display-state :window)))
              (should (eq (plist-get display-state :placement) 'replace))
              (should (eq viewport-window source-window))
              (should (= (window-total-width source-window) 33))
              (should (eq (window-buffer source-window) viewport-buffer))
              (should (eq (window-buffer right-window) other-buffer))
              (should (eq (window-buffer bottom-window) other-buffer))
              (ai-code-editor-viewport--restore-window
               display-state viewport-buffer)
              (should (eq (window-buffer source-window)
                          source-buffer)))))))))

(ert-deftest test-ai-code-editor-viewport--display-replaces-narrow-lateral-source ()
  "Side placement should reject a usable viewport beside a narrow side source."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-narrow-side-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-narrow-side-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (let* ((ai-code-editor-viewport-window-height 13)
               (fixture
                (ai-code-editor-viewport-test--display-from-lateral-side
                 'right source-buffer viewport-buffer 20))
               (source-window (plist-get fixture :source-window))
               (source-height (plist-get fixture :source-height))
               (main-window (plist-get fixture :main-window))
               (main-buffer (plist-get fixture :main-buffer))
               (display-state (plist-get fixture :display-state))
               (viewport-window (plist-get fixture :viewport-window)))
          (should (< source-height
                     (* 2 ai-code-editor-viewport-window-height)))
          (should (= (window-total-width source-window) 20))
          (should (eq (plist-get display-state :placement) 'replace))
          (should (eq viewport-window source-window))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (should (eq (window-buffer source-window) source-buffer))
          (should (eq (window-parameter source-window 'window-side) 'right))
          (should (window-live-p main-window))
          (should (eq (window-buffer main-window) main-buffer)))))))

(ert-deftest test-ai-code-editor-viewport--display-side-from-wide-lateral-side-uses-normal-window ()
  "A short wide lateral session should use a restorable normal side window."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-wide-lateral-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-wide-lateral-viewport*")
      (dolist (side '(left right))
        (save-window-excursion
          (delete-other-windows)
          (let* ((ai-code-editor-viewport-window-height 13)
                 (fixture
                  (ai-code-editor-viewport-test--display-from-lateral-side
                   side source-buffer viewport-buffer 55))
                 (source-window (plist-get fixture :source-window))
                 (source-height (plist-get fixture :source-height))
                 (source-width (plist-get fixture :source-width))
                 (main-window (plist-get fixture :main-window))
                 (main-buffer (plist-get fixture :main-buffer))
                 (display-state (plist-get fixture :display-state))
                 (viewport-window (plist-get fixture :viewport-window)))
            (should (< source-height
                       (* 2 ai-code-editor-viewport-window-height)))
            (should (>= source-width
                        (* 2 ai-code-editor-viewport-min-width)))
            (should (eq (plist-get display-state :placement) 'side))
            (should (window-live-p viewport-window))
            (should-not (eq viewport-window source-window))
            (should (eq (window-parameter source-window 'window-side) side))
            (should-not (window-parameter viewport-window 'window-side))
            (should (>= (window-total-width source-window)
                        ai-code-editor-viewport-min-width))
            (should (>= (window-total-width viewport-window)
                        ai-code-editor-viewport-min-width))
            (should (<= (abs (- (window-total-width source-window)
                                (window-total-width viewport-window)))
                        1))
            (should (>= (nth 0 (window-edges viewport-window))
                        (nth 2 (window-edges source-window))))
            (window--check)
            (ai-code-editor-viewport-test--transpose-parent-if-supported
             viewport-window)
            (window--check)
            (ai-code-editor-viewport--restore-window
             display-state viewport-buffer)
            (window--check)
            (should (window-live-p source-window))
            (should (eq (window-buffer source-window) source-buffer))
            (should (eq (window-parameter source-window 'window-side) side))
            (should (window-live-p main-window))
            (should (eq (window-buffer main-window) main-buffer))
            (should-not (get-buffer-window viewport-buffer t))))))))

(ert-deftest test-ai-code-editor-viewport--display-uses-right-when-below-unavailable ()
  "A viewport should use the right side when below placement is unavailable."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-right-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-right-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let* ((ai-code-editor-viewport-window-placement 'below)
               (source-window (selected-window))
               display-state)
          (cl-letf
              (((symbol-function
                 'ai-code-editor-viewport--display-below-action)
                (lambda (&rest _args) nil)))
            (setq display-state
                  (ai-code-editor-viewport--display
                   viewport-buffer source-buffer)))
          (let ((viewport-window (plist-get display-state :window)))
            (should (eq (plist-get display-state :placement) 'side))
            (should (window-live-p viewport-window))
            (should (>= (window-total-width source-window)
                        ai-code-editor-viewport-min-width))
            (should (>= (window-total-width viewport-window)
                        ai-code-editor-viewport-min-width))
            (should (<= (abs (- (window-total-width source-window)
                                (window-total-width viewport-window)))
                        1))
            (should (>= (nth 0 (window-edges viewport-window))
                        (nth 2 (window-edges source-window))))
            (ai-code-editor-viewport--restore-window
             display-state viewport-buffer)
            (should (window-live-p source-window))
            (should-not (get-buffer-window viewport-buffer t))))))))

(ert-deftest test-ai-code-editor-viewport--display-uses-below-for-tall-lateral-window ()
  "A tall lateral side session should display its viewport below."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-side-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-side-viewport*")
      (dolist (side '(left right))
        (save-window-excursion
          (delete-other-windows)
          (let* ((ai-code-editor-viewport-window-height 11)
                 (fixture
                  (ai-code-editor-viewport-test--display-from-lateral-side
                   side source-buffer viewport-buffer))
                 (source-window (plist-get fixture :source-window))
                 (source-height (plist-get fixture :source-height))
                 (main-window (plist-get fixture :main-window))
                 (main-buffer (plist-get fixture :main-buffer))
                 (display-state (plist-get fixture :display-state))
                 (viewport-window (plist-get fixture :viewport-window)))
            (should (>= source-height
                        (* 2 ai-code-editor-viewport-window-height)))
            (should (window-live-p viewport-window))
            (should-not (eq viewport-window source-window))
            (should (eq (plist-get display-state :placement) 'below))
            (should (eq (window-buffer source-window) source-buffer))
            (should (eq (window-buffer viewport-window) viewport-buffer))
            (should (>= (window-total-height source-window)
                        ai-code-editor-viewport-min-height))
            (should (>= (window-total-height viewport-window)
                        ai-code-editor-viewport-min-height))
            (should (= (window-total-height viewport-window)
                       ai-code-editor-viewport-window-height))
            (should (= (nth 0 (window-edges viewport-window))
                       (nth 0 (window-edges source-window))))
            (should (= (nth 2 (window-edges viewport-window))
                       (nth 2 (window-edges source-window))))
            (should (>= (nth 1 (window-edges viewport-window))
                        (nth 3 (window-edges source-window))))
            (ai-code-editor-viewport--restore-window
             display-state viewport-buffer)
            (should (window-live-p source-window))
            (should (window-live-p main-window))
            (should (eq (window-buffer main-window) main-buffer))
            (should-not (get-buffer-window viewport-buffer t))))))))

(ert-deftest test-ai-code-editor-viewport--display-replaces-narrow-lateral-window ()
  "A short lateral session should replace when a balanced side cannot fit."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-short-side-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-short-side-viewport*")
      (dolist (side '(left right))
        (save-window-excursion
          (delete-other-windows)
          (let* ((ai-code-editor-viewport-window-height 13)
                 (fixture
                  (ai-code-editor-viewport-test--display-from-lateral-side
                   side source-buffer viewport-buffer))
                 (source-window (plist-get fixture :source-window))
                 (source-height (plist-get fixture :source-height))
                 (source-width (window-total-width source-window))
                 (main-window (plist-get fixture :main-window))
                 (main-buffer (plist-get fixture :main-buffer))
                 (display-state (plist-get fixture :display-state))
                 (viewport-window (plist-get fixture :viewport-window)))
            (should (< source-height
                       (* 2 ai-code-editor-viewport-window-height)))
            (should (< source-width
                       (* 2 ai-code-editor-viewport-min-width)))
            (should (eq (plist-get display-state :placement) 'replace))
            (should (eq viewport-window source-window))
            (should (= (window-total-width source-window) source-width))
            (should (eq (window-buffer source-window) viewport-buffer))
            (ai-code-editor-viewport--restore-window
             display-state viewport-buffer)
            (should (window-live-p source-window))
            (should (eq (window-buffer source-window) source-buffer))
            (should (eq (window-parameter source-window 'window-side) side))
            (should (window-live-p main-window))
            (should (eq (window-buffer main-window) main-buffer))
            (should-not (get-buffer-window viewport-buffer t))))))))

(ert-deftest test-ai-code-editor-viewport--restore-created-sole-window-hides-viewport ()
  "Restoring a sole created window should hide the viewport without error."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-sole-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-sole-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let* ((display-state
                (ai-code-editor-viewport--display
                 viewport-buffer source-buffer))
               (viewport-window (plist-get display-state :window)))
          (let ((ignore-window-parameters t))
            (delete-other-windows viewport-window))
          (should-not (eq (window-deletable-p viewport-window) t))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (should-not (get-buffer-window viewport-buffer t)))))))

(ert-deftest test-ai-code-editor-viewport--display-below-from-side-uses-normal-window ()
  "A below-side layout should use a restorable normal viewport window."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-transpose-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-transpose-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (let* ((ai-code-editor-viewport-window-height 11)
               (fixture
                (ai-code-editor-viewport-test--display-from-lateral-side
                 'right source-buffer viewport-buffer))
               (source-window (plist-get fixture :source-window))
               (main-window (plist-get fixture :main-window))
               (main-buffer (plist-get fixture :main-buffer))
               (display-state (plist-get fixture :display-state))
               (viewport-window (plist-get fixture :viewport-window)))
          (should (= (length (window-list)) 3))
          (window--check)
          (should (eq (window-parameter source-window 'window-side) 'right))
          (should-not (window-parameter viewport-window 'window-side))
          (ai-code-editor-viewport-test--transpose-parent-if-supported
           viewport-window)
          (window--check)
          (should (eq (plist-get display-state :placement) 'below))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (window--check)
          (should (= (length (window-list)) 2))
          (should (window-live-p source-window))
          (should (eq (window-parameter source-window 'window-side) 'right))
          (should (window-live-p main-window))
          (should (eq (window-buffer main-window) main-buffer))
          (should-not (get-buffer-window viewport-buffer t)))))))

(ert-deftest test-ai-code-editor-viewport--display-replaces-without-space ()
  "Default placement should replace only after all placement attempts fail."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-no-space-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-no-space-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let ((ai-code-editor-viewport-window-placement 'below)
              (source-window (selected-window))
              display-state)
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _args) nil)))
            (setq display-state
                  (ai-code-editor-viewport--display
                   viewport-buffer source-buffer)))
          (should (eq (plist-get display-state :window) source-window))
          (should (eq (plist-get display-state :placement) 'replace))
          (should (eq (window-buffer source-window) viewport-buffer))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (should (eq (window-buffer source-window) source-buffer)))))))

(ert-deftest test-ai-code-editor-viewport--display-fallback-order ()
  "Default placement should try below, right, left, and replace in order."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-order-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-order-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let ((ai-code-editor-viewport-window-placement 'below)
              (ai-code-editor-viewport-window-height 11)
              attempts)
          (cl-letf
              (((symbol-function
                 'ai-code-editor-viewport--display-below-action)
                (lambda (&rest _args)
                  (push 'below attempts)
                  nil))
               ((symbol-function
                 'ai-code-editor-viewport--display-side-action)
                (lambda (direction &rest _args)
                  (push direction attempts)
                  nil))
               ((symbol-function 'ai-code-editor-viewport--replace-window)
                (lambda (_buffer window)
                  (push 'replace attempts)
                  (list :window window :placement 'replace))))
            (should
             (eq (plist-get
                  (ai-code-editor-viewport--display
                   viewport-buffer source-buffer)
                  :placement)
                 'replace)))
          (should
           (equal (nreverse attempts) '(below right left replace))))))))

(ert-deftest test-ai-code-editor-viewport--display-never-runs-frame-actions ()
  "Viewport placement should never delegate to a frame display action."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-no-frame-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-no-frame-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let ((ai-code-editor-viewport-window-placement 'below)
              (ai-code-editor-viewport-window-height 11)
              (frames-before (frame-list))
              frame-action-called
              display-state)
          (let ((display-buffer-overriding-action
                 (list
                  (list
                   (lambda (&rest _args)
                     (setq frame-action-called t)
                     nil)))))
            (setq display-state
                  (ai-code-editor-viewport--display
                   viewport-buffer source-buffer)))
          (should-not frame-action-called)
          (should (equal (frame-list) frames-before))
          (should (eq (plist-get display-state :placement) 'below)))))))

(ert-deftest test-ai-code-editor-viewport--display-scopes-source-to-selected-frame ()
  "Viewport placement should not look up its source on another frame."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-frame-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-frame-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (let ((ai-code-editor-viewport-window-placement 'replace)
              (origin-frame (selected-frame))
              requested-frame
              display-state)
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (_buffer frame)
                       (setq requested-frame frame)
                       nil)))
            (setq display-state
                  (ai-code-editor-viewport--display
                   viewport-buffer source-buffer)))
          (should (eq requested-frame origin-frame))
          (should (eq (window-frame (plist-get display-state :window))
                      origin-frame))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer))))))

(ert-deftest test-ai-code-editor-viewport--display-uses-dispatch-frame ()
  "Viewport placement should honor a live frame captured at dispatch time."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-captured-frame-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-captured-frame-viewport*")
      (let ((ai-code-editor-viewport-window-placement 'replace)
            (origin-frame 'origin-frame)
            (later-frame 'later-frame)
            (anchor-window 'origin-window)
            requested-frame)
        (cl-letf (((symbol-function 'frame-live-p)
                   (lambda (frame) (eq frame origin-frame)))
                  ((symbol-function 'selected-frame)
                   (lambda () later-frame))
                  ((symbol-function 'get-buffer-window)
                   (lambda (_buffer frame)
                     (setq requested-frame frame)
                     nil))
                  ((symbol-function 'frame-selected-window)
                   (lambda (frame)
                     (should (eq frame origin-frame))
                     anchor-window))
                  ((symbol-function 'ai-code-editor-viewport--replace-window)
                   (lambda (_buffer window)
                     (list :window window :placement 'replace))))
          (should
           (eq (plist-get
                (ai-code-editor-viewport--display
                 viewport-buffer source-buffer origin-frame)
                :window)
               anchor-window))
          (should (eq requested-frame origin-frame)))))))

(ert-deftest test-ai-code-editor-viewport--display-falls-back-from-deleted-frame ()
  "A deleted dispatch frame should fall back to the selected frame."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-deleted-frame-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-deleted-frame-viewport*")
      (let ((ai-code-editor-viewport-window-placement 'replace)
            (deleted-frame 'deleted-frame)
            (current-frame 'current-frame)
            (anchor-window 'current-window)
            requested-frame)
        (cl-letf (((symbol-function 'frame-live-p) #'ignore)
                  ((symbol-function 'selected-frame)
                   (lambda () current-frame))
                  ((symbol-function 'get-buffer-window)
                   (lambda (_buffer frame)
                     (setq requested-frame frame)
                     nil))
                  ((symbol-function 'frame-selected-window)
                   (lambda (frame)
                     (should (eq frame current-frame))
                     anchor-window))
                  ((symbol-function 'ai-code-editor-viewport--replace-window)
                   (lambda (_buffer window)
                     (list :window window :placement 'replace))))
          (should
           (eq (plist-get
                (ai-code-editor-viewport--display
                 viewport-buffer source-buffer deleted-frame)
                :window)
               anchor-window))
          (should (eq requested-frame current-frame)))))))

(ert-deftest test-ai-code-editor-viewport--display-ignores-external-action-alists ()
  "Viewport placement should ignore external display action constraints."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-no-alist-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-no-alist-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let ((ai-code-editor-viewport-window-placement 'below)
              (ai-code-editor-viewport-window-height 11)
              (display-buffer-alist
               `((,(regexp-quote (buffer-name viewport-buffer))
                  (window-min-height . 1000))))
              (display-buffer-base-action
               '((display-buffer-reuse-window)
                 (window-min-height . 1000)))
              (display-buffer-fallback-action
               '((display-buffer-pop-up-frame)
                 (window-min-height . 1000))))
          (let ((display-state
                 (ai-code-editor-viewport--display
                  viewport-buffer source-buffer)))
            (should (eq (plist-get display-state :placement) 'below))
            (ai-code-editor-viewport--restore-window
             display-state viewport-buffer)))))))

(ert-deftest test-ai-code-editor-viewport--display-side-from-horizontal-side-uses-normal-window ()
  "A short top or bottom session should use a restorable normal side window."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-horizontal-side-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-horizontal-side-viewport*")
      (dolist (side '(top bottom))
        (save-window-excursion
          (delete-other-windows)
          (let* ((ai-code-editor-viewport-window-placement 'below)
                 (window-sides-slots '(nil nil nil nil))
                 (source-window
                  (display-buffer-in-side-window
                   source-buffer
                   `((side . ,side)
                     (slot . 0)
                     (window-height . 8)
                     (preserve-size . (nil . t))
                     (window-parameters
                      . ((no-delete-other-windows . t)
                         (window-size-fixed . height))))))
                 (main-window (window-main-window))
                 (main-buffer (window-buffer main-window))
                 (display-state
                  (ai-code-editor-viewport--display
                   viewport-buffer source-buffer))
                 (viewport-window (plist-get display-state :window)))
            (should (eq (plist-get display-state :placement) 'side))
            (should (window-live-p viewport-window))
            (should-not (eq viewport-window source-window))
            (should (eq (window-parameter source-window 'window-side) side))
            (should-not (window-parameter viewport-window 'window-side))
            (should (>= (window-total-width source-window)
                        ai-code-editor-viewport-min-width))
            (should (>= (window-total-width viewport-window)
                        ai-code-editor-viewport-min-width))
            (should (>= (nth 0 (window-edges viewport-window))
                        (nth 2 (window-edges source-window))))
            (window--check)
            (ai-code-editor-viewport-test--transpose-parent-if-supported
             viewport-window)
            (window--check)
            (ai-code-editor-viewport--restore-window
             display-state viewport-buffer)
            (window--check)
            (should (window-live-p source-window))
            (should (eq (window-buffer source-window) source-buffer))
            (should (eq (window-parameter source-window 'window-side) side))
            (should (window-live-p main-window))
            (should (eq (window-buffer main-window) main-buffer))
            (should-not (get-buffer-window viewport-buffer t))))))))

(ert-deftest test-ai-code-editor-viewport--display-replace-restores-hidden-source-window ()
  "Replacement placement should restore the user's previous window buffer."
  (ai-code-editor-viewport-test--with-buffer
      (user-buffer "*ai-code-editor-user*")
    (ai-code-editor-viewport-test--with-buffer
        (source-buffer "*ai-code-editor-hidden-source*")
      (ai-code-editor-viewport-test--with-buffer
          (viewport-buffer "*ai-code-editor-hidden-viewport*")
        (switch-to-buffer user-buffer)
        (let* ((ai-code-editor-viewport-window-placement 'replace)
               (display-state
               (ai-code-editor-viewport--display
                viewport-buffer source-buffer)))
          (should (eq (window-buffer (selected-window)) viewport-buffer))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (should (eq (window-buffer (selected-window)) user-buffer)))))))

(ert-deftest test-ai-code-editor-viewport--edit-file-isolates-request-state ()
  "Nested editor requests for one file should use independent viewport buffers."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-isolation-source*")
    (let* ((directory (make-temp-file "ai-code-editor-isolation-" t))
           (file (expand-file-name "prompt.md" directory))
           first-viewport
           second-viewport
           inner-result
           (depth 0))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq depth (1+ depth))
                         (if (= depth 1)
                             (progn
                               (setq first-viewport (current-buffer))
                               (setq inner-result
                                     (ai-code-editor-viewport--edit-file
                                      file source-buffer))
                               (should ai-code-editor-viewport-mode)
                               (ai-code-editor-viewport-finish))
                           (setq second-viewport (current-buffer))
                           (ai-code-editor-viewport-test--confirm-cancel)))))
              (should (ai-code-editor-viewport--edit-file file source-buffer)))
            (should-not inner-result)
            (should-not (eq first-viewport second-viewport)))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-restores-source-cursor-position ()
  "A viewport should open at the source TUI's cursor within its draft."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[cursor position]*")
    (let* ((directory (make-temp-file "ai-code-editor-cursor-" t))
           (file (expand-file-name "prompt.md" directory))
           viewport-point)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha beta gamma"))
            (with-current-buffer source-buffer
              (insert "history\n› alpha beta gamma" (make-string 20 ?\s))
              (goto-char (point-min))
              (search-forward "alpha "))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq viewport-point (point))
                         (should (looking-at-p "beta"))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should (= viewport-point 7)))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-restores-multiline-cursor-position ()
  "A viewport should map a source cursor within a multiline draft."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[multiline cursor]*")
    (let* ((directory (make-temp-file "ai-code-editor-multiline-cursor-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "first line\nsecond line\nthird"))
            (with-current-buffer source-buffer
              (insert "› first line\n  second line\n  third")
              (goto-char (point-min))
              (search-forward "second "))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 19))
                         (should (looking-at-p "line"))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-disambiguates-repeated-draft-lines ()
  "A viewport should use source context when draft lines repeat."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[repeated draft lines]*")
    (let* ((directory (make-temp-file "ai-code-editor-repeated-lines-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "same\nsame"))
            (with-current-buffer source-buffer
              (insert "› same\n  same")
              (goto-char (point-min))
              (forward-line 1)
              (search-forward "sa"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 8))
                         (should (looking-at-p "me"))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-restores-cursor-on-empty-draft-line ()
  "A viewport should restore a cursor on an empty multiline draft line."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[empty draft line]*")
    (let* ((directory (make-temp-file "ai-code-editor-empty-line-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "first\n\nthird"))
            (with-current-buffer source-buffer
              (insert "› first\n  \n  third")
              (goto-char (point-min))
              (forward-line 1)
              (end-of-line))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 7))
                         (should (looking-at-p "\nthird"))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-uses-source-cursor-function ()
  "A viewport should use its terminal adapter's live cursor function."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[ghostel cursor]*")
    (let* ((directory (make-temp-file "ai-code-editor-ghostel-cursor-" t))
           (file (expand-file-name "prompt.md" directory))
           terminal-cursor)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha beta gamma"))
            (with-current-buffer source-buffer
              (insert "› alpha beta gamma" (make-string 20 ?\s)
                      "\nterminal status")
              (goto-char (point-min))
              (search-forward "alpha ")
              (setq terminal-cursor (point))
              (setq-local ai-code-editor-viewport-source-cursor-function
                          (lambda () terminal-cursor))
              (goto-char (point-max)))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 7))
                         (should (looking-at-p "beta"))
                         (ai-code-editor-viewport-test--confirm-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-file-cancel-does-not-leak-through-nested-save ()
  "Canceling one request should not let another request save its edits."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-transactions-source*")
    (let* ((directory (make-temp-file "ai-code-editor-transactions-" t))
           (file (expand-file-name "prompt.md" directory))
           inner-result
           (depth 0))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq depth (1+ depth))
                         (goto-char (point-max))
                         (if (= depth 1)
                             (progn
                               (insert " OUTER")
                               (setq inner-result
                                     (ai-code-editor-viewport--edit-file
                                      file source-buffer))
                               (ai-code-editor-viewport-test--confirm-cancel))
                           (insert " INNER")
                           (ai-code-editor-viewport-finish)))))
              (should-not
               (ai-code-editor-viewport--edit-file file source-buffer)))
            (should inner-result)
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original INNER"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(provide 'test_ai-code-editor-viewport)
;;; test_ai-code-editor-viewport.el ends here
