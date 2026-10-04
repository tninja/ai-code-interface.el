;;; ai-code-annotate-magit.el --- Persistent annotations on Magit hunks -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Optional Magit integration.  Enable `ai-code-annotate-magit-mode' in a status
;; or diff buffer.  Annotate a hunk or a region with C-c # a, preview all
;; repository notes with C-c # s, and copy the review with C-c # w.
;; Requires annotate.el 2.5.0 or newer (annotation IDs).
;; Notes are saved immediately, independently of Magit's disposable
;; buffer text.  Only exact hunk/context matches are displayed after a
;; refresh.  Unmatched notes remain available in the review report.
;; This module uses annotate's overlay creation and faces, but does not
;; enable its file-oriented save hooks in Magit buffers.

;;; Code:

(require 'cl-lib)

(declare-function annotate-create-annotation "annotate")
(declare-function annotate-annotation-id "annotate")
(declare-function annotate-all-annotations "annotate")
(declare-function annotate-annotation-property-annotation-face "annotate")
(defvar annotate-mode)
(defvar annotate-use-echo-area)
(require 'eieio)
(require 'magit nil t)
(require 'subr-x)

;; Keep Magit optional when package managers compile every bundled .el file.
(declare-function magit-toplevel "magit-git")
(declare-function magit-diff-type "magit-diff" (&optional section))
(declare-function magit-rev-parse "magit-git" (rev &rest args))
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-current-section "magit-section")
(declare-function org-id-uuid "org-id")

(defvar magit-buffer-diff-range)
(defvar magit-root-section)
(defvar ai-code-annotate-magit-mode)

(defgroup ai-code-annotate-magit nil
  "Persistent review notes on Magit diffs."
  :group 'ai-code)

(defcustom ai-code-annotate-magit-file
  (locate-user-emacs-file "annotate-magit-reviews")
  "Database for Magit reviews, separate from source file annotations."
  :type 'file
  :group 'ai-code-annotate-magit)

(defvar ai-code-annotate-magit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <punctuation> is the minor-mode space; C-c C-<letter> belongs to
    ;; Magit's major modes (e.g. C-c C-w is `magit-copy-thing').
    (define-key map (kbd "C-c # a") #'ai-code-annotate-magit-annotate)
    (define-key map (kbd "C-c # d") #'ai-code-annotate-magit-delete)
    (define-key map (kbd "C-c # s") #'ai-code-annotate-magit-review)
    (define-key map (kbd "C-c # w") #'ai-code-annotate-magit-copy-review)
    map)
  "Keymap for `ai-code-annotate-magit-mode'.")

(defvar-local ai-code--annotate-magit-overlays nil)
(defvar-local ai-code--annotate-magit-edit-origin nil)
(defvar-local ai-code--annotate-magit-edit-record nil)

(defun ai-code--annotate-missing (purpose)
  "Return why annotate.el cannot be used to PURPOSE, or nil if it can."
  (cond ((not (require 'annotate nil t))
         (format "Install annotate.el to %s" purpose))
        ;; Annotation IDs and `annotate-create-annotation's ID argument
        ;; first shipped in annotate.el 2.5.0.
        ((not (fboundp 'annotate-id-from-dump))
         (format "Upgrade annotate.el to 2.5.0 or newer to %s" purpose))))

(defun ai-code--annotate-magit-read ()
  "Read saved notes; refuse to overwrite an unreadable database."
  (if (not (file-exists-p ai-code-annotate-magit-file))
      nil
    (with-temp-buffer
      (insert-file-contents ai-code-annotate-magit-file)
      (condition-case err
          (let ((data (read (current-buffer))))
            (unless (and (equal (plist-get data :version) 1)
                         (listp (plist-get data :notes)))
              (error "Unsupported review database"))
            (skip-chars-forward " \t\r\n")
            (unless (eobp) (error "Trailing database content"))
            (plist-get data :notes))
        (error (user-error "Cannot read %s: %s" ai-code-annotate-magit-file
                           (error-message-string err)))))))

(defun ai-code--annotate-magit-write (notes)
  "Atomically save NOTES without modifying annotated source files."
  (let* ((file (expand-file-name ai-code-annotate-magit-file))
         (directory (file-name-directory file))
         temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary (make-temp-file (expand-file-name ".review-" directory)))
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil))
              (prin1 (list :version 1 :notes notes) (current-buffer))
              (insert "\n")))
          (rename-file temporary file t))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun ai-code--annotate-magit-repository ()
  "Return the current worktree root, retaining worktree isolation."
  (file-name-as-directory
   (file-truename (or (magit-toplevel) (user-error "Not in a Git repository")))))

(defun ai-code--annotate-magit-slot (section slot)
  "Read SLOT from SECTION without requiring Magit's classes at compile time."
  (slot-value section slot))

(defun ai-code--annotate-magit-ancestor (section type)
  "Find the ancestor of SECTION with TYPE, including SECTION itself."
  (while (and section (not (eq (ai-code--annotate-magit-slot section 'type) type)))
    (setq section (ai-code--annotate-magit-slot section 'parent)))
  section)

(defun ai-code--annotate-magit-context (section)
  "Return a conservative diff identity for SECTION.
Branch and HEAD are deliberately excluded: staged and unstaged hunks do
not change with unrelated commits or branch switches.  They are kept as
note metadata instead."
  (let ((type (magit-diff-type section)))
    (unless (memq type '(staged unstaged committed))
      (user-error "Only staged, unstaged and committed Git hunks are supported"))
    (list type
          (and (eq type 'committed)
               (or (bound-and-true-p magit-buffer-diff-range)
                   (bound-and-true-p magit-buffer-range)
                   (bound-and-true-p magit-buffer-revision-hash))))))

(defun ai-code--annotate-magit-hunk-data (section)
  "Return snapshot and buffer positions for a regular hunk SECTION."
  (let* ((file (ai-code--annotate-magit-ancestor section 'file))
         (start (marker-position (ai-code--annotate-magit-slot section 'start)))
         (end (marker-position (ai-code--annotate-magit-slot section 'end)))
         (header (save-excursion
                   (goto-char start)
                   (buffer-substring-no-properties start (line-end-position)))))
    (unless (and file (string-match-p
                      "\\`@@ -[0-9]+\\(?:,[0-9]+\\)? +[+]" header))
      (user-error "Select an ordinary two-sided diff hunk (not a merge or metadata hunk)"))
    (list :file (ai-code--annotate-magit-slot file 'value)
          :old-file (or (ai-code--annotate-magit-slot file 'source) (ai-code--annotate-magit-slot file 'value))
          :context (ai-code--annotate-magit-context section)
          :header header
          :hunk (buffer-substring-no-properties start end)
          :start start :end end)))

(defun ai-code--annotate-magit-line-ranges (hunk begin end)
  "Map offsets BEGIN..END in HUNK to old and new source line ranges.
Return (OLD-RANGE NEW-RANGE).  A missing side is nil, never a fictitious
line number for an added or deleted line."
  (with-temp-buffer
    (insert hunk)
    (goto-char (point-min))
    (unless (looking-at "@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +[+]\\([0-9]+\\)")
      (user-error "Unsupported hunk header"))
    (let ((old (string-to-number (match-string 1)))
          (new (string-to-number (match-string 2))) old-lines new-lines)
      (forward-line)
      (while (not (eobp))
        (let* ((prefix (char-after))
               (selected (and (< (1- (line-beginning-position)) end)
                              (> (1- (line-end-position)) begin))))
          (when (memq prefix '(?\s ?-))
            (when selected (push old old-lines))
            (setq old (1+ old)))
          (when (memq prefix '(?\s ?+))
            (when selected (push new new-lines))
            (setq new (1+ new))))
        (forward-line))
      (list (and old-lines (cons (car (last old-lines)) (car old-lines)))
            (and new-lines (cons (car (last new-lines)) (car new-lines)))))))

(defun ai-code--annotate-magit-at-point ()
  "Return the saved note ID displayed at point, if any."
  (cl-some (lambda (overlay) (overlay-get overlay 'ai-code-annotate-magit-id))
           (overlays-at (point))))

(defun ai-code--annotate-magit-snapshot ()
  "Capture a hunk or selected lines, rejecting cross-hunk regions."
  (let* ((section (ai-code--annotate-magit-ancestor (magit-current-section) 'hunk)))
    (unless section (user-error "Point must be on a diff hunk"))
    (let* ((data (ai-code--annotate-magit-hunk-data section))
           (start (plist-get data :start))
           (limit (plist-get data :end))
           (begin (if (use-region-p) (region-beginning) start))
           (end (if (use-region-p) (region-end) limit)))
      (unless (and (>= begin start) (<= end limit) (< begin end))
        (user-error "Select lines within a single hunk"))
      (let ((ranges (ai-code--annotate-magit-line-ranges
                     (plist-get data :hunk) (- begin start) (- end start))))
        (require 'org-id)
        (list :id (org-id-uuid)
              :repository (ai-code--annotate-magit-repository)
              :branch (magit-get-current-branch)
              :head (magit-rev-parse "HEAD")
              :file (plist-get data :file)
              :old-file (plist-get data :old-file)
              :context (plist-get data :context)
              :header (plist-get data :header)
              :hunk (plist-get data :hunk)
              :begin (- begin start) :end (- end start)
              :old-lines (car ranges) :new-lines (cadr ranges))))))

(defun ai-code--annotate-magit-hunks ()
  "Return regular hunk snapshots for the current Magit buffer."
  (let (hunks)
    (cl-labels ((walk (section)
                  (when (eq (ai-code--annotate-magit-slot section 'type) 'hunk)
                    (condition-case nil
                        (push (ai-code--annotate-magit-hunk-data section) hunks)
                      (user-error nil)))
                  (mapc #'walk (ai-code--annotate-magit-slot section 'children))))
      (when magit-root-section (walk magit-root-section)))
    (nreverse hunks)))

(defun ai-code--annotate-magit-match (note hunks)
  "Find an exact, unambiguous match for NOTE in HUNKS."
  (let ((matches
         (cl-remove-if-not
          (lambda (hunk)
            (and (equal (plist-get note :file) (plist-get hunk :file))
                 (equal (plist-get note :old-file) (plist-get hunk :old-file))
                 (equal (plist-get note :context) (plist-get hunk :context))
                 (equal (plist-get note :hunk) (plist-get hunk :hunk))))
          hunks)))
    (and (= (length matches) 1) (car matches))))

(defun ai-code--annotate-magit-clear ()
  "Remove only this module's display overlays."
  (mapc #'delete-overlay ai-code--annotate-magit-overlays)
  (setq ai-code--annotate-magit-overlays nil))

(defun ai-code--annotate-magit-display (note hunk)
  "Display saved NOTE on its matching HUNK using annotate overlays."
  (let* ((start (+ (plist-get hunk :start) (plist-get note :begin)))
         (end (+ (plist-get hunk :start) (plist-get note :end)))
         (id (plist-get note :id))
         ;; Display via after-string rather than annotate's newline text
         ;; properties: Magit owns the buffer text and its fontification.
         (annotate-use-echo-area t))
    (annotate-create-annotation start end (plist-get note :text) nil 0 nil id)
    (let ((overlays (cl-remove-if-not
                     (lambda (overlay) (equal (annotate-annotation-id overlay) id))
                     (annotate-all-annotations))))
      (dolist (overlay overlays)
        (overlay-put overlay 'ai-code-annotate-magit-id id)
        (overlay-put overlay 'help-echo (plist-get note :text)))
      (when overlays
        (let ((tail (car (sort (copy-sequence overlays)
                              (lambda (a b) (> (overlay-end a) (overlay-end b)))))))
          (overlay-put tail 'after-string
                       (propertize (concat "\n  REVIEW: " (plist-get note :text))
                                   'face (annotate-annotation-property-annotation-face tail)))))
      (setq ai-code--annotate-magit-overlays (append overlays ai-code--annotate-magit-overlays)))))

(defun ai-code--annotate-magit-refresh ()
  "Restore exact matching notes after Magit recreates its sections.
Report an unreadable database instead of signaling, so Magit still refreshes."
  (when ai-code-annotate-magit-mode
    (ai-code--annotate-magit-clear)
    (condition-case err
        (let ((root (ai-code--annotate-magit-repository))
              (hunks (ai-code--annotate-magit-hunks)))
          (dolist (note (ai-code--annotate-magit-read))
            (when (equal root (plist-get note :repository))
              (let ((hunk (ai-code--annotate-magit-match note hunks)))
                (when hunk (ai-code--annotate-magit-display note hunk))))))
      (user-error (message "%s" (error-message-string err))))))

(defun ai-code--annotate-magit-refresh-repository (root)
  "Update enabled Magit buffers belonging to ROOT."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and ai-code-annotate-magit-mode
                 (equal root (ai-code--annotate-magit-repository)))
        (ai-code--annotate-magit-refresh)))))

(defun ai-code--annotate-magit-save-note (note text)
  "Save NOTE with TEXT, merging other buffers' saved notes."
  (when (string-empty-p (string-trim text)) (user-error "Annotation text is empty"))
  (let* ((id (plist-get note :id))
         (notes (cl-remove-if (lambda (entry) (equal id (plist-get entry :id)))
                              (ai-code--annotate-magit-read))))
    (ai-code--annotate-magit-write
     (append notes (list (plist-put (copy-sequence note) :text text))))
    (ai-code--annotate-magit-refresh-repository (plist-get note :repository))))

(defvar ai-code-annotate-magit-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'ai-code-annotate-magit-edit-save)
    (define-key map (kbd "C-c C-k") #'ai-code-annotate-magit-edit-cancel)
    map))

(define-derived-mode ai-code-annotate-magit-edit-mode text-mode "Review Note"
  "Edit a review note.
\{ai-code-annotate-magit-edit-mode-map}")

;;;###autoload
(defun ai-code-annotate-magit-annotate ()
  "Create a multiline note on a hunk/region, or edit the note at point."
  (interactive)
  (unless ai-code-annotate-magit-mode (ai-code-annotate-magit-mode 1))
  (let* ((origin (current-buffer))
         (id (ai-code--annotate-magit-at-point))
         (note (or (and id (cl-find id (ai-code--annotate-magit-read)
                                   :key (lambda (entry) (plist-get entry :id))
                                   :test #'equal))
                   (ai-code--annotate-magit-snapshot)))
         (buffer (generate-new-buffer "*Magit Review Note*")))
    (pop-to-buffer buffer)
    (ai-code-annotate-magit-edit-mode)
    (setq ai-code--annotate-magit-edit-origin origin
          ai-code--annotate-magit-edit-record note)
    (insert (or (plist-get note :text) ""))
    (setq header-line-format "C-c C-c: save review note   C-c C-k: cancel")))

(defun ai-code-annotate-magit-edit-save ()
  "Persist the current editor's note, then return to its Magit buffer."
  (interactive)
  (unless ai-code--annotate-magit-edit-record (user-error "Not in a review note editor"))
  (ai-code--annotate-magit-save-note ai-code--annotate-magit-edit-record
                            (buffer-substring-no-properties (point-min) (point-max)))
  (ai-code-annotate-magit-edit-cancel))

(defun ai-code-annotate-magit-edit-cancel ()
  "Close the review note editor without saving further edits."
  (interactive)
  (let ((origin ai-code--annotate-magit-edit-origin))
    (kill-buffer (current-buffer))
    (when (buffer-live-p origin) (pop-to-buffer origin))))

;;;###autoload
(defun ai-code-annotate-magit-delete (&optional id)
  "Delete the note at point, or the saved note with ID."
  (interactive
   (list (or (ai-code--annotate-magit-at-point)
             (let* ((root (ai-code--annotate-magit-repository))
                    (choices
                     (cl-loop for note in (ai-code--annotate-magit-read)
                              when (equal root (plist-get note :repository))
                              collect
                              (cons (format "%s: %s [%s]"
                                            (plist-get note :file)
                                            (car (split-string (plist-get note :text) "\n"))
                                            (plist-get note :id))
                                    (plist-get note :id)))))
               (unless choices (user-error "No saved review notes for this worktree"))
               (cdr (assoc (completing-read "Delete review note: " choices nil t)
                           choices))))))
  (let ((id (or id (ai-code--annotate-magit-at-point)))
        (root (ai-code--annotate-magit-repository)))
    (unless id (user-error "No review note at point"))
    (ai-code--annotate-magit-write
     (cl-remove-if (lambda (note) (and (equal root (plist-get note :repository))
                                     (equal id (plist-get note :id))))
                   (ai-code--annotate-magit-read)))
    (ai-code--annotate-magit-refresh-repository root)))

(defun ai-code--annotate-magit-range-string (range)
  "Render source line RANGE without inventing a missing side."
  (cond ((null range) "none")
        ((= (car range) (cdr range)) (number-to-string (car range)))
        (t (format "%d-%d" (car range) (cdr range)))))

;;;###autoload
(defun ai-code-annotate-magit-review-string (&optional repository id)
  "Return all saved notes for this worktree as a Markdown AI handoff.
Includes original diff snapshots even when no longer displayed.  Exporting
does not resolve or delete notes.  REPOSITORY defaults to this worktree.
With ID, include only that note, or the notes in a list of IDs."
  (let* ((root (or repository (ai-code--annotate-magit-repository)))
         (checked (and (derived-mode-p 'magit-mode)
                       (equal root (ai-code--annotate-magit-repository))))
         (hunks (and checked (ai-code--annotate-magit-hunks)))
         (notes (cl-remove-if-not
                 (lambda (note) (and (equal root (plist-get note :repository))
                                     (or (null id)
                                         (if (listp id) (member (plist-get note :id) id)
                                           (equal id (plist-get note :id))))))
                 (ai-code--annotate-magit-read))))
    (unless notes (user-error "No saved review notes for this worktree"))
    (concat
     "# Code review notes\n\nRepository: " root
     "\n\nFocus on the annotations; use original diffs as supporting context.\n"
     "Do not perform a general code review or suggest unrelated changes.\n"
     "Suggest how to address these notes; do not modify files. Verify each\n"
     "original diff against current code. UNMATCHED notes may be outdated\n"
     "or outside the current diff view. NOT CHECKED notes were exported\n"
     "without a Magit view of this worktree. Wait for user approval.\n\n"
     (mapconcat
      (lambda (note)
        (let* ((hunk (plist-get note :hunk))
               ;; Long enough even if code contains Markdown fences.
               (fence (make-string (1+ (max 2 (cl-loop for line in (split-string hunk "\n")
                                                      maximize (if (string-match "`+" line)
                                                                   (length (match-string 0 line)) 0)))) ?`)))
          (format "## %s\n\nNote ID: %s\nStatus: %s\nDiff context: %S\nBranch: %s\nHEAD at review: %s\nOld file: %s\nOld lines: %s; new lines: %s\nSelected hunk offsets: %s-%s\n\nAnnotation (primary review request):\n%s\n\nOriginal hunk (supporting code change context):\n%sdiff\n%s%s\n"
                  (plist-get note :file) (plist-get note :id)
                  (cond ((not checked) "NOT CHECKED (no Magit view; verify snapshot)")
                        ((ai-code--annotate-magit-match note hunks) "MATCHED")
                        (t "UNMATCHED (verify snapshot)"))
                  (plist-get note :context) (or (plist-get note :branch) "detached")
                  (or (plist-get note :head) "unborn")
                  (plist-get note :old-file)
                  (ai-code--annotate-magit-range-string (plist-get note :old-lines))
                  (ai-code--annotate-magit-range-string (plist-get note :new-lines))
                  (plist-get note :begin) (plist-get note :end)
                  (plist-get note :text) fence hunk fence)))
      notes "\n"))))

(defun ai-code--annotate-show-report (name report)
  "Display REPORT read-only in the buffer called NAME."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert report)
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

;;;###autoload
(defun ai-code-annotate-magit-review ()
  "Preview all saved worktree notes, including unmatched snapshots."
  (interactive)
  (ai-code--annotate-show-report "*Magit Review Notes*"
                                 (ai-code-annotate-magit-review-string)))

;;;###autoload
(defun ai-code-annotate-magit-copy-review ()
  "Copy all worktree notes and diff snapshots for pasting to an AI."
  (interactive)
  (kill-new (ai-code-annotate-magit-review-string))
  (message "Copied review notes and original hunks"))

;;;###autoload
(define-minor-mode ai-code-annotate-magit-mode
  "Annotate Magit hunks with persistent notes, without changing the diff.
Do not enable `annotate-mode' in these buffers; this optional mode handles
their lifecycle instead.
\{ai-code-annotate-magit-mode-map}"
  :lighter " AnnReview"
  :keymap ai-code-annotate-magit-mode-map
  :group 'ai-code-annotate-magit
  (if ai-code-annotate-magit-mode
      (progn
        (when-let* ((problem (ai-code--annotate-missing "annotate diff hunks")))
          (setq ai-code-annotate-magit-mode nil)
          (user-error "%s" problem))
        (unless (require 'magit nil t)
          (setq ai-code-annotate-magit-mode nil)
          (user-error "Install Magit to annotate diff hunks"))
        (unless (derived-mode-p 'magit-mode)
          (setq ai-code-annotate-magit-mode nil)
          (user-error "This mode requires a Magit buffer"))
        (when annotate-mode
          (setq ai-code-annotate-magit-mode nil)
          (user-error "Disable annotate-mode before enabling ai-code-annotate-magit-mode"))
        (add-hook 'magit-refresh-buffer-hook #'ai-code--annotate-magit-refresh nil t)
        (ai-code--annotate-magit-refresh))
    (remove-hook 'magit-refresh-buffer-hook #'ai-code--annotate-magit-refresh t)
    (ai-code--annotate-magit-clear)))

(provide 'ai-code-annotate-magit)
;;; ai-code-annotate-magit.el ends here
