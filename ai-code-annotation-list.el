;;; ai-code-annotation-list.el --- Browse and select code annotations -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Browse source and Magit notes, including stale snapshots.  Mark notes to
;; request AI suggestions for a selection, a file, or the current worktree.
;; Editing a stale note changes its text, never guesses a new source location.

;;; Code:

(require 'tabulated-list)
(require 'ai-code-annotation)
(require 'ai-code-annotate-magit)

(declare-function magit-current-section "magit-section")
(declare-function annotate-id-from-dump "annotate")
(declare-function annotate-reply-to-from-dump "annotate")
(declare-function annotate-annotation-string "annotate")
(declare-function annotate-beginning-of-annotation "annotate")
(declare-function annotate-ending-of-annotation "annotate")
(declare-function annotate-annotated-text "annotate")

(defvar annotate-file)
(defvar ai-code-annotate-magit-mode)
(defvar-local ai-code--annotation-list-context nil)
(defvar-local ai-code--annotation-list-items nil)
(defvar-local ai-code--annotation-list-marks nil)
(defvar-local ai-code--annotation-list-edit-item nil)
(defvar-local ai-code--annotation-list-edit-origin nil)

(defun ai-code--annotation-list-current-file ()
  "Return the source file or Magit file section at point."
  (or buffer-file-name
      (when (derived-mode-p 'magit-mode)
        (when-let* ((section (ai-code--annotate-magit-ancestor (magit-current-section) 'file)))
          (expand-file-name (ai-code--annotate-magit-slot section 'value)
                            (ai-code--annotate-magit-repository))))))

(defun ai-code--annotation-list-context ()
  "Capture the calling buffer's repository and annotation database."
  (when-let* ((problem (ai-code--annotate-missing "browse code annotations")))
    (user-error "%s" problem))
  (pcase-let ((`(,root ,file) (ai-code--annotation-scope)))
    (list :root root :file file :origin (current-buffer)
          :database (expand-file-name annotate-file) :directory default-directory)))

(defun ai-code--annotation-list-source-items (source)
  "Build rows for SOURCE's root notes using one read of its current text."
  (let ((buffer (plist-get source :buffer))
        (file (plist-get source :file)))
    (cl-labels
        ((rows ()
           (save-restriction
             (widen)
             (cl-loop for note in (plist-get source :notes)
                      unless (annotate-reply-to-from-dump note)
                      collect
                      (let* ((report (condition-case nil
                                         (ai-code--annotation-source-note
                                          note (buffer-live-p buffer)
                                          (or (buffer-live-p buffer) (file-exists-p file)))
                                       (error "Status: SKIPPED\n")))
                             (status (if (string-match "Status: \\([^ (\n]+\\)" report)
                                         (match-string 1 report) "SKIPPED"))
                             (line (when (string-match "Current source lines: \\([0-9]+\\)" report)
                                     (string-to-number (match-string 1 report)))))
                        (list :key (list 'source file (or (annotate-id-from-dump note) note))
                              :kind 'source :file file :source source :note note
                              :line line :status status))))))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer (rows))
        (with-temp-buffer
          (condition-case nil (when (file-exists-p file) (insert-file-contents file))
            (file-error nil))
          (rows))))))

(defun ai-code--annotation-list-collect (context)
  "Collect source and hunk rows in CONTEXT without sending to AI."
  (let* ((root (plist-get context :root))
         (origin (plist-get context :origin))
         (annotate-file (plist-get context :database))
         (default-directory (plist-get context :directory))
         (checked (and (buffer-live-p origin)
                       (with-current-buffer origin
                         (and (derived-mode-p 'magit-mode)
                              (equal root (ai-code--annotate-magit-repository))))))
         (hunks (and checked (with-current-buffer origin (ai-code--annotate-magit-hunks)))))
    (append
     (cl-mapcan #'ai-code--annotation-list-source-items
                (ai-code--annotation-sources root (plist-get context :file)))
     (mapcar (lambda (note)
               (let* ((matched (and checked (ai-code--annotate-magit-match note hunks)))
                      (range (plist-get note :new-lines)))
                 (list :key (list 'magit (plist-get note :id)) :kind 'magit
                       :file (expand-file-name (plist-get note :file) root) :note note
                       :line (and matched (car range))
                       :status (cond (matched "MATCHED") (checked "UNMATCHED")
                                     (t "NOT CHECKED")))))
             (ai-code--annotation-magit-notes root)))))

(defun ai-code--annotation-list-summary (item)
  "Return a one-line summary of ITEM."
  (let ((text (if (eq (plist-get item :kind) 'source)
                  (annotate-annotation-string (plist-get item :note))
                (plist-get (plist-get item :note) :text))))
    (if (stringp text) (replace-regexp-in-string "[\n\r\t]+" " " text)
      "(unreadable annotation)")))

(defun ai-code-annotation-list-refresh (&optional items)
  "Refresh the annotation list and preserve surviving selections.
Use precollected ITEMS when supplied, avoiding redundant source file reads."
  (interactive)
  (setq ai-code--annotation-list-items
        (or items (ai-code--annotation-list-collect ai-code--annotation-list-context)))
  (setq ai-code--annotation-list-marks
        (cl-intersection ai-code--annotation-list-marks
                         (mapcar (lambda (item) (plist-get item :key)) ai-code--annotation-list-items)
                         :test #'equal))
  (let ((root (plist-get ai-code--annotation-list-context :root)))
    (setq tabulated-list-entries
          (mapcar (lambda (item)
                    (let ((key (plist-get item :key)))
                      (list key
                            (vector (if (member key ai-code--annotation-list-marks) "*" "")
                                    (symbol-name (plist-get item :kind))
                                    (if root (file-relative-name (plist-get item :file) root)
                                      (abbreviate-file-name (plist-get item :file)))
                                    (if (plist-get item :line)
                                        (number-to-string (plist-get item :line)) "?")
                                    (plist-get item :status)
                                    (ai-code--annotation-list-summary item)))))
                  ai-code--annotation-list-items)))
  (setq mode-name (format "Annotations (%d notes, %d marked)"
                          (length ai-code--annotation-list-items)
                          (length ai-code--annotation-list-marks)))
  (tabulated-list-print t))

(defun ai-code--annotation-list-item ()
  "Return the annotation on the current row."
  (or (cl-find (tabulated-list-get-id) ai-code--annotation-list-items
               :key (lambda (item) (plist-get item :key)) :test #'equal)
      (user-error "No annotation on this row")))

(defun ai-code-annotation-list-mark ()
  "Toggle selection of the current annotation, then move to the next row."
  (interactive)
  (let ((key (plist-get (ai-code--annotation-list-item) :key)))
    (if (member key ai-code--annotation-list-marks)
        (setq ai-code--annotation-list-marks (delete key ai-code--annotation-list-marks))
      (push key ai-code--annotation-list-marks))
    (ai-code-annotation-list-refresh ai-code--annotation-list-items)
    (forward-line)))

(defun ai-code--annotation-list-report (items context)
  "Render selected ITEMS and their replies in CONTEXT."
  (let ((sources (make-hash-table :test #'equal)) ids)
    (dolist (item items)
      (if (eq (plist-get item :kind) 'magit)
          (push (plist-get (plist-get item :note) :id) ids)
        (let* ((source (plist-get item :source))
               (file (plist-get item :file))
               (entry (or (gethash file sources) (plist-put (copy-sequence source) :notes nil))))
          (plist-put entry :notes
                     (cl-remove-duplicates
                      (append (plist-get entry :notes)
                              (ai-code--annotation-thread (plist-get source :notes)
                                                          (list (plist-get item :note))))
                      :test #'equal))
          (puthash file entry sources))))
    (let (reports)
      (maphash (lambda (_file source) (push (ai-code--annotation-source-report source) reports)) sources)
      (when ids
        (push (ai-code-annotate-magit-review-string (plist-get context :root) ids) reports))
      (string-join (nreverse reports) "\n"))))

(defun ai-code--annotation-list-send-items (items context)
  "Review and send ITEMS from CONTEXT's original buffer."
  (unless items (user-error "No code annotations in the selected scope"))
  (let* ((origin (plist-get context :origin))
         (annotate-file (plist-get context :database))
         (default-directory (plist-get context :directory))
         (send (lambda ()
                 (ai-code--annotation-send
                  (plist-get context :root) (ai-code--annotation-list-report items context)))))
    (if (buffer-live-p origin) (with-current-buffer origin (funcall send)) (funcall send))))

(defun ai-code-annotation-list-send-selected ()
  "Ask AI about marked annotations, or the current row when none are marked."
  (interactive)
  (let ((marked ai-code--annotation-list-marks))
    (ai-code-annotation-list-refresh)
    (ai-code--annotation-list-send-items
     (if marked
         (cl-remove-if-not (lambda (item) (member (plist-get item :key) ai-code--annotation-list-marks))
                           ai-code--annotation-list-items)
       (list (ai-code--annotation-list-item)))
     ai-code--annotation-list-context)))

(defun ai-code--annotation-list-send-file (file)
  "Ask AI about source and hunk notes for FILE, inferred at point if nil."
  (let* ((context (or ai-code--annotation-list-context (ai-code--annotation-list-context)))
         (file (or file (ai-code--annotation-list-current-file)
                   (user-error "Select a source file or a Magit file section; or browse annotations")))
         (items (cl-remove-if-not
                 (lambda (item) (equal (file-truename file) (file-truename (plist-get item :file))))
                 (ai-code--annotation-list-collect context))))
    (ai-code--annotation-list-send-items items context)))

(defun ai-code-annotation-list-send-file ()
  "Ask AI about all annotations for the file on the current row."
  (interactive)
  (ai-code--annotation-list-send-file (plist-get (ai-code--annotation-list-item) :file)))

(defun ai-code-annotation-list-send-worktree ()
  "Ask AI about every annotation in the browser's scope."
  (interactive)
  (ai-code-annotation-list-refresh)
  (ai-code--annotation-list-send-items ai-code--annotation-list-items ai-code--annotation-list-context))

(defun ai-code-annotation-list-preview ()
  "Preview the current annotation's complete text, replies and snapshot."
  (interactive)
  (ai-code--annotate-show-report
   "*AI Annotation Snapshot*"
   (ai-code--annotation-list-report (list (ai-code--annotation-list-item)) ai-code--annotation-list-context)))

(defun ai-code-annotation-list-jump ()
  "Visit the current annotation only if its exact source or hunk still matches.
Preview stale snapshots without guessing their current source location."
  (interactive)
  (ai-code-annotation-list-refresh)
  (let* ((item (ai-code--annotation-list-item)) (note (plist-get item :note)))
    (if (eq (plist-get item :kind) 'source)
        (let ((file (plist-get item :file)) (begin (annotate-beginning-of-annotation note))
              (end (annotate-ending-of-annotation note)))
          (if (and (equal (plist-get item :status) "MATCHED")
                   (or (buffer-live-p (plist-get (plist-get item :source) :buffer))
                       (file-exists-p file)))
              (let ((buffer (or (plist-get (plist-get item :source) :buffer)
                                (find-file-noselect file))))
                (if (with-current-buffer buffer
                      (save-restriction
                        (widen)
                        (and (<= (point-min) begin end (point-max))
                             (equal (annotate-annotated-text note)
                                    (buffer-substring-no-properties begin end)))))
                    (progn (pop-to-buffer buffer) (widen) (goto-char begin))
                  (ai-code-annotation-list-preview)))
            (ai-code-annotation-list-preview)))
      (let ((origin (plist-get ai-code--annotation-list-context :origin)))
        (if-let* ((hunk (and (buffer-live-p origin)
                            (with-current-buffer origin
                              (ai-code--annotate-magit-match note (ai-code--annotate-magit-hunks))))))
            (progn (pop-to-buffer origin) (goto-char (+ (plist-get hunk :start) (plist-get note :begin))))
          (ai-code-annotation-list-preview))))))

(defun ai-code-annotation-list-delete ()
  "Delete the current note and its replies after confirmation."
  (interactive)
  (let ((item (ai-code--annotation-list-item)))
    (when (yes-or-no-p (format "Delete annotation and replies: %s? " (ai-code--annotation-list-summary item)))
      (if (eq (plist-get item :kind) 'source)
          (ai-code--annotation-update-source (plist-get item :source) (plist-get item :note) nil)
        (let* ((note (plist-get item :note))
               (notes (ai-code--annotate-magit-read)))
          (unless (member note notes) (user-error "Annotation changed; refresh the list before deleting"))
          (ai-code--annotate-magit-write (delete note notes))
          (ai-code--annotate-magit-refresh-repository (plist-get note :repository))))
      (ai-code-annotation-list-refresh))))

(defvar ai-code-annotation-list-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'ai-code-annotation-list-edit-save)
    (define-key map (kbd "C-c C-k") #'ai-code-annotation-list-edit-cancel)
    map))

(define-derived-mode ai-code-annotation-list-edit-mode text-mode "Annotation"
  "Edit annotation text, retaining its original source or diff snapshot.
\{ai-code-annotation-list-edit-mode-map}")

(defun ai-code-annotation-list-edit ()
  "Edit the current annotation, including unmatched saved notes."
  (interactive)
  (let ((item (ai-code--annotation-list-item)) (origin (current-buffer)))
    (pop-to-buffer (generate-new-buffer "*Edit AI Annotation*"))
    (ai-code-annotation-list-edit-mode)
    (setq ai-code--annotation-list-edit-item item ai-code--annotation-list-edit-origin origin)
    (insert (if (eq (plist-get item :kind) 'source)
                (or (annotate-annotation-string (plist-get item :note)) "")
              (plist-get (plist-get item :note) :text)))
    (setq header-line-format "C-c C-c: save annotation   C-c C-k: cancel")))

(defun ai-code-annotation-list-edit-save ()
  "Save annotation text without modifying its source snapshot."
  (interactive)
  (unless ai-code--annotation-list-edit-item (user-error "Not in an annotation editor"))
  (let ((item ai-code--annotation-list-edit-item)
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-empty-p (string-trim text)) (user-error "Annotation text is empty"))
    (if (eq (plist-get item :kind) 'source)
        (ai-code--annotation-update-source (plist-get item :source) (plist-get item :note) text)
      (let ((note (plist-get item :note)))
        (unless (member note (ai-code--annotate-magit-read))
          (user-error "Annotation changed; refresh the list before editing"))
        (ai-code--annotate-magit-save-note note text))))
  (when (buffer-live-p ai-code--annotation-list-edit-origin)
    (with-current-buffer ai-code--annotation-list-edit-origin (ai-code-annotation-list-refresh)))
  (ai-code-annotation-list-edit-cancel))

(defun ai-code-annotation-list-edit-cancel ()
  "Close the annotation editor and return to its browser."
  (interactive)
  (let ((origin ai-code--annotation-list-edit-origin))
    (kill-buffer (current-buffer))
    (when (buffer-live-p origin) (pop-to-buffer origin))))

(defvar ai-code-annotation-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (dolist (binding '(("RET" . ai-code-annotation-list-jump)
                       ("v" . ai-code-annotation-list-preview)
                       ("e" . ai-code-annotation-list-edit)
                       ("d" . ai-code-annotation-list-delete)
                       ("m" . ai-code-annotation-list-mark)
                       ("s" . ai-code-annotation-list-send-selected)
                       ("f" . ai-code-annotation-list-send-file)
                       ("w" . ai-code-annotation-list-send-worktree)
                       ("g" . ai-code-annotation-list-refresh)))
      (define-key map (kbd (car binding)) (cdr binding)))
    map))

(define-derived-mode ai-code-annotation-list-mode tabulated-list-mode "Annotations"
  "Browse, mark and act on source and Magit annotations.
\{ai-code-annotation-list-mode-map}"
  (setq tabulated-list-format [("" 1 nil) ("Type" 7 t) ("File" 30 t)
                               ("Line" 6 t) ("Status" 12 t) ("Annotation" 0 t)]
        tabulated-list-padding 1)
  (tabulated-list-init-header)
  (setq header-line-format
        (list header-line-format "  RET jump/snapshot | v preview | e edit | d delete | m mark | s ask selected | f ask file | w ask all | g refresh")))

;;;###autoload
(defun ai-code-annotation-browse ()
  "Browse the current worktree's annotations, or current file outside Git."
  (interactive)
  (let* ((context (ai-code--annotation-list-context))
         (items (ai-code--annotation-list-collect context)))
    (unless items
      (user-error "No code annotations found for this %s"
                  (if (plist-get context :root) "worktree" "file")))
    (pop-to-buffer (get-buffer-create "*AI Code Annotations*"))
    (ai-code-annotation-list-mode)
    (setq ai-code--annotation-list-context context
          default-directory (plist-get context :directory))
    (ai-code-annotation-list-refresh items)))

(provide 'ai-code-annotation-list)
;;; ai-code-annotation-list.el ends here
