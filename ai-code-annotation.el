;;; ai-code-annotation.el --- Suggest responses to code annotations -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional annotate.el support for source files and persistent Magit notes.
;; `ai-code-address-code-annotation' adds, edits, deletes, clears and views
;; annotations, or asks the selected AI for suggestions on them.  Sending
;; never resolves or removes an annotation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar annotate-file)
(defvar annotate-mode)
(defvar ai-code-prompt-suffix-functions)
(defvar ai-code-annotate-magit-mode)
(defvar ai-code--annotation-action-history nil)
(defvar annotate-autosave)
(defvar annotate-annotation-confirm-deletion)
(defvar annotate-file-buffer-local)
(declare-function annotate-mode "annotate" (&optional arg))
(declare-function annotate-annotate "annotate" (&optional color-index))
(declare-function annotate-delete-annotation "annotate" (&optional point))
(declare-function annotate-annotation-at "annotate" (&optional pos))
(declare-function annotate-clear-annotations "annotate")
(declare-function annotate-all-annotations "annotate")
(declare-function annotate-annotation-id "annotate")
(declare-function annotate-annotation-set-annotation-text "annotate" (annotation text))
(declare-function annotate--filepath->local-database-name "annotate" (filepath))
(declare-function annotate-filename-from-dump "annotate")
(declare-function annotate-annotations-from-dump "annotate")
(declare-function annotate-describe-annotations "annotate")
(declare-function annotate-beginning-of-annotation "annotate")
(declare-function annotate-ending-of-annotation "annotate")
(declare-function annotate-annotated-text "annotate")
(declare-function annotate-annotation-string "annotate")
(declare-function annotate-id-from-dump "annotate")
(declare-function annotate-reply-to-from-dump "annotate")
(declare-function ai-code--git-root "ai-code-utils" (&optional dir))
(declare-function ai-code--backend-completion-table "ai-code-backends" (candidates))
(declare-function ai-code--write-prompt-to-file-and-send "ai-code-prompt-mode"
                  (prompt-text))
(declare-function ai-code-compose-read "ai-code-compose"
                  (prompt &optional initial-input candidate-list))
(declare-function ai-code--annotate-missing "ai-code-annotate-magit" (purpose))
(declare-function ai-code--annotate-magit-read "ai-code-annotate-magit")
(declare-function ai-code--annotate-magit-write "ai-code-annotate-magit" (notes))
(declare-function ai-code--annotate-magit-at-point "ai-code-annotate-magit")
(declare-function ai-code--annotate-magit-refresh-repository "ai-code-annotate-magit"
                  (root))
(declare-function ai-code--annotate-show-report "ai-code-annotate-magit" (name report))
(declare-function ai-code-annotate-magit-annotate "ai-code-annotate-magit")
(declare-function ai-code-annotate-magit-delete "ai-code-annotate-magit" (&optional id))
(declare-function ai-code-annotate-magit-review-string "ai-code-annotate-magit"
                  (&optional repository id))
(declare-function ai-code-annotation-browse "ai-code-annotation-list" ())
(declare-function ai-code--annotation-list-send-file "ai-code-annotation-list" (file))

(defconst ai-code--annotation-actions
  '(("1. Add / Edit annotation at point..." . ai-code--annotation-edit)
    ("2. Browse %s annotations..." . ai-code--annotation-view)
    ("3. Ask AI about annotation at point..." . ai-code--annotation-send-current)
    ("4. Ask AI about %s annotations..." . ai-code--annotation-send-all)
    ("5. Delete annotation at point" . ai-code--annotation-delete)
    ("6. Clear %s annotations..." . ai-code--annotation-clear-all))
  "Actions offered by `ai-code-address-code-annotation', in display order.")

(defun ai-code--annotation-file (file root current-file)
  "Normalize FILE when it belongs to ROOT, or equals CURRENT-FILE outside Git."
  (when (and (stringp file) (file-name-absolute-p file))
    (let ((path (file-truename (expand-file-name file))))
      (when (if root (file-in-directory-p path root)
              (equal path current-file))
        path))))

(defun ai-code--annotation-record-notes (record)
  "Return RECORD's annotations, accepting annotate.el's legacy layout.
Legacy records, written before checksums, list annotations directly
after the file name instead of nesting them in one list."
  (let ((notes (annotate-annotations-from-dump record)))
    (if (integerp (car-safe notes))
        (remq nil (cdr record))
      notes)))

(defun ai-code--annotation-read-database (file)
  "Read FILE without annotate.el's lossy conversion of legacy records."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (skip-chars-forward " \t\r\n")
      (unless (eobp)
        (condition-case err
            (let ((records (read (current-buffer))))
              (unless (proper-list-p records) (error "Invalid annotation database"))
              (dolist (record records)
                (unless (and (proper-list-p record) (stringp (car record)))
                  (error "Invalid annotation record")))
              (skip-chars-forward " \t\r\n")
              (unless (eobp) (error "Trailing database content"))
              records)
          (error (user-error "Cannot read %s: %s" file (error-message-string err))))))))

(defun ai-code--annotation-write-database (file records)
  "Atomically save RECORDS to FILE, preserving every legacy annotation.
Normalize legacy records before writing so annotate.el can read them too."
  (let* ((path (expand-file-name file))
         (directory (file-name-directory path)) temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary (make-temp-file (expand-file-name ".annotations-" directory)))
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil))
              (prin1 (mapcar (lambda (record)
                               (if (integerp (car-safe (cadr record)))
                                   (list (abbreviate-file-name (car record))
                                         (ai-code--annotation-record-notes record)
                                         (ai-code--annotation-file-checksum (car record)))
                                 record))
                             records)
                     (current-buffer))
              (insert "\n")))
          (when (file-exists-p path) (set-file-modes temporary (file-modes path)))
          (rename-file temporary path t))
      (when (and temporary (file-exists-p temporary)) (delete-file temporary)))))

(defun ai-code--annotation-file-checksum (file)
  "Return the current checksum of FILE for legacy record migration."
  (if-let* ((buffer (get-file-buffer (expand-file-name file))))
      (with-current-buffer buffer (save-restriction (widen) (secure-hash 'md5 (current-buffer))))
    (when (file-readable-p file)
      (with-temp-buffer (insert-file-contents file) (secure-hash 'md5 (current-buffer))))))

(defun ai-code--annotation-normalize-database (file)
  "Migrate legacy records in FILE before annotate.el can truncate them."
  (let ((records (ai-code--annotation-read-database file)))
    (when (cl-some (lambda (record) (integerp (car-safe (cadr record)))) records)
      (ai-code--annotation-write-database file records))))

(defun ai-code--annotation-thread (saved roots)
  "Return ROOTS and all their descendant replies from SAVED.
Exclude replies whose parent was deleted, including nested reply chains."
  (let ((ids (make-hash-table :test #'equal))
        (pending (cl-remove-if-not #'annotate-reply-to-from-dump saved))
        (result (copy-sequence roots)) changed)
    (dolist (note roots)
      (when-let* ((id (annotate-id-from-dump note))) (puthash id t ids)))
    (setq changed t)
    (while changed
      (setq changed nil)
      (setq pending
            (cl-remove-if
             (lambda (note)
               (when (gethash (annotate-reply-to-from-dump note) ids)
                 (unless (member note result) (setq result (append result (list note))))
                 (when-let* ((id (annotate-id-from-dump note))) (puthash id t ids))
                 (setq changed t)))
             pending)))
    result))

(defun ai-code--annotation-note-equal (a b)
  "Compare source notes A and B by ID, or by content for legacy notes."
  (if (annotate-id-from-dump b)
      (equal (annotate-id-from-dump a) (annotate-id-from-dump b))
    (equal a b)))

(defun ai-code--annotation-update-source (source note text)
  "Set SOURCE's NOTE to TEXT, or delete its thread when TEXT is nil.
Update saved records and live overlays without editing source text."
  (let* ((file (plist-get source :file))
         (buffer (plist-get source :buffer))
         updates found
         (live (and (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (cl-find note (annotate-describe-annotations)
                               :test #'ai-code--annotation-note-equal)))))
    (when (and (buffer-live-p buffer) (not live))
      (user-error "Annotation removed; refresh the list before editing"))
    (when (and live (not (equal (annotate-annotation-string live)
                                (annotate-annotation-string note))))
      (user-error "Annotation changed; refresh the list before editing"))
    (dolist (database (plist-get source :databases))
      (let* ((records (ai-code--annotation-read-database database))
             (record (cl-find file records
                              :key (lambda (entry) (ai-code--annotation-file (car entry) nil file))
                              :test #'equal))
             (notes (ai-code--annotation-record-notes record))
             (saved (cl-find note notes :test #'ai-code--annotation-note-equal)))
        (when (and saved (not live) (not (equal (annotate-annotation-string saved)
                                               (annotate-annotation-string note))))
          (user-error "Annotation changed; refresh the list before editing"))
        (when (or saved (and live (equal database
                                        (with-current-buffer buffer
                                          (expand-file-name annotate-file)))))
          (setq found t)
          (let* ((removed (ai-code--annotation-thread notes (list (or saved note))))
                 (updated (if text
                              (append (cl-remove-if
                                       (lambda (entry) (ai-code--annotation-note-equal entry note))
                                       notes)
                                      (list (let ((copy (copy-sequence (or live note))))
                                              (setf (nth 2 copy) text) copy)))
                            (cl-remove-if (lambda (entry) (member entry removed)) notes)))
                 (replacement (list (or (car record) (abbreviate-file-name file)) updated
                                    (if (integerp (car-safe (cadr record)))
                                        (ai-code--annotation-file-checksum file)
                                      (or (nth 2 record) (ai-code--annotation-file-checksum file))))))
            (push (cons database
                        (if record
                            (mapcar (lambda (entry) (if (eq entry record) replacement entry)) records)
                          (append records (list replacement))))
                  updates)))))
    (unless found (user-error "Annotation removed; refresh the list before editing"))
    (dolist (update updates)
      (ai-code--annotation-write-database (car update) (cdr update)))
    (when live
      (with-current-buffer buffer
        (let ((overlays (cl-remove-if-not
                         (lambda (overlay)
                           (equal (annotate-annotation-id overlay) (annotate-id-from-dump live)))
                         (annotate-all-annotations))))
          (if text
              (dolist (overlay overlays)
                (annotate-annotation-set-annotation-text overlay text)
                (overlay-put overlay 'help-echo text))
            (when overlays
              (let ((annotate-autosave nil) (annotate-annotation-confirm-deletion nil))
                (annotate-delete-annotation (overlay-start (car overlays)))))))
        (font-lock-flush)))))

(defun ai-code--annotation-live-buffers (root current-file)
  "Return (FILE . BUFFER) for live annotated buffers in ROOT.
Outside Git, only CURRENT-FILE is in scope."
  (let (live)
    (dolist (buffer (buffer-list) live)
      (with-current-buffer buffer
        (when (bound-and-true-p annotate-mode)
          (when-let* ((file (ai-code--annotation-file buffer-file-name root current-file)))
            (push (cons file buffer) live)))))))

(defun ai-code--annotation-databases (live)
  "Return the active annotation database and those used by LIVE buffers."
  (delete-dups
   (cons (expand-file-name annotate-file)
         (mapcar (lambda (entry)
                   (with-current-buffer (cdr entry) (expand-file-name annotate-file)))
                 live))))

(defun ai-code--annotation-sources (root current-file)
  "Collect source records in ROOT, or CURRENT-FILE outside Git.
Read the active annotation database and databases of live annotated buffers.
Live buffers override their saved records, including deleted annotations."
  (let ((sources (make-hash-table :test #'equal))
        (databases (make-hash-table :test #'equal))
        (live (ai-code--annotation-live-buffers root current-file)))
    (dolist (database (ai-code--annotation-databases live))
      (when (file-exists-p database)
        (let ((annotate-file database))
          (dolist (record (ai-code--annotation-read-database database))
            (when-let* ((file (ai-code--annotation-file
                               (annotate-filename-from-dump record) root current-file)))
              (puthash file (cons database (gethash file databases)) databases)
              (puthash file
                       (cl-remove-duplicates
                        (append (gethash file sources)
                                (ai-code--annotation-record-notes record))
                        :test #'equal)
                       sources))))))
    (dolist (entry live)
      (with-current-buffer (cdr entry)
        (save-restriction
          (widen)
          (puthash (car entry)
                   (ai-code--annotation-thread (gethash (car entry) sources)
                                               (annotate-describe-annotations))
                   sources)
          (puthash (car entry) (cons (expand-file-name annotate-file)
                                    (gethash (car entry) databases)) databases))))
    (let (result)
      (maphash (lambda (file notes)
                 (when notes
                   (push (list :file file :notes notes :buffer (cdr (assoc file live))
                               :databases (delete-dups (gethash file databases)))
                         result)))
               sources)
      (sort result (lambda (a b) (string-lessp (plist-get a :file) (plist-get b :file)))))))

(defun ai-code--annotation-quote (text)
  "Fence TEXT as literal annotation context, preserving embedded backticks."
  (let ((fence (make-string
                (1+ (max 2 (cl-loop for line in (split-string text "\n")
                                   maximize (if (string-match "`+" line)
                                                (length (match-string 0 line)) 0))))
                ?`)))
    (concat fence "\n" text (unless (string-suffix-p "\n" text) "\n") fence)))

(defun ai-code--annotation-source-note (note live-p available-p)
  "Render NOTE with LIVE-P buffer provenance and AVAILABLE-P source presence."
  (let* ((begin (annotate-beginning-of-annotation note))
         (end (annotate-ending-of-annotation note))
         (quote (or (annotate-annotated-text note) ""))
         (matches (and available-p (integerp begin) (integerp end)
                       (<= (point-min) begin end (point-max))
                       (equal quote (buffer-substring-no-properties begin end)))))
    (format "Note ID: %s; reply to: %s\nStatus: %s\nStored character positions: %s-%s\n%s\nAnnotation (primary review request):\n%s\nAnnotated source (supporting code context):\n%s\n"
            (or (annotate-id-from-dump note) "legacy")
            (or (annotate-reply-to-from-dump note) "none")
            (cond ((annotate-reply-to-from-dump note) "REPLY (see parent annotation)")
                  (matches (if live-p "MATCHED live buffer (may be unsaved)" "MATCHED disk"))
                  (t "UNMATCHED (missing or changed source; verify before recommending)"))
            begin end
            (if matches
                (format "Current source lines: %d-%d"
                        (line-number-at-pos begin)
                        (line-number-at-pos (max begin (1- end))))
              "Current source lines: unknown")
            (ai-code--annotation-quote (annotate-annotation-string note))
            (ai-code--annotation-quote quote))))

(defun ai-code--annotation-source-report (source)
  "Render SOURCE with current live or disk text, without visiting a file.
Mark SOURCE as skipped if its notes cannot be rendered."
  (let* ((file (plist-get source :file))
         (buffer (plist-get source :buffer))
         (available-p (or (buffer-live-p buffer) (file-exists-p file)))
         (render (lambda ()
                   (save-restriction
                     (widen)
                     (mapconcat (lambda (note)
                                  (ai-code--annotation-source-note
                                   note (buffer-live-p buffer) available-p))
                                (plist-get source :notes) "\n")))))
    (concat "## Source file: " file "\n\n"
            (condition-case err
                (if (buffer-live-p buffer)
                    (with-current-buffer buffer (funcall render))
                  (with-temp-buffer
                    (when (file-exists-p file) (insert-file-contents file))
                    (funcall render)))
              (error (format "SKIPPED (unreadable annotation record: %s)\n"
                             (error-message-string err)))))))

;; Menu actions.  "All" means the current worktree, or the current file
;; outside Git; Magit buffers act on hunk notes, other buffers on annotate.el.

(defun ai-code--annotation-scope ()
  "Return (ROOT FILE): the worktree root, or nil outside Git, and this file."
  (require 'ai-code-utils)
  (list (when-let* ((directory (ai-code--git-root)))
          (file-name-as-directory (file-truename directory)))
        (and buffer-file-name (file-truename buffer-file-name))))

(defun ai-code--annotation-magit-notes (root)
  "Return saved Magit notes for ROOT."
  (and root
       (cl-remove-if-not (lambda (note) (equal root (plist-get note :repository)))
                         (ai-code--annotate-magit-read))))

(defun ai-code--annotation-report (root file)
  "Render every source and Magit note for ROOT, or FILE outside Git."
  (let ((sources (ai-code--annotation-sources root file))
        (hunks (and (ai-code--annotation-magit-notes root)
                    (ai-code-annotate-magit-review-string root))))
    (unless (or sources hunks)
      (user-error "No code annotations found for this %s" (if root "worktree" "file")))
    (concat (mapconcat #'ai-code--annotation-source-report sources "\n")
            (when hunks (concat "\n" hunks)))))

(defun ai-code--annotation-send (root report)
  "Ask AI for suggestions only on REPORT, gathered in ROOT.
Review, edit, or cancel the prompt in the compose buffer first."
  (require 'ai-code-prompt-mode)
  (require 'ai-code-compose)
  (let ((prompt
         (concat "Address code annotations: provide suggestions ONLY.\n"
                 "Repository: " (or root "outside Git; current file only") "\n\n"
                 "Focus on the listed annotations and their replies.\n"
                 "Each annotation is the primary review request; associated source excerpts\n"
                 "and diff snapshots are supporting context for understanding that request.\n"
                 "Do not perform a general code review or suggest unrelated changes.\n"
                 "Annotations and code context are not permission to edit.\n\n"
                 report
                 "\n\nInstructions: verify each note against current code and identify stale notes.\n"
                 "For each annotation, cite its ID and file, interpret its concern or question,\n"
                 "and consider replies before deciding whether it still needs action.\n"
                 "Use only the related code or code change to explain a focused response.\n"
                 "Explain reasons, risks, and any minimal proposed patch and tests that\n"
                 "directly address that annotation. Present proposals only.\n"
                 "Do NOT modify files, stage or commit changes, run mutating commands, or\n"
                 "resolve/delete annotations. Do NOT implement even if a note requests it.\n"
                 "Wait for the user to choose whether and which suggestions to implement.\n"))
        ;; Do not append generic edit/test suffixes.
        (ai-code-prompt-suffix-functions nil)
        (default-directory (or root default-directory)))
    ;; Always review in the compose buffer; cancelling sends nothing.
    (when-let* ((reviewed (ai-code-compose-read
                          (format "Review annotation prompt (%d characters)" (length prompt)) prompt)))
      ;; Bypass `ai-code--insert-prompt' (path rewriting, Org summary offer)
      ;; and send from the caller, which stays the session's source buffer.
      (ai-code--write-prompt-to-file-and-send reviewed))))

(defun ai-code--annotation-at-point ()
  "Return the source record holding the annotation at point and its replies."
  (unless buffer-file-name (user-error "Buffer is not visiting a file"))
  (unless (bound-and-true-p annotate-mode)
    (user-error "Enable annotate-mode to select a source annotation at point; or browse saved annotations"))
  (let ((notes (save-restriction
                 (widen)
                 (cl-remove-if-not
                  (lambda (note)
                    (<= (annotate-beginning-of-annotation note) (point)
                        (1- (annotate-ending-of-annotation note))))
                  (annotate-describe-annotations)))))
    (unless notes (user-error "No annotation at point"))
    (pcase-let* ((`(,root ,file) (ai-code--annotation-scope))
                 (source (cl-find file (ai-code--annotation-sources root file)
                                  :key (lambda (entry) (plist-get entry :file)) :test #'equal)))
      (plist-put (copy-sequence source) :notes
                 (ai-code--annotation-thread (plist-get source :notes) notes)))))

(defun ai-code--annotation-edit ()
  "Add or edit the annotation at point."
  (if (derived-mode-p 'magit-mode)
      (call-interactively #'ai-code-annotate-magit-annotate)
    (unless buffer-file-name (user-error "Visit a source file to add an annotation"))
    (ai-code--annotation-normalize-database (expand-file-name annotate-file))
    (when (and (not annotate-mode) (bound-and-true-p annotate-file-buffer-local))
      (ai-code--annotation-normalize-database
       (expand-file-name (annotate--filepath->local-database-name buffer-file-name))))
    (unless annotate-mode (annotate-mode 1))
    (call-interactively #'annotate-annotate)))

(defun ai-code--annotation-delete ()
  "Delete the annotation at point."
  (if (derived-mode-p 'magit-mode)
      (call-interactively #'ai-code-annotate-magit-delete)
    (ai-code--annotation-at-point)
    (ai-code--annotation-normalize-database (expand-file-name annotate-file))
    (unless (annotate-annotation-at) (user-error "No annotation at point"))
    (call-interactively #'annotate-delete-annotation)))

(defun ai-code--annotation-clear-all ()
  "Delete every annotation in scope after confirmation."
  (pcase-let* ((`(,root ,file) (ai-code--annotation-scope))
               (live (ai-code--annotation-live-buffers root file))
               (magit (ai-code--annotation-magit-notes root))
               (count (+ (length magit)
                         (cl-loop for source in (ai-code--annotation-sources root file)
                                  sum (length (plist-get source :notes))))))
    (when (zerop count)
      (user-error "No code annotations found for this %s" (if root "worktree" "file")))
    (when (yes-or-no-p (format "Delete all %d annotation%s in %s? "
                               count (if (= count 1) "" "s") (or root file)))
      (dolist (database (ai-code--annotation-databases live))
        (when (file-exists-p database)
          (let* ((annotate-file database)
                 (records (ai-code--annotation-read-database database))
                 (kept (cl-remove-if (lambda (record)
                                       (ai-code--annotation-file
                                        (annotate-filename-from-dump record) root file))
                                     records)))
            (unless (equal kept records)
              (ai-code--annotation-write-database database kept)))))
      (dolist (entry live)
        (with-current-buffer (cdr entry)
          (save-restriction (widen) (annotate-clear-annotations))))
      (when magit
        (ai-code--annotate-magit-write
         (cl-set-difference (ai-code--annotate-magit-read) magit :test #'equal))
        (ai-code--annotate-magit-refresh-repository root))
      (message "Deleted %d annotation%s" count (if (= count 1) "" "s")))))

(defun ai-code--annotation-view ()
  "Browse annotations in scope without sending anything."
  (require 'ai-code-annotation-list)
  (ai-code-annotation-browse))

(defun ai-code--annotation-send-current ()
  "Ask AI for suggestions on the annotation at point."
  (pcase-let ((`(,root ,_) (ai-code--annotation-scope)))
    (ai-code--annotation-send
     root
     (if (derived-mode-p 'magit-mode)
         (progn
           (unless (bound-and-true-p ai-code-annotate-magit-mode)
             (user-error "Enable ai-code-annotate-magit-mode to select a hunk note at point; or browse saved annotations"))
           (ai-code-annotate-magit-review-string
            root (or (ai-code--annotate-magit-at-point) (user-error "No annotation at point"))))
       (ai-code--annotation-source-report (ai-code--annotation-at-point))))))

(defun ai-code--annotation-send-all ()
  "Ask AI for suggestions on every annotation in scope."
  (pcase-let ((`(,root ,file) (ai-code--annotation-scope)))
    (ai-code--annotation-send root (ai-code--annotation-report root file))))

;;;###autoload
(defun ai-code-annotation-send-file ()
  "Ask AI for suggestions on source and hunk annotations for the current file."
  (interactive)
  (require 'ai-code-annotation-list)
  (ai-code--annotation-list-send-file buffer-file-name))

;;;###autoload
(defun ai-code-address-code-annotation ()
  "Choose an action for code annotations from a menu.
Act on hunk notes in Magit buffers and on annotate.el notes elsewhere.
\"All\" means the current worktree, or just the current file outside Git.
Sending opens the compose buffer for review and asks only for
suggestions; implementing any of them needs a separate request.
Require optional annotate.el 2.5.0 or newer."
  (interactive)
  (require 'ai-code-annotate-magit)
  (when-let* ((problem (ai-code--annotate-missing "address code annotations")))
    (user-error "%s" problem))
  (require 'ai-code-backends)
  (pcase-let* ((`(,root ,file) (ai-code--annotation-scope))
               (scope (if root "worktree" "current file"))
               (actions (mapcar (lambda (entry)
                                  (cons (format (car entry) scope) (cdr entry)))
                                ai-code--annotation-actions))
               (count (condition-case nil
                          (+ (length (ai-code--annotation-magit-notes root))
                             (cl-loop for source in (ai-code--annotation-sources root file)
                                      sum (length (plist-get source :notes))))
                        (user-error nil)))
               (mode (if (derived-mode-p 'magit-mode)
                         (bound-and-true-p ai-code-annotate-magit-mode)
                       (bound-and-true-p annotate-mode)))
               (choice (completing-read
                        (format "Annotation action (%s %s; %s; %s): "
                                scope (abbreviate-file-name (or root file default-directory))
                                (if count (format "%d annotations" count) "database unreadable")
                                (if mode "at point enabled" "at point mode off"))
                        (ai-code--backend-completion-table (mapcar #'car actions))
                        nil t nil 'ai-code--annotation-action-history)))
    (funcall (or (cdr (assoc choice actions))
                 (user-error "No annotation action selected")))))

(provide 'ai-code-annotation)
;;; ai-code-annotation.el ends here
