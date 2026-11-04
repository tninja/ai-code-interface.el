;;; ai-code-task-evidence.el --- Durable task evidence references -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; A lightweight, explicit task-to-evidence graph built from Org IDs and
;; existing human/agent links, without a new SPEC format or network service.
;; Relationship labels reflect user input, NOT source authentication.

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defconst ai-code-task-evidence--relations
  '(("Original human requirement" . source)
    ("Human decision" . decision)
    ("Observation / log" . observation)
    ("Implementation artifact" . implements)
    ("Verification evidence" . verified-by)
    ("AI-derived note (not authoritative)" . derived))
  "Task-to-evidence relationships, explicitly distinguishing raw and derived data.")

(defun ai-code-task-evidence--task-heading ()
  "Return current task heading, stepping out of generated evidence nodes."
  (unless (derived-mode-p 'org-mode)
    (user-error "Evidence links require an Org task buffer"))
  (save-excursion
    (when (org-before-first-heading-p)
      (user-error "Place point inside an Org task"))
    (org-back-to-heading t)
    (when (org-entry-get nil "AI_CODE_EVIDENCE_RELATION")
      (unless (org-up-heading-safe)
        (user-error "Evidence entry has no parent section")))
    (when (string= (org-get-heading t t t t) "Evidence Links")
      (unless (org-up-heading-safe)
        (user-error "Evidence Links has no parent task")))
    (point)))

(defun ai-code-task-evidence--find-section ()
  "Find the direct Evidence Links child under the selected task."
  (let ((start (ai-code-task-evidence--task-heading)))
    (save-excursion
      (goto-char start)
      (let ((level (org-outline-level))
            (limit (save-excursion (org-end-of-subtree t t) (point))))
        (catch 'found
          (while (and (outline-next-heading) (< (point) limit))
            (when (and (= (org-outline-level) (1+ level))
                       (string= (org-get-heading t t t t) "Evidence Links"))
              (throw 'found (point))))
          nil)))))

(defun ai-code-task-evidence--ensure-section ()
  "Return the task's Evidence Links heading, creating it if absent."
  (or (ai-code-task-evidence--find-section)
      (save-excursion
        (goto-char (ai-code-task-evidence--task-heading))
        (let ((stars (make-string (1+ (org-outline-level)) ?*)))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (let ((start (point)))
            (insert stars " Evidence Links\n")
            start)))))

(defun ai-code-task-evidence-entries ()
  "Read evidence relationships from the selected task's generated section.
Return plists with relation, reference, ID, label and position."
  (when (derived-mode-p 'org-mode)
    (when-let ((section (ai-code-task-evidence--find-section)))
      (save-excursion
        (goto-char section)
        (let ((level (org-outline-level))
              (limit (save-excursion (org-end-of-subtree t t) (point)))
              (entries nil))
          (while (and (outline-next-heading) (< (point) limit))
            (when (= (org-outline-level) (1+ level))
              (push (list :relation (org-entry-get nil "AI_CODE_EVIDENCE_RELATION")
                          :reference (org-entry-get nil "AI_CODE_EVIDENCE_REFERENCE")
                          :label (org-entry-get nil "AI_CODE_EVIDENCE_LABEL")
                          :id (org-entry-get nil "ID")
                          :position (point))
                    entries)))
          (nreverse entries))))))

(defun ai-code-task-evidence--valid-reference-p (reference)
  "Check allowed link forms of REFERENCE, without fetching it."
  (and (stringp reference)
       (not (string-match-p "[[:space:]]" reference))
       (or (string-match-p "^https?://[^/[:space:]]+/.+" reference)
           (string-match-p "^rdar://[0-9]+$" reference)
           (string-match-p "^file:.+" reference)
           (string-match-p "^id:[[:alnum:]-]+$" reference))))

(defun ai-code-task-evidence--safe-label (label)
  "Normalize single-line LABEL for Org heading and link display."
  (string-trim
   (replace-regexp-in-string "[\n\r\\[\\]]+" " " label)))

;;;###autoload
(defun ai-code-task-evidence-record (relation reference label)
  "Record REFERENCE of RELATION type with descriptive LABEL in the task.
RELATION is one of source, decision, observation, implements, verified-by,
or derived.  Source links are user-supplied and never authenticated here."
  (interactive
   (let* ((choice (completing-read
                   "Evidence relationship: "
                   (mapcar #'car ai-code-task-evidence--relations)
                   nil t))
          (relation (cdr (assoc choice ai-code-task-evidence--relations)))
          (reference (read-string "Original URI or Org id:/file: reference: "))
          (label (read-string "Description (human-written, concise): ")))
     (list relation reference label)))
  (ai-code-task-evidence--task-heading)
  (unless (memq relation (mapcar #'cdr ai-code-task-evidence--relations))
    (user-error "Unknown evidence relationship"))
  (unless (ai-code-task-evidence--valid-reference-p reference)
    (user-error "Expected an HTTPS/HTTP, Radar, file: or id: reference without whitespace"))
  (unless (and (stringp label)
               (not (string-blank-p (ai-code-task-evidence--safe-label label))))
    (user-error "A short description is required"))
  (when (seq-some
         (lambda (entry)
           (and (equal (plist-get entry :reference) reference)
                (equal (plist-get entry :relation) (symbol-name relation))))
         (ai-code-task-evidence-entries))
    (user-error "This relationship and source are already linked to the task"))
  (let ((section (ai-code-task-evidence--ensure-section))
        (safe-label (ai-code-task-evidence--safe-label label)))
    (save-excursion
      (goto-char section)
      (let ((stars (make-string (1+ (org-outline-level)) ?*)))
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (let ((entry (point)))
          (insert stars " " (symbol-name relation) ": " safe-label "\n")
          (goto-char entry)
          (org-id-get-create)
          (org-entry-put nil "AI_CODE_EVIDENCE_RELATION" (symbol-name relation))
          (org-entry-put nil "AI_CODE_EVIDENCE_REFERENCE" reference)
          (org-entry-put nil "AI_CODE_EVIDENCE_LABEL" safe-label)
          (org-entry-put nil "AI_CODE_EVIDENCE_PROVENANCE"
                         "user-classified-not-source-authenticated")
          (goto-char entry)
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert (format "[[%s][%s]]\n"
                          (org-link-escape reference) safe-label))))))
  (message "Evidence reference recorded (source was not authenticated)"))

(defun ai-code-task-evidence-audit-report ()
  "Return structural issues in the current task's evidence relationships.
External pages are NOT fetched, so an empty report is not proof of validity."
  (let* ((entries (ai-code-task-evidence-entries))
         (has-human (seq-some
                     (lambda (entry)
                       (member (plist-get entry :relation) '("source" "decision")))
                     entries))
         (implemented (seq-some
                       (lambda (entry)
                         (equal (plist-get entry :relation) "implements"))
                       entries))
         (verified (seq-some
                    (lambda (entry)
                      (equal (plist-get entry :relation) "verified-by"))
                    entries))
         (issues nil)
         (seen (make-hash-table :test 'equal)))
    (unless has-human
      (push "No original human source/decision is linked; AI-derived material is not authoritative." issues))
    (when (and implemented (not verified))
      (push "Implementation exists without linked verification evidence." issues))
    (dolist (entry entries)
      (let* ((relation (plist-get entry :relation))
             (reference (plist-get entry :reference))
             (id (plist-get entry :id))
             (key (cons relation reference)))
        (unless (and id reference relation)
          (push "Evidence node is missing an ID, relationship or URI." issues))
        (when (gethash key seen)
          (push (format "Duplicate relationship: %s %s" relation reference) issues))
        (puthash key t seen)
        (when (and reference
                   (not (ai-code-task-evidence--valid-reference-p reference)))
          (push (format "Invalid reference: %s" reference) issues))
        (when (and reference (string-prefix-p "file:" reference))
          (let ((path (expand-file-name
                       (substring reference 5)
                       (or (and buffer-file-name
                                (file-name-directory buffer-file-name))
                           default-directory))))
            (unless (file-exists-p path)
              (push (format "Missing local file: %s" reference) issues))))
        (when (and reference (string-prefix-p "id:" reference))
          (let ((target-id (substring reference 3)))
            (unless (or (save-excursion
                          (goto-char (point-min))
                          (re-search-forward
                           (concat "^[[:space:]]*:ID:[[:space:]]+"
                                   (regexp-quote target-id) "[[:space:]]*$")
                           nil t))
                        (condition-case nil
                            (org-id-find target-id t)
                          (error nil)))
              (push (format "Unresolved Org ID: %s" reference) issues))))))
    (nreverse issues)))

;;;###autoload
(defun ai-code-task-evidence-audit ()
  "Inspect the current task's evidence graph without changing any file."
  (interactive)
  (let ((issues (ai-code-task-evidence-audit-report))
        (name (or (and (derived-mode-p 'org-mode)
                       (save-excursion
                         (goto-char (ai-code-task-evidence--task-heading))
                         (org-get-heading t t t t)))
                  "task")))
    (with-current-buffer (get-buffer-create "*AI Task Evidence Audit*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Task: " name "\n\n")
        (if issues
            (dolist (issue issues)
              (insert "- " issue "\n"))
          (insert "No structural issues found.\n"))
        (insert "\nExternal sources were not fetched or authenticated; "
                "no result here proves requirement coverage.\n")
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer "*AI Task Evidence Audit*")))

(provide 'ai-code-task-evidence)
;;; ai-code-task-evidence.el ends here
