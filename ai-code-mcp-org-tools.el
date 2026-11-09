;;; ai-code-mcp-org-tools.el --- Read-only Org context for MCP -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Let coding agents inspect the exact human-authored Org headline at the
;; prompt origin.  This tool never edits an Org buffer or generates summaries.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'subr-x)
(require 'ai-code-mcp-common)

(declare-function ai-code-mcp-get-session-context "ai-code-mcp-server")
(declare-function ai-code-mcp-make-tool "ai-code-mcp-server")

(defconst ai-code-mcp-org--reference-regexp
  "\\(?:https?://[^][ \t\n<>]+\\|rdar://\\(?:problem/\\)?[0-9]+\\)"
  "Regexp matching literal URLs and Radar links in raw Org text.")

(defun ai-code-mcp-org--references (start end)
  "Collect unique literal URLs and Radar links between START and END.
Scan the buffer rather than returned text so truncation cannot cut a
reference into a different one."
  (save-excursion
    (goto-char start)
    (let (links)
      (while (re-search-forward ai-code-mcp-org--reference-regexp end t)
        (push (match-string-no-properties 0) links))
      (vconcat (delete-dups (nreverse links))))))

(defun ai-code-mcp-org-heading-context (&optional buffer-name line include-children
                                                  max-chars)
  "Return human-authored Org heading context in BUFFER-NAME at LINE.
Default to the captured MCP prompt-origin position.  INCLUDE-CHILDREN
opt-in expands to the full subtree, while MAX-CHARS limits raw text."
  (let* ((context (ai-code-mcp-get-session-context))
         (source (plist-get context :source-buffer))
         (snapshot (plist-get context :source-snapshot))
         (buffer (cond
                  (buffer-name
                   (or (get-buffer buffer-name)
                       (user-error "No such buffer: %s" buffer-name)))
                  ((buffer-live-p source) source)
                  ((not context) (current-buffer))))
         (limit (or max-chars 16000))
         (children (and include-children
                        (not (memq include-children '(:json-false :false)))))
         (use-snapshot (and (not line) (not buffer-name)
                            (eq buffer source) snapshot)))
    (unless (and (integerp limit) (> limit 0))
      (user-error "max_chars must be a positive integer"))
    (when (and line (not (and (integerp line) (> line 0))))
      (user-error "line must be a positive integer"))
    (if (not buffer)
        (json-encode
         '((ok . :json-false)
           (status . "unavailable")
           (reason . "Prompt source buffer is no longer live")))
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-mode)
          (user-error "Expected an Org buffer, got %s" major-mode))
        (save-excursion
          (save-restriction
            (widen)
            (cond
             (line
              (goto-char (point-min))
              (unless (zerop (forward-line (1- line)))
                (user-error "Line %d is outside the Org buffer" line)))
             (use-snapshot
              (goto-char (min (point-max)
                              (plist-get snapshot :point)))))
            (when (org-before-first-heading-p)
              (user-error "No Org heading at the requested position"))
            (org-back-to-heading t)
            (let* ((heading-line (line-number-at-pos))
                   (heading (org-get-heading t t t t))
                   (todo (org-get-todo-state))
                   (tags (org-get-tags nil t))
                   ;; No USE-CACHE: `org-outline-path-cache' is global and
                   ;; keyed only by position, so it leaks across requests.
                   (parents (org-get-outline-path))
                   (properties (org-entry-properties nil 'standard))
                   (start (point))
                   (subtree-end (save-excursion
                                  (org-end-of-subtree t t)
                                  (point)))
                   (end (if children
                            subtree-end
                          (save-excursion
                            (outline-next-heading)
                            (min (point) subtree-end))))
                   (truncated (> (- end start) limit))
                   (output (buffer-substring-no-properties
                            start (min end (+ start limit))))
                   (stale (and use-snapshot
                               (integerp (plist-get snapshot :modification-tick))
                               (/= (plist-get snapshot :modification-tick)
                                   (buffer-chars-modified-tick)))))
              (json-encode
               `((ok . t)
                 (buffer_name . ,(buffer-name buffer))
                 (file_path . ,(buffer-file-name buffer))
                 (heading . ,heading)
                 (line . ,heading-line)
                 (todo . ,todo)
                 (tags . ,(vconcat tags))
                 (parent_headings . ,(vconcat parents))
                 (properties . ,properties)
                 (include_children . ,(ai-code-mcp--json-bool children))
                 (modification_tick . ,(buffer-chars-modified-tick))
                 (snapshot_stale . ,(ai-code-mcp--json-bool stale))
                 (raw_org . ,output)
                 (truncated . ,(ai-code-mcp--json-bool truncated))
                 (references . ,(ai-code-mcp-org--references start end)))))))))))

(defun ai-code-mcp-org-tools-setup ()
  "Register read-only Org headline inspection."
  (ai-code-mcp-make-tool
   :function #'ai-code-mcp-org-heading-context
   :name "get_org_heading_context"
   :description "Read the exact human-authored Org heading and raw references at the MCP prompt origin, without editing or summarizing.  References cover the whole section even when raw text is truncated; snapshot_stale flags edits since prompt capture."
   :args '((:name "buffer_name"
            :type string
            :description "Optional live Org buffer name, defaults to prompt source."
            :optional t)
           (:name "line"
            :type integer
            :description "Optional 1-based line overriding the captured prompt point."
            :optional t)
           (:name "include_children"
            :type boolean
            :description "Include nested headings; false by default."
            :optional t)
           (:name "max_chars"
            :type integer
            :description "Positive maximum raw Org characters, default 16000."
            :optional t))
   :category 'core
   :annotations
   '((readOnlyHint . t)
     (destructiveHint . :json-false)
     (idempotentHint . t)
     (openWorldHint . :json-false))))

(add-to-list 'ai-code-mcp-server-tool-setup-functions
             #'ai-code-mcp-org-tools-setup)

(provide 'ai-code-mcp-org-tools)

;;; ai-code-mcp-org-tools.el ends here
