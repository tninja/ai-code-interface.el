;;; ai-code-prompt-editing.el --- Editing support for prompt task files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Editing support for AI prompt task files, for people who write their
;; prompts in Emacs rather than in a chat box.
;;
;; Four concerns, in dependency order:
;;
;; Reference gating
;;   Prompt files routinely embed pasted source code, where "@" is a Java
;;   annotation rather than a file reference.  Firing a completion prompt on
;;   every "@" makes pasting code hostile, so completion is offered only at
;;   positions that can plausibly start a reference.
;;
;; Navigation
;;   Task files grow to thousands of lines with headings seven levels deep,
;;   so plain Org navigation is not enough: outline-path jumping, subtree
;;   focus, and per-file startup folding that leaves the global
;;   `org-startup-folded' setting alone.
;;
;; Writing
;;   Task files mix prose with pasted code and logs.  Keep the spell checker
;;   off the pasted parts, make wrapping pasted text in a block a single
;;   command, and offer an opt-in prose-friendly layout.
;;
;; Reuse
;;   Accumulated task files are the most useful prompt corpus available, but
;;   nothing reads them back.  Search them, offer snippets by name rather
;;   than by key abbreviation, and expose small context helpers for
;;   parameterising a prompt with the current file, branch, or function.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(declare-function ai-code--get-files-directory "ai-code-utils" ())
(declare-function ai-code--git-root "ai-code-git" (&optional dir))
(declare-function ai-code--prompt-filepath-candidates "ai-code-prompt-mode" ())
(declare-function company-mode "company" (&optional arg))
(declare-function helm-imenu "helm-imenu" ())
(declare-function magit-get-current-branch "magit-git" ())
(declare-function visual-fill-column-mode "visual-fill-column" (&optional arg))
(declare-function which-function "which-func" ())
(declare-function yas-expand-snippet "yasnippet" (snippet &optional start end expand-env))

(defvar ai-code-files-dir-name)
(defvar company-backends)


;;;; Reference gating

(defconst ai-code--prompt-block-begin-regexp
  "^[ \t]*#\\+begin_\\(src\\|example\\|quote\\|export\\|verse\\)\\_>"
  "Regexp matching the start of an Org block whose body is literal text.")

(defconst ai-code--prompt-block-end-regexp
  "^[ \t]*#\\+end_\\(src\\|example\\|quote\\|export\\|verse\\)\\_>"
  "Regexp matching the end of an Org block whose body is literal text.")

(defun ai-code--prompt-inside-literal-block-p (&optional pos)
  "Return non-nil when POS sits inside a literal Org block.
POS defaults to point.  Detection is textual rather than based on
`org-in-src-block-p' so that it also works in buffers that are not
derived from `org-mode', such as the temporary buffers used by tests."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (or pos (point)))
      (forward-line 0)
      (let ((case-fold-search t))
        ;; The nearest preceding delimiter decides: a begin line means the
        ;; position is enclosed, an end line means it is not.
        (and (re-search-backward
              (concat ai-code--prompt-block-begin-regexp
                      "\\|" ai-code--prompt-block-end-regexp)
              nil t)
             (looking-at-p ai-code--prompt-block-begin-regexp))))))

(defconst ai-code--prompt-reference-word-char-regexp "[[:alnum:]_]"
  "Regexp matching a character that glues \"@\" into a surrounding word.
Spelled as an explicit character class rather than a syntax-table query so
that gating behaves identically in buffers not derived from `org-mode'.")

(defun ai-code--prompt-reference-position-p (&optional sigil-pos)
  "Return non-nil when SIGIL-POS starts a file reference rather than prose.
SIGIL-POS is the position of the \"@\" character and defaults to the
character just before point, which is where the auto-trigger hook runs
immediately after \"@\" is typed.

A position qualifies when it is outside any literal Org block and the
\"@\" begins a word, so pasted annotations like \"@Override\" and address
fragments like \"user@\" are both rejected."
  (let ((sigil-pos (or sigil-pos (1- (point)))))
    (and (>= sigil-pos (point-min))
         (not (ai-code--prompt-inside-literal-block-p sigil-pos))
         (or (= sigil-pos (point-min))
             (not (string-match-p
                   ai-code--prompt-reference-word-char-regexp
                   (buffer-substring-no-properties (1- sigil-pos)
                                                   sigil-pos)))))))

(defconst ai-code--prompt-reference-chars "A-Za-z0-9_./-"
  "Character set that may follow \"@\" inside a file reference.")

(defvar helm-mode-no-completion-in-region-in-modes nil
  "Modes in which `helm-mode' leaves `completion-in-region' alone.
Defined here as well as in helm so that the opt-out can be registered
before helm loads; helm reuses this value when it loads later.")

;;;###autoload
(defun ai-code-prompt-complete-reference ()
  "Select a repository file and insert it as an @reference at point.
Unlike the automatic trigger, this command is explicit, so it works
everywhere including inside source blocks, where auto-triggering is
deliberately suppressed."
  (interactive)
  (unless (ai-code--git-root)
    (user-error "Not inside a Git repository"))
  (let ((candidates (ai-code--prompt-filepath-candidates)))
    (unless candidates
      (user-error "No candidate files found"))
    (let ((choice (completing-read "File: " candidates nil nil)))
      (when (and choice (not (string-empty-p choice)))
        ;; Replace an already-typed partial reference so the command is
        ;; usable both before and after "@" has been entered.
        (let ((start (save-excursion
                       (skip-chars-backward ai-code--prompt-reference-chars)
                       (if (eq (char-before) ?@) (1- (point)) (point)))))
          (when (< start (point))
            (delete-region start (point))))
        (insert choice)))))

(defun ai-code--prompt-opt-out-of-helm-completion-in-region ()
  "Register `ai-code-prompt-mode' in helm's opt-out list for that completion.
Tolerates the option being unbound, because helm may not have loaded yet
and aborting here would prevent the major mode from activating at all."
  (unless (boundp 'helm-mode-no-completion-in-region-in-modes)
    (setq helm-mode-no-completion-in-region-in-modes nil))
  (add-to-list 'helm-mode-no-completion-in-region-in-modes 'ai-code-prompt-mode))

(defun ai-code--prompt-setup-inline-completion ()
  "Prefer inline, non-blocking completion in the current prompt buffer.
`helm-mode' otherwise routes `completion-in-region' through a blocking
window; helm supports opting a mode out by name, which leaves helm's
behavior in every other buffer untouched.  Company, when available, is
enabled buffer-locally so `global-company-mode' stays off."
  ;; Use helm's documented opt-out rather than advising helm internals.
  (ai-code--prompt-opt-out-of-helm-completion-in-region)
  ;; Fuzzy matching is worth having for long repository-relative paths.
  (unless (memq 'flex completion-styles)
    (setq-local completion-styles (append completion-styles '(flex))))
  (when (fboundp 'company-mode)
    ;; Only capf, so company offers exactly the @reference candidates.
    (setq-local company-backends '(company-capf))
    (company-mode 1)))


;;;; Navigation

(defgroup ai-code-prompt-nav nil
  "Navigation inside AI prompt task files."
  :group 'ai-code
  :prefix "ai-code-prompt-")

(defcustom ai-code-prompt-startup-folded 'content
  "How AI prompt task files are folded when first opened.
Only task files are affected, so the global `org-startup-folded' setting
keeps applying to every other Org buffer.

`content' shows all headings and hides body text, `overview' shows only
top-level headings, and nil disables folding entirely."
  :type '(choice (const :tag "Show all headings" content)
                 (const :tag "Show top-level headings only" overview)
                 (const :tag "Do not fold" nil))
  :group 'ai-code-prompt-nav)

(defcustom ai-code-prompt-focus-style 'narrow
  "How `ai-code-prompt-focus-subtree' isolates a subtree.
`narrow' restricts the current buffer, while `indirect' opens the subtree
in a separate indirect buffer and leaves the original untouched."
  :type '(choice (const :tag "Narrow the current buffer" narrow)
                 (const :tag "Open an indirect buffer" indirect))
  :group 'ai-code-prompt-nav)

(defun ai-code--prompt-heading-candidates ()
  "Return an alist of (OUTLINE-PATH-LABEL . POSITION) for every heading.
The label is the full outline path, so headings that share a short name at
different depths stay distinguishable."
  (let (candidates)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let* ((path (append (org-get-outline-path)
                                (list (org-get-heading t t t t))))
                  (label (mapconcat #'identity (delq nil path) " / ")))
             (push (cons label (point)) candidates))))))
    (nreverse candidates)))

;;;###autoload
(defun ai-code-prompt-goto-heading ()
  "Jump to a heading chosen by its full outline path.
Use `helm-imenu' when helm is available, since it offers live narrowing
over the same Org outline; otherwise fall back to `completing-read'."
  (interactive)
  (if (and (fboundp 'helm-imenu) (not noninteractive))
      (helm-imenu)
    (let ((candidates (ai-code--prompt-heading-candidates)))
      (unless candidates
        (user-error "No headings in this buffer"))
      (let* ((label (completing-read "Heading: " candidates nil t))
             (position (cdr (assoc label candidates))))
        (unless position
          (user-error "No heading matches %s" label))
        (widen)
        (goto-char position)
        (when (fboundp 'org-fold-show-context)
          (ignore-errors (org-fold-show-context 'org-goto)))))))

;;;###autoload
(defun ai-code-prompt-focus-subtree ()
  "Focus the subtree at point, or restore the full view when already focused.
The behavior is controlled by `ai-code-prompt-focus-style'."
  (interactive)
  (cond
   ;; Already focused, so this invocation restores the full buffer.
   ((buffer-narrowed-p) (widen))
   ((eq ai-code-prompt-focus-style 'indirect)
    (org-tree-to-indirect-buffer))
   (t (org-narrow-to-subtree))))

(defun ai-code--prompt-task-directory-name ()
  "Return the directory name that identifies AI prompt task files."
  (if (boundp 'ai-code-files-dir-name)
      ai-code-files-dir-name
    ".ai.code.files"))

(defun ai-code--prompt-task-file-p (&optional file)
  "Return non-nil when FILE lives under the AI prompt task directory.
FILE defaults to the current buffer's file name."
  (let ((file (or file buffer-file-name)))
    (and file
         (string-match-p (concat "/" (regexp-quote
                                      (ai-code--prompt-task-directory-name))
                                 "/")
                         file)
         t)))

(defun ai-code--prompt-apply-startup-folding ()
  "Fold the current buffer when it is a task file and folding is enabled."
  (when (and ai-code-prompt-startup-folded
             (ai-code--prompt-task-file-p))
    (pcase ai-code-prompt-startup-folded
      ('overview (org-overview))
      (_ (org-content)))))


;;;; Writing

(defgroup ai-code-prompt-writing nil
  "Long-form writing support for AI prompt files."
  :group 'ai-code
  :prefix "ai-code-prompt-")

(defcustom ai-code-prompt-writing-fill-column 90
  "Text width used by `ai-code-prompt-writing-mode'.
Applies only when `visual-fill-column-mode' is available; the underlying
buffer text is never re-wrapped, so prompts sent to an agent are
unaffected."
  :type 'integer
  :group 'ai-code-prompt-writing)

(defun ai-code--prompt-flyspell-verify ()
  "Return non-nil when the word at point should be spell checked.
Words inside literal Org blocks are code, logs, or transcripts rather than
prose, so checking them produces only noise."
  (not (ai-code--prompt-inside-literal-block-p)))

;;;###autoload
(defun ai-code-prompt-wrap-region-in-block (type)
  "Wrap the active region in an Org block of TYPE.
TYPE is the text following `#+begin_', so \"example\" or \"src java\" are
both valid.  Only the first word is repeated in the closing delimiter."
  (interactive
   (list (completing-read "Block type: "
                          '("example" "quote" "src" "src java" "src python"
                            "src emacs-lisp" "src sh")
                          nil nil nil nil "example")))
  (unless (use-region-p)
    (user-error "No active region to wrap"))
  (let* ((start (region-beginning))
         (end (region-end))
         ;; The closing delimiter names the block, never its arguments.
         (keyword (car (split-string type " " t))))
    (save-excursion
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert (format "#+end_%s\n" keyword))
      (goto-char start)
      (forward-line 0)
      (insert (format "#+begin_%s\n" type)))))

;;;###autoload
(define-minor-mode ai-code-prompt-writing-mode
  "Minor mode for comfortable long-form prompt writing.
Wraps long lines visually and, when `visual-fill-column-mode' is
available, centres the text at `ai-code-prompt-writing-fill-column'.  Only
the display changes; buffer text is never modified."
  :init-value nil
  :lighter " AIWrite"
  :group 'ai-code-prompt-writing
  (if ai-code-prompt-writing-mode
      (progn
        (visual-line-mode 1)
        (when (fboundp 'visual-fill-column-mode)
          (setq-local visual-fill-column-width ai-code-prompt-writing-fill-column)
          (visual-fill-column-mode 1)))
    (visual-line-mode -1)
    (when (fboundp 'visual-fill-column-mode)
      (visual-fill-column-mode -1))))


;;;; Reuse

(defgroup ai-code-prompt-reuse nil
  "Reuse of existing prompts and snippets."
  :group 'ai-code
  :prefix "ai-code-prompt-")

(defcustom ai-code-prompt-corpus-additional-roots nil
  "Extra task directories to search with a prefix argument.
Each entry is a directory holding task Org files, typically the
`.ai.code.files' directory of another repository."
  :type '(repeat directory)
  :group 'ai-code-prompt-reuse)

(defcustom ai-code-prompt-corpus-min-length 25
  "Minimum length of a corpus line offered for reuse.
Short lines are usually fragments rather than reusable prompts."
  :type 'integer
  :group 'ai-code-prompt-reuse)

(defun ai-code--prompt-corpus-roots (&optional all)
  "Return the task directories to search, most relevant first.
Search only the current repository unless ALL is non-nil, in which case
`ai-code-prompt-corpus-additional-roots' is included as well."
  (let ((roots (list (ai-code--get-files-directory))))
    (when all
      (setq roots (append roots ai-code-prompt-corpus-additional-roots)))
    (delete-dups (delq nil roots))))

(defun ai-code--prompt-corpus-files (roots)
  "Return every task Org file under ROOTS."
  (cl-loop for root in roots
           when (and root (file-directory-p root))
           append (directory-files-recursively root "\\.org\\'")))

(defun ai-code--prompt-corpus-candidate-p (line)
  "Return non-nil when LINE is worth offering as a reusable prompt.
Headings, comments, keywords, block delimiters, and drawers describe the
document rather than the prompt, so they are filtered out."
  (and (>= (length line) ai-code-prompt-corpus-min-length)
       (not (string-match-p "\\`[ \t]*\\*+ " line))
       (not (string-match-p "\\`[ \t]*#" line))
       (not (string-match-p "\\`[ \t]*:" line))))

(defun ai-code--prompt-corpus-lines (roots)
  "Return deduplicated reusable prompt lines from the task files under ROOTS."
  (let (lines)
    (dolist (file (ai-code--prompt-corpus-files roots))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (string-trim
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))))
              (when (ai-code--prompt-corpus-candidate-p line)
                (push line lines)))
            (forward-line 1)))))
    (delete-dups (nreverse lines))))

;;;###autoload
(defun ai-code-prompt-insert-from-history (arg)
  "Insert a previously written prompt, chosen from past task files, at point.
With a prefix argument ARG, widen the search to
`ai-code-prompt-corpus-additional-roots' as well as the current repository."
  (interactive "P")
  (let* ((roots (ai-code--prompt-corpus-roots arg))
         (lines (ai-code--prompt-corpus-lines roots)))
    (unless lines
      (user-error "No reusable prompts found in %s"
                  (mapconcat #'identity roots ", ")))
    (let ((choice (completing-read "Previous prompt: " lines nil t)))
      (when (and choice (not (string-empty-p choice)))
        (insert choice)))))

(defun ai-code--prompt-snippet-directory ()
  "Return the directory holding the prompt-mode snippets, or nil."
  (when-let* ((library (locate-library "ai-code"))
              (dir (expand-file-name
                    "snippets/ai-code-prompt-mode"
                    (file-name-directory (file-truename library)))))
    (and (file-directory-p dir) dir)))

(defun ai-code--prompt-parse-snippet-file (file)
  "Return a (NAME . KEY) pair parsed from snippet FILE, or nil.
Metadata comes from the yasnippet header comments, so no yasnippet
internals are needed to list the library."
  (when (file-readable-p file)
    (with-temp-buffer
      ;; The header is short; reading the whole file would be wasteful.
      (insert-file-contents file nil 0 2048)
      (let ((name nil) (key nil))
        (goto-char (point-min))
        (when (re-search-forward "^# *name: *\\(.*\\)$" nil t)
          (setq name (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^# *key: *\\(.*\\)$" nil t)
          (setq key (string-trim (match-string 1))))
        ;; Fall back to the file name, which is the key by convention.
        (unless key (setq key (file-name-nondirectory file)))
        (when (and name (not (string-empty-p name)))
          (cons name key))))))

(defun ai-code--prompt-snippet-candidates ()
  "Return an alist of (NAME . KEY) for every prompt-mode snippet."
  (when-let* ((dir (ai-code--prompt-snippet-directory)))
    (let (candidates)
      (dolist (file (directory-files dir t "\\`[^.]"))
        (when (file-regular-p file)
          (when-let* ((parsed (ai-code--prompt-parse-snippet-file file)))
            (push (cons (format "%s (%s)" (car parsed) (cdr parsed))
                        (cdr parsed))
                  candidates))))
      (sort candidates (lambda (a b) (string< (car a) (car b)))))))

;;;###autoload
(defun ai-code-prompt-insert-snippet ()
  "Insert a prompt snippet chosen by its descriptive name.
Snippet keys are terse, so browsing by name is the discoverable path into
the snippet library."
  (interactive)
  (let ((candidates (ai-code--prompt-snippet-candidates)))
    (unless candidates
      (user-error "No prompt snippets found"))
    (let* ((label (completing-read "Snippet: " candidates nil t))
           (key (cdr (assoc label candidates))))
      (unless key
        (user-error "No snippet matches %s" label))
      (insert key)
      ;; Expand through yasnippet when available, else leave the key so the
      ;; user can expand it manually.
      (when (fboundp 'yas-expand)
        (ignore-errors (funcall (intern "yas-expand")))))))

(defun ai-code--prompt-ctx-file ()
  "Return the current buffer's repository-relative file name, or nil."
  (when-let* ((file buffer-file-name))
    (let ((root (ignore-errors (ai-code--git-root))))
      (if root
          (file-relative-name file root)
        (file-name-nondirectory file)))))

(defun ai-code--prompt-ctx-branch ()
  "Return the current Git branch name, or nil when unavailable."
  (ignore-errors (magit-get-current-branch)))

(defun ai-code--prompt-ctx-defun ()
  "Return the name of the function surrounding point, or nil."
  (and (require 'which-func nil t)
       (ignore-errors (which-function))))

(defun ai-code--prompt-ctx-diff ()
  "Return the staged diff of the current repository, or nil when empty."
  (when-let* ((root (ignore-errors (ai-code--git-root))))
    (let ((default-directory root))
      (let ((diff (ignore-errors
                    (with-output-to-string
                      (with-current-buffer standard-output
                        (call-process "git" nil t nil "diff" "--staged"))))))
        (and diff (not (string-empty-p (string-trim diff))) diff)))))

(provide 'ai-code-prompt-editing)

;;; ai-code-prompt-editing.el ends here
