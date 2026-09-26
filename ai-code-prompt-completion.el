;;; ai-code-prompt-completion.el --- Reuse hand-written prompts -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Completion source over prompts you wrote by hand, harvested from every
;; `ai-code-prompt-file-name' history file this machine knows about.  Prompts
;; that ai-code generated itself are dropped by prefix, so the candidates are
;; the wording you would otherwise retype.  Point
;; `ai-code-prompt-completion-files' at documents of your own -- a prompt
;; library you keep by hand, a diary, any Org file whose wording you reuse --
;; to complete from every headline and the text under it as well, and set
;; `ai-code-prompt-completion-use-org-roam' when those documents are notes
;; org-roam tracks.
;;
;; The capf is installed in `ai-code-prompt-mode' buffers on load, so
;; `completion-at-point' (M-TAB) offers your earlier prompts with no setup
;; at all.  When cape happens to be installed it is loaded there too and
;; its dictionary words join the same candidate list.
;;
;; Choosing the popup front-end is left to you.  For `company':
;;
;;   (add-hook 'ai-code-prompt-mode-hook #'ai-code-prompt-completion-setup)
;;
;; `cape' and `company' are optional: neither is required by this package,
;; and neither is loaded when it is not installed.  When you already merge
;; capfs with cape, add `ai-code-prompt-completion-capf' to your own
;; `cape-wrap-super' list -- a capf your config installed at the same hook
;; depth answers first and this one never gets a turn.
;;
;; The capf matches the word before point, the same bounds `cape-dict' uses,
;; because `cape-capf-super' silently drops capfs whose start position
;; disagrees with the first one.
;;
;; A prompt is recalled from any word in its first line, not only from the
;; opening one: every word start is indexed as a candidate of its own, and
;; choosing one inserts the whole prompt.  Picking such a candidate rewrites
;; the line from its start whenever what you typed there is the opening of
;; that same prompt, so a prompt you were halfway through retyping does not
;; end up with its first words doubled.
;;
;; Indexing word starts rather than matching non-prefix is deliberate.  The
;; capf is non-exclusive so it never shadows `cape-dict', the `@file' capf or
;; Copilot, and `completion--capf-wrapper' only offers a non-exclusive capf
;; once plain prefix completion could succeed.  Making the candidates
;; prefix-matchable keeps that guarantee; a `flex' style, where the user has
;; one, then matches the rest of the candidate as a bonus.
;;
;; The index is built on first use and cached for the rest of the session.
;; Run `ai-code-prompt-completion-refresh' to pick up prompts written since.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-utils)
(require 'ai-code-prompt-mode)

(declare-function cape-wrap-super "cape" (&rest capfs))
(declare-function cape-dict "cape" (&optional interactive))
(declare-function company-mode "company" (&optional arg))
(declare-function org-roam-list-files "org-roam" ())

;;;###autoload
(defcustom ai-code-prompt-completion-template-prefixes
  '("Goal:"
    "Review pull request"
    "Check unresolved feedback"
    "How to fix the error in this code?"
    "Please implement code for this Org headline"
    "Create a pull request from branch")
  "Prefixes marking a stored prompt as generated rather than hand-written.
History entries starting with any of these strings are left out of
`ai-code-prompt-completion-capf' candidates, because ai-code can already
regenerate them on demand.  Matching ignores case.  Add a prefix here
when a command starts producing prompts you do not want offered back."
  :type '(repeat string)
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-completion-max-lines 5
  "Longest stored prompt, in lines, still offered as a candidate.
Longer history entries are one-off pastes such as stack traces, which
are noise in a completion popup."
  :type 'integer
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-completion-roots nil
  "Extra directories searched for prompt history files.
Each directory is checked for `ai-code-prompt-file-name' directly and
under `ai-code-files-dir-name'.  When `projectile' is loaded its known
projects are searched too, so this option is only needed for
repositories projectile does not track."
  :type '(repeat directory)
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-completion-files nil
  "Extra files to complete prompts from, beside the recorded history.
Point this at a prompt library you keep by hand, at a diary, at any Org
file whose wording you reuse.  Every headline in such a file offers two
prompts, however deep it sits: the headline itself and the text written
under it.  Whatever stands before the first headline, which is the whole
of a file written without any, is split on blank lines.  None of the
history filters apply here -- you wrote these files to complete from --
so a prompt is offered whatever it starts with or how long it runs, save
for a heading of a word or two, quicker typed than picked from a popup.
Remote (Tramp) files are skipped."
  :type '(repeat file)
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-completion-use-org-roam nil
  "Whether the notes org-roam tracks join the prompt candidates.
They are read like `ai-code-prompt-completion-files': every headline and
the text under it, nothing filtered out.  That suits a note written as a
prompt library and not a whole zettelkasten, where the sections are
prose and there can be thousands of them, which is why this is off by
default.  Turning it on loads org-roam, when installed, to ask it which
notes it tracks."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-completion-enable-company t
  "Whether `ai-code-prompt-completion-setup' turns `company-mode' on.
Set this to nil when another front-end such as corfu displays the
candidates, or when you prefer to call `completion-at-point' by hand.
Company is optional either way: it is loaded only if installed."
  :type 'boolean
  :group 'ai-code)

(defconst ai-code-prompt-completion--display-width 60
  "Width at which a candidate's first line is truncated in the popup.")
(defvar ai-code-prompt-completion--index nil
  "Cached index as (CANDIDATES EXPANSIONS FILE-COUNT), or nil when unbuilt.
CANDIDATES is the list of popup strings, EXPANSIONS a hash table mapping
each popup string back to the full prompt text it abbreviates.")

(defun ai-code-prompt-completion--roots ()
  "Return directories that may hold a prompt history file."
  (delete-dups
   (append (when (and (boundp 'projectile-known-projects)
                      (listp (symbol-value 'projectile-known-projects)))
             (copy-sequence (symbol-value 'projectile-known-projects)))
           ai-code-prompt-completion-roots
           (list default-directory))))

(defun ai-code-prompt-completion--files ()
  "Return readable prompt history files across all known roots.
Remote roots are skipped: probing one would open a Tramp connection
from inside a completion callback."
  (let ((directories (append (ai-code-prompt-completion--roots)
                             (when ai-code-prompt-fallback-directory
                               (list ai-code-prompt-fallback-directory))))
        files)
    (dolist (root directories)
      (when (and (stringp root) (not (file-remote-p root)))
        (dolist (file (list (expand-file-name ai-code-prompt-file-name root)
                            (expand-file-name
                             ai-code-prompt-file-name
                             (expand-file-name ai-code-files-dir-name root))))
          (when (file-readable-p file)
            (push (file-truename file) files)))))
    (delete-dups (nreverse files))))

(defun ai-code-prompt-completion--org-roam-files ()
  "Return the org-roam notes to complete from, if any.
Org-roam is asked only when it is installed, and a broken install is
ignored rather than allowed to break completion."
  (when (and ai-code-prompt-completion-use-org-roam
             (fboundp 'org-roam-list-files))
    (ignore-errors (org-roam-list-files))))

(defun ai-code-prompt-completion--extra-files ()
  "Return the readable files completed from beside the recorded history.
Those are `ai-code-prompt-completion-files' and the org-roam notes.
Remote files are skipped for the same reason remote roots are."
  (let (files)
    (dolist (file (append ai-code-prompt-completion-files
                          (ai-code-prompt-completion--org-roam-files)))
      (when (and (stringp file)
                 (not (file-remote-p file))
                 (file-readable-p file))
        (push (file-truename file) files)))
    (delete-dups (nreverse files))))

(defun ai-code-prompt-completion--strip-drawer (text)
  "Return TEXT without a leading Org property drawer."
  (if (string-match "\\`[ \t\n]*:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n" text)
      (substring text (match-end 0))
    text))

(defun ai-code-prompt-completion--generated-p (text)
  "Return non-nil when TEXT starts with a known generated-prompt prefix."
  (cl-some (lambda (prefix)
             (and (stringp prefix)
                  (not (string-empty-p prefix))
                  (string-prefix-p prefix text t)))
           ai-code-prompt-completion-template-prefixes))

(defun ai-code-prompt-completion--reusable-entry (body)
  "Return trimmed BODY when it is a reusable hand-written prompt, else nil."
  (let ((text (string-trim (ai-code-prompt-completion--strip-drawer body))))
    (unless (or (string-empty-p text)
                (> (length (split-string text "\n"))
                   ai-code-prompt-completion-max-lines)
                (ai-code-prompt-completion--generated-p text))
      text)))

(defconst ai-code-prompt-completion--timestamp-headline-regexp
  "\\`[[<][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regexp matching the Org timestamp headline ai-code writes per prompt.")

(defun ai-code-prompt-completion--recorded-entry-p (headline body)
  "Return non-nil when HEADLINE and BODY look like a prompt ai-code stored.
Every recorded prompt carries either an Org timestamp headline or a
property drawer.  Pasted text whose own lines start with \"* \" is read
by Org as extra headlines and has neither, so this keeps one pasted
document from splitting into a pile of candidate fragments."
  (or (string-match-p ai-code-prompt-completion--timestamp-headline-regexp
                      (string-trim headline))
      (string-match-p "\\`[ \t\n]*:PROPERTIES:" body)))

(defun ai-code-prompt-completion--parse-buffer ()
  "Return the prompts recorded under the top-level headlines in this buffer.
Only the top level is read: pasted text whose own lines start with
\"* \" would otherwise split one recorded prompt into fragments."
  (let (entries)
    (goto-char (point-min))
    (while (re-search-forward "^\\* \\(.*\\)$" nil t)
      (let ((headline (match-string-no-properties 1)))
        (forward-line 1)
        (let* ((start (point))
               (end (if (re-search-forward "^\\* " nil t)
                        (match-beginning 0)
                      (point-max)))
               (body (buffer-substring-no-properties start end)))
          (goto-char end)
          (when (ai-code-prompt-completion--recorded-entry-p headline body)
            (let ((entry (ai-code-prompt-completion--reusable-entry body)))
              (when entry
                (push entry entries)))))))
    (nreverse entries)))

(defconst ai-code-prompt-completion--metadata-line-regexp
  "\\`[ \t]*\\(?:#\\|:[A-Za-z_]+:\\)"
  "An Org comment, keyword or drawer line, which is never prompt text.")

(defun ai-code-prompt-completion--strip-metadata (text)
  "Return TEXT without the Org bookkeeping it opens with.
A note opens with an ID drawer and a `#+title:', an archived entry with
a drawer of its own; none of that is something to offer back."
  (let ((lines (split-string text "\n")))
    (while (and lines
                (string-match-p ai-code-prompt-completion--metadata-line-regexp
                                (car lines)))
      (setq lines (cdr lines)))
    (string-trim (string-join lines "\n"))))

(defconst ai-code-prompt-completion--minimum-length 12
  "Shortest text a document is asked to offer as a prompt.
A heading of a word or two is quicker to type than to pick out of a
popup, and a diary is full of them: \"Note\", \"Work\", \"Scrum\".")

(defun ai-code-prompt-completion--document-prompt (text)
  "Return the prompt TEXT offers a document's reader, or nil when none."
  (let ((prompt (ai-code-prompt-completion--strip-metadata text)))
    (when (>= (length prompt) ai-code-prompt-completion--minimum-length)
      prompt)))

(defconst ai-code-prompt-completion--tags-regexp
  "[ \t]+:\\(?:[[:alnum:]_@#%]+:\\)+\\'"
  "The tags Org writes at the end of a headline, which are not prompt text.")

(defconst ai-code-prompt-completion--cookie-regexp
  "[ \t]*\\[[0-9]*\\(?:/[0-9]*\\|%\\)\\]"
  "A statistics cookie, which counts children rather than saying anything.
Dropping it also keeps \"Work [0/3]\" and \"Work [1/3]\" from standing as
two prompts.")

(defconst ai-code-prompt-completion--link-regexp
  "\\[\\[\\([^][]+\\)\\]\\(?:\\[\\([^][]+\\)\\]\\)?\\]"
  "An Org link, whose description or target is the text it shows.")

(defconst ai-code-prompt-completion--calendar-headline-regexp
  "\\`[0-9]\\{4\\}\\(?:-[0-9]\\{2\\}\\)\\{0,2\\}\\(?:[ \t]+[[:alpha:]]+\\)?\\'"
  "A datetree headline: a year, a month or a day, and maybe its name.
Those say where in a diary an entry sits, never what to prompt with.")

(defun ai-code-prompt-completion--unlink (text)
  "Return TEXT with each Org link replaced by the words it shows."
  (replace-regexp-in-string
   ai-code-prompt-completion--link-regexp
   (lambda (link) (or (match-string 2 link) (match-string 1 link)))
   text t t))

(defun ai-code-prompt-completion--headline-text (headline)
  "Return the prompt HEADLINE itself is, or nil when it is not one.
The markup a headline is written with -- trailing tags, a statistics
cookie, a link wrapper -- is not part of the prompt, and a datetree
headline is scaffolding that drops out while the entries under it stay."
  (let ((text (string-trim
               (ai-code-prompt-completion--unlink
                (replace-regexp-in-string
                 ai-code-prompt-completion--cookie-regexp ""
                 (replace-regexp-in-string
                  ai-code-prompt-completion--tags-regexp ""
                  (string-trim headline)))))))
    (unless (or (string-empty-p text)
                (string-match-p
                 ai-code-prompt-completion--calendar-headline-regexp text))
      text)))

(defun ai-code-prompt-completion--headline-body ()
  "Return the text written under the headline point is on, or nil when none.
Point ends at the next headline, of whatever depth, so a section offers
its own text and leaves its children to speak for themselves."
  (forward-line 1)
  (let* ((start (point))
         (end (if (re-search-forward "^\\*+ " nil t)
                  (match-beginning 0)
                (point-max))))
    (goto-char end)
    (ai-code-prompt-completion--document-prompt
     (buffer-substring-no-properties start end))))

(defun ai-code-prompt-completion--parse-blocks (text)
  "Return the prompts TEXT holds, one per blank-line-separated block."
  (delq nil
        (mapcar #'ai-code-prompt-completion--document-prompt
                (split-string text "\n[ \t]*\n" t "[ \t\n]+"))))

(defun ai-code-prompt-completion--parse-document-buffer ()
  "Return the prompts a document offers: a prompt library, a diary, a note.
Every headline counts, however deep and whether or not it has children,
because a leaf section of a note is as much a prompt as a top-level one.
A headline offers two prompts: the headline itself, and the text written
directly under it.  Whatever sits before the first headline, which is
the whole of a note written without any, is split on blank lines.
Nothing is filtered the way recorded history is -- you wrote these files
to complete from -- so a prompt is offered whatever it starts with and
however long it runs."
  (goto-char (point-min))
  (let* ((start (if (re-search-forward "^\\*+ " nil t)
                    (match-beginning 0)
                  (point-max)))
         (entries (nreverse
                   (ai-code-prompt-completion--parse-blocks
                    (buffer-substring-no-properties (point-min) start)))))
    (goto-char start)
    (while (re-search-forward "^\\*+ \\(.*\\)$" nil t)
      (let* ((title (ai-code-prompt-completion--headline-text
                     (match-string-no-properties 1)))
             (body (ai-code-prompt-completion--headline-body)))
        (when (and title
                   (>= (length title)
                       ai-code-prompt-completion--minimum-length))
          (push title entries))
        (when body (push body entries))))
    (nreverse entries)))

(defun ai-code-prompt-completion--read-entries (file parser)
  "Return the prompts PARSER finds in FILE, or nil when it cannot be read."
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents file)
      (error nil))
    (funcall parser)))

(defun ai-code-prompt-completion--display (text)
  "Return the popup string for TEXT, truncated to fit."
  (if (> (length text) ai-code-prompt-completion--display-width)
      (concat (substring text 0 (1- ai-code-prompt-completion--display-width))
              "…")
    text))

(defconst ai-code-prompt-completion--word-regexp "[A-Za-z0-9_-]\\{3,\\}"
  "Regexp for a word worth recalling a prompt by.
Shorter words such as \"to\" would drag every prompt into the popup on
two keystrokes.")

(defun ai-code-prompt-completion--word-offsets (line)
  "Return the offsets in LINE a candidate may start at.
Zero comes first, so the whole line stays a candidate, followed by the
start of every word long enough to type.  Those are what let a word in
the middle of a prompt recall the prompt around it."
  (let ((start 0)
        (offsets (list 0)))
    (while (string-match ai-code-prompt-completion--word-regexp line start)
      (unless (zerop (match-beginning 0))
        (push (match-beginning 0) offsets))
      (setq start (match-end 0)))
    (nreverse offsets)))

(defun ai-code-prompt-completion--build ()
  "Scan every prompt file and return a fresh index.
Candidates are ordered by how often the prompt was written, so the
wording you reach for most lands at the top of the popup.  A prompt you
curated by hand outranks a recorded one written as often, and within one
prompt the whole first line comes before the fragments cut out of it."
  (let* ((counts (make-hash-table :test #'equal))
         (expansions (make-hash-table :test #'equal))
         (files (ai-code-prompt-completion--files))
         (extra-files (ai-code-prompt-completion--extra-files))
         (entries
          (append
           (mapcan (lambda (file)
                     (ai-code-prompt-completion--read-entries
                      file #'ai-code-prompt-completion--parse-document-buffer))
                   extra-files)
           (mapcan (lambda (file)
                     (ai-code-prompt-completion--read-entries
                      file #'ai-code-prompt-completion--parse-buffer))
                   files)))
         (order '())
         (candidates '()))
    (dolist (entry entries)
      (unless (gethash entry counts)
        (push entry order))
      (puthash entry (1+ (or (gethash entry counts) 0)) counts))
    (dolist (entry (sort (nreverse order)
                         (lambda (a b) (> (gethash a counts) (gethash b counts)))))
      (let ((line (car (split-string entry "\n"))))
        (dolist (offset (ai-code-prompt-completion--word-offsets line))
          (let ((display (ai-code-prompt-completion--display
                          (substring line offset))))
            ;; Two prompts can share a first line or a tail; the more
            ;; frequent one, which sorts first, keeps the slot.
            (unless (gethash display expansions)
              (puthash display entry expansions)
              (push display candidates))))))
    (list (nreverse candidates) expansions
          (+ (length files) (length extra-files)))))

(defun ai-code-prompt-completion--ensure-index ()
  "Return the cached index, building it on first use."
  (or ai-code-prompt-completion--index
      (setq ai-code-prompt-completion--index (ai-code-prompt-completion--build))))

(defconst ai-code-prompt-completion--line-prefix-regexp
  "[ \t]*\\(?:\\*+ \\|[-+] \\|[0-9]+[.)] \\)?"
  "Org headline stars, list bullet or list number opening a line.
Kept in place when a candidate expands back to the start of the line.")

(defun ai-code-prompt-completion--line-content-start ()
  "Return where the text of the current line starts.
That is after any Org bullet or headline stars, so expanding a prompt
inside a list item does not eat the item's own marker."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (looking-at ai-code-prompt-completion--line-prefix-regexp)
      (goto-char (match-end 0)))
    (point)))

(defun ai-code-prompt-completion--exit (candidate status)
  "Expand CANDIDATE to the full prompt it abbreviates once STATUS is `finished'.
A candidate can start at a word in the middle of the prompt, so the
replacement reaches back to the start of the line whenever what is
already typed there opens that same prompt.  Anything else on the line,
a \"TODO: \" you wrote yourself for instance, is left alone."
  (when (eq status 'finished)
    (let* ((key (substring-no-properties candidate))
           (full (gethash key (nth 1 (ai-code-prompt-completion--ensure-index)))))
      (when full
        (let* ((word-start (max (point-min) (- (point) (length candidate))))
               (line-start (ai-code-prompt-completion--line-content-start))
               (start (if (and (< line-start word-start)
                               (string-prefix-p
                                (buffer-substring-no-properties line-start
                                                                word-start)
                                full t))
                          line-start
                        word-start)))
          (unless (equal full (buffer-substring-no-properties start (point)))
            (delete-region start (point))
            (insert full)))))))

;;;###autoload
(defun ai-code-prompt-completion-capf ()
  "Complete the word before point against hand-written prompt history.
The word may be any word of a stored prompt, not only its first, so a
distinctive word in the middle recalls the prompt around it.  Return nil
when there is no word at point, so the capf stays out of the way.
Choosing a candidate inserts the full prompt it abbreviates.

`ai-code-prompt-mode' buffers install
`ai-code-prompt-completion-dict-capf' on their own; register this one
when you merge capfs yourself.  See the commentary in
ai-code-prompt-completion.el for `cape' and `company' setups."
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (index (ai-code-prompt-completion--ensure-index))
              (candidates (nth 0 index)))
    (list (car bounds) (cdr bounds) candidates
          :exclusive 'no
          :annotation-function (lambda (_candidate) " prompt")
          :exit-function #'ai-code-prompt-completion--exit)))

;;;###autoload
(defun ai-code-prompt-completion-dict-capf ()
  "Complete prompts you wrote, merged with English words from `cape-dict'.
Falls back to `ai-code-prompt-completion-capf' alone when cape is not
installed, so the prompt candidates are there either way."
  (if (fboundp 'cape-wrap-super)
      (cape-wrap-super #'ai-code-prompt-completion-capf #'cape-dict)
    (ai-code-prompt-completion-capf)))

(defun ai-code-prompt-completion--enable ()
  "Install the prompt completion capf in the current buffer.
Added to `ai-code-prompt-mode-hook' when this file loads, so prompt
buffers complete out of the box.  Remove it from that hook to opt out.

Loads cape when it is installed, which is what lets the dictionary words
join the prompt candidates for someone who has cape but never configured
it."
  (require 'cape nil t)
  ;; Negative depth so the merged capf is consulted before whatever the
  ;; major mode installed, the same slot a cape word-completion setup uses.
  ;; A capf your own config already added at this depth still wins: equal
  ;; depths run in the order they were added, and `text-mode-hook' runs
  ;; before `ai-code-prompt-mode-hook'.
  (add-hook 'completion-at-point-functions
            #'ai-code-prompt-completion-dict-capf -90 t))

;;;###autoload
(defun ai-code-prompt-completion-setup ()
  "Turn on prompt completion with a `company-mode' popup.
The capf is already installed in prompt buffers on its own, so this adds
the popup on top of it:

  (add-hook \\='ai-code-prompt-mode-hook #\\='ai-code-prompt-completion-setup)

Candidates are the prompts you wrote before plus, when cape is
installed, English words from `cape-dict'.  Set
`ai-code-prompt-completion-enable-company' to nil when another
front-end such as corfu shows them instead.

Skip this when you already merge capfs with cape: only the first capf at
a given hook depth answers, so add `ai-code-prompt-completion-capf' to
your own `cape-wrap-super' list instead of racing it."
  (interactive)
  (ai-code-prompt-completion--enable)
  (when (and ai-code-prompt-completion-enable-company
             (require 'company nil t)
             (fboundp 'company-mode))
    (company-mode 1)))

;;;###autoload
(defun ai-code-prompt-completion-refresh ()
  "Rebuild the prompt completion index from disk."
  (interactive)
  (let ((index (setq ai-code-prompt-completion--index
                     (ai-code-prompt-completion--build))))
    (message "AI Code: indexed %d prompt(s) from %d file(s)"
             (length (nth 0 index)) (nth 2 index))))

;; Prompt buffers complete out of the box; the popup front-end stays a
;; separate, explicit choice.
(add-hook 'ai-code-prompt-mode-hook #'ai-code-prompt-completion--enable)

(provide 'ai-code-prompt-completion)

;;; ai-code-prompt-completion.el ends here
