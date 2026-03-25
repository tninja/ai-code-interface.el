;;; ai-code-session-link.el --- Shared session link helpers -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Internal helpers shared by session linkification and navigation.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)

(declare-function ai-code-session-navigate-link-at-mouse "ai-code-input" (event))
(declare-function ai-code-session-navigate-link-at-point "ai-code-input" ())
(declare-function helm-gtags-find-tag "helm-gtags" (tagname))
(declare-function xref-find-definitions "xref" (identifier))

(defvar ai-code-backends-infra--session-directory nil
  "Session working directory set by ai-code-backends-infra buffers.")

(defcustom ai-code-session-link-enabled t
  "When non-nil, make supported links clickable in AI session buffers.

Disable this if you prefer to avoid the extra linkification work on
terminal output redraw."
  :type 'boolean
  :group 'ai-code)

(defvar ai-code-session-link--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ai-code-session-navigate-link-at-mouse)
    (define-key map [mouse-2] #'ai-code-session-navigate-link-at-mouse)
    (define-key map (kbd "RET") #'ai-code-session-navigate-link-at-point)
    map)
  "Keymap used for clickable session links.")

(declare-function ai-code-session-link-navigate-symbol-at-mouse "ai-code-session-link" (event))
(declare-function ai-code-session-link-navigate-symbol-at-point "ai-code-session-link" ())

(defvar ai-code-session-link--symbol-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ai-code-session-link-navigate-symbol-at-mouse)
    (define-key map [mouse-2] #'ai-code-session-link-navigate-symbol-at-mouse)
    (define-key map (kbd "RET") #'ai-code-session-link-navigate-symbol-at-point)
    map)
  "Keymap used for clickable session symbols.")

(defconst ai-code-session-link--linkify-min-tail-width 512
  "Minimum number of tail characters to rescan for session links.")

(defconst ai-code-session-link--url-pattern-regexp
  "\\(https?://[^][(){}<>\"' \t\n]+\\)"
  "Regexp matching http/https URLs in session buffers.")

(defconst ai-code-session-link--symbol-neighborhood-max-width 168
  "Maximum number of characters to scan for symbols near a file link.")

(defconst ai-code-session-link--symbol-neighborhood-max-lines 3
  "Maximum number of lines to scan for symbols near a file link.")

(defconst ai-code-session-link--symbol-neighborhood-max-candidates 24
  "Maximum number of raw symbol candidates to inspect near one file link.")

(defconst ai-code-session-link--symbol-neighborhood-max-links 12
  "Maximum number of symbol links to apply near one file link.")

(defconst ai-code-session-link--path-base-regexp
  "@?[[:alnum:]_./~-]*[./][[:alnum:]_./~-]+"
  "Regexp matching a local file-like or directory-like path.")

(defconst ai-code-session-link--symbol-identifier-regexp
  "[[:alpha:]_][[:alnum:]_*!?]*"
  "Regexp matching one conservative code identifier segment.")

(defconst ai-code-session-link--camel-case-symbol-regexp
  "[[:upper:]][[:alnum:]]+"
  "Regexp matching a bare CamelCase-style symbol candidate.")

(defconst ai-code-session-link--snake-case-symbol-regexp
  "_*[[:lower:]][[:lower:][:digit:]]*\\(?:_[[:lower:][:digit:]]+\\)+"
  "Regexp matching a bare snake_case-style symbol candidate.")

(defconst ai-code-session-link--symbol-candidate-regexp
  (concat
   "\\("
   "\\(?:"
   ai-code-session-link--symbol-identifier-regexp
   "\\(?:\\.\\|::\\|#\\)"
   ai-code-session-link--symbol-identifier-regexp
   "\\(?:\\(?:\\.\\|::\\|#\\)"
   ai-code-session-link--symbol-identifier-regexp
   "\\)*"
   "\\(?:()\\)?"
   "\\|"
   ai-code-session-link--symbol-identifier-regexp
   "()"
   "\\|"
   ai-code-session-link--symbol-identifier-regexp
   "\\(?:-[[:alnum:]_*!?]*\\)+"
   "\\|"
   ai-code-session-link--camel-case-symbol-regexp
   "\\|"
   ai-code-session-link--snake-case-symbol-regexp
   "\\)"
   "\\)")
  "Regexp matching conservative symbol candidates near a file link.")

(defun ai-code-session-link--path-pattern (suffix)
  "Return a session link regexp for `ai-code-session-link--path-base-regexp' plus SUFFIX."
  (concat "\\(" ai-code-session-link--path-base-regexp "\\)" suffix))

(defconst ai-code-session-link--file-patterns
  (list
   (list (ai-code-session-link--path-pattern
          "#L\\([0-9]+\\)\\(?:-L?\\([0-9]+\\)\\)?")
         1 2 nil)
   (list (ai-code-session-link--path-pattern
          ":L\\([0-9]+\\)\\(?:-L?\\([0-9]+\\)\\)?")
         1 2 nil)
   (list (ai-code-session-link--path-pattern
          ":\\([0-9]+\\):\\([0-9]+\\)\\>")
         1 2 3)
   (list (ai-code-session-link--path-pattern
          ":\\([0-9]+\\)-\\([0-9]+\\)\\>")
         1 2 nil)
   (list (ai-code-session-link--path-pattern
          ":\\([0-9]+\\)\\>")
         1 2 nil)
   (list (ai-code-session-link--path-pattern "\\>")
         1 nil nil))
  "Patterns used to detect file-like session links.")

(defvar-local ai-code-session-link--linkify-timer nil
  "Timer used to re-linkify recent terminal output after redraw settles.")

(defvar-local ai-code-session-link--pending-tail-width 0
  "Pending tail width to rescan when delayed session linkification runs.")

(defvar ai-code-session-link--project-files-cache nil
  "Dynamic cache of project file lists used during one linkify pass.")

(defvar ai-code-session-link--resolved-path-cache nil
  "Dynamic cache of resolved session paths used during one linkify pass.")

(defconst ai-code-session-link--cache-miss (make-symbol "cache-miss")
  "Sentinel used by per-pass caches when no value has been stored yet.")

(defun ai-code-session-link--normalize-file (filename)
  "Normalize session link FILENAME for project lookup."
  (when (stringp filename)
    (let* ((trimmed (string-trim filename))
           (without-at (string-remove-prefix "@" trimmed))
           (normalized (string-remove-prefix "file://" without-at)))
      (unless (string-empty-p normalized)
        normalized))))

(defun ai-code-session-link--cache-get-or-compute (cache key compute)
  "Return cached value from CACHE for KEY, or call COMPUTE and store it."
  (if cache
      (let ((cached (gethash key cache ai-code-session-link--cache-miss)))
        (if (eq cached ai-code-session-link--cache-miss)
            (let ((value (funcall compute)))
              (puthash key value cache)
              value)
          cached))
    (funcall compute)))

(defun ai-code-session-link--project-files (root)
  "Return absolute project files for ROOT."
  (when (file-directory-p root)
    (ai-code-session-link--cache-get-or-compute
     ai-code-session-link--project-files-cache
     (expand-file-name root)
     (lambda ()
       (or (ignore-errors
             (when-let ((project (project-current nil root)))
               (let ((project-root (expand-file-name (project-root project))))
                 (mapcar (lambda (file)
                           (if (file-name-absolute-p file)
                               (expand-file-name file)
                             (expand-file-name file project-root)))
                         (project-files project)))))
           (directory-files-recursively root ".*" t))))))

(defun ai-code-session-link--in-project-file-p (file root &optional project-files)
  "Return non-nil when FILE exists and belongs to ROOT."
  (let* ((project-root (and root (file-name-as-directory (expand-file-name root))))
         (candidate (and file (expand-file-name file)))
         (project-files (or project-files
                            (and project-root
                                 (ai-code-session-link--project-files project-root)))))
    (and project-root
         candidate
         (file-exists-p candidate)
         (string-prefix-p project-root (file-name-directory candidate))
         (member candidate project-files))))

(defun ai-code-session-link--matching-project-files (path root &optional project-files)
  "Return project files in ROOT that match PATH exactly or by basename."
  (when-let* ((project-root (and root (file-name-as-directory (expand-file-name root))))
              (normalized (ai-code-session-link--normalize-file path)))
    (let* ((relative-path (replace-regexp-in-string "\\`\\./" "" normalized))
           (basename (file-name-nondirectory relative-path))
           (project-files (or project-files
                              (ai-code-session-link--project-files project-root))))
      (cl-remove-if-not
       (lambda (file)
         (or (string= (file-relative-name file project-root) relative-path)
             (string= (file-name-nondirectory file) basename)))
       project-files))))

(defun ai-code-session-link--project-root-for-paths ()
  "Return the current session project root directory with trailing slash."
  (let ((root (or ai-code-backends-infra--session-directory
                  (and (fboundp 'project-current)
                       (when-let ((project (project-current nil default-directory)))
                         (expand-file-name (project-root project))))
                  default-directory)))
    (and root (file-name-as-directory (expand-file-name root)))))

(defun ai-code-session-link--local-path-candidates (path root)
  "Return local candidate paths for PATH using ROOT and `default-directory'."
  (delete-dups
   (delq nil
         (list (and (file-name-absolute-p path)
                    (expand-file-name path))
               (and root
                    (expand-file-name path root))
               (expand-file-name path default-directory)))))

(defun ai-code-session-link--resolve-existing-local-path (path root)
  "Resolve PATH to an existing local file or directory using ROOT."
  (seq-find #'file-exists-p
            (ai-code-session-link--local-path-candidates path root)))

(defun ai-code-session-link--resolve-session-file (path)
  "Resolve PATH to an existing local path or a matching project file."
  (let* ((root (ai-code-session-link--project-root-for-paths))
         (normalized (ai-code-session-link--normalize-file path))
         (cache-key (and root normalized
                         (cons (expand-file-name root) normalized))))
    (if cache-key
        (ai-code-session-link--cache-get-or-compute
         ai-code-session-link--resolved-path-cache
         cache-key
         (lambda ()
           (or (ai-code-session-link--resolve-existing-local-path normalized root)
               (let* ((project-files (ai-code-session-link--project-files root))
                      (candidate (if (file-name-absolute-p normalized)
                                     (expand-file-name normalized)
                                   (expand-file-name normalized root))))
                 (cond
                  ((ai-code-session-link--in-project-file-p candidate root project-files) candidate)
                  ((not (file-name-absolute-p normalized))
                   (car (ai-code-session-link--matching-project-files normalized root project-files)))
                  (t nil))))))
      nil)))

(defun ai-code-session-link--parse-file-link-text (text)
  "Parse file-like session link TEXT into a plist."
  (when (stringp text)
    (catch 'parsed
      (dolist (pattern ai-code-session-link--file-patterns)
        (let ((regexp (concat "\\`" (car pattern) "\\'")))
          (when (string-match regexp text)
            (throw
             'parsed
             (list :file (match-string (nth 1 pattern) text)
                   :line-start (when-let ((group (nth 2 pattern))
                                          (line (match-string group text)))
                                 (string-to-number line))
                   :column-start (when-let ((group (nth 3 pattern))
                                            (column (match-string group text)))
                                   (string-to-number column))))))))))

(defun ai-code-session-link--apply-properties (start end &optional text help-echo)
  "Apply session link properties from START to END."
  (add-text-properties
   start end
   (list 'ai-code-session-link (or text
                                   (buffer-substring-no-properties start end))
         'mouse-face 'highlight
         'help-echo help-echo
         'keymap ai-code-session-link--keymap
         'follow-link t
         'font-lock-face 'link
         'face 'link)))

(defun ai-code-session-link--apply-symbol-properties (start end symbol file-link)
  "Apply clickable symbol properties from START to END."
  (add-text-properties
   start end
   (list 'ai-code-session-link file-link
         'ai-code-session-symbol-link symbol
         'ai-code-session-symbol-file file-link
         'mouse-face 'highlight
         'help-echo "mouse-1: Jump to symbol context"
         'keymap ai-code-session-link--symbol-keymap
         'follow-link t
         'font-lock-face 'link
         'face 'link)))

(defun ai-code-session-link--elisp-symbol-candidate-p (candidate)
  "Return non-nil when CANDIDATE looks like an Elisp symbol worth linking."
  (or (intern-soft candidate)
      (string-match-p "--" candidate)
      (string-match-p
       "\\(?:-p\\|-mode\\|-hook\\|-function\\|-command\\|-local\\|\\*\\|\\?\\)\\'"
       candidate)))

(defun ai-code-session-link--case-sensitive-match-p (regexp candidate)
  "Return non-nil when CANDIDATE fully matches REGEXP with case-sensitive search."
  (let ((case-fold-search nil))
    (string-match-p regexp candidate)))

(defun ai-code-session-link--java-camel-case-symbol-p (candidate)
  "Return non-nil when CANDIDATE looks like a Java-style CamelCase symbol."
  (and (ai-code-session-link--case-sensitive-match-p
        (concat "\\`" ai-code-session-link--camel-case-symbol-regexp "\\'")
        candidate)
       (ai-code-session-link--case-sensitive-match-p "[[:lower:]]" candidate)
       (ai-code-session-link--case-sensitive-match-p
        "[[:upper:]][[:lower:][:digit:]]+[[:upper:]]"
        candidate)))

(defun ai-code-session-link--snake-case-symbol-p (candidate)
  "Return non-nil when CANDIDATE looks like a bare snake_case symbol."
  (ai-code-session-link--case-sensitive-match-p
   "\\`_*[[:lower:]][[:lower:][:digit:]]*\\(?:_[[:lower:][:digit:]]+\\)+\\'"
   candidate))

(defun ai-code-session-link--bare-symbol-candidate-p (candidate)
  "Return non-nil when bare CANDIDATE looks like a supported code symbol."
  (or (ai-code-session-link--java-camel-case-symbol-p candidate)
      (ai-code-session-link--snake-case-symbol-p candidate)
      (and (string-match-p "-" candidate)
           (ai-code-session-link--elisp-symbol-candidate-p candidate))))

(defun ai-code-session-link--symbol-candidate-p (candidate)
  "Return non-nil when CANDIDATE is worth linkifying."
  (and (stringp candidate)
       (> (length candidate) 2)
       (not (string-match-p "\\`https?://" candidate))
       (not (string-match-p "[/\\\\]" candidate))
       (not (string-match-p "\\`[0-9]" candidate))
       (not (string-match-p "\\(?:[:#][Ll]?[0-9]+\\)\\'" candidate))
       (or (string-match-p "\\." candidate)
           (string-match-p "::" candidate)
           (string-match-p "#" candidate)
           (string-suffix-p "()" candidate)
           (ai-code-session-link--bare-symbol-candidate-p candidate))))

(defun ai-code-session-link--line-budget-end (start end line-count)
  "Return the position after moving forward LINE-COUNT line breaks from START up to END."
  (save-excursion
    (goto-char start)
    (when (and (< (point) end)
               (eq (char-after) ?\n))
      (forward-char 1))
    (dotimes (_ line-count)
      (if (search-forward "\n" end t)
          (goto-char (point))
        (goto-char end)))
    (point)))

(defun ai-code-session-link--symbol-window-end (start hard-end)
  "Return the fixed nearby symbol scan boundary from START up to HARD-END."
  (min hard-end
       (+ start ai-code-session-link--symbol-neighborhood-max-width)
       (ai-code-session-link--line-budget-end
        start hard-end ai-code-session-link--symbol-neighborhood-max-lines)))

(defun ai-code-session-link--within-symbol-scan-budget-p (candidate-count link-count)
  "Return non-nil when nearby scanning can continue with CANDIDATE-COUNT and LINK-COUNT."
  (and (< candidate-count ai-code-session-link--symbol-neighborhood-max-candidates)
       (< link-count ai-code-session-link--symbol-neighborhood-max-links)))

(defun ai-code-session-link--next-nearby-symbol-boundary (start end &optional next-file-start)
  "Return the next boundary after START for symbol scanning up to END."
  (let ((boundary end))
    (save-excursion
      (goto-char start)
      (when (re-search-forward ai-code-session-link--url-pattern-regexp end t)
        (setq boundary (min boundary (match-beginning 1)))))
    (if next-file-start
        (min boundary next-file-start)
      boundary)))

(defun ai-code-session-link--symbol-scan-end (scan-start end &optional next-file-start)
  "Return the final nearby symbol scan boundary for SCAN-START up to END."
  (ai-code-session-link--next-nearby-symbol-boundary
   scan-start
   (ai-code-session-link--symbol-window-end scan-start end)
   next-file-start))

(defun ai-code-session-link--linkify-symbols-near-file (file-link scan-start end &optional next-file-start)
  "Linkify code-like symbols near FILE-LINK from SCAN-START up to END."
  (let ((scan-end (ai-code-session-link--symbol-scan-end scan-start end next-file-start)))
    (when (< scan-start scan-end)
      (save-excursion
        (let ((case-fold-search nil)
              (candidate-count 0)
              (link-count 0))
          (goto-char scan-start)
          (while (and (ai-code-session-link--within-symbol-scan-budget-p
                       candidate-count link-count)
                      (re-search-forward ai-code-session-link--symbol-candidate-regexp scan-end t))
            (let ((symbol-start (match-beginning 1))
                  (symbol-end (match-end 1))
                  (candidate (match-string-no-properties 1)))
              (setq candidate-count (1+ candidate-count))
              (when (and (not (get-text-property symbol-start 'ai-code-session-link))
                         (ai-code-session-link--symbol-candidate-p candidate))
                (ai-code-session-link--apply-symbol-properties
                 symbol-start symbol-end candidate file-link)
                (setq link-count (1+ link-count))))))))))

(defun ai-code-session-link--collect-file-links (start end)
  "Return resolved file link matches between START and END."
  (let ((seen-starts (make-hash-table :test 'eql))
        file-links)
    (save-excursion
      (dolist (pattern ai-code-session-link--file-patterns)
        (goto-char start)
        (while (re-search-forward (car pattern) end t)
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0)))
            (unless (gethash match-start seen-starts)
              (let ((path (match-string-no-properties (nth 1 pattern))))
                (when (ai-code-session-link--resolve-session-file path)
                  (puthash match-start t seen-starts)
                  (push (list :start match-start
                              :end match-end
                              :text (buffer-substring-no-properties match-start match-end))
                        file-links))))))))
    (nreverse file-links)))

(defun ai-code-session-link--linkify-url-region (start end)
  "Apply URL session links between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward ai-code-session-link--url-pattern-regexp end t)
      (let* ((url-start (match-beginning 1))
             (raw-url (match-string-no-properties 1))
             (trimmed-url (replace-regexp-in-string "[.,;:!?]+\\'" "" raw-url))
             (url-end (+ url-start (length trimmed-url))))
        (ai-code-session-link--apply-properties
         url-start url-end trimmed-url "mouse-1: Open URL")))))

(defun ai-code-session-link--linkify-file-region (start end)
  "Apply file session links between START and END."
  (let ((ai-code-session-link--project-files-cache (make-hash-table :test 'equal))
        (ai-code-session-link--resolved-path-cache (make-hash-table :test 'equal)))
    (let ((file-links (ai-code-session-link--collect-file-links start end)))
      (while file-links
        (let* ((file-link (car file-links))
               (next-file-link (cadr file-links))
               (match-start (plist-get file-link :start))
               (match-end (plist-get file-link :end))
               (link-text (plist-get file-link :text)))
          (unless (get-text-property match-start 'ai-code-session-link)
            (ai-code-session-link--apply-properties
             match-start match-end link-text "mouse-1: Visit file")
            (ai-code-session-link--linkify-symbols-near-file
             link-text match-end end
             (and next-file-link (plist-get next-file-link :start))))
          (setq file-links (cdr file-links)))))))

(defun ai-code-session-link--property-at-point (property)
  "Return PROPERTY at point or immediately before point."
  (or (get-text-property (point) property)
      (when (> (point) (point-min))
        (get-text-property (1- (point)) property))))

(defun ai-code-session-link--symbol-search-terms (symbol)
  "Return conservative search terms for SYMBOL."
  (let* ((trimmed (string-remove-suffix "()" symbol))
         (parts (split-string trimmed "\\(?:\\.\\|::\\|#\\)" t))
         (tail (car (last parts))))
    (delete-dups (delq nil (list trimmed tail)))))

(defun ai-code-session-link--primary-symbol-search-term (symbol)
  "Return the primary lookup term for SYMBOL."
  (car (last (ai-code-session-link--symbol-search-terms symbol))))

(defun ai-code-session-link--open-file-link (text)
  "Open the file described by file-like link TEXT."
  (when-let* ((link (ai-code-session-link--parse-file-link-text text))
              (file (plist-get link :file))
              (abs-file (ai-code-session-link--resolve-session-file file)))
    (find-file-other-window abs-file)
    (goto-char (point-min))
    (when-let ((line-start (plist-get link :line-start)))
      (forward-line (1- line-start)))
    (when-let ((column-start (plist-get link :column-start)))
      (when (> column-start 0)
        (move-to-column (1- column-start))))
    t))

(defun ai-code-session-link--try-xref-definition (symbol)
  "Try xref lookup for SYMBOL in the current buffer."
  (when-let ((lookup (ai-code-session-link--primary-symbol-search-term symbol)))
    (when (fboundp 'xref-find-definitions)
      (condition-case nil
          (progn
            (xref-find-definitions lookup)
            t)
        (error nil)))))

(defun ai-code-session-link--try-helm-gtags-definition (symbol)
  "Try helm-gtags lookup for SYMBOL in the current buffer."
  (when-let ((lookup (ai-code-session-link--primary-symbol-search-term symbol)))
    (when (fboundp 'helm-gtags-find-tag)
      (condition-case nil
          (progn
            (helm-gtags-find-tag lookup)
            t)
        (error nil)))))

(defun ai-code-session-link--search-symbol-in-current-buffer (symbol)
  "Search for SYMBOL in the current buffer."
  (catch 'found
    (dolist (term (ai-code-session-link--symbol-search-terms symbol))
      (goto-char (point-min))
      (when (search-forward term nil t)
        (goto-char (match-beginning 0))
        (throw 'found t)))))

;;;###autoload
(defun ai-code-session-link-navigate-symbol-at-point ()
  "Navigate using the clickable symbol at point."
  (interactive)
  (when-let* ((symbol (ai-code-session-link--property-at-point 'ai-code-session-symbol-link))
              (file-link (or (ai-code-session-link--property-at-point 'ai-code-session-symbol-file)
                             (ai-code-session-link--property-at-point 'ai-code-session-link))))
    (if (ai-code-session-link--open-file-link file-link)
        (progn
          (or (ai-code-session-link--try-xref-definition symbol)
              (ai-code-session-link--try-helm-gtags-definition symbol)
              (ai-code-session-link--search-symbol-in-current-buffer symbol))
          (message "Navigated to %s via %s" symbol file-link)
          t)
      (progn
        (message "Unable to resolve symbol context: %s" symbol)
        nil))))

;;;###autoload
(defun ai-code-session-link-navigate-symbol-at-mouse (event)
  "Navigate using the clickable symbol clicked by mouse EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (position (posn-point start)))
    (when (window-live-p window)
      (select-window window)
      (when (integer-or-marker-p position)
        (goto-char position)
        (ai-code-session-link-navigate-symbol-at-point)))))

(defun ai-code-session-link--linkify-session-region (start end)
  "Make supported URLs and in-project file references clickable from START to END."
  (when (and ai-code-session-link-enabled
             (< start end))
    (let ((inhibit-read-only t))
      (save-excursion
        (save-restriction
          (widen)
          (setq start (max (point-min) start)
                end (min (point-max) end))
          (let ((pos start))
            (while (< pos end)
              (let ((next (or (next-single-property-change
                               pos 'ai-code-session-link nil end)
                              end)))
                (when (get-text-property pos 'ai-code-session-link)
                  (remove-text-properties
                   pos next
                   '(ai-code-session-link nil
                     ai-code-session-symbol-link nil
                     ai-code-session-symbol-file nil
                     mouse-face nil
                     help-echo nil
                     keymap nil
                     follow-link nil
                     font-lock-face nil
                     face nil)))
                (setq pos next))))
          (ai-code-session-link--linkify-url-region start end)
          (ai-code-session-link--linkify-file-region start end))))))

(defun ai-code-session-link--recent-output-tail-width (output)
  "Return the tail width to rescan after OUTPUT."
  (max ai-code-session-link--linkify-min-tail-width
       (* 2 (length (or output "")))))

(defun ai-code-session-link--flush-scheduled-linkify ()
  "Apply any delayed session linkification pending in the current buffer."
  (let ((tail-width ai-code-session-link--pending-tail-width))
    (setq ai-code-session-link--pending-tail-width 0
          ai-code-session-link--linkify-timer nil)
    (when (> tail-width 0)
      (let ((end (point-max)))
        (ai-code-session-link--linkify-session-region
         (max (point-min) (- end tail-width))
         end)))))

(defun ai-code-session-link--schedule-linkify-recent-output (buffer output)
  "Linkify recent OUTPUT in BUFFER after terminal redraw settles."
  (when (and ai-code-session-link-enabled
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (setq ai-code-session-link--pending-tail-width
            (max ai-code-session-link--pending-tail-width
                 (ai-code-session-link--recent-output-tail-width output)))
      (unless ai-code-session-link--linkify-timer
        (setq ai-code-session-link--linkify-timer
              (run-at-time
               0 nil
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (ai-code-session-link--flush-scheduled-linkify))))
               buffer))))))

(defun ai-code-session-link--linkify-recent-output (output)
  "Linkify the recent tail of the current session buffer after OUTPUT."
  (when ai-code-session-link-enabled
    (let* ((visible-width (ai-code-session-link--recent-output-tail-width output))
           (end (point-max))
           (start (max (point-min) (- end visible-width))))
      (ai-code-session-link--linkify-session-region start end))))


(provide 'ai-code-session-link)

;;; ai-code-session-link.el ends here
