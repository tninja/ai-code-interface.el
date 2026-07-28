;;; ai-code-session-link.el --- Shared session link helpers -*- lexical-binding: t; -*-

;; Author: Kang Tu, realazy (cxa)
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Internal helpers shared by session linkification and navigation.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'project)
(require 'rx)
(require 'subr-x)
(require 'url-util)

(declare-function ai-code-session-navigate-link-at-mouse "ai-code-input" (event))
(declare-function ai-code-session-navigate-link-at-point "ai-code-input" ())
(declare-function helm-gtags-find-tag "helm-gtags" (tagname))
(declare-function xref-find-definitions "xref" (identifier))

(defvar ai-code-backends-infra--session-directory nil
  "Session working directory set by ai-code-backends-infra buffers.")

(defvar ai-code-backends-infra--session-terminal-backend nil
  "Terminal backend symbol set by ai-code-backends-infra buffers.")

(defcustom ai-code-session-link-enabled t
  "When non-nil, make supported links clickable in AI session buffers.

Disable this if you prefer to avoid the extra linkification work on
terminal output redraw."
  :type 'boolean
  :group 'ai-code)

(defcustom ai-code-session-link-ghostel-image-preview-enabled t
  "When non-nil, preview local image file links in Ghostel AI sessions.

This only affects AI session buffers managed by the Ghostel terminal backend.
It complements Ghostel's native Kitty graphics support by previewing image
files when an AI CLI prints a local image path such as screenshot.png."
  :type 'boolean
  :group 'ai-code)

(defcustom ai-code-session-link-ghostel-image-preview-max-bytes
  (* 10 1024 1024)
  "Maximum local image file size, in bytes, previewed in Ghostel sessions."
  :type 'integer
  :group 'ai-code)

(defcustom ai-code-session-link-ghostel-image-preview-max-width nil
  "Maximum displayed image width for Ghostel previews, in logical pixels.
When nil, previews fit the session window's body width.  Source images only
ever scale down; on HiDPI frames, one logical pixel may use multiple source
pixels.  Set an integer to impose a fixed hard cap instead."
  :type '(choice (const :tag "Fit session window" nil)
                 (integer :tag "Fixed pixel cap"))
  :group 'ai-code)

(defcustom ai-code-session-link-ghostel-image-preview-max-height nil
  "Maximum displayed image height for Ghostel previews, in logical pixels.
When nil, preview height is not capped; the width cap controls default
scaling and tall previews can be inspected by scrolling.  Set an integer to
impose a fixed hard cap instead."
  :type '(choice (const :tag "No height cap" nil)
                 (integer :tag "Fixed pixel cap"))
  :group 'ai-code)

(defcustom ai-code-session-link-ghostel-image-preview-vertical-margin 'auto
  "Vertical margin around Ghostel session image previews.
The default value `auto' uses 0.3 times the session window's line height, so
the spacing follows font-size changes.  Set a non-negative integer to use a
fixed pixel margin instead; zero disables the margin."
  :type '(choice (const :tag "Automatic (0.3 line height)" auto)
                 (natnum :tag "Fixed pixels"))
  :group 'ai-code)

(defconst ai-code-session-link--image-preview-auto-margin-ratio 0.3
  "Line-height fraction used for automatic image preview margins.")

(defconst ai-code-session-link--visible-image-preview-max-line-width 2048
  "Maximum candidate width inspected by strict visible image preview scanning.")

(defconst ai-code-session-link--visible-image-preview-max-candidates 8
  "Maximum image candidates inspected by one strict visible preview scan.")

(defconst ai-code-session-link--visible-image-preview-prefix-max-lines 8
  "Maximum previous terminal rows used to rebuild a wrapped image path.")

(defvar ai-code-session-link--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ai-code-session-navigate-link-at-mouse)
    (define-key map [mouse-2] #'ai-code-session-navigate-link-at-mouse)
    (define-key map (kbd "RET") #'ai-code-session-navigate-link-at-point)
    map)
  "Keymap used for clickable session links.")

(defvar ai-code-session-link--symbol-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ai-code-session-link-navigate-symbol-at-mouse)
    (define-key map [mouse-2] #'ai-code-session-link-navigate-symbol-at-mouse)
    (define-key map (kbd "RET") #'ai-code-session-link-navigate-symbol-at-point)
    map)
  "Keymap used for clickable session symbols.")

(defconst ai-code-session-link--managed-properties
  '(ai-code-session-link nil
    ai-code-session-symbol-link nil
    ai-code-session-symbol-file nil
    ai-code-session-hover-link nil
    mouse-face nil
    help-echo nil
    keymap nil
    follow-link nil
    font-lock-face nil
    face nil)
  "Text properties managed by AI Code session linkification.")

(defconst ai-code-session-link--linkify-rules-version 2
  "Version of session linkification rules used by unchanged-region caching.")

(defconst ai-code-session-link--linkify-min-tail-width 512
  "Minimum number of tail characters to rescan for session links.")

(defconst ai-code-session-link--linkify-redraw-delay 0
  "Seconds to wait before relinkifying recent terminal output.")

(defconst ai-code-session-link--linkify-inhibited-retry-delay 0.05
  "Seconds to wait before retrying linkification after inhibition.")

(defvar-local ai-code-session-link-inhibit-functions nil
  "Abnormal hook run before delayed session linkification.

Each function is called with the current buffer.  If any function
returns non-nil, delayed linkification is retried later instead of
touching text properties immediately.")

(defvar-local ai-code-session-link-image-preview-position-function nil
  "Optional owner that decides whether a preview position is stable.
The function receives START and END buffer positions.  A nil return suppresses
the preview while leaving the underlying file link intact.")

(defvar-local ai-code-session-link-image-preview-transaction-function nil
  "Optional owner for an atomic image-preview update.
The function receives START, END, and a zero-argument FUNCTION.  It must call
FUNCTION exactly once and return its result after preserving any external
display or window invariants.")

(defvar-local ai-code-session-link-image-preview-source-function nil
  "Optional resolver for a previously captured image source.
The function receives LINK-TEXT and may return a plist containing `:file',
`:data', and `:signature'.  This lets a terminal backend preserve temporary
image bytes before its renderer materializes the link in the Emacs buffer.")

(defconst ai-code-session-link--url-pattern-regexp
  "\\(https?://[^][(){}<>\"' \t\n]+\\)"
  "Regexp matching http/https URLs in session buffers.")

(defconst ai-code-session-link--url-fragment-regexp
  "[^][(){}<>\"' \t\n\r]+"
  "Regexp matching one terminal row fragment of a URL.")

(defconst ai-code-session-link--url-continuation-end-cue-regexp
  "[?#&=._%+-]"
  "Regexp matching URL punctuation that can end a wrapped row.")

(defconst ai-code-session-link--url-continuation-start-cue-regexp
  "[/?#&=._%+-]"
  "Regexp matching URL punctuation that can start a wrapped row.")

(defconst ai-code-session-link--url-query-fragment-regexp
  "[?#&=]"
  "Regexp matching query punctuation inside a wrapped URL fragment.")

(defconst ai-code-session-link--url-mid-token-cue-regexp
  "[-._~%]"
  "Regexp matching path token punctuation that suggests a URL wraps mid-token.")

(defconst ai-code-session-link--url-mid-token-fragment-regexp
  "\\`[[:alnum:]][[:alnum:]._~%-]*\\'"
  "Regexp matching a URL path token fragment split across terminal rows.")

(defconst ai-code-session-link--wrapped-url-max-lines 8
  "Maximum number of terminal rows inspected for one wrapped URL.")

(defconst ai-code-session-link--symbol-neighborhood-max-width 168
  "Maximum number of characters to scan for symbols near a file link.")

(defconst ai-code-session-link--symbol-neighborhood-max-lines 3
  "Maximum number of lines to scan for symbols near a file link.")

(defconst ai-code-session-link--symbol-neighborhood-max-candidates 24
  "Maximum number of raw symbol candidates to inspect near one file link.")

(defconst ai-code-session-link--symbol-neighborhood-max-links 12
  "Maximum number of symbol links to apply near one file link.")

(defconst ai-code-session-link--path-base-regexp
  "@?\\(?:file:\\(?://localhost\\|//\\)?\\)?[[:alnum:]_./~%+-]*[./][[:alnum:]_./~%+-]+"
  "Regexp matching a local file-like or directory-like path.")

(defconst ai-code-session-link--path-fragment-regexp
  "[-[:alnum:]_./~%+:\\\\]+"
  "Regexp matching one terminal row fragment of a local path.")

(defconst ai-code-session-link--wrapped-path-seed-regexp
  "@?\\(?:file:\\(?://localhost\\|//\\)?\\)?[-[:alnum:]_./~%+:\\\\]*[./][-[:alnum:]_./~%+:\\\\]*"
  "Regexp matching a first-row path fragment that may wrap.
Unlike `ai-code-session-link--path-base-regexp', this accepts fragments
ending at a directory separator, such as \"src/\".")

(defconst ai-code-session-link--wrapped-path-max-lines 8
  "Maximum number of terminal rows inspected for one wrapped path.")

(defconst ai-code-session-link--file-suffix-regexp
  (concat
   "\\(?:"
   "#L[0-9]+\\(?:-L?[0-9]+\\)?"
   "\\|"
   ":L[0-9]+\\(?:-L?[0-9]+\\)?"
   "\\|"
   ":[0-9]+:[0-9]+\\>"
   "\\|"
   ":[0-9]+-[0-9]+\\>"
   "\\|"
   ":[0-9]+\\>"
   "\\)")
  "Regexp matching the optional line or column suffix of a file link.")

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

(defconst ai-code-session-link--recent-output-candidate-regexp
  (concat
   "\\(?:https?://"
   "\\|"
   ai-code-session-link--path-base-regexp
   "\\(?:[#:(][[:alnum:],L-]+\\)?"
   "\\|"
   ai-code-session-link--symbol-identifier-regexp "()"
   "\\|"
   ai-code-session-link--symbol-identifier-regexp
   "\\(?:\\.\\|::\\|#\\)"
   ai-code-session-link--symbol-identifier-regexp
   "\\|"
   ai-code-session-link--snake-case-symbol-regexp
   "\\|"
   "[[:alpha:]_][[:alnum:]_*!?]*--[[:alpha:]_*!?-]+"
   "\\|"
   "[[:alpha:]_][[:alnum:]_*!?]*-\\(?:mode\\|hook\\|command\\|function\\|local\\|p\\)\\)")
  "Regexp matching recent output that may contain complete session links.")

(defun ai-code-session-link--path-pattern (suffix)
  "Return a session link regexp for path base plus SUFFIX."
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

(defconst ai-code-session-link--basename-file-extensions
  '("bash" "c" "cc" "cjs" "clj" "cpp" "cs" "css" "cxx" "el" "erl" "ex"
    "exs" "fish" "go" "h" "hh" "hpp" "hrl" "html" "java" "js" "json" "jsx"
    "kt" "kts" "less" "lock" "m" "md" "mjs" "mm" "org" "php" "py" "rb" "rs"
    "scala" "scss" "sh" "sql" "svelte" "swift" "toml" "ts" "tsx" "txt" "vue"
    "xml" "yaml" "yml" "zsh")
  "Lowercase extensions accepted for basename file links.")

(defconst ai-code-session-link--image-file-extensions
  '("apng" "avif" "bmp" "gif" "heic" "heif" "jpeg" "jpg" "pbm" "pgm"
    "png" "ppm" "svg" "tif" "tiff" "webp" "xbm" "xpm")
  "Lowercase image extensions accepted for Ghostel session previews.")

(defconst ai-code-session-link--file-url-prefix-regexp
  "file:\\(?://localhost\\|//\\)?"
  "Regexp matching local file URL prefixes accepted in session output.")

(defconst ai-code-session-link--image-extension-regexp
  (regexp-opt ai-code-session-link--image-file-extensions)
  "Regexp matching image extensions accepted for Ghostel previews.")

(defconst ai-code-session-link--wrapped-path-continuation-regexp
  "\\(?:\n[ \t]*[-[:alnum:]_./~%+:\\\\]+\\)*"
  "Regexp matching terminal-wrapped continuation segments of a path.
Ghostel hard-wraps long lines, so a single file path can span several
terminal rows (a common case for tools, such as Claude, that print bare
paths rather than `file://' URLs).  Each continuation row is glued back
together by `ai-code-session-link--normalize-file', which strips the
embedded newline and leading indentation.")

(defconst ai-code-session-link--file-url-image-regexp
  (concat
   "\\(" ai-code-session-link--file-url-prefix-regexp
   "\\(?:\n[ \t]*\\)?[-[:alnum:]_./~%+:\\\\]+"
   "\\(?:\n[ \t]*[-[:alnum:]_./~%+:\\\\]+\\)*"
   "\\.\\(?:"
   ai-code-session-link--image-extension-regexp
   "\\)\\)")
  "Regexp matching file URL image references, including terminal wrapping.")

(defconst ai-code-session-link--local-image-path-regexp
  (concat
   "\\(?:~\\|/\\|\\.\\.?/\\|[[:alnum:]_.~-]+/\\)"
   "[^][(){}<>\"'\n\r]*"
   ai-code-session-link--wrapped-path-continuation-regexp
   "\\.\\(?:"
   ai-code-session-link--image-extension-regexp
   "\\)")
  "Regexp matching local image paths that may contain spaces.
Tolerates Ghostel hard-wrapping via
`ai-code-session-link--wrapped-path-continuation-regexp'.")

(defconst ai-code-session-link--wrapped-image-reference-regexp
  (concat
   "[^][(){}<>\"'\n\r]*"
   ai-code-session-link--wrapped-path-continuation-regexp
   "\\.\\(?:"
   ai-code-session-link--image-extension-regexp
   "\\)")
  "Regexp matching image references inside an explicit wrapper.
Tolerates Ghostel hard-wrapping via
`ai-code-session-link--wrapped-path-continuation-regexp'.")

(defconst ai-code-session-link--image-reference-patterns
  (list
   (list ai-code-session-link--file-url-image-regexp 1)
   (list (concat "[\"'`]\\(" ai-code-session-link--wrapped-image-reference-regexp "\\)[\"'`]")
         1)
   (list (concat "[(<]\\(" ai-code-session-link--wrapped-image-reference-regexp "\\)[)>]")
         1)
   (list (concat "\\[\\[\\(file:[^]\n\r]*\\.\\(?:"
                 ai-code-session-link--image-extension-regexp
                 "\\)\\)\\(?:\\]\\[[^]\n\r]*\\)?\\]\\]")
         1)
   (list (concat "\\(?:^\\|[ \t:]\\)\\(" ai-code-session-link--local-image-path-regexp "\\)")
         1))
  "Patterns used to detect image file references with looser path syntax.")

(defconst ai-code-session-link--reference-wrapper-left-regexp
  (rx (+ (any "\"'`<([{")))
  "Regexp matching wrapper characters before a session file reference.")

(defconst ai-code-session-link--reference-wrapper-right-regexp
  (rx (+ (any "\"'`>)}],.;:!?")))
  "Regexp matching wrapper characters after a session file reference.")

(defvar-local ai-code-session-link--linkify-timer nil
  "Timer used to re-linkify recent terminal output after redraw settles.")

(defvar-local ai-code-session-link--pending-tail-width 0
  "Pending tail width to rescan when delayed session linkification runs.")

(defvar-local ai-code-session-link--buffer-project-files-cache nil
  "Buffer-local project file cache reused across session relinkify passes.")

(defvar-local ai-code-session-link--last-region-bounds nil
  "Last relinkified region bounds used to skip unchanged property churn.")

(defvar-local ai-code-session-link--last-region-text nil
  "Last relinkified region text used to skip unchanged property churn.")

(defvar-local ai-code-session-link--last-region-rules-version nil
  "Linkification rules version used for the last relinkified region.")

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
           (unwrapped (replace-regexp-in-string "[\n\r][ \t]*" "" trimmed))
           (without-wrappers
            (string-trim
             unwrapped
             ai-code-session-link--reference-wrapper-left-regexp
             ai-code-session-link--reference-wrapper-right-regexp))
           (without-at (string-remove-prefix "@" without-wrappers))
           (file-url-p (string-match-p "\\`file:" without-at))
           (without-file-prefix
            (replace-regexp-in-string
             (concat "\\`" ai-code-session-link--file-url-prefix-regexp)
             "" without-at))
           (without-shell-escaped-spaces
            (replace-regexp-in-string "\\\\ " " " without-file-prefix))
           (normalized (if file-url-p
                           (or (ignore-errors
                                 (url-unhex-string without-shell-escaped-spaces))
                               without-shell-escaped-spaces)
                         without-shell-escaped-spaces)))
      (unless (string-empty-p normalized)
        normalized))))

(defun ai-code-session-link--normalize-link-text (text)
  "Normalize visible session link TEXT for stored navigation data."
  (when (stringp text)
    (replace-regexp-in-string "[\n\r][ \t]*" "" text)))

(defun ai-code-session-link--normalize-url-link-text (text)
  "Normalize visible URL TEXT across terminal-wrapped rows."
  (when (stringp text)
    (replace-regexp-in-string "[ \t]*[\n\r][ \t]*" "" text)))

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

(defun ai-code-session-link--buffer-project-files-cache ()
  "Return the buffer-local cache of enumerated project files."
  (or ai-code-session-link--buffer-project-files-cache
      (setq ai-code-session-link--buffer-project-files-cache
            (make-hash-table :test 'equal))))

(defun ai-code-session-link--unchanged-region-p (bounds region-text)
  "Return non-nil when BOUNDS and REGION-TEXT match the last relinkified region."
  (and (equal ai-code-session-link--last-region-bounds bounds)
       (equal ai-code-session-link--last-region-rules-version
              ai-code-session-link--linkify-rules-version)
       (equal ai-code-session-link--last-region-text region-text)))

(defun ai-code-session-link--project-files (root)
  "Return absolute project files for ROOT."
  (when (file-directory-p root)
    (ai-code-session-link--cache-get-or-compute
     ai-code-session-link--project-files-cache
     (expand-file-name root)
     (lambda ()
       (or (ignore-errors
             (when-let* ((project (project-current nil root)))
               (let ((project-root (expand-file-name (project-root project))))
                 (mapcar (lambda (file)
                           (if (file-name-absolute-p file)
                               (expand-file-name file)
                             (expand-file-name file project-root)))
                         (project-files project)))))
           (directory-files-recursively root ".*" t))))))

(defun ai-code-session-link--in-project-file-p (file root &optional project-files)
  "Return non-nil when FILE exists and belongs to ROOT.
Optional PROJECT-FILES supplies the project file list."
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
                       (when-let* ((project (project-current nil default-directory)))
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
  (seq-find (lambda (candidate)
              (and (not (file-remote-p candidate))
                   (file-exists-p candidate)))
            (ai-code-session-link--local-path-candidates path root)))

(defun ai-code-session-link--syntactic-file-link-candidate-p (path)
  "Return non-nil when PATH is syntactically file-like."
  (when-let* ((normalized (ai-code-session-link--normalize-file path)))
    (let ((extension (file-name-extension normalized)))
      (or (string-match-p "\\`file:" normalized)
          (file-name-absolute-p normalized)
          (string-prefix-p "~/" normalized)
          (string-prefix-p "./" normalized)
          (string-prefix-p "../" normalized)
          (string-match-p "[/\\\\]" normalized)
          (and extension
               (member (downcase extension)
                       ai-code-session-link--basename-file-extensions))))))

(defun ai-code-session-link--cheap-file-link-candidate-p
    (path &optional root allow-local-probing)
  "Return non-nil when PATH is worth linkifying without project scans.
Optional ROOT is the session project root used for bounded local existence
checks.  When ALLOW-LOCAL-PROBING is nil, only syntactic checks are used.
Expensive project-wide resolution stays in
`ai-code-session-link--resolve-session-file' on activation."
  (when-let* ((normalized (ai-code-session-link--normalize-file path)))
    (let ((extension (file-name-extension normalized)))
      (or (and allow-local-probing
               (ai-code-session-link--resolve-existing-local-path
                normalized root))
          (and (not (file-name-absolute-p normalized))
               (or (string-prefix-p "./" normalized)
                   (string-prefix-p "../" normalized)
                   (string-match-p "[/\\\\]" normalized)
                   (and extension
                        (member (downcase extension)
                                ai-code-session-link--basename-file-extensions))))
          (and (not allow-local-probing)
               (ai-code-session-link--syntactic-file-link-candidate-p
                normalized))))))

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

(defun ai-code-session-link--ghostel-session-p ()
  "Return non-nil when the current buffer is a Ghostel-managed AI session."
  (and (boundp 'ai-code-backends-infra--session-terminal-backend)
       (eq ai-code-backends-infra--session-terminal-backend 'ghostel)))

(defun ai-code-session-link--trusted-local-session-p ()
  "Return non-nil when session file probing is safe for local preview."
  (not (file-remote-p
        (or ai-code-backends-infra--session-directory
            default-directory))))

(defun ai-code-session-link--image-preview-enabled-p ()
  "Return non-nil when image previews should be applied in this buffer."
  (and ai-code-session-link-ghostel-image-preview-enabled
       (ai-code-session-link--ghostel-session-p)
       (ai-code-session-link--trusted-local-session-p)
       (display-images-p)))

(defun ai-code-session-link--image-extension-p (file)
  "Return non-nil when FILE has a known image file extension."
  (when-let* ((extension (file-name-extension file)))
    (member (downcase extension)
            ai-code-session-link--image-file-extensions)))

(defun ai-code-session-link--safe-local-image-file-p (file)
  "Return non-nil when FILE is a local image safe enough to preview."
  (and (stringp file)
       (ai-code-session-link--image-extension-p file)
       (not (file-remote-p file))
       (file-regular-p file)
       (file-readable-p file)
       (let ((attrs (file-attributes file 'integer)))
         (and attrs
              (<= (file-attribute-size attrs)
                  ai-code-session-link-ghostel-image-preview-max-bytes)))))

(defun ai-code-session-link--image-preview-file-signature (file)
  "Return a size and modification-time signature for image FILE."
  (when-let* ((attributes (file-attributes file 'integer)))
    (cons (file-attribute-size attributes)
          (file-attribute-modification-time attributes))))

(defun ai-code-session-link--parse-file-link-text (text)
  "Parse file-like session link TEXT into a plist."
  (when-let* ((text (ai-code-session-link--normalize-link-text text)))
    (if (or (string-match-p "\\`file:" text)
            (when-let* ((normalized (ai-code-session-link--normalize-file text)))
              (ai-code-session-link--image-extension-p normalized)))
        (list :file text)
      (catch 'parsed
        (dolist (pattern ai-code-session-link--file-patterns)
          (let ((regexp (concat "\\`" (car pattern) "\\'")))
            (when (string-match regexp text)
              (throw
               'parsed
               (list :file (match-string (nth 1 pattern) text)
                     :line-start (when-let* ((group (nth 2 pattern))
                                             (line (match-string group text)))
                                   (string-to-number line))
                     :column-start (when-let* ((group (nth 3 pattern))
                                               (column (match-string group text)))
                                     (string-to-number column)))))))))))

(defun ai-code-session-link--image-preview-file (link-text)
  "Return an absolute local image file resolved from LINK-TEXT, or nil."
  (when-let* ((link (ai-code-session-link--parse-file-link-text link-text))
              (file (plist-get link :file))
              ((ai-code-session-link--image-extension-p file))
              (abs-file (ai-code-session-link--resolve-session-file file))
              ((ai-code-session-link--safe-local-image-file-p abs-file)))
    abs-file))

(defun ai-code-session-link--delete-image-preview-overlays
    (start end &optional stale-only)
  "Delete image preview overlays touching START through END.
When STALE-ONLY is non-nil, preserve overlays still anchored to their image
link text."
  (let* ((line-start (save-excursion
                       (goto-char start)
                       (line-beginning-position)))
         (line-end (save-excursion
                     (goto-char end)
                     (line-end-position)))
         (scan-end (min (point-max) (1+ line-end))))
    (dolist (overlay (delete-dups
                      (append (overlays-in line-start scan-end)
                              (overlays-at line-start)
                              (overlays-at start)
                              (overlays-at end)
                              (overlays-at line-end)
                              (overlays-at scan-end))))
      (when (and (overlay-get overlay 'ai-code-session-image-preview)
                 (or (not stale-only)
                     (not
                      (ai-code-session-link--live-image-preview-overlay-p
                       overlay))))
        (ai-code-session-link--delete-image-preview-overlay overlay)))))

(defun ai-code-session-link--delete-image-preview-overlay (overlay)
  "Delete image preview OVERLAY and its companion suffix overlay."
  (when-let* ((suffix-overlay
               (overlay-get overlay
                            'ai-code-session-image-preview-suffix-overlay))
              ((overlayp suffix-overlay)))
    (delete-overlay suffix-overlay))
  (delete-overlay overlay))

(defun ai-code-session-link--image-preview-tree-prefix-p (match-start)
  "Return non-nil when MATCH-START follows a leading tree prefix.
The prefix may appear on the image link's row or on the immediately preceding
row, as it does in multiline tool-result output.  Accept both `╰' and the
square-corner `└' emitted by terminal clients."
  (save-excursion
    (goto-char match-start)
    (let* ((line-start (line-beginning-position))
           (same-row-prefix
            (buffer-substring-no-properties line-start match-start)))
      (or (string-match-p "\\`[ \t]*[╰└][ \t]*\\'" same-row-prefix)
          (and (> line-start (point-min))
               (progn
                 (goto-char line-start)
                 (forward-line -1)
                 (string-match-p
                  "\\`[ \t]*[╰└][ \t]*\\'"
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))))))))

(defun ai-code-session-link--image-preview-layout (match-start)
  "Return (SIZE-INDENT . BEFORE-STRING) for MATCH-START.
SIZE-INDENT preserves the width allowance for the path's leading gutter.
BEFORE-STRING starts the preview on its own row at the same indentation.
The source path remains visible on its original terminal row."
  (save-excursion
    (goto-char match-start)
    (let* ((column (current-column))
           (line-start (line-beginning-position))
           (line-end (line-end-position))
           (leading-indent
            (progn
              (goto-char line-start)
              (skip-chars-forward " \t" line-end)
              (buffer-substring-no-properties line-start (point)))))
      (let ((indent
             (if (ai-code-session-link--image-preview-tree-prefix-p match-start)
                 (make-string column ?\s)
               leading-indent)))
        (cons indent (concat "\n" indent))))))

(defun ai-code-session-link--image-preview-link-file (link-text)
  "Return the local image file for previewable LINK-TEXT, or nil."
  (when (ai-code-session-link--image-preview-enabled-p)
    (ai-code-session-link--image-preview-file link-text)))

(defun ai-code-session-link--image-preview-vertical-margin ()
  "Return the current image preview vertical margin in pixels."
  (let ((setting ai-code-session-link-ghostel-image-preview-vertical-margin))
    (cond
     ((integerp setting) (max 0 setting))
     ((eq setting 'auto)
      (let* ((window (get-buffer-window (current-buffer) t))
             (line-height
              (ignore-errors
                (if window
                    (with-selected-window window (default-line-height))
                  (default-line-height)))))
        (if (and (numberp line-height) (> line-height 0))
            (max 1
                 (round
                  (* ai-code-session-link--image-preview-auto-margin-ratio
                     line-height)))
          6)))
     (t 0))))

(defun ai-code-session-link--live-image-preview-overlay-p (overlay)
  "Return non-nil when OVERLAY is still anchored to its image link text."
  (and (overlayp overlay)
       (eq (overlay-buffer overlay) (current-buffer))
       (overlay-get overlay 'ai-code-session-image-preview)
       (< (overlay-start overlay) (overlay-end overlay))
       (let* ((link-start
               (overlay-get overlay 'ai-code-session-image-link-start))
              (link-text (overlay-get overlay 'ai-code-session-image-link-text))
             (file (overlay-get overlay 'ai-code-session-image-file))
             (file-signature
              (overlay-get overlay 'ai-code-session-image-file-signature))
             (current-file-signature
              (and file
                   (ai-code-session-link--image-preview-file-signature file)))
             (vertical-margin
              (overlay-get overlay 'ai-code-session-image-vertical-margin))
             (render-context
              (overlay-get overlay 'ai-code-session-image-render-context))
             (layout
              (ai-code-session-link--image-preview-layout
               link-start))
             (before-string (cdr layout))
             (display-text
              (or (overlay-get overlay 'ai-code-session-image-display-text)
                  (overlay-get overlay 'ai-code-session-image-link-text)))
             (display-end
              (or (overlay-get overlay 'ai-code-session-image-display-end)
                  (overlay-end overlay))))
         (and (integer-or-marker-p link-start)
              (<= (point-min) link-start)
              (< link-start (point-max))
              link-text
              file
              file-signature
              ;; Previews contain decoded image data, so a vanished temporary
              ;; source does not invalidate an otherwise stable snapshot.
              (or (not current-file-signature)
                  (equal file-signature current-file-signature))
              (integerp vertical-margin)
              (= vertical-margin
                 (ai-code-session-link--image-preview-vertical-margin))
              render-context
              (equal
               render-context
                (ai-code-session-link--image-preview-render-context
                 (ai-code-session-link--image-preview-max-dimensions
                 (car layout))))
              (equal (overlay-get overlay 'before-string) before-string)
              display-text
              (integer-or-marker-p display-end)
              (<= link-start display-end)
              (<= display-end (point-max))
              (= (overlay-start overlay) display-end)
              (= (overlay-end overlay) (1+ display-end))
              (eq (char-after display-end) ?\n)
              (equal (overlay-get overlay 'after-string) "\n")
              (equal (buffer-substring-no-properties
                      link-start display-end)
                     display-text)))))

(defun ai-code-session-link--live-image-preview-overlays-in-region (start end)
  "Return live image preview overlays between START and END."
  (cl-remove-if-not
   #'ai-code-session-link--live-image-preview-overlay-p
   (delete-dups
    (append (overlays-in start end)
            (overlays-at start)
            (overlays-at end)))))

(defun ai-code-session-link--image-preview-overlay-at
    (start end display-text)
  "Return the live preview at START through END for DISPLAY-TEXT, or nil."
  (let* ((line-start (save-excursion
                       (goto-char start)
                       (line-beginning-position)))
         (line-end (save-excursion
                     (goto-char end)
                     (line-end-position)))
         (scan-end (min (point-max) (1+ line-end))))
    (cl-find-if
     (lambda (overlay)
       (and (ai-code-session-link--live-image-preview-overlay-p overlay)
            (= (overlay-get overlay 'ai-code-session-image-link-start)
               start)
            (<= end
                (overlay-get overlay 'ai-code-session-image-link-end))
            (let ((overlay-link
                   (overlay-get overlay 'ai-code-session-image-link-text))
                  (overlay-display
                   (overlay-get overlay 'ai-code-session-image-display-text)))
              (or (equal display-text overlay-link)
                  (equal display-text overlay-display)))))
     (delete-dups
      (append (overlays-in line-start scan-end)
              (overlays-at line-start)
              (overlays-at line-end)
              (overlays-at scan-end))))))

(defun ai-code-session-link--image-preview-overlay-present-p
    (start end display-text)
  "Return non-nil when START to END already has a preview for DISPLAY-TEXT."
  (and (ai-code-session-link--image-preview-overlay-at
        start end display-text)
       t))

(defun ai-code-session-link--image-preview-overlays-overlapping-row-span
    (start end)
  "Return live image previews whose owned row span overlaps START through END.
An image preview semantically owns its source row even though its display
overlay covers only the row's terminating newline."
  (let ((line-start (save-excursion
                      (goto-char start)
                      (line-beginning-position)))
        (line-end (save-excursion
                    (goto-char end)
                    (line-end-position))))
    (cl-remove-if-not
     (lambda (overlay)
       (let ((link-start
              (or (overlay-get overlay 'ai-code-session-image-link-start)
                  (overlay-start overlay)))
             (display-end
              (or (overlay-get overlay
                               'ai-code-session-image-display-end)
                  (overlay-end overlay))))
         (and (ai-code-session-link--live-image-preview-overlay-p overlay)
              (< link-start end)
              (< start display-end))))
     (overlays-in line-start (min (point-max) (1+ line-end))))))

(defun ai-code-session-link--text-may-contain-image-reference-p (text)
  "Return non-nil when TEXT may contain an image reference."
  (and (stringp text)
       (string-match-p
        (concat "\\(?:file:\\|\\.\\(?:" ai-code-session-link--image-extension-regexp "\\)\\)")
        text)))

(defun ai-code-session-link--image-preview-refresh-needed-p (start end)
  "Return non-nil when image previews between START and END need refreshing."
  (catch 'missing-preview
    (let ((ai-code-session-link--project-files-cache
           (ai-code-session-link--buffer-project-files-cache))
          (ai-code-session-link--resolved-path-cache
           (make-hash-table :test 'equal)))
      (dolist (file-link (ai-code-session-link--collect-file-links
                          start end t))
        (let ((link-start (plist-get file-link :start))
              (link-end (plist-get file-link :end))
              (link-text (plist-get file-link :text)))
          (when (and (ai-code-session-link--image-preview-link-file link-text)
                     (not (ai-code-session-link--image-preview-overlay-present-p
                           link-start link-end link-text)))
            (throw 'missing-preview t)))))
    nil))

(defun ai-code-session-link--image-preview-format-hint (file)
  "Return an image MIME-style data hint for FILE, or nil."
  (when-let* ((extension (file-name-extension file)))
    (intern (format "image/%s"
                    (pcase (downcase extension)
                      ("jpg" "jpeg")
                      ("tif" "tiff")
                      (other other))))))

(defun ai-code-session-link--image-preview-data (file)
  "Return unibyte image data read from FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun ai-code-session-link--image-preview-frame ()
  "Return the frame used to render the current buffer's image preview."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (window-frame window)
    (selected-frame)))

(defun ai-code-session-link--image-preview-backing-scale ()
  "Return the image scale matching the current frame's backing pixels.
Emacs image dimensions are expressed in logical pixels, while a HiDPI frame
uses multiple backing pixels for each logical pixel.  Applying the reciprocal
scale lets a high-resolution source provide those backing pixels instead of
first rasterizing it at the lower logical-pixel width."
  (let* ((frame (ai-code-session-link--image-preview-frame))
         (frame-scale
          (if (fboundp 'frame-scale-factor)
              (frame-scale-factor frame)
            1.0)))
    (/ 1.0 (if (and (numberp frame-scale) (> frame-scale 0))
               frame-scale
             1.0))))

(defun ai-code-session-link--image-preview-render-properties
    (source-image max-width max-height)
  "Return HiDPI-aware render properties for SOURCE-IMAGE.
MAX-WIDTH and MAX-HEIGHT are logical-pixel caps.  Explicit raster dimensions
are expressed in backing pixels and then reduced by `:scale', so Emacs does
not discard Retina source pixels before the window server draws the image."
  (let* ((frame (ai-code-session-link--image-preview-frame))
         (size (ignore-errors (image-size source-image t frame)))
         (source-width (and (consp size) (car size)))
         (source-height (and (consp size) (cdr size)))
         (backing-scale
          (ai-code-session-link--image-preview-backing-scale)))
    (if (and (numberp source-width) (> source-width 0)
             (numberp source-height) (> source-height 0))
        (let* ((display-factor
                (apply #'min
                       (delq nil
                             (list 1.0
                                   backing-scale
                                   (and max-width
                                        (/ (float max-width) source-width))
                                   (and max-height
                                        (/ (float max-height)
                                           source-height))))))
               (display-width
                (max 1 (round (* source-width display-factor))))
               (display-height
                (max 1 (round (* source-height display-factor))))
               (raster-width
                (max 1 (round (/ display-width backing-scale))))
               (raster-height
                (max 1 (round (/ display-height backing-scale)))))
          (list :width raster-width
                :height raster-height
                :scale backing-scale))
      ;; If Emacs cannot inspect the source dimensions, retain a bounded
      ;; fallback instead of refusing to display an otherwise valid image.
      (append (list :scale backing-scale)
              (when max-width (list :max-width max-width))
              (when max-height (list :max-height max-height))))))

(defun ai-code-session-link--image-preview-render-context (dimensions)
  "Return the display context governing a preview with DIMENSIONS.
The result changes when the active frame's backing scale or the logical-pixel
caps change, so an existing overlay can be rebuilt instead of reusing a stale
1x raster after a frame move or window resize."
  (list :backing-scale (ai-code-session-link--image-preview-backing-scale)
        :max-width (car dimensions)
        :max-height (cdr dimensions)))

(defconst ai-code-session-link--image-preview-fallback-max-width 960
  "Width cap used when no window is available to fit the preview to.")

(defun ai-code-session-link--image-preview-max-dimensions (indent)
  "Return a logical-pixel (MAX-WIDTH . MAX-HEIGHT) image preview cap.
An explicit integer custom is used verbatim as a hard cap.  When the custom
is nil the preview fits the session window's body width, reduced by INDENT so
the image never overflows into a horizontal scroll.  Height is uncapped by
default so tall previews can be inspected at arbitrary pixel offsets.  These
are upper bounds only: source images are never enlarged.  On HiDPI frames,
the renderer requests the corresponding backing-pixel dimensions.  A fixed
width fallback applies when no window shows the buffer."
  (let* ((window (get-buffer-window (current-buffer) t))
         (char-width (frame-char-width (if window
                                           (window-frame window)
                                         (selected-frame))))
         (indent-pixels (* (string-width (or indent "")) char-width)))
    (cons
     (cond
      ((integerp ai-code-session-link-ghostel-image-preview-max-width)
       ai-code-session-link-ghostel-image-preview-max-width)
      (window
       (max 1 (- (window-body-width window t) indent-pixels char-width)))
      (t ai-code-session-link--image-preview-fallback-max-width))
     (and (integerp ai-code-session-link-ghostel-image-preview-max-height)
          ai-code-session-link-ghostel-image-preview-max-height))))

(defun ai-code-session-link--create-image-preview
    (file max-width max-height vertical-margin &optional captured-data)
  "Create a data-backed preview image for FILE.
MAX-WIDTH and MAX-HEIGHT are logical-pixel caps.  A nil cap leaves that
dimension unbounded.  VERTICAL-MARGIN is the pixel gap above and below it.
Optional CAPTURED-DATA supplies bytes read before FILE disappeared.

Using image data instead of a file-backed image spec keeps inline previews
stable when `image-mode' opens and flushes the same image file.  On a HiDPI
frame the final descriptor requests enough backing pixels for the preview's
logical display size instead of letting Emacs rasterize it at 1x first."
  (let* ((data (or captured-data
                   (ai-code-session-link--image-preview-data file)))
         (data-p (or (ai-code-session-link--image-preview-format-hint file)
                     t))
         (type (ignore-errors (image-type data nil data-p))))
    (ignore-errors
      (when-let* ((source-image
                   (create-image data type data-p :scale 1.0)))
        (unwind-protect
            (apply #'create-image
                   data type data-p
                   (append
                    (ai-code-session-link--image-preview-render-properties
                     source-image max-width max-height)
                    (when (> vertical-margin 0)
                      (list :margin (cons 0 vertical-margin)))))
          ;; Test doubles and optional image types do not necessarily support
          ;; cache flushing.  A cleanup failure must not discard the valid
          ;; preview produced above.
          (ignore-errors (image-flush source-image)))))))

(defun ai-code-session-link--apply-image-preview (match-start match-end link-text)
  "Display an image preview for LINK-TEXT after MATCH-START through MATCH-END."
  (when (ai-code-session-link--image-preview-enabled-p)
    (when-let* ((file (ai-code-session-link--image-preview-file link-text)))
      (ai-code-session-link--apply-image-preview-for-file
       match-start match-end link-text file))))

(defun ai-code-session-link--apply-image-preview-for-file
    (match-start match-end link-text file &optional display-text captured-data
                 captured-signature)
  "Display FILE preview for LINK-TEXT after MATCH-START through MATCH-END.
Optional DISPLAY-TEXT is the terminal text owned by the preview.
CAPTURED-DATA and CAPTURED-SIGNATURE preserve a temporary file snapshot."
  (when (ai-code-session-link--image-preview-enabled-p)
    (if (and
         (functionp ai-code-session-link-image-preview-position-function)
         (not
          (funcall ai-code-session-link-image-preview-position-function
                   match-start match-end)))
        (ai-code-session-link--delete-image-preview-overlays
         match-start match-end)
      (let* (;; The preview replaces the row's real terminating newline, not
             ;; the source path.  A virtual newline before and after the image
             ;; gives it a dedicated display row while a real buffer character
             ;; continues to carry its measurable jumbo line height.
           (display-end
            (save-excursion
              (goto-char match-end)
              (line-end-position)))
           (display-anchor-end
            (and (< display-end (point-max))
                 (eq (char-after display-end) ?\n)
                 (1+ display-end)))
           (display-text
            (or display-text
                (buffer-substring-no-properties match-start match-end)))
           (display-text
            (if (> display-end match-end)
                (concat display-text
                        (buffer-substring-no-properties match-end display-end))
              display-text))
           (layout
            (ai-code-session-link--image-preview-layout match-start))
           (indent (car layout))
           (before-string (cdr layout))
           (vertical-margin
            (ai-code-session-link--image-preview-vertical-margin))
           (dimensions
            (ai-code-session-link--image-preview-max-dimensions indent))
           (existing
            (ai-code-session-link--image-preview-overlay-at
             match-start match-end display-text))
           (overlapping
            (ai-code-session-link--image-preview-overlays-overlapping-row-span
             match-start display-end))
           (earlier
            (cl-some
             (lambda (overlay)
               (< (or (overlay-get overlay
                                   'ai-code-session-image-link-start)
                      (overlay-start overlay))
                  match-start))
             overlapping)))
      ;; The first preview on a terminal row owns the row through DISPLAY-END.
      ;; A later image-looking suffix remains ordinary text replayed below that
      ;; preview.  In particular, never leave two live image overlays fighting
      ;; a companion suffix overlay for the same characters: Ghostel redraws
      ;; can otherwise alternate between the two resulting row heights.
      (if earlier
          (dolist (overlay overlapping)
            (when (>= (or (overlay-get overlay
                                       'ai-code-session-image-link-start)
                          (overlay-start overlay))
                      match-start)
              (ai-code-session-link--delete-image-preview-overlay overlay)))
        (dolist (overlay overlapping)
          (unless (eq overlay existing)
            (ai-code-session-link--delete-image-preview-overlay overlay)))
        (unless (or existing (not display-anchor-end))
          (when-let* ((image (ai-code-session-link--create-image-preview
                              file (car dimensions) (cdr dimensions)
                              vertical-margin captured-data)))
            (let ((overlay
                   (make-overlay display-end display-anchor-end nil nil nil)))
              (overlay-put overlay 'ai-code-session-image-preview t)
              (overlay-put overlay 'ai-code-session-image-file file)
              (overlay-put overlay 'ai-code-session-image-file-signature
                           (or captured-signature
                               (ai-code-session-link--image-preview-file-signature
                                file)))
              (overlay-put overlay 'ai-code-session-image-vertical-margin
                           vertical-margin)
              (overlay-put overlay 'ai-code-session-image-render-context
                           (ai-code-session-link--image-preview-render-context
                            dimensions))
              (overlay-put overlay 'ai-code-session-image-link-start match-start)
              (overlay-put overlay 'ai-code-session-image-link-text link-text)
              (overlay-put overlay 'ai-code-session-image-link-end match-end)
              (overlay-put overlay 'ai-code-session-image-display-text display-text)
              (overlay-put overlay 'ai-code-session-image-display-end display-end)
              ;; A display property anchored to real terminal text gives Emacs
              ;; a measurable jumbo line.  `ultra-scroll' can then preserve
              ;; partial image pixels, unlike a virtual after-string image.  By
              ;; anchoring it to the terminating newline, the linked path stays
              ;; visible and clickable on the preceding row.
              (overlay-put overlay 'before-string before-string)
              (overlay-put overlay 'display image)
              (overlay-put overlay 'after-string "\n")
              (overlay-put overlay 'help-echo (format "Image preview: %s" file))
              overlay))))))))

(defun ai-code-session-link--apply-properties (start end &optional text help-echo)
  "Apply session link properties from START to END.
Optional TEXT overrides the stored link text.
Optional HELP-ECHO overrides the hover help text."
  (let ((link-text
         (ai-code-session-link--normalize-link-text
          (or text (buffer-substring-no-properties start end)))))
    (ai-code-session-link--apply-link-properties-to-visible-text
     start end
     (list 'ai-code-session-link link-text
           'mouse-face 'highlight
           'help-echo help-echo
           'keymap ai-code-session-link--keymap
           'follow-link t
           'font-lock-face 'link
           'face 'link))))

(defun ai-code-session-link--restore-live-image-preview-link-properties
    (start end)
  "Restore image-link properties for live previews touching START through END.
This keeps a data-backed preview navigable after its temporary source file has
disappeared and generic relinkification has cleared managed properties."
  (dolist (overlay
           (delete-dups
            (append (overlays-in start end)
                    (overlays-at start)
                    (overlays-at end))))
    (when (ai-code-session-link--live-image-preview-overlay-p overlay)
      (when-let* ((link-text
                   (overlay-get overlay
                                'ai-code-session-image-link-text))
                  (link-start
                   (overlay-get overlay
                                'ai-code-session-image-link-start))
                  (link-end
                   (or (overlay-get overlay
                                    'ai-code-session-image-link-end)
                       (overlay-end overlay)))
                  ((<= link-start link-end))
                  ((<= link-end (point-max))))
        (ai-code-session-link--apply-properties
         link-start
         link-end
         link-text
         "mouse-1: Visit image file")))))

(defun ai-code-session-link--apply-link-properties-to-visible-text
    (start end properties)
  "Apply PROPERTIES from START to END, skipping wrapped-line indentation.

The visual link face is applied only to visible path fragments, but
`mouse-face' spans the full wrapped range so hovering any fragment highlights
the complete path."
  (when-let* ((mouse-face (plist-get properties 'mouse-face)))
    (add-text-properties
     start end
     (list 'ai-code-session-hover-link t
           'mouse-face mouse-face)))
  (save-excursion
    (goto-char start)
    (let ((segment-start start)
          segment-end)
      (while (< segment-start end)
        (goto-char segment-start)
        (skip-chars-forward " \t" (min end (line-end-position)))
        (setq segment-start (point)
              segment-end (min end (line-end-position)))
        (let ((visible-end segment-end))
          (while (and (< segment-start visible-end)
                      (memq (char-before visible-end) '(?\s ?\t)))
            (setq visible-end (1- visible-end)))
          (when (< segment-start visible-end)
            (add-text-properties segment-start visible-end properties)))
        (setq segment-start
              (if (< segment-end end)
                  (1+ segment-end)
                end))))))

(defun ai-code-session-link--remove-managed-properties-in-spans
    (start end property)
  "Remove managed properties from START to END in spans carrying PROPERTY."
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos property nil end)
                      end)))
        (when (get-text-property pos property)
          (remove-text-properties
           pos next ai-code-session-link--managed-properties))
        (setq pos next)))))

(defun ai-code-session-link--remove-managed-properties (start end)
  "Remove managed session link properties from START to END."
  (ai-code-session-link--remove-managed-properties-in-spans
   start end 'ai-code-session-hover-link)
  ;; Keep removing older linkified regions that predate
  ;; `ai-code-session-hover-link'.
  (ai-code-session-link--remove-managed-properties-in-spans
   start end 'ai-code-session-link))

(defun ai-code-session-link--apply-symbol-properties (start end symbol file-link)
  "Apply clickable SYMBOL properties from START to END using FILE-LINK."
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
  "Return non-nil when CANDIDATE resembles an Elisp symbol worth linking."
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
  "Return non-nil when CANDIDATE resembles a Java-style CamelCase symbol."
  (and (ai-code-session-link--case-sensitive-match-p
        (concat "\\`" ai-code-session-link--camel-case-symbol-regexp "\\'")
        candidate)
       (ai-code-session-link--case-sensitive-match-p "[[:lower:]]" candidate)
       (ai-code-session-link--case-sensitive-match-p
        "[[:upper:]][[:lower:][:digit:]]+[[:upper:]]"
        candidate)))

(defun ai-code-session-link--snake-case-symbol-p (candidate)
  "Return non-nil when CANDIDATE resembles a bare snake_case symbol."
  (ai-code-session-link--case-sensitive-match-p
   "\\`_*[[:lower:]][[:lower:][:digit:]]*\\(?:_[[:lower:][:digit:]]+\\)+\\'"
   candidate))

(defun ai-code-session-link--bare-symbol-candidate-p (candidate)
  "Return non-nil when bare CANDIDATE resembles a supported code symbol."
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
  "Return position after moving LINE-COUNT lines from START.
Do not move beyond END."
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

(defun ai-code-session-link--within-symbol-scan-budget-p
    (candidate-count link-count)
  "Return non-nil when nearby symbol scanning can continue.
CANDIDATE-COUNT and LINK-COUNT are the current scan totals."
  (and (< candidate-count ai-code-session-link--symbol-neighborhood-max-candidates)
       (< link-count ai-code-session-link--symbol-neighborhood-max-links)))

(defun ai-code-session-link--next-nearby-symbol-boundary (start end &optional next-file-start)
  "Return the next boundary after START for symbol scanning up to END.
Optional NEXT-FILE-START caps the returned boundary."
  (let ((boundary end))
    (save-excursion
      (goto-char start)
      (when (re-search-forward ai-code-session-link--url-pattern-regexp end t)
        (setq boundary (min boundary (match-beginning 1)))))
    (if next-file-start
        (min boundary next-file-start)
      boundary)))

(defun ai-code-session-link--symbol-scan-end (scan-start end &optional next-file-start)
  "Return the final nearby symbol scan boundary for SCAN-START up to END.
Optional NEXT-FILE-START caps the returned boundary."
  (ai-code-session-link--next-nearby-symbol-boundary
   scan-start
   (ai-code-session-link--symbol-window-end scan-start end)
   next-file-start))

(defun ai-code-session-link--linkify-symbols-near-file (file-link scan-start end &optional next-file-start)
  "Linkify code-like symbols near FILE-LINK from SCAN-START up to END.
Optional NEXT-FILE-START caps the scan boundary."
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

(defun ai-code-session-link--sort-and-prune-links (links)
  "Return LINKS ordered by start, with contained links removed."
  (let ((sorted
         (sort links
               (lambda (left right)
                 (let ((left-start (plist-get left :start))
                       (right-start (plist-get right :start))
                       (left-end (plist-get left :end))
                       (right-end (plist-get right :end)))
                   (if (= left-start right-start)
                       (> left-end right-end)
                     (< left-start right-start)))))))
    (let (kept)
      (dolist (link sorted)
        (let ((link-start (plist-get link :start))
              (link-end (plist-get link :end)))
          (unless (cl-some
                   (lambda (kept-link)
                     (and (<= (plist-get kept-link :start) link-start)
                          (<= link-end (plist-get kept-link :end))))
                   kept)
            (push link kept))))
      (nreverse kept))))

(defun ai-code-session-link--blank-between-p (start end)
  "Return non-nil when buffer text between START and END is blank."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t" end)
    (>= (point) end)))

(defun ai-code-session-link--blank-or-url-closer-between-p (start end)
  "Return non-nil when START to END is blank or URL closing syntax."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t" end)
    (while (and (< (point) end)
                (memq (char-after) '(?\" ?' ?\) ?\] ?\})))
      (forward-char 1)
      (skip-chars-forward " \t" end))
    (>= (point) end)))

(defun ai-code-session-link--path-token-separator-at-p (position limit)
  "Return non-nil when POSITION is a path token separator before LIMIT."
  (save-excursion
    (goto-char position)
    (and (< (point) limit)
         (memq (char-after) '(?\s ?\t)))))

(defun ai-code-session-link--wrapped-suffix-prefix-between-p (start end)
  "Return non-nil when text between START and END may precede a wrapped suffix."
  (let ((suffix-prefix
         (string-trim
          (buffer-substring-no-properties start end))))
    (or (string-empty-p suffix-prefix)
        (member suffix-prefix '(":" ":L" "#" "#L")))))

(defun ai-code-session-link--line-path-fragment-bounds (start end)
  "Return path fragment bounds between START and END after indentation."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t" end)
    (let ((fragment-start (point)))
      (when (looking-at ai-code-session-link--path-fragment-regexp)
        (let ((fragment-end (min (match-end 0) end)))
          (when (< fragment-start fragment-end)
            (cons fragment-start fragment-end)))))))

(defun ai-code-session-link--file-suffix-end-at (position limit)
  "Return end of a file suffix at POSITION, bounded by LIMIT."
  (save-excursion
    (goto-char position)
    (when (looking-at ai-code-session-link--file-suffix-regexp)
      (let ((suffix-end (match-end 0)))
        (and (<= suffix-end limit) suffix-end)))))

(defun ai-code-session-link--file-link-end-at (base-end line-end)
  "Return file link end from BASE-END, including a suffix before LINE-END."
  (or (ai-code-session-link--file-suffix-end-at base-end line-end)
      base-end))

(defun ai-code-session-link--wrapped-file-link-candidate-p
    (text root allow-local-probing)
  "Return non-nil when wrapped link TEXT is a file reference.
ROOT is used for local existence checks when ALLOW-LOCAL-PROBING is non-nil."
  (when-let* ((parsed (ai-code-session-link--parse-file-link-text text))
              (file (plist-get parsed :file))
              (normalized (ai-code-session-link--normalize-file file)))
    (if allow-local-probing
        (ai-code-session-link--resolve-existing-local-path normalized root)
      (ai-code-session-link--syntactic-file-link-candidate-p normalized))))

(defun ai-code-session-link--wrapped-file-link-at
    (match-start match-end scan-end root allow-local-probing)
  "Return a wrapped file link candidate starting at MATCH-START.
MATCH-END is the end of the first-row path match.  SCAN-END bounds the
search, and ROOT is the session project root.  ALLOW-LOCAL-PROBING
controls whether local existence checks are allowed."
  (save-excursion
    (goto-char match-start)
    (let ((first-line-end (min scan-end (line-end-position)))
          (current-line-end nil)
          (best-link nil)
          (continued-lines 0)
          (scan t))
      (setq current-line-end first-line-end)
      (when (ai-code-session-link--wrapped-suffix-prefix-between-p
             match-end first-line-end)
        (while (and scan
                    (< continued-lines ai-code-session-link--wrapped-path-max-lines)
                    (< current-line-end scan-end)
                    (eq (char-after current-line-end) ?\n))
          (let* ((next-line-start (1+ current-line-end))
                 (next-line-end
                  (save-excursion
                    (goto-char next-line-start)
                    (min scan-end (line-end-position))))
                 (fragment
                  (ai-code-session-link--line-path-fragment-bounds
                   next-line-start next-line-end)))
            (if (not fragment)
                (setq scan nil)
              (let* ((base-end (cdr fragment))
                     (link-end
                      (ai-code-session-link--file-link-end-at
                       base-end next-line-end))
                     (blank-after-link
                      (ai-code-session-link--blank-between-p
                       link-end next-line-end))
                     (token-ends-after-link
                      (ai-code-session-link--path-token-separator-at-p
                       link-end next-line-end)))
                (if (not (or blank-after-link token-ends-after-link))
                    (setq scan nil)
                  (cl-incf continued-lines)
                  (let ((link-text
                         (ai-code-session-link--normalize-link-text
                          (buffer-substring-no-properties
                           match-start link-end))))
                    (when (ai-code-session-link--wrapped-file-link-candidate-p
                           link-text root allow-local-probing)
                      (setq best-link
                            (list :start match-start
                                  :end link-end
                                  :text link-text))))
                  (setq current-line-end next-line-end
                        scan blank-after-link)))))))
      best-link)))

(defun ai-code-session-link--collect-wrapped-file-links
    (start end root allow-local-probing)
  "Return hard-wrapped file link matches between START and END for ROOT.
ALLOW-LOCAL-PROBING controls whether local existence checks are allowed."
  (let (file-links)
    (save-excursion
      (goto-char start)
      (while (re-search-forward ai-code-session-link--wrapped-path-seed-regexp end t)
        (when-let* ((file-link
                     (ai-code-session-link--wrapped-file-link-at
                      (match-beginning 0)
                      (match-end 0)
                      end
                      root
                      allow-local-probing)))
          (push file-link file-links))))
    (nreverse file-links)))

(defun ai-code-session-link--collect-file-links
    (start end allow-local-probing)
  "Return file link matches between START and END.
When ALLOW-LOCAL-PROBING is nil, only syntactic checks are used."
  (let ((root (ai-code-session-link--project-root-for-paths))
        (seen-starts (make-hash-table :test 'eql))
        file-links)
    (setq file-links
          (ai-code-session-link--collect-wrapped-file-links
           start end root allow-local-probing))
    (cl-labels
        ((add-link
          (match-start match-end link-text &optional candidate-text)
          (unless (gethash match-start seen-starts)
            (when (ai-code-session-link--cheap-file-link-candidate-p
                   (or candidate-text link-text) root allow-local-probing)
              (puthash match-start t seen-starts)
              (push (list :start match-start
                          :end match-end
                          :text link-text)
                    file-links)))))
      (save-excursion
        (dolist (pattern ai-code-session-link--image-reference-patterns)
          (goto-char start)
          (while (re-search-forward (car pattern) end t)
            (let* ((capture (cadr pattern))
                   (match-start (match-beginning capture))
                   (match-end (match-end capture))
                   (link-text (match-string-no-properties capture)))
              (add-link match-start match-end link-text))))
        (dolist (pattern ai-code-session-link--file-patterns)
          (goto-char start)
          (while (re-search-forward (car pattern) end t)
            (let ((match-start (match-beginning (nth 1 pattern)))
                  (match-end (match-end 0))
                  (path (match-string-no-properties (nth 1 pattern))))
              (add-link
               match-start match-end
               (buffer-substring-no-properties match-start match-end)
               path))))))
    (ai-code-session-link--sort-and-prune-links file-links)))

(defun ai-code-session-link--trim-url-end (start end)
  "Return URL end from START to END without terminal punctuation."
  (while (and (< start end)
              (memq (char-before end) '(?. ?, ?\; ?: ?! ??)))
    (setq end (1- end)))
  end)

(defun ai-code-session-link--line-hard-wrapped-p (start end)
  "Return non-nil when START to END appears to fill the session window."
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (>= (string-width (buffer-substring-no-properties start end))
        (max 1 (window-body-width window)))))

(defun ai-code-session-link--line-url-fragment-bounds (start end)
  "Return URL fragment bounds between START and END after indentation."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t" end)
    (let ((fragment-start (point)))
      (when (looking-at ai-code-session-link--url-fragment-regexp)
        (let ((fragment-end (min (match-end 0) end)))
          (when (< fragment-start fragment-end)
            (cons fragment-start fragment-end)))))))

(defun ai-code-session-link--url-path-tail (url)
  "Return the final path token in URL, or nil when URL has no path."
  (when (string-match "\\`https?://[^/]+/\\(.+\\)\\'" url)
    (let* ((path (replace-regexp-in-string "[?#].*\\'" "" (match-string 1 url)))
           (tail (car (last (split-string path "/" t)))))
      (and (not (string-empty-p tail)) tail))))

(defun ai-code-session-link--url-mid-token-continuation-p
    (fragment previous-fragment)
  "Return non-nil when FRAGMENT continues PREVIOUS-FRAGMENT mid-token."
  (when-let* ((tail (ai-code-session-link--url-path-tail previous-fragment)))
    (and (not (string-suffix-p "/" previous-fragment))
         (string-match-p ai-code-session-link--url-mid-token-cue-regexp tail)
         (string-match-p ai-code-session-link--url-mid-token-fragment-regexp
                         fragment))))

(defun ai-code-session-link--url-continuation-fragment-p
    (fragment previous-fragment hard-wrap-p)
  "Return non-nil when FRAGMENT can continue PREVIOUS-FRAGMENT.
HARD-WRAP-P means the previous terminal row appears to fill the window."
  (or hard-wrap-p
      (string-match-p
       (concat ai-code-session-link--url-continuation-end-cue-regexp "\\'")
       previous-fragment)
      (string-match-p
       (concat "\\`" ai-code-session-link--url-continuation-start-cue-regexp)
       fragment)
      (string-match-p ai-code-session-link--url-query-fragment-regexp
                      fragment)
      (ai-code-session-link--url-mid-token-continuation-p
       fragment previous-fragment)))

(defun ai-code-session-link--wrapped-url-at (match-start match-end scan-end)
  "Return a wrapped URL candidate starting at MATCH-START.
MATCH-END is the end of the first-row URL match.  SCAN-END bounds the search."
  (save-excursion
    (goto-char match-start)
    (let* ((current-line-start (line-beginning-position))
           (current-line-end (min scan-end (line-end-position)))
           (current-line-hard-wrap
            (ai-code-session-link--line-hard-wrapped-p
             current-line-start current-line-end))
           (first-link-end
            (ai-code-session-link--trim-url-end match-start match-end))
           (previous-fragment
            (buffer-substring-no-properties match-start first-link-end))
           (continued-lines 0)
           (best-link nil)
           (scan (and (= first-link-end match-end)
                      (ai-code-session-link--blank-between-p
                       match-end current-line-end))))
      (while (and scan
                  (< continued-lines ai-code-session-link--wrapped-url-max-lines)
                  (< current-line-end scan-end)
                  (eq (char-after current-line-end) ?\n))
        (let* ((next-line-start (1+ current-line-end))
               (next-line-end
                (save-excursion
                  (goto-char next-line-start)
                  (min scan-end (line-end-position))))
               (fragment
                (ai-code-session-link--line-url-fragment-bounds
                 next-line-start next-line-end)))
          (if (not fragment)
              (setq scan nil)
            (let* ((fragment-start (car fragment))
                   (fragment-end (cdr fragment))
                   (trimmed-end
                    (ai-code-session-link--trim-url-end
                     fragment-start fragment-end))
                   (fragment-text
                    (buffer-substring-no-properties
                     fragment-start fragment-end)))
              (if (or (not (ai-code-session-link--blank-or-url-closer-between-p
                            fragment-end next-line-end))
                      (not
                       (ai-code-session-link--url-continuation-fragment-p
                        fragment-text previous-fragment current-line-hard-wrap)))
                  (setq scan nil)
                (cl-incf continued-lines)
                (setq best-link
                      (list :start match-start
                            :end trimmed-end
                            :text
                            (ai-code-session-link--normalize-url-link-text
                             (buffer-substring-no-properties
                              match-start trimmed-end))))
                (setq previous-fragment fragment-text
                      current-line-start next-line-start
                      current-line-end next-line-end
                      current-line-hard-wrap
                      (ai-code-session-link--line-hard-wrapped-p
                       current-line-start current-line-end)))))))
      best-link)))

(defun ai-code-session-link--collect-url-links (start end)
  "Return URL link matches between START and END."
  (let (url-links)
    (save-excursion
      (goto-char start)
      (while (re-search-forward ai-code-session-link--url-pattern-regexp end t)
        (let* ((match-start (match-beginning 1))
               (match-end (match-end 1))
               (wrapped-link
                (ai-code-session-link--wrapped-url-at
                 match-start match-end end))
               (link-end (or (plist-get wrapped-link :end)
                             (ai-code-session-link--trim-url-end
                              match-start match-end)))
               (link-text
                (or (plist-get wrapped-link :text)
                    (buffer-substring-no-properties match-start link-end))))
          (push (list :start match-start
                      :end link-end
                      :text link-text)
                url-links))))
    (ai-code-session-link--sort-and-prune-links url-links)))

(defun ai-code-session-link--linkify-url-region (start end)
  "Apply URL session links between START and END."
  (dolist (url-link (ai-code-session-link--collect-url-links start end))
    (ai-code-session-link--apply-properties
     (plist-get url-link :start)
     (plist-get url-link :end)
     (plist-get url-link :text)
     "mouse-1: Open URL")))

(defun ai-code-session-link--linkify-file-region-1
    (start end allow-local-probing)
  "Apply file session links between START and END.
ALLOW-LOCAL-PROBING controls local existence checks and image previews."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (ai-code-session-link--delete-image-preview-overlays start end t)
    (ai-code-session-link--restore-live-image-preview-link-properties
     start end)
    (let ((ai-code-session-link--project-files-cache
           (ai-code-session-link--buffer-project-files-cache))
          (ai-code-session-link--resolved-path-cache
           (make-hash-table :test 'equal)))
      (let ((file-links (ai-code-session-link--collect-file-links
                         start end allow-local-probing)))
        (while file-links
          (let* ((file-link (car file-links))
                 (next-file-link (cadr file-links))
                 (match-start (plist-get file-link :start))
                 (match-end (plist-get file-link :end))
                 (link-text (plist-get file-link :text))
                 (image-link-file
                  (and allow-local-probing
                       (ai-code-session-link--image-preview-link-file
                        link-text))))
            (if image-link-file
                (ai-code-session-link--apply-properties
                 match-start match-end link-text
                 "mouse-1: Visit image file")
              (unless (get-text-property match-start 'ai-code-session-link)
                (ai-code-session-link--apply-properties
                 match-start match-end link-text "mouse-1: Visit file")
                (ai-code-session-link--linkify-symbols-near-file
                 link-text match-end end
                 (and next-file-link (plist-get next-file-link :start)))))
            (when allow-local-probing
              (ai-code-session-link--apply-image-preview
               match-start match-end link-text))
            (setq file-links (cdr file-links))))))))

(defun ai-code-session-link--linkify-file-region
    (start end allow-local-probing)
  "Apply file session links between START and END.
ALLOW-LOCAL-PROBING controls local existence checks and image previews."
  (let ((function
         (lambda ()
           (ai-code-session-link--linkify-file-region-1
            start end allow-local-probing))))
    (if (and allow-local-probing
             (functionp
              ai-code-session-link-image-preview-transaction-function))
        (funcall ai-code-session-link-image-preview-transaction-function
                 start end function)
      (funcall function))))

(defun ai-code-session-link--strict-image-candidate-bounds (match-start match-end)
  "Return strict local image path bounds around MATCH-START and MATCH-END."
  (let ((start match-start)
        (end match-end)
        (min-start (line-beginning-position)))
    (while (and (> start min-start)
                (not (memq (char-before start)
                           '(?\s ?\t ?\n ?\r ?\" ?' ?` ?< ?> ?\( ?\)
                             ?\[ ?\] ?{ ?}))))
      (setq start (1- start)))
    (when (and (< start end)
               (<= (- end start)
                   ai-code-session-link--visible-image-preview-max-line-width))
      (cons start end))))

(defun ai-code-session-link--strict-image-path-suffix-info (text)
  "Return (OFFSET . PATH) for a strong local path suffix in TEXT.
The suffix must follow punctuation and begin with `file:', `/`, `~/', `./',
or `../'.  A TEXT that already starts with one of those prefixes needs no
suffix candidate and returns nil."
  (let ((prefix-regexp "\\(?:file:\\|/\\|~/\\|\\.\\.?/\\)"))
    (unless (string-match-p (concat "\\`" prefix-regexp) text)
      (when (string-match (concat "[[:punct:]]\\(" prefix-regexp "\\)")
                          text)
        (let ((offset (match-beginning 1)))
          (cons offset (substring text offset)))))))

(defun ai-code-session-link--image-preview-existing-local-file (link-text)
  "Return existing local image file for LINK-TEXT without project scanning."
  (when-let* ((link (ai-code-session-link--parse-file-link-text link-text))
              (file (plist-get link :file))
              (normalized (ai-code-session-link--normalize-file file))
              ((ai-code-session-link--image-extension-p normalized))
              (root (ai-code-session-link--project-root-for-paths))
              (abs-file
               (ai-code-session-link--resolve-existing-local-path
                normalized root))
              ((ai-code-session-link--safe-local-image-file-p abs-file)))
    abs-file))

(defun ai-code-session-link--captured-image-preview-source (link-text)
  "Return a backend-captured image source for LINK-TEXT, or nil."
  (when (functionp ai-code-session-link-image-preview-source-function)
    (when-let* ((source
                 (with-demoted-errors
                     "AI Code captured image source error: %S"
                   (funcall ai-code-session-link-image-preview-source-function
                            link-text)))
                (file (plist-get source :file))
                (data (plist-get source :data))
                (signature (plist-get source :signature))
                ((stringp file))
                ((ai-code-session-link--image-extension-p file))
                ((not (file-remote-p file)))
                ((stringp data))
                ((consp signature)))
      source)))

(defun ai-code-session-link--strict-path-continuation-line-p (line)
  "Return non-nil when LINE is safe to treat as a wrapped path fragment."
  (and (stringp line)
       (not (string-empty-p line))
       (<= (length line)
           ai-code-session-link--visible-image-preview-max-line-width)
       (string-match-p "\\`[-[:alnum:]_./~%+:\\\\]+\\'" line)))

(defun ai-code-session-link--strict-absolute-path-suffix (line)
  "Return an absolute path suffix from LINE, or nil."
  (when (and (stringp line)
             (string-match
              "\\(?:\\`\\|[ \t]\\)\\(/[^] \t\n\r\"'`<>(){}[]+\\)\\'"
              line))
    (match-string 1 line)))

(defun ai-code-session-link--strict-previous-line-prefix (position)
  "Return a safe wrapped path prefix before POSITION, or nil."
  (cdr (ai-code-session-link--strict-previous-line-prefix-info position)))

(defun ai-code-session-link--strict-previous-line-prefix-info (position)
  "Return a (START . PREFIX) pair before POSITION, or nil."
  (save-excursion
    (goto-char position)
    (let ((suffix "")
          (lines 0)
          result
          stop)
      (while (and (not result)
                  (not stop)
                  (< lines
                     ai-code-session-link--visible-image-preview-prefix-max-lines)
                  (not (bobp)))
        (forward-line -1)
        (setq lines (1+ lines))
        (let* ((raw-line
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
               (line (string-trim raw-line))
               (content-start
                (+ (line-beginning-position)
                   (or (string-match-p "[^ \t]" raw-line)
                       (length raw-line))))
               (too-long
                (> (+ (length line) (length suffix))
                   ai-code-session-link--visible-image-preview-max-line-width)))
          (cond
           (too-long
            (setq stop t))
           ((string-match "file:" line)
            (setq result
                  (cons (+ content-start (match-beginning 0))
                        (concat (substring line (match-beginning 0))
                                suffix))))
           ((string-match
             "\\(?:\\`\\|[ \t]\\)\\(/[^] \t\n\r\"'`<>(){}[]+\\)\\'"
             line)
            (setq result
                  (cons (+ content-start (match-beginning 1))
                        (concat (match-string 1 line) suffix))))
           ((and (ai-code-session-link--strict-path-continuation-line-p line)
                 (or suffix
                     (string-match-p "[/-]\\'" line)
                     (string-match-p "/" line)))
            (setq suffix (concat line suffix)))
           (t
            (setq stop t)))))
      result)))

(defun ai-code-session-link--strict-image-candidates
    (start end link-text)
  "Return strict image candidates for LINK-TEXT between START and END."
  (let* ((trimmed (string-trim link-text))
         (normalized (ai-code-session-link--normalize-file trimmed))
         (suffix-info
          (ai-code-session-link--strict-image-path-suffix-info trimmed))
         (prefix-info
          (and normalized
               (not (string-prefix-p "file:" normalized))
               (ai-code-session-link--strict-previous-line-prefix-info start)))
         (current-candidate
          (list :start start
                :end end
                :link-text trimmed
                :display-text link-text)))
    (let* ((base-candidates
            (if-let* ((candidate-start (car-safe prefix-info))
                      (candidate-text (concat (cdr prefix-info) trimmed))
                      (display-text
                       (buffer-substring-no-properties candidate-start end))
                      (prefixed-candidate
                       (list :start candidate-start
                             :end end
                             :link-text candidate-text
                             :display-text display-text)))
                ;; A hard wrap immediately before a slash makes the next row
                ;; look like a standalone absolute path.  Preserve standalone
                ;; precedence, then try the joined candidate if needed.
                (if (file-name-absolute-p normalized)
                    (list current-candidate prefixed-candidate)
                  (list prefixed-candidate current-candidate))
              (list current-candidate)))
           (suffix-candidate
            (when-let* ((offset (car-safe suffix-info))
                        (path (cdr-safe suffix-info))
                        (candidate-start (+ start offset)))
              (list :start candidate-start
                    :end end
                    :link-text path
                    :display-text
                    (buffer-substring-no-properties candidate-start end)))))
      ;; Prose such as "see：/tmp/image.png" is one whitespace-delimited token.
      ;; Try its strong path suffix first, but retain the original candidate as
      ;; a fallback for legal filenames whose directory contains punctuation.
      (if suffix-candidate
          (cons suffix-candidate base-candidates)
        base-candidates))))

(defun ai-code-session-link--strict-image-candidate-texts (start link-text)
  "Return strict image candidate texts for LINK-TEXT at START."
  (let* ((trimmed (string-trim link-text))
         (normalized (ai-code-session-link--normalize-file trimmed))
         (prefix (and normalized
                      (not (file-name-absolute-p normalized))
                      (not (string-prefix-p "file:" normalized))
                      (ai-code-session-link--strict-previous-line-prefix start))))
    (let ((candidates (list trimmed)))
      (when prefix
        (push (concat prefix trimmed) candidates))
      (delete-dups
       (delq nil
             (nreverse candidates))))))

(defun ai-code-session-link--linkify-strict-image-preview-region-1 (start end)
  "Apply strict local image previews between START and END."
  (when (ai-code-session-link--image-preview-enabled-p)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (ai-code-session-link--delete-image-preview-overlays start end t)
      (save-excursion
        (let ((case-fold-search t)
              (candidate-count 0)
              (extension-regexp
               (concat "\\.\\(?:" ai-code-session-link--image-extension-regexp "\\)")))
          (goto-char start)
          (while (and (< candidate-count
                         ai-code-session-link--visible-image-preview-max-candidates)
                      (re-search-forward extension-regexp end t))
            (setq candidate-count (1+ candidate-count))
            (let* ((bounds
                    (ai-code-session-link--strict-image-candidate-bounds
                     (match-beginning 0)
                     (match-end 0)))
                   (match-start (car-safe bounds))
                   (match-end (cdr-safe bounds))
                   (link-text
                    (and bounds
                         (buffer-substring-no-properties match-start match-end))))
              (when link-text
                (catch 'previewed
                  (dolist (candidate
                           (ai-code-session-link--strict-image-candidates
                            match-start match-end link-text))
                    (when-let* ((link-text (plist-get candidate :link-text))
                                (source
                                 (or
                                  (when-let* ((file
                                               (ai-code-session-link--image-preview-existing-local-file
                                                link-text)))
                                    (list :file file))
                                  (ai-code-session-link--captured-image-preview-source
                                   link-text)))
                                (file (plist-get source :file)))
                      (ai-code-session-link--apply-properties
                       (plist-get candidate :start)
                       (plist-get candidate :end)
                       link-text
                       "mouse-1: Visit image file")
                      (ai-code-session-link--apply-image-preview-for-file
                       (plist-get candidate :start)
                       (plist-get candidate :end)
                       link-text
                       file
                       (plist-get candidate :display-text)
                       (plist-get source :data)
                       (plist-get source :signature))
                      (throw 'previewed t))))))))))))

(defun ai-code-session-link--linkify-strict-image-preview-region (start end)
  "Apply image previews between START and END using strict local path parsing.
This avoids broad path regexps and project scans, making it suitable for
visible-window recovery in large terminal scrollback."
  (let ((function
         (lambda ()
           (ai-code-session-link--linkify-strict-image-preview-region-1
            start end))))
    (if (functionp ai-code-session-link-image-preview-transaction-function)
        (funcall ai-code-session-link-image-preview-transaction-function
                 start end function)
      (funcall function))))

(defun ai-code-session-link--property-at-point (property)
  "Return PROPERTY at point or immediately before point."
  (or (get-text-property (point) property)
      (when (not (bobp))
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
    (when-let* ((line-start (plist-get link :line-start)))
      (forward-line (1- line-start)))
    (when-let* ((column-start (plist-get link :column-start)))
      (when (> column-start 0)
        (move-to-column (1- column-start))))
    t))

(defun ai-code-session-link--try-xref-definition (symbol)
  "Try xref lookup for SYMBOL in the current buffer."
  (when-let* ((lookup (ai-code-session-link--primary-symbol-search-term symbol)))
    (when (fboundp 'xref-find-definitions)
      (condition-case nil
          (progn
            (xref-find-definitions lookup)
            t)
        (error nil)))))

(defun ai-code-session-link--try-helm-gtags-definition (symbol)
  "Try helm-gtags lookup for SYMBOL in the current buffer."
  (when-let* ((lookup (ai-code-session-link--primary-symbol-search-term symbol)))
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
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (save-excursion
        (save-restriction
          (widen)
          (setq start (max (point-min) start)
                end (min (point-max) end))
          (let ((bounds (cons start end))
                (region-text (buffer-substring-no-properties start end)))
            (if (ai-code-session-link--unchanged-region-p bounds region-text)
                (when (and (ai-code-session-link--image-preview-enabled-p)
                           (ai-code-session-link--text-may-contain-image-reference-p
                            region-text)
                           (ai-code-session-link--image-preview-refresh-needed-p
                            start end))
                  (ai-code-session-link--linkify-file-region start end t))
              (ai-code-session-link--remove-managed-properties start end)
              (ai-code-session-link--linkify-url-region start end)
              (ai-code-session-link--linkify-file-region
               start end
               (ai-code-session-link--trusted-local-session-p))
              (setq ai-code-session-link--last-region-bounds bounds
                    ai-code-session-link--last-region-text region-text
                    ai-code-session-link--last-region-rules-version
                    ai-code-session-link--linkify-rules-version))))))))

(defun ai-code-session-link--recent-output-tail-width (output)
  "Return the tail width to rescan after OUTPUT."
  (max ai-code-session-link--linkify-min-tail-width
       (* 2 (length (or output "")))))

(defun ai-code-session-link--recent-output-plain-text (output)
  "Return OUTPUT with terminal control sequences removed."
  (let* ((text (or output ""))
         (text (replace-regexp-in-string
                "\x1b\\][^\x07\x1b]*\\(?:\x07\\|\x1b\\\\\\)" "" text))
         (text (replace-regexp-in-string
                "\x1b\\[[0-9;?]*[ -/]*[@-~]" "" text))
         (text (replace-regexp-in-string "[\x00-\x1f\x7f]" "" text)))
    text))

(defun ai-code-session-link--recent-output-may-contain-links-p (output)
  "Return non-nil when OUTPUT may introduce session links worth rescanning."
  (let ((text (ai-code-session-link--recent-output-plain-text output)))
    (and (not (string-empty-p text))
         (string-match-p ai-code-session-link--recent-output-candidate-regexp text))))

(defun ai-code-session-link--should-linkify-recent-output-p (buffer output)
  "Return non-nil when BUFFER and OUTPUT should trigger hot-path relinkification."
  (and ai-code-session-link-enabled
       (buffer-live-p buffer)
       (ai-code-session-link--recent-output-may-contain-links-p output)))

(defun ai-code-session-link--linkify-inhibited-p (&optional buffer)
  "Return non-nil when BUFFER should defer delayed linkification."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (with-demoted-errors "ai-code-session-link-inhibit-functions error: %S"
             (run-hook-with-args-until-success
              'ai-code-session-link-inhibit-functions buffer))))))

(defun ai-code-session-link--schedule-pending-linkify (buffer delay)
  "Schedule delayed linkification for BUFFER after DELAY seconds."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless ai-code-session-link--linkify-timer
        (setq ai-code-session-link--linkify-timer
              (run-at-time
               delay nil
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (ai-code-session-link--flush-scheduled-linkify))))
               buffer))))))

(defun ai-code-session-link--flush-scheduled-linkify ()
  "Apply any delayed session linkification pending in the current buffer."
  (let ((tail-width ai-code-session-link--pending-tail-width))
    (setq ai-code-session-link--linkify-timer nil)
    (when (> tail-width 0)
      (if (ai-code-session-link--linkify-inhibited-p)
          (ai-code-session-link--schedule-pending-linkify
           (current-buffer)
           ai-code-session-link--linkify-inhibited-retry-delay)
        (setq ai-code-session-link--pending-tail-width 0)
        (let ((end (point-max)))
          (ai-code-session-link--linkify-session-region
           (max (point-min) (- end tail-width))
           end))))))

(defun ai-code-session-link--schedule-linkify-recent-output (buffer output &optional delay)
  "Linkify recent OUTPUT in BUFFER after terminal redraw settles.
Optional DELAY overrides the default redraw delay in seconds."
  (when (ai-code-session-link--should-linkify-recent-output-p buffer output)
    (with-current-buffer buffer
      (setq ai-code-session-link--pending-tail-width
            (max ai-code-session-link--pending-tail-width
                 (ai-code-session-link--recent-output-tail-width output)))
      (ai-code-session-link--schedule-pending-linkify
       buffer
       (or delay ai-code-session-link--linkify-redraw-delay)))))

(defun ai-code-session-link--linkify-recent-output (output)
  "Linkify the recent tail of the current session buffer after OUTPUT."
  (when (ai-code-session-link--should-linkify-recent-output-p
         (current-buffer)
         output)
    (let* ((visible-width (ai-code-session-link--recent-output-tail-width output))
           (end (point-max))
           (start (max (point-min) (- end visible-width))))
      (ai-code-session-link--linkify-session-region start end))))


(provide 'ai-code-session-link)

;;; ai-code-session-link.el ends here
