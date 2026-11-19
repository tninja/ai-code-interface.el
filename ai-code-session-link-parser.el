;;; ai-code-session-link-parser.el --- Session link syntax and normalization -*- lexical-binding: t; -*-

;; Author: Kang Tu, realazy (cxa)
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Parse and normalize session link text separately from terminal rendering.
;; Existing internal function names and return values are unchanged.

;;; Code:

(require 'rx)
(require 'subr-x)
(require 'url-util)

(defconst ai-code-session-link--path-base-regexp
  "@?\\(?:file:\\(?://localhost\\|//\\)?\\)?[[:alnum:]_./~%+-]*[./][[:alnum:]_./~%+-]+"
  "Regexp matching a local file-like or directory-like path.")

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

(defconst ai-code-session-link--reference-wrapper-left-regexp
  ;; `in' rather than its synonym `any': package-lint reads the latter as a
  ;; call to the Emacs 31.1 function of that name and errors on our
  ;; (emacs "29.1") requirement.
  (rx (+ (in "\"'`<([{")))
  "Regexp matching wrapper characters before a session file reference.")

(defconst ai-code-session-link--reference-wrapper-right-regexp
  (rx (+ (in "\"'`>)}],.;:!?")))
  "Regexp matching wrapper characters after a session file reference.")

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

(defun ai-code-session-link--image-extension-p (file)
  "Return non-nil when FILE has a known image file extension."
  (when-let* ((extension (file-name-extension file)))
    (member (downcase extension)
            ai-code-session-link--image-file-extensions)))

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

(provide 'ai-code-session-link-parser)

;;; ai-code-session-link-parser.el ends here
