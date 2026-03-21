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

(defvar ai-code-backends-infra--session-directory nil
  "Session working directory set by ai-code-backends-infra buffers.")

(defvar ai-code-session-link--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ai-code-session-navigate-link-at-mouse)
    (define-key map [mouse-2] #'ai-code-session-navigate-link-at-mouse)
    (define-key map (kbd "RET") #'ai-code-session-navigate-link-at-point)
    map)
  "Keymap used for clickable session links.")

(defconst ai-code-session-link--linkify-min-tail-width 512
  "Minimum number of tail characters to rescan for session links.")

(defconst ai-code-session-link--url-pattern-regexp
  "\\(https?://[^][(){}<>\"' \t\n]+\\)"
  "Regexp matching http/https URLs in session buffers.")

(defconst ai-code-session-link--path-base-regexp
  "@?[[:alnum:]_./~-]*[./][[:alnum:]_./~-]+"
  "Regexp matching a local file-like or directory-like path.")

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

(defun ai-code-session-link--normalize-file (filename)
  "Normalize session link FILENAME for project lookup."
  (when (stringp filename)
    (let* ((trimmed (string-trim filename))
           (without-at (string-remove-prefix "@" trimmed))
           (normalized (string-remove-prefix "file://" without-at)))
      (unless (string-empty-p normalized)
        normalized))))

(defun ai-code-session-link--project-files (root)
  "Return absolute project files for ROOT."
  (when (file-directory-p root)
    (or (ignore-errors
          (when-let ((project (project-current nil root)))
            (let ((project-root (expand-file-name (project-root project))))
              (mapcar (lambda (file)
                        (if (file-name-absolute-p file)
                            (expand-file-name file)
                          (expand-file-name file project-root)))
                      (project-files project)))))
        (directory-files-recursively root ".*" t))))

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
         (normalized (ai-code-session-link--normalize-file path)))
    (when (and root normalized)
      (or (ai-code-session-link--resolve-existing-local-path normalized root)
          (let* ((project-files (ai-code-session-link--project-files root))
                 (candidate (if (file-name-absolute-p normalized)
                                (expand-file-name normalized)
                              (expand-file-name normalized root))))
            (cond
             ((ai-code-session-link--in-project-file-p candidate root project-files) candidate)
             ((not (file-name-absolute-p normalized))
              (car (ai-code-session-link--matching-project-files normalized root project-files)))
             (t nil)))))))

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
  (save-excursion
    (dolist (pattern ai-code-session-link--file-patterns)
      (goto-char start)
      (while (re-search-forward (car pattern) end t)
        (unless (get-text-property (match-beginning 0) 'ai-code-session-link)
          (let* ((match-start (match-beginning 0))
                 (match-end (match-end 0))
                 (path (match-string-no-properties (nth 1 pattern))))
            (when (ai-code-session-link--resolve-session-file path)
              (ai-code-session-link--apply-properties
               match-start match-end
               (buffer-substring-no-properties match-start match-end)
               "mouse-1: Visit file"))))))))

(defun ai-code-session-link--linkify-session-region (start end)
  "Make supported URLs and in-project file references clickable from START to END."
  (when (< start end)
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
  (when (buffer-live-p buffer)
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
  (let* ((visible-width (ai-code-session-link--recent-output-tail-width output))
         (end (point-max))
         (start (max (point-min) (- end visible-width))))
    (ai-code-session-link--linkify-session-region start end)))

(provide 'ai-code-session-link)

;;; ai-code-session-link.el ends here
