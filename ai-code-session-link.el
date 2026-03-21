;;; ai-code-session-link.el --- Shared session link helpers -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Internal helpers shared by session linkification and navigation.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)

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

(provide 'ai-code-session-link)

;;; ai-code-session-link.el ends here
