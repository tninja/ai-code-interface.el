;;; test_ai-code-dired-context.el --- Dired context regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise real Dired marks across context-consuming commands.

;;; Code:

(require 'ert)
(require 'dired)
(require 'ai-code)

(defmacro ai-code-dired-test--with-files (root &rest body)
  "Create files and a directory under ROOT, then run BODY in Dired."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((,root (file-name-as-directory
                 (file-truename (make-temp-file "ai-code-dired-context-" t))))
          (dired-buffer nil))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "a file.el" ,root) (insert ";; a\n"))
           (with-temp-file (expand-file-name "b.el" ,root) (insert ";; b\n"))
           (make-directory (expand-file-name "selected dir" ,root))
           (setq dired-buffer (dired-noselect ,root))
           (with-current-buffer dired-buffer
             (cl-letf (((symbol-function 'ai-code--git-root)
                        (lambda (&optional _dir) ,root)))
               ,@body)))
       (when (buffer-live-p dired-buffer) (kill-buffer dired-buffer))
       (delete-directory ,root t))))

(ert-deftest ai-code-test-dired-context-distinguishes-marks-from-point ()
  "A single explicit mark remains distinct from the fallback at point."
  (ai-code-dired-test--with-files root
    (let ((file (expand-file-name "a file.el" root)))
      (dired-goto-file file)
      (should (equal (plist-get (ai-code--dired-context) :files) (list file)))
      (should-not (plist-get (ai-code--dired-context) :marked-p))
      (dired-mark 1)
      (dired-goto-file file)
      (should (equal (plist-get (ai-code--dired-context) :files) (list file)))
      (should (plist-get (ai-code--dired-context) :marked-p))
      (goto-char (point-min))
      (should (equal (plist-get (ai-code--dired-context) :files) (list file)))
      (dired-unmark-all-marks)
      (goto-char (point-min))
      (should-not (plist-get (ai-code--dired-context) :files)))))

(ert-deftest ai-code-test-dired-question-keeps-marked-file-at-point ()
  "Asking about two marks includes the marked file under point."
  (ai-code-dired-test--with-files root
    (let ((first (expand-file-name "a file.el" root))
          (second (expand-file-name "b.el" root))
          prompt)
      (dired-goto-file first)
      (dired-mark 1)
      (dired-goto-file second)
      (dired-mark 1)
      (dired-goto-file first)
      (cl-letf (((symbol-function 'ai-code-read-string) (lambda (&rest _) "Explain"))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq prompt text))))
        (ai-code-ask-question nil))
      (should (string-match-p (regexp-quote "@a file.el\n@b.el") prompt)))))

(ert-deftest ai-code-test-dired-explain-single-mark-uses-selected-type ()
  "A marked directory wins over an unmarked file and is called a directory."
  (ai-code-dired-test--with-files root
    (let ((directory (expand-file-name "selected dir" root)) prompt)
      (dired-goto-file directory)
      (dired-mark 1)
      (dired-goto-file (expand-file-name "b.el" root))
      (cl-letf (((symbol-function 'ai-code--confirm-and-send)
                 (lambda (_label text) (setq prompt text))))
        (ai-code-explain))
      (should (string-match-p "explain the directory" prompt))
      (should (string-match-p (regexp-quote "@selected dir") prompt))
      (should-not (string-match-p (regexp-quote "@b.el") prompt)))))

(ert-deftest ai-code-test-dired-actions-share-the-same-targets ()
  "Change, refactor, insert, copy and stored context agree on Dired targets."
  (ai-code-dired-test--with-files root
    (let ((first (expand-file-name "a file.el" root))
          (second (expand-file-name "b.el" root)))
      (dolist (marked (list nil (list first) (list first second)))
        (dired-unmark-all-marks)
        (dolist (file marked) (dired-goto-file file) (dired-mark 1))
        (dired-goto-file second)
        (let ((expected (or marked (list second)))
              (ai-code--repo-context-info (make-hash-table :test #'equal))
              (kill-ring nil)
              prompt)
          (should (equal (plist-get (ai-code--get-refactoring-context) :dired-targets)
                         expected))
          (should (equal (ai-code-send--buffer-files) expected))
          (ai-code-copy-buffer-file-name-to-clipboard t)
          (should (equal (current-kill 0) (mapconcat #'identity expected "\n")))
          (ai-code-add-context)
          (should (equal (sort (copy-sequence (gethash root ai-code--repo-context-info)) #'string<)
                         (sort (copy-sequence expected) #'string<)))
          (cl-letf (((symbol-function 'ai-code-read-string) (lambda (&rest _) "Change"))
                    ((symbol-function 'ai-code--format-repo-context-info) (lambda () nil))
                    ((symbol-function 'ai-code--insert-prompt)
                     (lambda (text) (setq prompt text))))
            (ai-code-code-change nil))
          (should (string-match-p
                   (regexp-quote (concat "Selected files/directories:\n"
                                        (mapconcat #'identity expected "\n")))
                   prompt)))))))

(ert-deftest ai-code-test-dired-header-keeps-action-specific-fallbacks ()
  "Without a target, copy uses the directory and file actions reject it."
  (ai-code-dired-test--with-files root
    (goto-char (point-min))
    (let ((kill-ring nil) prompt)
      (ai-code-copy-buffer-file-name-to-clipboard t)
      (should (equal (current-kill 0) root))
      (should-not (ai-code-send--buffer-files))
      (should-error (ai-code--get-refactoring-context) :type 'user-error)
      (should-error (ai-code-code-change nil) :type 'user-error)
      (cl-letf (((symbol-function 'ai-code-read-string) (lambda (&rest _) "Explain"))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq prompt text))))
        (should-error (ai-code-explain) :type 'user-error)
        (should-not prompt)
        (ai-code-ask-question nil))
      (should (string-match-p (regexp-quote (concat "Current directory: " root))
                              prompt)))))

(ert-deftest ai-code-test-dired-outside-git-keeps-absolute-targets ()
  "Outside Git, Ask and Explain fall back to absolute target paths."
  (ai-code-dired-test--with-files root
    (let ((first (expand-file-name "a file.el" root))
          (second (expand-file-name "b.el" root))
          prompt)
      (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _dir) nil))
                ((symbol-function 'ai-code-read-string) (lambda (&rest _) "Explain"))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq prompt text)))
                ((symbol-function 'ai-code--confirm-and-send)
                 (lambda (_label text) (setq prompt text))))
        (dired-goto-file first)
        (ai-code-explain)
        (should (string-match-p (regexp-quote (concat "explain the file at path @" first))
                                prompt))
        (dolist (file (list first second)) (dired-goto-file file) (dired-mark 1))
        (ai-code-explain)
        (should (string-match-p (regexp-quote (concat "@" first "\n@" second)) prompt))
        (ai-code-ask-question nil)
        (should (string-match-p (regexp-quote (concat "Selected files/directories:\n@"
                                                      first "\n@" second))
                                prompt))))))

(ert-deftest ai-code-test-dired-insert-region-still-takes-precedence ()
  "Insert keeps its explicit region selection ahead of unrelated marks."
  (ai-code-dired-test--with-files root
    (let ((first (expand-file-name "a file.el" root))
          (second (expand-file-name "b.el" root))
          (transient-mark-mode t))
      (dired-goto-file (expand-file-name "selected dir" root))
      (dired-mark 1)
      (dired-goto-file first)
      (beginning-of-line)
      (set-mark (point))
      (dired-goto-file second)
      (forward-line 1)
      (activate-mark)
      (should (equal (ai-code-send--buffer-files) (list first second))))))

(provide 'test_ai-code-dired-context)

;;; test_ai-code-dired-context.el ends here
