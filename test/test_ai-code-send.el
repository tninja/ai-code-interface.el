;;; test_ai-code-send.el --- Tests for ai-code-send.el -*- lexical-binding: t; -*-

;; Author: realazy
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for inserting files and editor selections into ai-code sessions.

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))

(require 'ai-code-send)

(ert-deftest test-ai-code-send--file-formats-project-reference ()
  "File sends should separate an @-prefixed project-relative reference."
  (let* ((root (make-temp-file "ai-code-send-root-" t))
         (file (expand-file-name "src/main.el" root))
         inserted)
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "(message \"hello\")"))
          (with-temp-buffer
            (setq default-directory root)
            (cl-letf (((symbol-function 'ai-code--session-project-root)
                       (lambda () root))
                      ((symbol-function 'ai-code-send--default-session)
                       (lambda () 'session-buffer))
                      ((symbol-function 'ai-code-backends-infra-insert-string)
                       (lambda (text buffer)
                         (setq inserted (list text buffer))))
                      ((symbol-function 'ai-code-send--buffer-files)
                       (lambda () (list file))))
              (ai-code-send-file)
              (should (equal inserted '("\n\n@src/main.el\n\n"
                                        session-buffer))))))
      (delete-directory root t))))

(ert-deftest test-ai-code-send--file-to-uses-selected-session-root ()
  "Cross-project file targets should receive an absolute source reference."
  (let* ((source-root (make-temp-file "ai-code-send-source-" t))
         (target-root (make-temp-file "ai-code-send-target-" t))
         (file (expand-file-name "src/main.el" source-root))
         (session (generate-new-buffer " *ai-code-target-session*"))
         inserted)
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "(message \"hello\")"))
          (with-temp-buffer
            (setq default-directory source-root)
            (cl-letf (((symbol-function 'ai-code--session-project-root)
                       (lambda () source-root))
                      ((symbol-function 'ai-code-send--files)
                       (lambda (&optional _prompt-for-file) (list file)))
                      ((symbol-function 'ai-code-send--viewport-buffer)
                       (lambda (&optional _session) nil))
                      ((symbol-function 'ai-code-send--select-session)
                       (lambda () session))
                      ((symbol-function
                        'ai-code-backends-infra-session-directory)
                       (lambda (buffer)
                         (and (eq buffer session) target-root)))
                      ((symbol-function 'ai-code-backends-infra-insert-string)
                       (lambda (text buffer)
                         (setq inserted (list text buffer)))))
              (ai-code-send-file-to)
              (should (equal inserted
                             (list (concat "\n\n@" file "\n\n")
                                   session))))))
      (kill-buffer session)
      (delete-directory source-root t)
      (delete-directory target-root t))))

(ert-deftest test-ai-code-send--region-to-uses-selected-session-root ()
  "Cross-project region targets should receive an absolute source location."
  (let* ((source-root (make-temp-file "ai-code-send-source-" t))
         (target-root (make-temp-file "ai-code-send-target-" t))
         (file (expand-file-name "src/main.el" source-root))
         (session (generate-new-buffer " *ai-code-target-session*"))
         inserted)
    (unwind-protect
        (with-temp-buffer
          (insert "first\nsecond\n")
          (setq buffer-file-name file
                default-directory source-root)
          (goto-char (point-min))
          (set-mark (point-max))
          (activate-mark)
          (cl-letf (((symbol-function 'ai-code--session-project-root)
                     (lambda () source-root))
                    ((symbol-function 'ai-code-send--viewport-buffer)
                     (lambda (&optional _session) nil))
                    ((symbol-function 'ai-code-send--select-session)
                     (lambda () session))
                    ((symbol-function
                      'ai-code-backends-infra-session-directory)
                     (lambda (buffer)
                       (and (eq buffer session) target-root)))
                    ((symbol-function 'ai-code-backends-infra-insert-string)
                     (lambda (text buffer)
                       (setq inserted (list text buffer)))))
            (ai-code-send-region-to)
            (should (eq (cadr inserted) session))
            (should (string-prefix-p
                     (format "\n\n@%s#L1-L2" file)
                     (car inserted)))))
      (kill-buffer session)
      (delete-directory source-root t)
      (delete-directory target-root t))))

(ert-deftest test-ai-code-send--current-file-ignores-active-region ()
  "Current-file insertion should insert the file even when a region is active."
  (let (inserted)
    (with-temp-buffer
      (insert "selected text")
      (setq buffer-file-name "/tmp/current.el")
      (set-mark (point-min))
      (activate-mark)
      (cl-letf (((symbol-function 'ai-code-send--dispatch-files)
                 (lambda (files &optional pick-session)
                   (setq inserted (list files pick-session))))
                ((symbol-function 'ai-code-send-region)
                 (lambda (&rest _args)
                   (ert-fail "Current-file insertion must not insert the region"))))
        (ai-code-send-current-file)
        (should (equal inserted '(("/tmp/current.el") nil)))))))

(ert-deftest test-ai-code-send--other-file-ignores-active-region ()
  "Other-file insertion should prompt for a file even when a region is active."
  (let (inserted)
    (with-temp-buffer
      (insert "selected text")
      (setq buffer-file-name "/tmp/current.el")
      (set-mark (point-min))
      (activate-mark)
      (cl-letf (((symbol-function 'ai-code-send--read-file)
                 (lambda () "/tmp/other.el"))
                ((symbol-function 'ai-code-send--dispatch-files)
                 (lambda (files &optional pick-session)
                   (setq inserted (list files pick-session))))
                ((symbol-function 'ai-code-send-region)
                 (lambda (&rest _args)
                   (ert-fail "Other-file insertion must not insert the region"))))
        (ai-code-send-other-file)
        (should (equal inserted '(("/tmp/other.el") nil)))))))

(ert-deftest test-ai-code-send--read-file-falls-back-without-candidates ()
  "File insertion should use a file prompt outside Git repositories."
  (let ((root "/tmp/project/")
        read-arguments)
    (cl-letf (((symbol-function 'ai-code--prompt-filepath-candidates)
               (lambda () nil))
              ((symbol-function 'ai-code--session-project-root)
               (lambda () root))
              ((symbol-function 'completing-read)
               (lambda (&rest _arguments)
                 (ert-fail "Empty candidate sets must use a file prompt")))
              ((symbol-function 'read-file-name)
               (lambda (&rest arguments)
                 (setq read-arguments arguments)
                 "/tmp/project/example.el")))
      (should (equal (ai-code-send--read-file)
                     "/tmp/project/example.el"))
      (should (equal read-arguments
                     '("Insert file: " "/tmp/project/" nil t))))))

(ert-deftest test-ai-code-send--file-ignores-blank-region ()
  "File insertion should use the file when the active region is blank."
  (let (inserted)
    (with-temp-buffer
      (insert " \n\t")
      (setq buffer-file-name "/tmp/current.el")
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (cl-letf (((symbol-function 'ai-code-send--dispatch-files)
                 (lambda (files &optional pick-session)
                   (setq inserted (list files pick-session))))
                ((symbol-function 'ai-code-send-region)
                 (lambda (&rest _args)
                   (ert-fail "Blank regions must not replace file insertion"))))
        (ai-code-send-file)
        (should (equal inserted '(("/tmp/current.el") nil)))))))

(ert-deftest test-ai-code-send--file-ignores-active-region ()
  "File insertion should insert the file even when a region is active."
  (let (inserted)
    (with-temp-buffer
      (insert "selected text")
      (setq buffer-file-name "/tmp/current.el")
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (cl-letf (((symbol-function 'ai-code-send--dispatch-files)
                 (lambda (files &optional pick-session)
                   (setq inserted (list files pick-session))))
                ((symbol-function 'ai-code-send-region)
                 (lambda (&rest _args)
                   (ert-fail "File insertion must not insert the region"))))
        (ai-code-send-file)
        (should (equal inserted '(("/tmp/current.el") nil)))))))

(ert-deftest test-ai-code-send--file-to-ignores-active-region ()
  "File-to insertion should insert the file even when a region is active."
  (let (inserted)
    (with-temp-buffer
      (insert "selected text")
      (setq buffer-file-name "/tmp/current.el")
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (cl-letf (((symbol-function 'ai-code-send--dispatch-files)
                 (lambda (files &optional pick-session)
                   (setq inserted (list files pick-session))))
                ((symbol-function 'ai-code-send-region)
                 (lambda (&rest _args)
                   (ert-fail "File-to insertion must not insert the region"))))
        (ai-code-send-file-to)
        (should (equal inserted '(("/tmp/current.el") t)))))))

(ert-deftest test-ai-code-send--region-includes-location-and-code ()
  "Region sends should include a file location and code block."
  (let (inserted)
    (with-temp-buffer
      (insert "first\nsecond\n")
      (emacs-lisp-mode)
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (setq buffer-file-name "/tmp/example.el")
      (cl-letf (((symbol-function 'ai-code--session-project-root)
                 (lambda () "/tmp/"))
                ((symbol-function 'ai-code-send--default-session)
                 (lambda () 'session-buffer))
                ((symbol-function 'ai-code-backends-infra-insert-string)
                 (lambda (text buffer)
                   (setq inserted (list text buffer)))))
        (ai-code-send-region nil)
        (should (eq (cadr inserted) 'session-buffer))
        (should (string-match-p "@example.el#L1-L2" (car inserted)))
        (should (string-match-p "```emacs-lisp" (car inserted)))
        (should (string-match-p "first\nsecond" (car inserted)))))))

(ert-deftest test-ai-code-send--region-available-p-requires-nonblank-text ()
  "Region availability should reject empty and whitespace-only selections."
  (with-temp-buffer
    (setq-local transient-mark-mode t)
    (let ((use-empty-active-region t))
      (set-mark (point))
      (activate-mark)
      (should-not (ai-code-send--region-available-p))
      (insert " \n\t")
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (should-not (ai-code-send--region-available-p))
      (erase-buffer)
      (insert "selected text")
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (should (ai-code-send--region-available-p)))))

(ert-deftest test-ai-code-send--dispatch-selects-session-before-insert ()
  "A pick-session insert should write to the selected session buffer."
  (let (inserted selected)
    (cl-letf (((symbol-function 'ai-code-send--select-session)
               (lambda () (setq selected 'session-buffer)
                 selected))
              ((symbol-function 'ai-code-backends-infra-insert-string)
               (lambda (text buffer)
                 (setq inserted (list text buffer)))))
      (ai-code-send--dispatch "hello" t)
      (should (equal selected 'session-buffer))
      (should (equal inserted '("\n\nhello\n\n" session-buffer))))))

(ert-deftest test-ai-code-send--dispatch-survives-partial-upgrade ()
  "Session insertion should not depend on a newly introduced variable."
  (let ((was-bound (boundp 'ai-code-send--separator))
        (old-value (and (boundp 'ai-code-send--separator)
                        (symbol-value 'ai-code-send--separator)))
        inserted)
    (unwind-protect
        (progn
          (makunbound 'ai-code-send--separator)
          (cl-letf (((symbol-function 'ai-code-backends-infra-insert-string)
                     (lambda (text buffer)
                       (setq inserted (list text buffer)))))
            (ai-code-send--insert-at-destination
             '(:buffer session-buffer :viewport nil :root nil)
             "hello")
            (should (equal inserted
                           '("\n\nhello\n\n" session-buffer)))))
      (if was-bound
          (set-default-toplevel-value 'ai-code-send--separator old-value)
        (makunbound 'ai-code-send--separator)))))

(ert-deftest test-ai-code-send--insert-into-session-omits-return ()
  "A multiline insert should use terminal paste and never send Return."
  (let (sent displayed)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
      (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-send-string)
                 (lambda (text &optional paste)
                   (setq sent (list text paste))))
                ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                 (lambda ()
                   (ert-fail "Insert commands must not send Return")))
                ((symbol-function
                  'ai-code-backends-infra--display-buffer-in-side-window)
                 (lambda (buffer)
                   (setq displayed buffer)))
                ((symbol-function 'get-buffer-process)
                 (lambda (_buffer) 'session-process))
                ((symbol-function 'process-live-p)
                 (lambda (process) (eq process 'session-process))))
        (ai-code-backends-infra-insert-string
         "first\nsecond" (current-buffer))
        (should (equal sent '("first\nsecond" t)))
        (should (eq displayed (current-buffer)))))))

(ert-deftest test-ai-code-send--dispatch-errors-without-destination ()
  "An insert should fail clearly when there is no destination."
  (cl-letf (((symbol-function 'ai-code-send--viewport-buffer)
             (lambda (&optional _session) nil))
            ((symbol-function
              'ai-code-backends-infra-current-buffer-session)
             (lambda (&optional _buffer) nil)))
    (should-error (ai-code-send--dispatch "hello") :type 'user-error)))

(ert-deftest test-ai-code-send--default-session-uses-current-buffer-session ()
  "The default destination should be the session attached to this buffer."
  (let ((session (generate-new-buffer " *ai-code-current-session*")))
    (unwind-protect
        (cl-letf (((symbol-function
                    'ai-code-backends-infra-current-buffer-session)
                   (lambda (&optional _buffer) session)))
          (should (eq (ai-code-send--default-session) session)))
      (kill-buffer session))))

(ert-deftest test-ai-code-send--default-session-reports-other-project-only ()
  "An unattached buffer should report that only another project has a session."
  (cl-letf (((symbol-function
              'ai-code-backends-infra-current-buffer-session)
             (lambda (&optional _buffer) nil))
            ((symbol-function 'ai-code-backends-infra-session-buffers)
             (lambda () '(other-project-session))))
    (let ((result (should-error (ai-code-send--default-session)
                                :type 'user-error)))
      (should (equal (cadr result) "No AI session for this project")))))

(ert-deftest test-ai-code-send--default-session-reports-no-global-session ()
  "An unattached buffer should distinguish the absence of all sessions."
  (cl-letf (((symbol-function
              'ai-code-backends-infra-current-buffer-session)
             (lambda (&optional _buffer) nil))
            ((symbol-function 'ai-code-backends-infra-session-buffers)
             (lambda () nil)))
    (let ((result (should-error (ai-code-send--default-session)
                                :type 'user-error)))
      (should (equal (cadr result)
                     "No AI sessions are running; start one first")))))

(ert-deftest test-ai-code-send--prepare-menu-reports-other-project-session ()
  "Preparing Insert should report when only another project has a session."
  (let (message-text)
    (cl-letf (((symbol-function 'ai-code-send--default-destination-p)
               (lambda () nil))
              ((symbol-function 'ai-code-backends-infra-session-buffers)
               (lambda () '(other-project-session)))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq message-text
                       (apply #'format format-string arguments)))))
      (should (ai-code-send-prepare-menu))
      (should (equal message-text "No AI session for this project")))))

(ert-deftest test-ai-code-send--prepare-menu-reports-no-global-session ()
  "Preparing Insert should fail clearly when no session is running."
  (cl-letf (((symbol-function 'ai-code-send--default-destination-p)
             (lambda () nil))
            ((symbol-function 'ai-code-backends-infra-session-buffers)
             (lambda () nil)))
    (let ((error-data (should-error (ai-code-send-prepare-menu)
                                    :type 'user-error)))
      (should (equal (cadr error-data)
                     "No AI sessions are running; start one first")))))

(ert-deftest test-ai-code-send--prepare-menu-rejects-orphaned-viewport ()
  "A viewport without a running source session should not be a destination."
  (let ((source (generate-new-buffer " *ai-code-stopped-source*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local ai-code-editor-viewport-mode t)
          (setq-local ai-code-editor-viewport--source-buffer source)
          (cl-letf (((symbol-function
                      'ai-code-backends-infra-session-buffers)
                     (lambda () nil)))
            (let ((error-data (should-error (ai-code-send-prepare-menu)
                                            :type 'user-error)))
              (should (equal (cadr error-data)
                             "No AI sessions are running; start one first")))))
      (kill-buffer source))))

(ert-deftest test-ai-code-send--select-session-offers-global-sessions ()
  "Explicit selection should offer sessions outside the current project."
  (let ((session (generate-new-buffer " *ai-code-other-project-session*"))
        candidates)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra-session-buffers)
                   (lambda () (list session)))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (setq candidates collection)
                     (car collection))))
          (should (eq (ai-code-send--select-session) session))
          (should (equal candidates (list (buffer-name session)))))
      (kill-buffer session))))

(ert-deftest test-ai-code-send--select-session-reports-no-global-sessions ()
  "Explicit selection should report when no sessions are running."
  (cl-letf (((symbol-function 'ai-code-backends-infra-session-buffers)
             (lambda () nil)))
    (let ((result (should-error (ai-code-send--select-session)
                                :type 'user-error)))
      (should (equal (cadr result)
                     "No AI sessions are running; start one first")))))

(ert-deftest test-ai-code-send--select-session-rejects-stopped-selection ()
  "Explicit selection should reject a session that stops during completion."
  (let ((session (generate-new-buffer " *ai-code-stopping-session*"))
        (lookup-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra-session-buffers)
                   (lambda ()
                     (setq lookup-count (1+ lookup-count))
                     (and (= lookup-count 1) (list session))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _arguments)
                     (car collection))))
          (let ((error-data (should-error (ai-code-send--select-session)
                                          :type 'user-error)))
            (should (equal (cadr error-data)
                           "AI session is no longer available"))))
      (kill-buffer session))))

(ert-deftest test-ai-code-send--dispatch-prefers-open-viewport ()
  "Sending should insert into an open viewport instead of a session."
  (let ((source (generate-new-buffer " *ai-code-live-viewport-source*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local ai-code-editor-viewport-mode t)
          (setq-local ai-code-editor-viewport--source-buffer source)
          (cl-letf (((symbol-function
                      'ai-code-backends-infra-session-buffers)
                     (lambda () (list source)))
                    ((symbol-function 'ai-code-backends-infra-insert-string)
                     (lambda (&rest _args)
                       (ert-fail
                        "The TUI session should not receive viewport text"))))
            (ai-code-send--dispatch "hello")
            (should (equal (buffer-string) "hello\n\n"))))
      (kill-buffer source))))

(ert-deftest test-ai-code-send--file-appends-viewport-attachment-separator ()
  "File sends should separate nonempty viewports and append a separator."
  (dolist (case '(("" . "[attachment]\n\n")
                  ("draft" . "draft\n\n[attachment]\n\n")))
    (let ((source (generate-new-buffer " *ai-code-file-viewport-source*"))
          inserted)
      (unwind-protect
          (with-temp-buffer
            (insert (car case))
            (setq-local ai-code-editor-viewport-mode t)
            (setq-local ai-code-editor-viewport--source-buffer source)
            (cl-letf (((symbol-function
                        'ai-code-backends-infra-session-buffers)
                       (lambda () (list source)))
                      ((symbol-function 'ai-code-send--files)
                       (lambda (&optional _prompt-for-file)
                         '("/tmp/example.png")))
                      ((symbol-function 'ai-code-editor-viewport-insert-files)
                       (lambda (files)
                         (setq inserted files)
                         (insert "[attachment]")))
                      ((symbol-function 'ai-code-send--dispatch)
                       (lambda (&rest _args)
                         (ert-fail
                          "The TUI session should not receive viewport files"))))
              (ai-code-send-file)
              (should (equal inserted '("/tmp/example.png")))
              (should (equal (buffer-string) (cdr case)))))
        (kill-buffer source)))))

(ert-deftest test-ai-code-send--explicit-target-uses-selected-session-viewport ()
  "An explicit target should use the viewport belonging to that session."
  (let ((session-a (generate-new-buffer " *ai-code-session-a*"))
        (session-b (generate-new-buffer " *ai-code-session-b*"))
        (viewport-b (generate-new-buffer " *ai-code-viewport-b*"))
        (viewport-a (generate-new-buffer " *ai-code-viewport-a*")))
    (unwind-protect
        (progn
          (with-current-buffer viewport-a
            (insert "draft")
            (setq-local ai-code-editor-viewport-mode t)
            (setq-local ai-code-editor-viewport--source-buffer session-a))
          (with-current-buffer viewport-b
            (setq-local ai-code-editor-viewport-mode t)
            (setq-local ai-code-editor-viewport--source-buffer session-b))
          (cl-letf (((symbol-function 'ai-code-send--select-session)
                     (lambda () session-a))
                    ((symbol-function 'ai-code-backends-infra-insert-string)
                     (lambda (&rest _args)
                       (ert-fail "The selected session viewport should win"))))
            (with-temp-buffer
              (ai-code-send--dispatch "selected" t)))
          (should (equal
                   (with-current-buffer viewport-a (buffer-string))
                   "draft\n\nselected\n\n"))
          (should (string-empty-p
                   (with-current-buffer viewport-b (buffer-string)))))
      (dolist (buffer (list session-a session-b viewport-a viewport-b))
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-send--unrelated-viewport-does-not-hijack-normal-send ()
  "A viewport for another session should not become an implicit destination."
  (let ((other-session (generate-new-buffer " *ai-code-other-session*"))
        (other-viewport (generate-new-buffer " *ai-code-other-viewport*")))
    (unwind-protect
        (progn
          (with-current-buffer other-viewport
            (setq-local ai-code-editor-viewport-mode t)
            (setq-local ai-code-editor-viewport--source-buffer other-session))
          (cl-letf (((symbol-function
                      'ai-code-backends-infra-current-buffer-session)
                     (lambda (&optional _buffer) nil))
                    ((symbol-function 'ai-code-backends-infra-session-buffers)
                     (lambda () (list other-session))))
            (with-temp-buffer
              (should-error (ai-code-send--dispatch "local")
                            :type 'user-error)))
          (should (string-empty-p
                   (with-current-buffer other-viewport (buffer-string)))))
      (kill-buffer other-session)
      (kill-buffer other-viewport))))

(ert-deftest test-ai-code-send--viewport-buffer-finds-session-viewport ()
  "Viewport lookup should find the editor associated with a session."
  (let ((session (generate-new-buffer " *ai-code-hidden-session*"))
        (viewport (generate-new-buffer " *ai-code-hidden-viewport*")))
    (unwind-protect
        (progn
          (with-current-buffer viewport
            (setq-local ai-code-editor-viewport-mode t)
            (setq-local ai-code-editor-viewport--source-buffer session))
          (should (eq (ai-code-send--viewport-buffer session) viewport)))
      (kill-buffer session)
      (kill-buffer viewport))))

(ert-deftest test-ai-code-send--diagnostic-text-includes-all-messages ()
  "Diagnostic insertion should retain every diagnostic at point."
  (with-temp-buffer
    (insert "current line")
    (cl-letf (((symbol-function 'ai-code-send--diagnostic-messages)
               (lambda () '("first problem" "second problem"))))
      (let ((text (ai-code-send--diagnostic-text)))
        (should (string-match-p "first problem" text))
        (should (string-match-p "second problem" text))))))

(ert-deftest test-ai-code-send--diagnostic-text-uses-destination-root ()
  "Cross-project diagnostics should use an absolute source location."
  (let* ((source-root (make-temp-file "ai-code-send-source-" t))
         (target-root (make-temp-file "ai-code-send-target-" t))
         (file (expand-file-name "src/main.el" source-root)))
    (unwind-protect
        (with-temp-buffer
          (insert "current line")
          (setq buffer-file-name file
                default-directory source-root)
          (cl-letf (((symbol-function 'ai-code-send--diagnostic-messages)
                     (lambda () '("problem"))))
            (should
             (string-prefix-p
              (format "Diagnostics at @%s#L1" file)
              (ai-code-send--diagnostic-text target-root)))))
      (delete-directory source-root t)
      (delete-directory target-root t))))

(ert-deftest test-ai-code-send--dwim-prefix-keeps-implicit-session ()
  "A prefix should not retarget the normal DWIM command globally."
  (let (pick-session)
    (cl-letf (((symbol-function 'ai-code-send--dispatch)
               (lambda (_content pick)
                 (setq pick-session pick))))
      (with-temp-buffer
        (insert "current line")
        (let ((current-prefix-arg '(4)))
          (call-interactively #'ai-code-send-dwim)))
      (should-not pick-session))))

(ert-deftest test-ai-code-send--dwim-to-selects-session ()
  "The DWIM to command should explicitly select a destination session."
  (let (pick-session)
    (cl-letf (((symbol-function 'ai-code-send--dispatch)
               (lambda (_content pick)
                 (setq pick-session pick))))
      (with-temp-buffer
        (insert "current line")
        (ai-code-send-dwim-to))
      (should pick-session))))

(ert-deftest test-ai-code-send--dwim-prefers-region-over-diagnostic ()
  "DWIM should preserve an explicit region even when point has diagnostics."
  (let (inserted)
    (with-temp-buffer
      (insert "selected text")
      (set-mark (point-min))
      (activate-mark)
      (cl-letf (((symbol-function 'ai-code-send--region-text)
                 (lambda (&optional _root) "region selection"))
                ((symbol-function 'ai-code-send--diagnostic-text)
                 (lambda (&optional _root) "diagnostic details"))
                ((symbol-function 'ai-code-send--dispatch)
                 (lambda (content pick-session)
                   (setq inserted
                         (list (ai-code-send--content-text content "/target/")
                               pick-session)))))
        (ai-code-send-dwim)
        (should (equal inserted '("region selection" nil)))))))

(ert-deftest test-ai-code-send--dwim-ignores-blank-region ()
  "DWIM should fall through to editor input when the region is blank."
  (let (inserted)
    (with-temp-buffer
      (insert " \n\t")
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (cl-letf (((symbol-function 'ai-code-send--region-text)
                 (lambda (&optional _root)
                   (ert-fail "Blank regions must not be inserted")))
                ((symbol-function 'ai-code-send--diagnostic-text)
                 (lambda (&optional _root) nil))
                ((symbol-function 'ai-code-send--point-text)
                 (lambda (&optional _root) "current line"))
                ((symbol-function 'ai-code-send--dispatch)
                 (lambda (content pick-session)
                   (setq inserted
                         (list (ai-code-send--content-text content "/target/")
                               pick-session)))))
        (ai-code-send-dwim)
        (should (equal inserted '("current line" nil)))))))

(ert-deftest test-ai-code-send--clipboard-prefix-keeps-implicit-session ()
  "A prefix should not retarget the normal clipboard image command globally."
  (let (pick-session)
    (cl-letf (((symbol-function 'ai-code-send--dispatch-generated-file)
               (lambda (_producer pick)
                 (setq pick-session pick))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'ai-code-send-clipboard-image))
      (should-not pick-session))))

(ert-deftest test-ai-code-send--clipboard-image-to-selects-session ()
  "The clipboard-image-to command should select a destination session."
  (let (pick-session)
    (cl-letf (((symbol-function 'ai-code-send--dispatch-generated-file)
               (lambda (_producer pick)
                 (setq pick-session pick))))
      (ai-code-send-clipboard-image-to)
      (should pick-session))))

(ert-deftest test-ai-code-send--clipboard-image-supports-terminal-emacs ()
  "Clipboard images should use configured handlers in terminal Emacs."
  (let (producer)
    (cl-letf (((symbol-function 'window-system) (lambda () nil))
              ((symbol-function 'ai-code-send--dispatch-generated-file)
               (lambda (file-producer _pick-session)
                 (setq producer file-producer))))
      (ai-code-send-clipboard-image)
      (should (eq producer
                  #'ai-code-editor-viewport-save-clipboard-image)))))

(ert-deftest test-ai-code-send--dwim-inserts-dired-files ()
  "DWIM should insert selected Dired files before considering buffer text."
  (let (inserted)
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest _modes) t))
              ((symbol-function 'ai-code-send--buffer-files)
               (lambda () '("/tmp/first.el" "/tmp/second.el")))
              ((symbol-function 'ai-code-send--dispatch-files)
               (lambda (files pick-session)
                 (setq inserted (list files pick-session))))
              ((symbol-function 'ai-code-send--dispatch)
               (lambda (&rest _args)
                 (ert-fail "Dired DWIM should dispatch files"))))
      (ai-code-send-dwim)
      (should (equal inserted
                     '(("/tmp/first.el" "/tmp/second.el") nil))))))

(ert-deftest test-ai-code-send--dired-region-files-returns-region-files ()
  "A Dired region should select the files displayed in that region."
  (let* ((root (make-temp-file "ai-code-send-dired-" t))
         (first (expand-file-name "first.el" root))
         (second (expand-file-name "second.el" root)))
    (unwind-protect
        (progn
          (with-temp-file first (insert "first"))
          (with-temp-file second (insert "second"))
          (with-current-buffer (dired-noselect root)
            (dired-goto-file first)
            (let ((start (line-beginning-position)))
              (dired-goto-file second)
              (let ((end (line-end-position)))
                (goto-char start)
                (set-mark end)
                (activate-mark)
                (should (equal (ai-code-send--dired-region-files)
                               (list first second)))))))
      (delete-directory root t))))

(ert-deftest test-ai-code-send--point-text-includes-current-line ()
  "Point sends should include the current file location and line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"hello\")\n")
    (goto-char (point-min))
    (setq buffer-file-name "/tmp/example.el")
    (cl-letf (((symbol-function 'ai-code--session-project-root)
               (lambda () "/tmp/")))
      (let ((text (ai-code-send--point-text)))
        (should (string-match-p "@example.el#L1" text))
        (should (string-match-p "```emacs-lisp" text))
        (should (string-match-p "message" text))))))

(ert-deftest test-ai-code-send--capture-screenshot-creates-destination ()
  "Screenshot capture should append a destination path and verify it exists."
  (let* ((directory (make-temp-file "ai-code-send-screenshot-" t))
         (ai-code-send-screenshot-command '("mock-capture" "--select"))
         captured-file)
    (unwind-protect
        (cl-letf (((symbol-function
                    'ai-code-editor-viewport-ensure-attachment-directory)
                   (lambda () directory))
                  ((symbol-function 'executable-find)
                   (lambda (program)
                     (when (string= program "mock-capture") program)))
                  ((symbol-function 'call-process)
                   (lambda (_program _in _out _display &rest args)
                     (setq captured-file (car (last args)))
                     (with-temp-file captured-file (insert "png"))
                     0)))
          (let ((file (ai-code-send--capture-screenshot)))
            (should (file-in-directory-p file directory))
            (should-not (equal file captured-file))
            (should (file-exists-p file))
            (should-not (file-exists-p captured-file))))
      (delete-directory directory t))))

(ert-deftest test-ai-code-send--capture-screenshot-stages-locally-for-tramp ()
  "Screenshot capture should copy a local image to a remote destination."
  (let* ((remote-directory "/ssh:example:/project/.ai.code.files/")
         (remote-file (concat remote-directory "screenshot.png"))
         (ai-code-send-screenshot-command '("mock-capture"))
         (make-temp-file-function (symbol-function 'make-temp-file))
         (call-process-function (symbol-function 'call-process))
         local-file
         process-file
         copied)
    (cl-letf (((symbol-function
                'ai-code-editor-viewport-ensure-attachment-directory)
               (lambda () remote-directory))
              ((symbol-function 'make-temp-file)
               (lambda (prefix &rest arguments)
                 (if (file-remote-p prefix)
                     remote-file
                   (setq local-file
                         (apply make-temp-file-function prefix arguments)))))
              ((symbol-function 'executable-find)
               (lambda (program) program))
              ((symbol-function 'call-process)
               (lambda (program input output display &rest arguments)
                 (if (string= program "mock-capture")
                     (progn
                       (setq process-file (car (last arguments)))
                       (when (file-remote-p process-file)
                         (ert-fail
                          "Local capture commands cannot write TRAMP paths"))
                       (with-temp-file process-file (insert "png"))
                       0)
                   (apply call-process-function
                          program input output display arguments))))
              ((symbol-function 'copy-file)
               (lambda (source destination &rest _arguments)
                 (setq copied (list source destination)))))
      (should (equal (ai-code-send--capture-screenshot) remote-file))
      (should (equal process-file local-file))
      (should (equal copied (list local-file remote-file)))
      (should-not (file-exists-p local-file)))))

(ert-deftest test-ai-code-send--screenshot-to-selects-before-capture ()
  "Canceling session selection should not create a screenshot."
  (let (captured)
    (cl-letf (((symbol-function 'ai-code-send--select-session)
               (lambda () (signal 'quit nil)))
              ((symbol-function 'ai-code-send--capture-screenshot)
               (lambda ()
                 (setq captured t)
                 "/tmp/unexpected-screenshot.png")))
      (should-not
       (condition-case nil
           (progn (ai-code-send-screenshot-to) t)
         (quit nil)))
      (should-not captured))))

(ert-deftest test-ai-code-send--generated-file-is-removed-on-insert-failure ()
  "A newly generated attachment should be removed if insertion fails."
  (let ((file (make-temp-file "ai-code-send-generated-" nil ".png"))
        (destination-buffer (generate-new-buffer
                             " *ai-code-generated-destination*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-send--capture-screenshot)
                   (lambda () file))
                  ((symbol-function 'ai-code-send--resolve-destination)
                   (lambda (&optional _pick-session)
                     (list :buffer destination-buffer)))
                  ((symbol-function 'ai-code-send--insert-at-destination)
                   (lambda (&rest _arguments)
                     (user-error "Insertion failed"))))
          (should-error (ai-code-send-screenshot) :type 'user-error)
          (should-not (file-exists-p file)))
      (when (buffer-live-p destination-buffer)
        (kill-buffer destination-buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ai-code-send--screenshot-to-uses-target-session-directory ()
  "A selected session should own the generated screenshot attachment."
  (let* ((source-directory (make-temp-file "ai-code-send-source-" t))
         (target-directory (make-temp-file "ai-code-send-target-" t))
         (session (generate-new-buffer " *ai-code-screenshot-target*"))
         (ai-code-send-screenshot-command '("mock-capture"))
         captured-file)
    (unwind-protect
        (progn
          (with-current-buffer session
            (setq default-directory target-directory))
          (with-temp-buffer
            (setq default-directory source-directory)
            (cl-letf (((symbol-function 'ai-code-send--select-session)
                       (lambda () session))
                      ((symbol-function 'ai-code-send--viewport-buffer)
                       (lambda (&optional _session) nil))
                      ((symbol-function
                        'ai-code-backends-infra-session-directory)
                       (lambda (_buffer) target-directory))
                      ((symbol-function 'executable-find)
                       (lambda (program) program))
                      ((symbol-function 'call-process)
                       (lambda (_program _in _out _display &rest arguments)
                         (setq captured-file (car (last arguments)))
                         (with-temp-file captured-file
                           (insert "png"))
                         0))
                      ((symbol-function 'ai-code-backends-infra-insert-string)
                       (lambda (&rest _arguments) nil)))
              (ai-code-send-screenshot-to)
              (let ((files
                     (directory-files-recursively target-directory
                                                  "\\.png\\'")))
                (should (length= files 1))
                (should (file-in-directory-p (car files)
                                             target-directory))
                (should-not (file-exists-p captured-file))))))
      (when (buffer-live-p session)
        (kill-buffer session))
      (delete-directory source-directory t)
      (delete-directory target-directory t))))

(provide 'test_ai-code-send)

;;; test_ai-code-send.el ends here
