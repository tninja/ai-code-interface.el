;;; test_ai-code-session.el --- Tests for ai-code-session.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the AI session registry and dashboard.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-session)

(defmacro ai-code-test-session--with-clean-registry (&rest body)
  "Run BODY with a fresh session registry."
  `(let ((ai-code-session--sessions (make-hash-table :test 'equal))
         (ai-code-session--next-id 0))
     ,@body))

(ert-deftest ai-code-test-session-register-reuses-existing-buffer-entry ()
  "Registering the same buffer twice should update the existing session."
  (ai-code-test-session--with-clean-registry
   (let ((buffer (get-buffer-create "*codex[test]*")))
     (unwind-protect
         (let* ((first (ai-code-session-register
                        :buffer buffer
                        :backend "codex"
                        :repo-root "/tmp/repo/"
                        :task-file "/tmp/repo/.ai.code.files/task-a.org"))
                (second (ai-code-session-register
                         :buffer buffer
                         :backend "codex"
                         :repo-root "/tmp/repo/"
                         :task-file "/tmp/repo/.ai.code.files/task-b.org")))
           (should (equal (ai-code-session-id first)
                          (ai-code-session-id second)))
           (should (= (length (ai-code-session-list)) 1))
           (should (equal (ai-code-session-task-file second)
                          "/tmp/repo/.ai.code.files/task-b.org")))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest ai-code-test-session-register-canonicalizes-repo-root-alias ()
  "Store one canonical repository identity for a symlinked root."
  (ai-code-test-session--with-clean-registry
   (let* ((root (make-temp-file "ai-code-registry-root-" t))
          (alias-parent (make-temp-file "ai-code-registry-alias-" t))
          (alias-root (expand-file-name "repo" alias-parent))
          (buffer (generate-new-buffer " *ai-code-registry-session*")))
     (unwind-protect
         (progn
           (make-symbolic-link root alias-root)
           (let ((session (ai-code-session-register
                           :buffer buffer
                           :backend "codex"
                           :repo-root alias-root)))
             (should (equal (ai-code-session-repo-root session)
                            (file-name-as-directory
                             (file-truename root))))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (ignore-errors (delete-directory alias-parent t))
       (ignore-errors (delete-directory root t))))))

(ert-deftest ai-code-test-session-refresh-populates-branch-status-and-dirty-count ()
  "Refreshing should populate simple git/process metadata."
  (ai-code-test-session--with-clean-registry
   (let ((buffer (get-buffer-create "*codex[test-refresh]*"))
         (repo-root (make-temp-file "ai-code-session-refresh-" t)))
     (unwind-protect
         (cl-letf (((symbol-function 'get-buffer-process)
                    (lambda (_buffer) 'mock-process))
                   ((symbol-function 'process-live-p)
                    (lambda (_process) t))
                   ((symbol-function 'magit-get-current-branch)
                    (lambda () "feature/dashboard"))
                   ((symbol-function 'magit-git-lines)
                    (lambda (&rest _args) '("a" "b" "c"))))
           (let ((session (ai-code-session-register
                           :buffer buffer
                           :backend "codex"
                           :repo-root repo-root
                           :task-file "/tmp/repo/.ai.code.files/task.org")))
             (ai-code-session-refresh)
             (setq session (ai-code-session-get (ai-code-session-id session)))
             (should (equal (plist-get (ai-code-session-metadata session) :branch)
                            "feature/dashboard"))
             (should (equal (plist-get (ai-code-session-metadata session) :status)
                            "running"))
             (should (= (plist-get (ai-code-session-metadata session) :dirty-count)
                        3))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))))))

(ert-deftest ai-code-test-session-refresh-prunes-dead-buffers ()
  "Refreshing should drop sessions whose buffers are no longer live."
  (ai-code-test-session--with-clean-registry
   (let ((buffer (get-buffer-create "*codex[test-dead]*")))
     (ai-code-session-register
      :buffer buffer
      :backend "codex"
      :repo-root "/tmp/repo/")
     (kill-buffer buffer)
     (ai-code-session-refresh)
     (should-not (ai-code-session-list)))))

(ert-deftest ai-code-test-session-get-invalid-targets-return-nil ()
  "Invalid `ai-code-session-get' targets should return nil."
  (ai-code-test-session--with-clean-registry
   (should-not (ai-code-session-get nil))
   (should-not (ai-code-session-get "missing-session-id"))))

(defun ai-code-test-session-dashboard-goto-first-entry ()
  "Move point to the first dashboard entry."
  (goto-char (point-min))
  (re-search-forward "^S[0-9]+" nil t)
  (beginning-of-line))

(ert-deftest ai-code-test-session-dashboard-entry-shows-attention ()
  "The Status column shows the same states as the tab badge."
  (ai-code-test-session--with-clean-registry
   (let ((buffer (generate-new-buffer " *codex[attention]*")))
     (unwind-protect
         (cl-flet ((status (metadata)
                     (let ((session (ai-code-session-register
                                     :buffer buffer :backend "codex"
                                     :metadata metadata)))
                       (aref (cadr (ai-code-session-dashboard--entry session)) 5))))
           (should (equal (status '(:status "running")) "idle"))
           (should (equal (status '(:working t)) "working"))
           (should (equal (status '(:attention done)) "done unseen"))
           (should (equal (status '(:attention waiting)) "needs input"))
           (should (equal (status '(:status "stopped" :attention nil :working nil))
                          "stopped")))
       (kill-buffer buffer)))))

(ert-deftest ai-code-test-session-prompt-summary-uses-goal-line ()
  "A structured prompt is summarized by the first line of its Goal."
  (should (equal (ai-code-session--prompt-summary
                  "Goal:\nRename the   helper\nand its callers\n\nScope:\nFile: a.el\n")
                 "Rename the helper")))

(ert-deftest ai-code-test-session-prompt-summary-puts-subject-first ()
  "A scope that names a headline or comment puts that subject first."
  (should (equal (ai-code-session--prompt-summary
                  (concat "Goal:\nPlease answer my question about this Org headline. ultrathink\n\n"
                          "Scope:\nOrg headline on line 2 (notes.org#L2-L3):\n"
                          "** Boris Cherny start talking\n- what are his tips?\n"
                          "Function: Boris Cherny start talking\n\nInstruction:\nAnswer only."))
                 "Boris Cherny start talking — Please answer my question about this Org headline. ultrathink")))

(ert-deftest ai-code-test-session-prompt-summary-plain-text ()
  "A plain prompt is summarized by its first non-empty line."
  (should (equal (ai-code-session--prompt-summary "\n\n  fix the flaky login test  \nmore detail\n")
                 "fix the flaky login test")))

(ert-deftest ai-code-test-session-note-prompt-keeps-bounded-copy ()
  "Recording a prompt keeps at most a bounded prefix of it."
  (ai-code-test-session--with-clean-registry
   (let ((buffer (generate-new-buffer " *codex[prompt]*")))
     (unwind-protect
         (progn
           (ai-code-session-register :buffer buffer :backend "codex")
           (ai-code-session-note-prompt buffer (make-string 5000 ?x))
           (should (= (length (plist-get (ai-code-session-metadata
                                          (ai-code-session-get buffer))
                                         :last-prompt))
                      ai-code-session--prompt-max-chars)))
       (kill-buffer buffer)))))

(ert-deftest ai-code-test-session-diff-shortstat-parsing ()
  "Insertions and deletions are read from git's shortstat line."
  (should (equal (ai-code-session--parse-shortstat
                  " 3 files changed, 120 insertions(+), 34 deletions(-)")
                 '(120 . 34)))
  (should (equal (ai-code-session--parse-shortstat " 1 file changed, 2 insertions(+)")
                 '(2 . 0)))
  (should (equal (ai-code-session--parse-shortstat " 1 file changed, 5 deletions(-)")
                 '(0 . 5)))
  (should-not (ai-code-session--parse-shortstat nil)))

(ert-deftest ai-code-test-session-dashboard-entry-shows-since-changes-and-prompt ()
  "The dashboard shows time in state, change size, and the last prompt."
  (ai-code-test-session--with-clean-registry
   (let ((buffer (generate-new-buffer " *codex[columns]*"))
         (now (float-time))
         (prompt (concat "Goal:\n" (make-string 100 ?y) "\n\nScope:\nFile: a.el")))
     (unwind-protect
         (let* ((session (ai-code-session-register
                          :buffer buffer :backend "codex"
                          :metadata (list :status "running" :attention 'done
                                          :attention-since (- now 180)
                                          :dirty-count 3 :insertions 120 :deletions 34
                                          :last-prompt prompt)))
                (row (cadr (ai-code-session-dashboard--entry session)))
                (prompt-cell (aref row 8)))
           (should (equal (aref row 6) "3m"))
           (should (equal (aref row 7) "3 files +120 −34"))
           (should (= (string-width prompt-cell) ai-code-session-dashboard--prompt-width))
           (should (string-suffix-p "…" prompt-cell))
           (should (string-match-p "Scope:" (get-text-property 0 'help-echo prompt-cell))))
       (kill-buffer buffer)))))

(ert-deftest ai-code-test-session-dashboard-since-and-changes-formats ()
  "Since follows the current state; Changes says clean when nothing changed."
  (let ((now (float-time)))
    (should (equal (ai-code-session-dashboard--since
                    (list :working t :working-since (- now 45)) now)
                   "45s"))
    (should (equal (ai-code-session-dashboard--since
                    (list :quiet-since (- now 7200)) now)
                   "2h"))
    (should (equal (ai-code-session-dashboard--since (list :status "stopped") now) ""))
    (should (equal (ai-code-session-dashboard--changes (list :dirty-count 0)) "clean"))
    (should (equal (ai-code-session-dashboard--changes (list :dirty-count 2)) "2 files"))))

(ert-deftest ai-code-test-session-dashboard-toggles-bottom-window ()
  "The dashboard opens in a bottom side window and the same command closes it."
  (ai-code-test-session--with-clean-registry
   (save-window-excursion
     (delete-other-windows)
     (unwind-protect
         (progn
           (ai-code-session-dashboard)
           (let ((window (get-buffer-window ai-code-session-dashboard-buffer-name)))
             (should window)
             (should (eq (window-parameter window 'window-side) 'bottom))
             (should (eq (selected-window) window))
             ;; The key hint lives in the mode line only.
             (should-not (string-match-p
                          (regexp-quote ai-code-session-dashboard-shortcuts-hint)
                          (with-current-buffer (window-buffer window)
                            (buffer-string)))))
           (ai-code-session-dashboard)
           (should-not (get-buffer-window ai-code-session-dashboard-buffer-name)))
       (when (get-buffer ai-code-session-dashboard-buffer-name)
         (kill-buffer ai-code-session-dashboard-buffer-name))))))

(ert-deftest ai-code-test-session-dashboard-renders-mvp-columns ()
  "Dashboard should render the MVP session information."
  (ai-code-test-session--with-clean-registry
   (let ((session-buffer (get-buffer-create "*codex[demo]*"))
         (repo-root (make-temp-file "ai-code-dashboard-demo-" t))
         dashboard-buffer)
     (unwind-protect
         (progn
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :task-file "/tmp/demo-repo/.ai.code.files/task_x.org"
            :metadata '(:branch "feature/x" :status "running" :dirty-count 3))
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'magit-get-current-branch)
                      (lambda () "feature/x"))
                     ((symbol-function 'magit-git-lines)
                      (lambda (&rest _args) '("a" "b" "c")))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer))))
            (ai-code-session-dashboard))
            (with-current-buffer dashboard-buffer
              (should (derived-mode-p 'ai-code-session-dashboard-mode))
              (should (equal (length tabulated-list-entries) 1))
              (should
               (equal
                (substring-no-properties
                 (cadr (buffer-local-value 'mode-line-format
                                           dashboard-buffer)))
                ai-code-session-dashboard-shortcuts-hint))
              (let ((entry (cadar tabulated-list-entries)))
                (should (equal (aref entry 1)
                               (file-name-nondirectory
                                (directory-file-name repo-root))))
               (should (equal (aref entry 2) "task_x.org"))
               (should (equal (aref entry 3) "Codex"))
               (should (equal (aref entry 4) "feature/x"))
               (should (equal (aref entry 5) "idle"))
               (should (equal (aref entry 7) "3 files")))))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-visit-switches-to-session-buffer ()
  "RET should jump to the selected session buffer."
  (ai-code-test-session--with-clean-registry
   (let ((session-buffer (get-buffer-create "*codex[visit]*"))
         (repo-root (make-temp-file "ai-code-dashboard-visit-" t))
         dashboard-buffer
         visited-buffer)
     (unwind-protect
         (progn
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :metadata '(:status "running" :dirty-count 0))
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (if (eq buffer session-buffer)
                            (setq visited-buffer buffer)
                          (setq dashboard-buffer buffer))
                        (get-buffer-window buffer))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
               (ai-code-test-session-dashboard-goto-first-entry)
               (ai-code-session-dashboard-visit))
             (should (eq visited-buffer session-buffer))))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-refresh-preserves-selected-entry ()
  "Refreshing should keep point on the selected dashboard entry."
  (ai-code-test-session--with-clean-registry
   (let ((session-buffer (get-buffer-create "*codex[refresh-point]*"))
         (repo-root (make-temp-file "ai-code-dashboard-refresh-point-" t))
         dashboard-buffer
         session-id)
     (unwind-protect
         (progn
           (setq session-id
                 (ai-code-session-id
                  (ai-code-session-register
                   :buffer session-buffer
                   :backend "codex"
                   :repo-root repo-root
                   :metadata '(:status "running" :dirty-count 0))))
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
               (ai-code-test-session-dashboard-goto-first-entry)
               (ai-code-session-dashboard-refresh)
               (should (equal (tabulated-list-get-id) session-id)))))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-open-diff-uses-magit-status-setup-buffer ()
  "D should open Magit status setup for the session repository."
  (ai-code-test-session--with-clean-registry
   (let ((session-buffer (get-buffer-create "*codex[diff]*"))
         ;; Resolve the temporary directory, because the dashboard reports the
         ;; repository root canonicalized and /var is a symlink on macOS.
         (repo-root (file-truename (make-temp-file "ai-code-dashboard-diff-" t)))
         dashboard-buffer
         opened-repo)
     (unwind-protect
         (progn
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :metadata '(:status "running" :dirty-count 0))
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer)))
                     ((symbol-function 'magit-status)
                      (lambda (&rest _args)
                        (ert-fail
                         "Dashboard should avoid interactive `magit-status'")))
                     ((symbol-function 'magit-status-setup-buffer)
                      (lambda (directory &rest _args)
                        (setq opened-repo directory))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
                (ai-code-test-session-dashboard-goto-first-entry)
                (ai-code-session-dashboard-open-diff))
             (should (equal opened-repo
                            (file-name-as-directory repo-root)))))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-kill-session-unregisters-live-buffer ()
  "Killing a session should unregister it even when the buffer stays live."
  (ai-code-test-session--with-clean-registry
   (let ((session-buffer (get-buffer-create "*codex[kill-live-buffer]*"))
         (repo-root (make-temp-file "ai-code-dashboard-kill-live-buffer-" t))
         dashboard-buffer
         process)
     (unwind-protect
         (progn
           (setq process (start-process "ai-code-dashboard-kill-live-buffer"
                                        session-buffer
                                        "sleep"
                                        "60"))
           (set-process-query-on-exit-flag process nil)
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :metadata '(:status "running" :dirty-count 0))
           (cl-letf (((symbol-function 'y-or-n-p)
                      (lambda (&rest _args) t))
                     ((symbol-function 'kill-buffer)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
               (ai-code-test-session-dashboard-goto-first-entry)
               (ai-code-session-dashboard-kill-session))
             (should (buffer-live-p session-buffer))
             (should-not (ai-code-session-get session-buffer))
             (should-not (process-live-p process))))
       (when (and process (process-live-p process))
         (delete-process process))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-kill-session-removes-registry-entry ()
  "Killing a running dashboard session should unregister it."
  (ai-code-test-session--with-clean-registry
   (let ((session-buffer (get-buffer-create "*codex[kill]*"))
         (repo-root (make-temp-file "ai-code-dashboard-kill-" t))
         dashboard-buffer
         process)
     (unwind-protect
         (progn
           (setq process (start-process "ai-code-dashboard-kill" session-buffer
                                        "sleep" "60"))
           (set-process-query-on-exit-flag process nil)
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :metadata '(:status "running" :dirty-count 0))
           (cl-letf (((symbol-function 'y-or-n-p)
                      (lambda (&rest _args) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
                (ai-code-test-session-dashboard-goto-first-entry)
                (ai-code-session-dashboard-kill-session))
             (should-not (ai-code-session-list))
             (should-not (buffer-live-p session-buffer))
             (should-not (process-live-p process))))
       (when (and process (process-live-p process))
         (delete-process process))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(provide 'test_ai-code-session)

;;; test_ai-code-session.el ends here
