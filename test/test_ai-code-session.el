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
               (should (equal (aref entry 5) "running"))
               (should (equal (aref entry 6) "3")))))
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

(ert-deftest ai-code-test-session-status-distinguishes-working-ready-and-idle ()
  "Session status should report working, ready, or idle from output activity."
  (ai-code-test-session--with-clean-registry
   (let ((buffer (get-buffer-create "*codex[status-detail]*"))
         (repo-root (make-temp-file "ai-code-session-status-detail-" t)))
     (unwind-protect
         (cl-letf (((symbol-function 'get-buffer-process)
                    (lambda (_buffer) 'mock-process))
                   ((symbol-function 'process-live-p)
                    (lambda (_process) t))
                   ((symbol-function 'magit-get-current-branch)
                    (lambda () "main"))
                   ((symbol-function 'magit-git-lines)
                    (lambda (&rest _args) nil)))
           (let ((session (ai-code-session-register
                           :buffer buffer
                           :backend "codex"
                           :repo-root repo-root)))
             ;; Recent output within idle delay -> working
             (with-current-buffer buffer
               (setq-local ai-code-backends-infra--last-meaningful-output-time
                           (float-time))
               (setq-local ai-code-backends-infra--response-seen nil))
             (ai-code-session-refresh)
             (should (equal (plist-get (ai-code-session-metadata
                                        (ai-code-session-get (ai-code-session-id session)))
                                       :status)
                            "working"))
             ;; Output older than idle delay and unseen -> ready
             (with-current-buffer buffer
               (setq-local ai-code-backends-infra--last-meaningful-output-time
                           (- (float-time) 20.0))
               (setq-local ai-code-backends-infra--response-seen nil))
             (ai-code-session-refresh)
             (should (equal (plist-get (ai-code-session-metadata
                                        (ai-code-session-get (ai-code-session-id session)))
                                       :status)
                            "ready"))
             ;; Output older than idle delay and seen -> idle
             (with-current-buffer buffer
               (setq-local ai-code-backends-infra--response-seen t))
             (ai-code-session-refresh)
             (should (equal (plist-get (ai-code-session-metadata
                                        (ai-code-session-get (ai-code-session-id session)))
                                       :status)
                            "idle"))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))))))

(ert-deftest ai-code-test-session-dashboard-visit-marks-response-seen ()
  "Visiting a ready session from the dashboard should mark its response seen."
  (ai-code-test-session--with-clean-registry
   (let ((session-buffer (get-buffer-create "*codex[visit-seen]*"))
         (repo-root (make-temp-file "ai-code-dashboard-visit-seen-" t))
         dashboard-buffer)
     (unwind-protect
         (progn
           (with-current-buffer session-buffer
             (setq-local ai-code-backends-infra--last-meaningful-output-time
                         (- (float-time) 20.0))
             (setq-local ai-code-backends-infra--response-seen nil))
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root)
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (unless (eq buffer session-buffer)
                          (setq dashboard-buffer buffer))
                        (get-buffer-window buffer))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
               (ai-code-test-session-dashboard-goto-first-entry)
               (ai-code-session-dashboard-visit))
             (should (buffer-local-value 'ai-code-backends-infra--response-seen
                                         session-buffer))))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-open-task-visits-task-file ()
  "Pressing `t' in dashboard should open the session's associated task file."
  (ai-code-test-session--with-clean-registry
   (let* ((session-buffer (get-buffer-create "*codex[task]*"))
          (repo-root (make-temp-file "ai-code-dashboard-task-" t))
          (task-file (expand-file-name "task_demo.org" repo-root))
          dashboard-buffer
          opened-file)
     (unwind-protect
         (progn
           (with-temp-file task-file
             (insert "* Demo Task\n"))
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :task-file task-file)
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer)))
                     ((symbol-function 'find-file)
                      (lambda (filename &rest _args)
                        (setq opened-file filename))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
               (should (eq (lookup-key ai-code-session-dashboard-mode-map (kbd "t"))
                           #'ai-code-session-dashboard-open-task))
               (ai-code-test-session-dashboard-goto-first-entry)
               (ai-code-session-dashboard-open-task))
             (should (equal opened-file (expand-file-name task-file)))))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-handoff-or-checkpoint-targets-session ()
  "Pressing `H' in dashboard should run checkpoint or handoff for that session."
  (ai-code-test-session--with-clean-registry
   (let* ((session-buffer (get-buffer-create "*codex[handoff]*"))
          (repo-root (make-temp-file "ai-code-dashboard-handoff-" t))
          (task-file (expand-file-name "task_handoff.org" repo-root))
          dashboard-buffer
          checkpoint-buffer
          handoff-file)
     (unwind-protect
         (progn
           (with-temp-file task-file
             (insert "* Handoff Task\n"))
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :task-file task-file)
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer)))
                     ((symbol-function 'ai-code-session-checkpoint)
                      (lambda ()
                        (setq checkpoint-buffer (current-buffer))))
                     ((symbol-function 'ai-code-agent-handoff)
                      (lambda (&optional _arg)
                        (setq handoff-file buffer-file-name))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
               (should (eq (lookup-key ai-code-session-dashboard-mode-map (kbd "H"))
                           #'ai-code-session-dashboard-handoff-or-checkpoint))
               (ai-code-test-session-dashboard-goto-first-entry)
               (ai-code-session-dashboard-handoff-or-checkpoint nil)
               (should (eq checkpoint-buffer session-buffer))
               (ai-code-session-dashboard-handoff-or-checkpoint '(4))
               (should (equal (file-truename handoff-file)
                              (file-truename task-file))))))
       (when-let ((task-buf (find-buffer-visiting task-file)))
         (kill-buffer task-buf))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(ert-deftest ai-code-test-session-dashboard-review-turn-diff-targets-session-repo ()
  "Pressing `d' in dashboard should call `ai-code-git-review-turn-diff' for the session repo."
  (ai-code-test-session--with-clean-registry
   (let* ((session-buffer (get-buffer-create "*codex[turn-diff]*"))
          (repo-root (file-truename (make-temp-file "ai-code-dashboard-turn-diff-" t)))
          dashboard-buffer
          reviewed-args)
     (unwind-protect
         (progn
           (ai-code-session-register
            :buffer session-buffer
            :backend "codex"
            :repo-root repo-root
            :metadata '(:status "idle" :dirty-count 1))
           (cl-letf (((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) t))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buffer &rest _args)
                        (setq dashboard-buffer buffer)
                        (get-buffer-window buffer)))
                     ((symbol-function 'ai-code-git-review-turn-diff)
                      (lambda (&optional git-root select-from-history)
                        (setq reviewed-args (list git-root select-from-history)))))
             (ai-code-session-dashboard)
             (with-current-buffer dashboard-buffer
               (should (eq (lookup-key ai-code-session-dashboard-mode-map (kbd "d"))
                           #'ai-code-session-dashboard-review-turn-diff))
               (ai-code-test-session-dashboard-goto-first-entry)
               (ai-code-session-dashboard-review-turn-diff nil)
               (should (equal reviewed-args
                              (list (file-name-as-directory repo-root) nil)))
               (ai-code-session-dashboard-review-turn-diff '(4))
               (should (equal reviewed-args
                              (list (file-name-as-directory repo-root) '(4)))))))
       (when (buffer-live-p session-buffer)
         (kill-buffer session-buffer))
       (when (file-directory-p repo-root)
         (delete-directory repo-root t))
       (when (buffer-live-p dashboard-buffer)
         (kill-buffer dashboard-buffer))))))

(provide 'test_ai-code-session)

;;; test_ai-code-session.el ends here

