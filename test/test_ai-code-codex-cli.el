;;; test_ai-code-codex-cli.el --- Tests for ai-code-codex-cli -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-codex-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-codex-cli)
(require 'ai-code-mcp-agent nil t)

(declare-function ai-code--set-session-project-root "ai-code-utils" (root))

(ert-deftest ai-code-test-codex-cli-reuses-session-across-repo-alias-and-nested-project ()
  "Reuse one Git worktree session across path aliases and nested projects."
  (let* ((root (make-temp-file "ai-code-session-repo-" t))
         (alias-parent (make-temp-file "ai-code-session-alias-" t))
         (alias-root (expand-file-name "repo" alias-parent))
         (task-dir (expand-file-name ".ai.code.files" alias-root))
         (task-file (expand-file-name "task.org" task-dir))
         (nested-dir (expand-file-name "packages/app" root))
         (code-file (expand-file-name "main.el" nested-dir))
         (task-buffer (generate-new-buffer " *ai-code-task-source*"))
         (code-buffer (generate-new-buffer " *ai-code-code-source*"))
         (session-buffer (generate-new-buffer " *ai-code-session-target*"))
         (ai-code-codex-cli--processes (make-hash-table :test #'equal))
         process
         sent-from)
    (unwind-protect
        (progn
          (should (zerop (process-file "git" nil nil nil "-C" root "init" "--quiet")))
          (make-symbolic-link root alias-root)
          (make-directory task-dir t)
          (make-directory nested-dir t)
          (with-temp-file task-file (insert "* Task\n"))
          (with-temp-file code-file (insert ";;; main.el\n"))
          (with-current-buffer task-buffer
            (setq buffer-file-name task-file
                  default-directory task-dir))
          (with-current-buffer code-buffer
            (setq buffer-file-name code-file
                  default-directory nested-dir))
          (setq process (start-process "ai-code-session-test" session-buffer "cat"))
          (cl-letf (((symbol-function 'magit-toplevel)
                     (lambda (&optional dir)
                       (car (process-lines
                             "git" "-C" (or dir default-directory)
                             "rev-parse" "--show-toplevel"))))
                    ((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt dir)
                       (cons 'test-project
                             (if (file-in-directory-p
                                  (expand-file-name (or dir default-directory))
                                  nested-dir)
                                 nested-dir
                               alias-root))))
                    ((symbol-function 'project-root) #'cdr)
                    ((symbol-function 'ai-code-mcp-agent-prepare-launch)
                     (lambda (_backend _working-dir argv)
                       (list :argv argv)))
                    ((symbol-function 'ai-code-backends-infra--create-terminal-session)
                     (lambda (buffer-name working-dir _command _env-vars)
                       (with-current-buffer session-buffer
                         (rename-buffer buffer-name t)
                         (setq-local ai-code-backends-infra--session-directory
                                     (ai-code-backends-infra--normalize-session-directory
                                      working-dir)))
                       (cons session-buffer process)))
                    ((symbol-function 'ai-code-backends-infra--configure-session-buffer)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (_line &optional _paste)
                       (setq sent-from (current-buffer))))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda () nil))
                    ((symbol-function 'sleep-for) (lambda (&rest _args) nil))
                    ((symbol-function 'sit-for) (lambda (&rest _args) nil)))
            (with-current-buffer task-buffer
              (ai-code-codex-cli))
            (with-current-buffer code-buffer
              (ai-code-codex-cli-send-command "Explain this file"))
            (should (eq sent-from session-buffer))))
      (when (and process (process-live-p process))
        (delete-process process))
      (dolist (buffer (list task-buffer code-buffer session-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (ignore-errors (delete-directory alias-parent t))
      (ignore-errors (delete-directory root t)))))

(ert-deftest ai-code-test-codex-cli-keeps-shared-task-attachments-worktree-local ()
  "Keep public session switching local when worktrees share one task file."
  (let* ((main-root (make-temp-file "ai-code-main-worktree-" t))
         (linked-root (make-temp-file "ai-code-linked-worktree-" t))
         (shared-file (expand-file-name "shared-task.org" main-root))
         (main-source (generate-new-buffer " *ai-code-main-task*"))
         (linked-source (generate-new-buffer " *ai-code-linked-task*"))
         (main-session (generate-new-buffer "*codex[main:default]*"))
         (linked-session (generate-new-buffer "*codex[linked:default]*"))
         displayed)
    (unwind-protect
        (progn
          (clrhash ai-code-backends-infra--directory-buffer-map)
          (clrhash ai-code-backends-infra--file-session-map)
          (with-temp-file shared-file (insert "* Shared task\n"))
          (dolist (source (list main-source linked-source))
            (with-current-buffer source
              (setq buffer-file-name shared-file)))
          (with-current-buffer main-source
            (setq default-directory main-root)
            (ai-code--set-session-project-root main-root))
          (with-current-buffer linked-source
            (setq default-directory linked-root)
            (ai-code--set-session-project-root linked-root))
          (with-current-buffer main-session
            (setq-local ai-code-backends-infra--session-directory
                        (ai-code-backends-infra--normalize-session-directory
                         main-root)))
          (ai-code-backends-infra--remember-file-session-buffer
           "codex" main-source main-session)
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (buffer)
                       (push buffer displayed))))
            (with-current-buffer linked-source
              (should-error (ai-code-codex-cli-switch-to-buffer)
                            :type 'user-error))
            (with-current-buffer linked-session
              (setq-local ai-code-backends-infra--session-directory
                          (ai-code-backends-infra--normalize-session-directory
                           linked-root)))
            (ai-code-backends-infra--remember-file-session-buffer
             "codex" linked-source linked-session)
            (should-not
             (equal
              (ai-code-backends-infra--file-session-map-key
               "codex" main-source)
              (ai-code-backends-infra--file-session-map-key
               "codex" linked-source)))
            (with-current-buffer main-source
              (ai-code-codex-cli-switch-to-buffer))
            (with-current-buffer linked-source
              (ai-code-codex-cli-switch-to-buffer)))
          (should (equal (nreverse displayed)
                         (list main-session linked-session))))
      (clrhash ai-code-backends-infra--directory-buffer-map)
      (clrhash ai-code-backends-infra--file-session-map)
      (dolist (buffer (list main-source linked-source main-session linked-session))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (ignore-errors (delete-directory linked-root t))
      (ignore-errors (delete-directory main-root t)))))

(ert-deftest ai-code-test-codex-cli-start-injects-session-mcp-config ()
  "Starting Codex should inject an Emacs MCP server URL and lifecycle hooks."
  (should (fboundp 'ai-code-codex-cli))
  (let ((captured-command nil)
        (captured-cleanup-fn nil)
        (captured-post-start-fn nil)
        (captured-env-vars nil)
        (registered nil)
        (attached nil)
        (unregistered nil)
        (builtins-called nil)
        (ensure-called nil)
        (session-buffer (generate-new-buffer " *ai-code-codex-mcp*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
                   (lambda () "/tmp/test-codex"))
                  ((symbol-function 'ai-code-backends-infra--resolve-start-command)
                   (lambda (&rest _args)
                     (list :command "codex --full-auto"
                           :argv '("codex" "--full-auto"))))
                  ((symbol-function 'ai-code-mcp-builtins-setup)
                   (lambda () (setq builtins-called t)))
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda ()
                     (setq ensure-called t)
                     8765))
                  ((symbol-function 'ai-code-mcp--random-secret)
                   (lambda () "test-codex-token"))
                  ((symbol-function 'ai-code-mcp-register-session)
                   (lambda (session-id project-dir buffer metadata)
                     (setq registered
                           (list session-id project-dir buffer metadata))))
                  ((symbol-function 'ai-code-mcp-attach-agent-buffer)
                   (lambda (session-id buffer)
                     (setq attached (list session-id buffer))))
                  ((symbol-function 'ai-code-mcp-unregister-session)
                   (lambda (session-id)
                     (setq unregistered session-id)))
                  ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
                   (lambda (&rest args)
                     (cl-destructuring-bind
                         (_working-dir _buffer-name _process-table command
                                       &optional _escape-fn cleanup-fn
                                       _instance-name _prefix _force-prompt
                                       env-vars _multiline-input-sequence
                                       post-start-fn)
                         args
                       (setq captured-command command)
                       (setq captured-cleanup-fn cleanup-fn)
                       (setq captured-post-start-fn post-start-fn)
                       (setq captured-env-vars env-vars))
                     nil)))
          (ai-code-codex-cli)
          (should builtins-called)
          (should ensure-called)
          (let ((config-args (member "-c" captured-command)))
            (should config-args)
            (should
             (string-match-p
              "mcp_servers\\.emacs_tools"
              (cadr config-args)))
            (should
             (string-match-p
              "bearer_token_env_var"
              (cadr config-args))))
          (should-not (string-match-p "test-codex-token"
                                      (cadr (member "-c" captured-command))))
          (should (equal captured-env-vars
                         '("AI_CODE_MCP_BEARER_TOKEN=test-codex-token")))
          (should (functionp captured-cleanup-fn))
          (should (functionp captured-post-start-fn))
          (funcall captured-post-start-fn session-buffer nil "default")
          (should (equal "/tmp/test-codex" (nth 1 registered)))
          (should-not (eq session-buffer (nth 2 registered)))
          (should (equal "test-codex-token"
                         (plist-get (nth 3 registered) :token)))
          (should (equal (list (car registered) session-buffer) attached))
          (with-current-buffer session-buffer
            (should (fboundp 'ai-code-mcp-agent-buffer-status))
            (let ((status (ai-code-mcp-agent-buffer-status)))
              (should (eq 'codex (plist-get status :backend)))
              (should (equal (format "http://127.0.0.1:8765/mcp/%s"
                                     (car registered))
                             (plist-get status :server-url)))))
          (funcall captured-cleanup-fn)
          (should (equal (car registered) unregistered)))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(provide 'test_ai-code-codex-cli)

;;; test_ai-code-codex-cli.el ends here
