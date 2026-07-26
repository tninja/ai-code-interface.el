;;; test_ai-code-mcp-agent.el --- Tests for ai-code-mcp-agent -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-mcp-agent module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-mcp-agent)

(ert-deftest ai-code-test-mcp-agent-show-buffer-status-displays-help-buffer ()
  "The interactive MCP status command should display current buffer status."
  (should (commandp 'ai-code-mcp-agent-show-buffer-status))
  (let ((source-buffer (generate-new-buffer " *ai-code-mcp-status-source*"))
        (status-buffer-name "*AI Code MCP Status*"))
    (unwind-protect
        (with-current-buffer source-buffer
          (setq-local ai-code-mcp-agent--backend 'codex
                      ai-code-mcp-agent--session-id "codex-session-1"
                      ai-code-mcp-agent--server-url "http://127.0.0.1:8765/mcp/codex-session-1")
          (save-window-excursion
            (ai-code-mcp-agent-show-buffer-status))
          (with-current-buffer status-buffer-name
            (should (string-match-p "codex" (buffer-string)))
            (should (string-match-p "codex-session-1" (buffer-string)))
            (should (string-match-p "127\\.0\\.0\\.1:8765" (buffer-string)))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (when (buffer-live-p (get-buffer status-buffer-name))
        (kill-buffer status-buffer-name)))))

(ert-deftest ai-code-test-mcp-agent-show-buffer-status-reports-missing-session ()
  "The interactive MCP status command should report missing session state."
  (let ((source-buffer (generate-new-buffer " *ai-code-mcp-status-empty*"))
        (captured-message nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq captured-message (apply #'format format-string args)))))
          (with-current-buffer source-buffer
            (ai-code-mcp-agent-show-buffer-status))
          (should (equal "No MCP session is attached to the current buffer."
                         captured-message)))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-agent-claude-cleanup-removes-temp-config ()
  "Claude launch cleanup should remove its temporary MCP config file."
  (let ((ai-code-mcp-agent-enabled-backends '(claude-code))
        launch
        config-file)
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure) (lambda () 8765))
              ((symbol-function 'ai-code-mcp-agent--make-session-id)
               (lambda (_backend) "claude-code-test-session"))
              ((symbol-function 'ai-code-mcp-unregister-session) #'ignore))
      (setq launch
            (ai-code-mcp-agent-prepare-launch
             'claude-code default-directory "claude"))
      (let* ((args (split-string-shell-command (plist-get launch :command)))
             (config-flag (member "--mcp-config" args)))
        (should config-flag)
        (setq config-file (cadr config-flag)))
      (unwind-protect
          (progn
            (should (stringp config-file))
            (should (file-exists-p config-file))
            (funcall (plist-get launch :cleanup-fn))
            (should-not (file-exists-p config-file)))
        (when (and config-file (file-exists-p config-file))
          (delete-file config-file))))))

(ert-deftest ai-code-test-mcp-agent-cleanup-is-idempotent ()
  "MCP launch cleanup should release each resource only once."
  (let ((ai-code-mcp-agent-enabled-backends '(claude-code))
        (unregister-count 0)
        launch
        config-file)
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure) (lambda () 8765))
              ((symbol-function 'ai-code-mcp-agent--make-session-id)
               (lambda (_backend) "claude-code-test-session"))
              ((symbol-function 'ai-code-mcp-unregister-session)
               (lambda (_session-id)
                 (cl-incf unregister-count))))
      (setq launch
            (ai-code-mcp-agent-prepare-launch
             'claude-code default-directory "claude"))
      (let* ((args (split-string-shell-command (plist-get launch :command)))
             (config-flag (member "--mcp-config" args)))
        (setq config-file (cadr config-flag)))
      (unwind-protect
          (let ((cleanup-fn (plist-get launch :cleanup-fn)))
            (funcall cleanup-fn)
            (funcall cleanup-fn)
            (should (= unregister-count 1))
            (should-not (file-exists-p config-file)))
        (when (and config-file (file-exists-p config-file))
          (delete-file config-file))))))

(ert-deftest test-ai-code-mcp-agent--prepare-launch-cleanup-retries-runtime-file-deletion ()
  "Cleanup should retry runtime files after a transient deletion failure."
  (let ((ai-code-mcp-agent-enabled-backends '(claude-code))
        (delete-attempts 0)
        (unregister-count 0)
        (real-delete-file (symbol-function 'delete-file))
        launch
        config-file)
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure) (lambda () 8765))
              ((symbol-function 'ai-code-mcp-agent--make-session-id)
               (lambda (_backend) "claude-code-retry-session"))
              ((symbol-function 'ai-code-mcp-unregister-session)
               (lambda (_session-id)
                 (cl-incf unregister-count))))
      (setq launch
            (ai-code-mcp-agent-prepare-launch
             'claude-code default-directory "claude"))
      (let* ((args (split-string-shell-command (plist-get launch :command)))
             (config-flag (member "--mcp-config" args)))
        (should config-flag)
        (setq config-file (cadr config-flag)))
      (unwind-protect
          (cl-letf (((symbol-function 'delete-file)
                     (lambda (path &optional trash)
                       (cl-incf delete-attempts)
                       (if (= delete-attempts 1)
                           (signal 'file-error
                                   (list "Transient deletion failure" path))
                         (funcall real-delete-file path trash))))
                    ((symbol-function 'display-warning) #'ignore))
            (let ((cleanup-fn (plist-get launch :cleanup-fn)))
              (funcall cleanup-fn)
              (should (file-exists-p config-file))
              (should (= unregister-count 1))
              (funcall cleanup-fn)
              (should-not (file-exists-p config-file))
              (should (= delete-attempts 2))
              (should (= unregister-count 1))))
        (when (and config-file (file-exists-p config-file))
          (funcall real-delete-file config-file))))))

(provide 'test_ai-code-mcp-agent)

;;; test_ai-code-mcp-agent.el ends here
