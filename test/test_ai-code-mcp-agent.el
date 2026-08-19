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

(ert-deftest ai-code-test-mcp-agent-launch-url-matches-http-endpoint ()
  "Launch metadata should use the exact path accepted by the HTTP server."
  (let ((ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        launch)
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda () 8765))
              ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore)
              ((symbol-function 'ai-code-mcp-agent--make-session-id)
               (lambda (_backend) "codex-test-session")))
      (unwind-protect
          (progn
            (setq launch
                  (ai-code-mcp-agent-prepare-launch
                   'codex default-directory '("codex")))
            (should (equal "http://127.0.0.1:8765/mcp"
                           (plist-get launch :mcp-server-url)))
            (should (equal
                     "mcp_servers.emacs_tools={ url = \"http://127.0.0.1:8765/mcp\", bearer_token_env_var = \"AI_CODE_MCP_BEARER_TOKEN\" }"
                     (car (last (plist-get launch :argv))))))
        (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
          (funcall cleanup-fn))))))

(ert-deftest ai-code-test-mcp-agent-prepare-launch-registers-source-before-process-start ()
  "Preparing a launch should register its source before the CLI starts."
  (let ((ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-server-tools nil)
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (project-dir (make-temp-file "ai-code-mcp-launch-" t))
        (source-buffer (generate-new-buffer " *ai-code-mcp-launch-source*"))
        launch)
    (unwind-protect
        (progn
          (with-current-buffer source-buffer
            (setq-local default-directory project-dir)
            (setq launch
                  (ai-code-mcp-agent-prepare-launch
                   'codex project-dir '("codex"))))
          (let* ((session-id (plist-get launch :mcp-session-id))
                 (context (and session-id
                               (ai-code-mcp-get-session-context session-id))))
            (should (stringp session-id))
            (should (eq 'pending (plist-get context :state)))
            (should (eq source-buffer (plist-get context :source-buffer)))
            (should-not (plist-get context :agent-buffer))
            (should (equal (file-name-as-directory project-dir)
                           (file-name-as-directory
                            (plist-get context :project-dir))))
            (should (equal
                     (format "http://127.0.0.1:%d/mcp"
                             ai-code-mcp-http-server--port)
                     (plist-get launch :mcp-server-url)))))
      (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
        (funcall cleanup-fn))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-agent-post-start-attaches-agent-without-replacing-source ()
  "Post-start should attach the agent while preserving prompt origin."
  (let ((ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-server-tools nil)
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (project-dir (make-temp-file "ai-code-mcp-attach-" t))
        (source-buffer (generate-new-buffer " *ai-code-mcp-attach-source*"))
        (agent-buffer (generate-new-buffer " *ai-code-mcp-attach-agent*"))
        launch)
    (unwind-protect
        (progn
          (with-current-buffer source-buffer
            (setq launch
                  (ai-code-mcp-agent-prepare-launch
                   'codex project-dir '("codex"))))
          (funcall (plist-get launch :post-start-fn)
                   agent-buffer nil "default")
          (let* ((session-id (plist-get launch :mcp-session-id))
                 (context (ai-code-mcp-get-session-context session-id))
                 (status (ai-code-mcp-agent-buffer-status agent-buffer)))
            (should (eq source-buffer (plist-get context :source-buffer)))
            (should (eq agent-buffer (plist-get context :agent-buffer)))
            (should (eq 'ready (plist-get context :state)))
            (should (equal session-id (plist-get status :session-id)))))
      (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
        (funcall cleanup-fn))
      (ai-code-mcp-http-server-stop)
      (dolist (buffer (list source-buffer agent-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-agent-backend-configs-reference-secret-environment ()
  "Backend launch configs should authenticate without exposing the token."
  (let ((ai-code-mcp-agent-enabled-backends
         '(codex open-interpreter github-copilot-cli claude-code))
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (secret (make-string 64 ?a))
        (source-buffer (generate-new-buffer " *ai-code-mcp-secret-source*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp--random-secret)
                   (lambda () secret))
                  ((symbol-function 'ai-code-mcp-agent--make-session-id)
                   (lambda (backend) (format "%s-test-session" backend))))
          (dolist (backend ai-code-mcp-agent-enabled-backends)
            (let* ((launch
                    (with-current-buffer source-buffer
                      (ai-code-mcp-agent-prepare-launch
                       backend default-directory (list (symbol-name backend)))))
                   (argv (plist-get launch :argv))
                   (session-id (plist-get launch :mcp-session-id))
                   (context (ai-code-mcp-get-session-context session-id))
                   (env-vars (plist-get launch :env-vars)))
              (unwind-protect
                  (progn
                    (should (equal secret (plist-get context :token)))
                    (should (plist-get context :expires-at))
                    (should (equal
                             (list (concat "AI_CODE_MCP_BEARER_TOKEN=" secret))
                             env-vars))
                    (should-not (member (regexp-quote secret) argv))
                    (pcase backend
                      ((or 'codex 'open-interpreter)
                       (let ((flag (member "-c" argv)))
                         (should flag)
                         (should (string-match-p
                                  "bearer_token_env_var[ ]*=[ ]*\\\"AI_CODE_MCP_BEARER_TOKEN\\\""
                                  (cadr flag)))))
                      ('github-copilot-cli
                       (let* ((flag (member "--additional-mcp-config" argv))
                              (config (json-parse-string
                                       (cadr flag) :object-type 'alist))
                              (server (alist-get
                                       'emacs_tools
                                       (alist-get 'mcpServers config))))
                         (should flag)
                         (should (equal
                                  "Bearer ${AI_CODE_MCP_BEARER_TOKEN}"
                                  (alist-get 'Authorization
                                             (alist-get 'headers server))))))
                      ('claude-code
                       (let* ((flag (member "--mcp-config" argv))
                              (config-file (cadr flag))
                              (config
                               (with-temp-buffer
                                 (insert-file-contents config-file)
                                 (json-parse-buffer :object-type 'alist)))
                              (server (alist-get
                                       'emacs_tools
                                       (alist-get 'mcpServers config))))
                         (should flag)
                         (should (equal
                                  "Bearer ${AI_CODE_MCP_BEARER_TOKEN}"
                                  (alist-get 'Authorization
                                             (alist-get 'headers server)))))))
                    (should (equal "http://127.0.0.1:8765/mcp"
                                   (plist-get launch :mcp-server-url))))
                (funcall (plist-get launch :cleanup-fn))))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-agent-show-buffer-status-displays-help-buffer ()
  "The interactive MCP status command should display current buffer status."
  (should (commandp 'ai-code-mcp-agent-show-buffer-status))
  (let ((source-buffer (generate-new-buffer " *ai-code-mcp-status-source*"))
        (status-buffer-name "*AI Code MCP Status*"))
    (unwind-protect
        (with-current-buffer source-buffer
          (setq-local ai-code-mcp-agent--backend 'codex
                      ai-code-mcp-agent--session-id "codex-session-1"
                      ai-code-mcp-agent--server-url "http://127.0.0.1:8765/mcp")
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

(ert-deftest ai-code-test-mcp-agent-claude-launch-preserves-argv-boundaries ()
  "Claude MCP launch metadata should preserve every argument verbatim."
  (let ((ai-code-mcp-agent-enabled-backends '(claude-code))
        (config-file
         "c:/Users/Test User/AppData/Local/Temp/ai-code-mcp-claude-code.json"))
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda () 8765))
              ((symbol-function 'ai-code-mcp-unregister-session) #'ignore)
              ((symbol-function 'make-temp-file)
               (lambda (&rest _args) config-file))
              ((symbol-function 'write-region)
               (lambda (&rest _args) nil)))
      (let ((launch
             (ai-code-mcp-agent-prepare-launch
              'claude-code
              default-directory
              '("claude" "--model" "model with spaces"))))
        (unwind-protect
            (should
             (equal
              (plist-get launch :argv)
              (list "claude"
                    "--model"
                    "model with spaces"
                    "--mcp-config"
                    config-file)))
          (funcall (plist-get launch :cleanup-fn)))))))

(ert-deftest ai-code-test-mcp-agent-copilot-launch-preserves-inline-config-argument ()
  "Copilot MCP launch metadata should keep inline JSON in one argument."
  (let ((ai-code-mcp-agent-enabled-backends '(github-copilot-cli)))
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda () 8765))
              ((symbol-function 'ai-code-mcp-unregister-session) #'ignore)
              ((symbol-function 'format-time-string)
               (lambda (&rest _args) "20260730223000"))
              ((symbol-function 'random)
               (lambda (&rest _args) 42)))
      (let ((launch
             (ai-code-mcp-agent-prepare-launch
              'github-copilot-cli
              default-directory
              '("copilot" "--banner" "value with spaces"))))
        (unwind-protect
            (should
             (equal
              (plist-get launch :argv)
              '("copilot"
                "--banner"
                "value with spaces"
                "--additional-mcp-config"
                "{\"mcpServers\":{\"emacs_tools\":{\"type\":\"http\",\"url\":\"http://127.0.0.1:8765/mcp\",\"headers\":{\"Authorization\":\"Bearer ${AI_CODE_MCP_BEARER_TOKEN}\"},\"tools\":[\"*\"]}}}")))
          (funcall (plist-get launch :cleanup-fn)))))))

(ert-deftest ai-code-test-mcp-agent-codex-launch-preserves-config-override-argument ()
  "Codex MCP launch metadata should keep its config override in one argument."
  (let ((ai-code-mcp-agent-enabled-backends '(codex)))
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda () 8765))
              ((symbol-function 'ai-code-mcp-unregister-session) #'ignore)
              ((symbol-function 'format-time-string)
               (lambda (&rest _args) "20260730223000"))
              ((symbol-function 'random)
               (lambda (&rest _args) 42)))
      (let ((launch
             (ai-code-mcp-agent-prepare-launch
              'codex
              default-directory
              '("codex" "--profile" "work profile"))))
        (unwind-protect
            (should
             (equal
              (plist-get launch :argv)
              '("codex"
                "--profile"
                "work profile"
                "-c"
                "mcp_servers.emacs_tools={ url = \"http://127.0.0.1:8765/mcp\", bearer_token_env_var = \"AI_CODE_MCP_BEARER_TOKEN\" }")))
          (funcall (plist-get launch :cleanup-fn)))))))

(ert-deftest ai-code-test-mcp-agent-open-interpreter-preserves-config-override-argument ()
  "Open Interpreter launch metadata should keep config in one argument."
  (let ((ai-code-mcp-agent-enabled-backends '(open-interpreter)))
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda () 8765))
              ((symbol-function 'ai-code-mcp-unregister-session) #'ignore)
              ((symbol-function 'format-time-string)
               (lambda (&rest _args) "20260730223000"))
              ((symbol-function 'random)
               (lambda (&rest _args) 42)))
      (let ((launch
             (ai-code-mcp-agent-prepare-launch
              'open-interpreter
              default-directory
              '("interpreter" "--model" "provider/model name"))))
        (unwind-protect
            (should
             (equal
              (plist-get launch :argv)
              '("interpreter"
                "--model"
                "provider/model name"
                "-c"
                "mcp_servers.emacs_tools={ url = \"http://127.0.0.1:8765/mcp\", bearer_token_env_var = \"AI_CODE_MCP_BEARER_TOKEN\" }")))
          (funcall (plist-get launch :cleanup-fn)))))))

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
             'claude-code default-directory '("claude")))
      (let ((config-flag (member "--mcp-config" (plist-get launch :argv))))
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
             'claude-code default-directory '("claude")))
      (let ((config-flag (member "--mcp-config" (plist-get launch :argv))))
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
             'claude-code default-directory '("claude")))
      (let ((config-flag (member "--mcp-config" (plist-get launch :argv))))
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
