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

(defun ai-code-test-mcp-agent--file-contents (path)
  "Return the literal contents of PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ai-code-test-mcp-agent--json-file (path)
  "Parse the JSON object stored at PATH as an alist."
  (json-parse-string
   (ai-code-test-mcp-agent--file-contents path)
   :object-type 'alist
   :array-type 'list
   :null-object :null
   :false-object :json-false))

(ert-deftest ai-code-test-mcp-agent-enables-antigravity-by-default ()
  "Antigravity should receive automatic Emacs MCP integration by default."
  (should (memq 'antigravity ai-code-mcp-agent-enabled-backends)))

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

(ert-deftest ai-code-test-mcp-agent-prompt-renews-idle-launch-token ()
  "A new local prompt should restore an idle launch using its own lifetime."
  (let ((ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-mcp-agent-token-lifetime-seconds 3600)
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (now (seconds-to-time 1000000))
        launch)
    (with-temp-buffer
      (let ((source-buffer (current-buffer)))
        (with-temp-buffer
          (cl-letf (((symbol-function 'current-time) (lambda () now))
                    ((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                    ((symbol-function 'ai-code-mcp-http-server-live-p) #'ignore)
                    ((symbol-function 'ai-code-mcp-http-server-ensure)
                     (lambda () 8765))
                    ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore))
            (unwind-protect
                (progn
                  (setq launch
                        (with-current-buffer source-buffer
                          (ai-code-mcp-agent-prepare-launch
                           'codex default-directory '("codex"))))
                  (funcall (plist-get launch :post-start-fn)
                           (current-buffer) nil "default")
                  (let* ((id (plist-get launch :mcp-session-id))
                         (token (plist-get (ai-code-mcp-get-session-context id)
                                           :token)))
                    (setq now (time-add now (* 3 86400)))
                    (should-not (ai-code-mcp-find-session-by-token token))
                    ;; Later customization must not change this launch's lease.
                    (let ((ai-code-mcp-agent-token-lifetime-seconds 7200))
                      (ai-code-mcp-agent-refresh-source-context
                       (current-buffer) source-buffer))
                    (should (equal id
                                   (car (ai-code-mcp-find-session-by-token token))))
                    (should (time-equal-p
                             (time-add now 3600)
                             (plist-get (ai-code-mcp-get-session-context id)
                                        :expires-at)))
                    (funcall (plist-get launch :cleanup-fn))
                    (should-not (ai-code-mcp-find-session-by-token token))
                    (should-error
                     (ai-code-mcp-agent-refresh-source-context
                      (current-buffer) source-buffer))
                    (should (zerop (ai-code-mcp-session-count)))))
              (when-let ((cleanup (plist-get launch :cleanup-fn)))
                (funcall cleanup)))))))))

(ert-deftest ai-code-test-mcp-agent-terminal-interactions-renew-idle-token-preserving-origin ()
  "Terminal and viewport Return paths should renew idle tokens without replacing origin."
  (require 'ai-code-backends-infra)
  (let ((ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-mcp-agent-token-lifetime-seconds 3600)
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (now (seconds-to-time 1000000))
        (nav-mode nil)
        launch)
    (with-temp-buffer
      (let ((source-buffer (current-buffer)))
        (with-temp-buffer
          (let ((agent-buffer (current-buffer)))
            (cl-letf (((symbol-function 'current-time) (lambda () now))
                      ((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                      ((symbol-function 'ai-code-mcp-http-server-live-p) #'ignore)
                      ((symbol-function 'ai-code-mcp-http-server-ensure)
                       (lambda () 8765))
                      ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore)
                      ((symbol-function 'ai-code-backends-infra--terminal-dispatch)
                       #'ignore)
                      ((symbol-function 'ai-code-backends-infra--terminal-navigation-mode-p)
                       (lambda () nav-mode)))
              (unwind-protect
                  (progn
                    (setq launch
                          (with-current-buffer source-buffer
                            (ai-code-mcp-agent-prepare-launch
                             'codex default-directory '("codex"))))
                    (funcall (plist-get launch :post-start-fn)
                             agent-buffer nil "default")
                    (let* ((id (plist-get launch :mcp-session-id))
                           (token (plist-get (ai-code-mcp-get-session-context id)
                                             :token)))
                      ;; 1. Viewport/programmatic Return renews expired token and keeps origin.
                      (setq now (time-add now (* 2 86400)))
                      (should-not (ai-code-mcp-find-session-by-token token t))
                      (with-current-buffer agent-buffer
                        (ai-code-backends-infra--terminal-send-return))
                      (should (equal id (car (ai-code-mcp-find-session-by-token token t))))
                      (should (eq source-buffer
                                  (plist-get (ai-code-mcp-get-session-context id)
                                             :source-buffer)))
                      ;; 2. Copy/navigation mode Return and non-Return keys do not revive.
                      (setq now (time-add now (* 2 86400)))
                      (should-not (ai-code-mcp-find-session-by-token token t))
                      (setq nav-mode t)
                      (with-current-buffer agent-buffer
                        (cl-letf (((symbol-function 'this-command-keys-vector)
                                   (lambda () [return])))
                          (run-hooks 'pre-command-hook)))
                      (should-not (ai-code-mcp-find-session-by-token token t))
                      (setq nav-mode nil)
                      (with-current-buffer agent-buffer
                        (cl-letf (((symbol-function 'this-command-keys-vector)
                                   (lambda () [?a])))
                          (run-hooks 'pre-command-hook)))
                      (should-not (ai-code-mcp-find-session-by-token token t))
                      ;; 3. Live interactive Return in terminal buffer revives expired token.
                      (with-current-buffer agent-buffer
                        (cl-letf (((symbol-function 'this-command-keys-vector)
                                   (lambda () [?\r])))
                          (run-hooks 'pre-command-hook)))
                      (should (equal id (car (ai-code-mcp-find-session-by-token token t))))
                      ;; 4. Unregistered session is never revived by terminal Return.
                      (funcall (plist-get launch :cleanup-fn))
                      (with-current-buffer agent-buffer
                        (ai-code-backends-infra--terminal-send-return))
                      (should-not (ai-code-mcp-find-session-by-token token t))
                      (should (zerop (ai-code-mcp-session-count)))))
                (when-let* ((cleanup (plist-get launch :cleanup-fn)))
                  (funcall cleanup))))))))))

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

(defun ai-code-test-mcp-agent--antigravity-add-dir (launch)
  "Return the Antigravity `--add-dir' path from LAUNCH argv."
  (cadr (member "--add-dir" (plist-get launch :argv))))

(ert-deftest ai-code-test-mcp-agent-antigravity-writes-private-temp-add-dir-config ()
  "Antigravity should use a private temp `--add-dir' without touching workspace."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--active-cleanups (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-ws-" t))
         (workspace-agents (expand-file-name ".agents" working-dir))
         (secret (make-string 64 ?b))
         launch
         config-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore)
                  ((symbol-function 'ai-code-mcp--random-secret)
                   (lambda () secret))
                  ((symbol-function 'ai-code-mcp-agent--make-session-id)
                   (lambda (_backend) "antigravity-test-session")))
          (setq launch
                (ai-code-mcp-agent-prepare-launch
                 'antigravity working-dir '("agy" "--continue")))
          (setq config-dir (ai-code-test-mcp-agent--antigravity-add-dir launch))
          (should (stringp config-dir))
          (should (equal (list "agy" "--continue" "--add-dir" config-dir)
                         (plist-get launch :argv)))
          (should-not (file-exists-p workspace-agents))
          (should (file-directory-p config-dir))
          (should (zerop (logand (file-modes config-dir) #o077)))
          (let* ((agents-dir (expand-file-name ".agents" config-dir))
                 (config-file (expand-file-name "mcp_config.json" agents-dir))
                 (config (ai-code-test-mcp-agent--json-file config-file))
                 (server (alist-get
                          'emacs_tools
                          (alist-get 'mcpServers config))))
            (should (file-directory-p agents-dir))
            (should (zerop (logand (file-modes agents-dir) #o077)))
            (should (file-regular-p config-file))
            (should (zerop (logand (file-modes config-file) #o077)))
            (should (equal "http://127.0.0.1:8765/mcp"
                           (alist-get 'serverUrl server)))
            (should (equal (concat "Bearer " secret)
                           (alist-get 'Authorization
                                      (alist-get 'headers server))))
            (should-not
             (string-match-p
              (regexp-quote ai-code-mcp-agent--token-environment-variable)
              (ai-code-test-mcp-agent--file-contents config-file))))
          (let ((context
                 (ai-code-mcp-get-session-context
                  (plist-get launch :mcp-session-id))))
            (should (ai-code-mcp-http-server--modern-protocol-enabled-p
                     context)))
          (funcall (plist-get launch :cleanup-fn))
          (funcall (plist-get launch :cleanup-fn))
          (setq launch nil)
          (should-not (file-exists-p config-dir))
          (should (zerop (hash-table-count ai-code-mcp-agent--active-cleanups))))
      (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
        (funcall cleanup-fn))
      (when (and config-dir (file-directory-p config-dir))
        (delete-directory config-dir t))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-multiple-instances-are-independent ()
  "Concurrent Antigravity launches should get isolated dirs, tokens, and sessions."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--active-cleanups (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-multi-" t))
         (workspace-config-dir (expand-file-name ".agents" working-dir))
         (workspace-config-file
          (expand-file-name "mcp_config.json" workspace-config-dir))
         (original-workspace-config
          "{\"mcpServers\":{\"user_server\":{\"command\":\"keep-me\"}}}\n")
         (source-a (generate-new-buffer " *ai-code-agy-source-a*"))
         (source-b (generate-new-buffer " *ai-code-agy-source-b*"))
         (session-ids '("antigravity-session-1" "antigravity-session-2"))
         (secrets (list (make-string 64 ?d) (make-string 64 ?e)))
         first-launch
         second-launch)
    (make-directory workspace-config-dir t)
    (with-temp-file workspace-config-file
      (insert original-workspace-config))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore)
                  ((symbol-function 'ai-code-mcp--random-secret)
                   (lambda () (pop secrets)))
                  ((symbol-function 'ai-code-mcp-agent--make-session-id)
                   (lambda (_backend) (pop session-ids))))
          (setq first-launch
                (with-current-buffer source-a
                  (ai-code-mcp-agent-prepare-launch
                   'antigravity working-dir '("agy"))))
          (setq second-launch
                (with-current-buffer source-b
                  (ai-code-mcp-agent-prepare-launch
                   'antigravity working-dir '("agy"))))
          (let* ((dir-1 (ai-code-test-mcp-agent--antigravity-add-dir first-launch))
                 (dir-2 (ai-code-test-mcp-agent--antigravity-add-dir second-launch))
                 (cfg-1 (expand-file-name ".agents/mcp_config.json" dir-1))
                 (cfg-2 (expand-file-name ".agents/mcp_config.json" dir-2)))
            (should-not (equal dir-1 dir-2))
            (should (equal original-workspace-config
                           (ai-code-test-mcp-agent--file-contents
                            workspace-config-file)))
            (should (string-match-p
                     (regexp-quote (concat "Bearer " (make-string 64 ?d)))
                     (ai-code-test-mcp-agent--file-contents cfg-1)))
            (should (string-match-p
                     (regexp-quote (concat "Bearer " (make-string 64 ?e)))
                     (ai-code-test-mcp-agent--file-contents cfg-2)))
            (should (eq source-a
                        (plist-get (ai-code-mcp-get-session-context
                                    "antigravity-session-1")
                                   :source-buffer)))
            (should (eq source-b
                        (plist-get (ai-code-mcp-get-session-context
                                    "antigravity-session-2")
                                   :source-buffer)))
            ;; Closing instance 1 must leave instance 2 intact.
            (funcall (plist-get first-launch :cleanup-fn))
            (setq first-launch nil)
            (should-not (file-exists-p dir-1))
            (should-not (ai-code-mcp-get-session-context "antigravity-session-1"))
            (should (file-exists-p cfg-2))
            (should (ai-code-mcp-get-session-context "antigravity-session-2"))
            (funcall (plist-get second-launch :cleanup-fn))
            (setq second-launch nil)
            (should-not (file-exists-p dir-2))
            (should-not (ai-code-mcp-get-session-context "antigravity-session-2"))
            (should (equal original-workspace-config
                           (ai-code-test-mcp-agent--file-contents
                            workspace-config-file)))))
      (when-let ((cleanup-fn (plist-get first-launch :cleanup-fn)))
        (funcall cleanup-fn))
      (when-let ((cleanup-fn (plist-get second-launch :cleanup-fn)))
        (funcall cleanup-fn))
      (dolist (buf (list source-a source-b))
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-rolls-back-failed-registration ()
  "A failed Antigravity registration should remove its temp dir and stop server."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--active-cleanups (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp-http-server--server nil)
         (ai-code-mcp-http-server--port nil)
         (working-dir (make-temp-file "ai-code-mcp-antigravity-rollback-" t))
         (real-config-dir-fn (symbol-function 'ai-code-mcp-agent--antigravity-config-dir))
         (created-dir nil)
         (stop-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop)
                   (lambda () (cl-incf stop-count)))
                  ((symbol-function 'ai-code-mcp-agent--antigravity-config-dir)
                   (lambda (url token)
                     (setq created-dir (funcall real-config-dir-fn url token))))
                  ((symbol-function 'ai-code-mcp-register-session)
                   (lambda (&rest _args) (error "Registration failed"))))
          (should-error
           (ai-code-mcp-agent-prepare-launch
            'antigravity working-dir '("agy"))
           :type 'error)
          (should (stringp created-dir))
          (should-not (file-exists-p created-dir))
          (should (zerop (hash-table-count ai-code-mcp-agent--active-cleanups)))
          (should (= 1 stop-count)))
      (when (and created-dir (file-directory-p created-dir))
        (delete-directory created-dir t))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-kill-emacs-hook-cleans-active-sessions ()
  "The `kill-emacs-hook' handler should clean up active Antigravity temp dirs."
  (should (memq #'ai-code-mcp-agent--cleanup-all-sessions kill-emacs-hook))
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--active-cleanups (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-exit-" t))
         launch
         config-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore))
          (setq launch
                (ai-code-mcp-agent-prepare-launch
                 'antigravity working-dir '("agy")))
          (setq config-dir (ai-code-test-mcp-agent--antigravity-add-dir launch))
          (should (file-directory-p config-dir))
          (should (= 1 (hash-table-count ai-code-mcp-agent--active-cleanups)))
          (ai-code-mcp-agent--cleanup-all-sessions)
          (should-not (file-exists-p config-dir))
          (should (zerop (ai-code-mcp-session-count)))
          (should (zerop (hash-table-count ai-code-mcp-agent--active-cleanups))))
      (when (and config-dir (file-directory-p config-dir))
        (delete-directory config-dir t))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-skips-remote-working-directory ()
  "Preparing MCP launch in a remote TRAMP directory should skip injection."
  (let ((ai-code-mcp-agent-enabled-backends '(antigravity))
        (ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ensure-called nil))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (path &rest _args)
                 (string-prefix-p "/ssh:" path)))
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda ()
                 (setq ensure-called t)
                 8765)))
      (should-not
       (ai-code-mcp-agent-prepare-launch
        'antigravity "/ssh:example:/repo/" '("agy")))
      (should-not ensure-called)
      (should (zerop (ai-code-mcp-session-count))))))

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

;;; Pi MCP integration

(defun ai-code-test-mcp-agent--pi-launch-file (launch)
  "Return the Pi extension file path from LAUNCH argv."
  (cadr (member "--extension" (plist-get launch :argv))))

(defun ai-code-test-mcp-agent--file-string (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(ert-deftest ai-code-test-mcp-agent-pi-enabled-by-default ()
  "Pi should receive automatic MCP integration by default."
  (should (memq 'pi (default-value 'ai-code-mcp-agent-enabled-backends))))

(ert-deftest ai-code-test-mcp-agent-pi-extension-source-reads-token-from-env ()
  "Pi extension should read token from env and expose codemode plus anchors."
  (let ((source (ai-code-mcp-agent--pi-extension-source
                 "http://127.0.0.1:8765/mcp" "AI_CODE_MCP_BEARER_TOKEN")))
    (should (string-match-p "registerMcpServer(\"emacs_tools\"" source))
    (should (string-match-p "url: \"http://127.0.0.1:8765/mcp\"" source))
    (should (string-match-p
             "process.env\\[\"AI_CODE_MCP_BEARER_TOKEN\"\\]" source))
    (should (string-match-p "exposure: \"codemode\"" source))
    (should (string-match-p "\"diagnostics_baseline\" *: *\"direct\"" source))
    (should (string-match-p "\"get_diagnostics\" *: *\"direct\"" source))
    (should (string-match-p "\"get_org_heading_context\" *: *\"direct\"" source))
    (should (string-match-p "typeof pi.registerMcpServer" source))))

(ert-deftest ai-code-test-mcp-agent-pi-extension-source-respects-custom-exposure ()
  "Pi extension should honor custom exposure and omit empty toolExposure."
  (let* ((ai-code-mcp-agent-pi-exposure "direct")
         (ai-code-mcp-agent-pi-tool-exposure nil)
         (source (ai-code-mcp-agent--pi-extension-source
                  "http://127.0.0.1:8765/mcp" "AI_CODE_MCP_BEARER_TOKEN")))
    (should (string-match-p "exposure: \"direct\"" source))
    (should-not (string-match-p "toolExposure" source))))

(ert-deftest ai-code-test-mcp-agent-pi-extension-source-rejects-invalid-exposure ()
  "Pi extension should fail closed on invalid exposures and serialize valid ones."
  (dolist (valid '("codemode" "deferred" "direct" "hidden"))
    (let* ((ai-code-mcp-agent-pi-exposure valid)
           (ai-code-mcp-agent-pi-tool-exposure
            (list (cons "get_*" valid)
                  (cons "buffer_query" "codemode")))
           (source (ai-code-mcp-agent--pi-extension-source
                    "http://127.0.0.1:8765/mcp" "AI_CODE_MCP_BEARER_TOKEN")))
      (should (string-match-p (format "exposure: %S" valid) source))
      (should (string-match-p (format "\"get_\\*\" *: *%S" valid) source))
      (should (string-match-p "\"buffer_query\" *: *\"codemode\"" source))))
  (let ((ai-code-mcp-agent-pi-exposure "bogus"))
    (should-error
     (ai-code-mcp-agent--pi-extension-source
      "http://127.0.0.1:8765/mcp" "AI_CODE_MCP_BEARER_TOKEN")))
  (let ((ai-code-mcp-agent-pi-tool-exposure '(("get_diagnostics" . "bogus"))))
    (should-error
     (ai-code-mcp-agent--pi-extension-source
      "http://127.0.0.1:8765/mcp" "AI_CODE_MCP_BEARER_TOKEN")))
  (let ((ai-code-mcp-agent-pi-tool-exposure '(("" . "direct"))))
    (should-error
     (ai-code-mcp-agent--pi-extension-source
      "http://127.0.0.1:8765/mcp" "AI_CODE_MCP_BEARER_TOKEN"))))

(ert-deftest ai-code-test-mcp-agent-pi-launch-adds-extension-and-cleans-up ()
  "Pi launch should append a temp extension and remove it on cleanup."
  (let ((ai-code-mcp-agent-enabled-backends '(pi))
        launch extension-file)
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda () 8765))
              ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore))
      (setq launch (ai-code-mcp-agent-prepare-launch
                    'pi default-directory '("pi" "--tui-mode" "regular")))
      (setq extension-file (ai-code-test-mcp-agent--pi-launch-file launch))
      (unwind-protect
          (progn
            (should (equal (plist-get launch :argv)
                           (list "pi" "--tui-mode" "regular"
                                 "--extension" extension-file)))
            (should (file-exists-p extension-file))
            (let ((token (cadr (split-string
                                (car (plist-get launch :env-vars)) "="))))
              (should-not
               (string-match-p
                (regexp-quote token)
                (ai-code-test-mcp-agent--file-string extension-file))))
            (funcall (plist-get launch :cleanup-fn))
            (should-not (file-exists-p extension-file))
            (should-not (ai-code-mcp-get-session-context
                         (plist-get launch :mcp-session-id))))
        (when (file-exists-p extension-file)
          (delete-file extension-file))))))

(ert-deftest ai-code-test-mcp-agent-pi-multiple-instances-are-independent ()
  "Concurrent Pi launches should get isolated sessions, tokens, and files."
  (let ((ai-code-mcp-agent-enabled-backends '(pi))
        first second)
    (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
              ((symbol-function 'ai-code-mcp-http-server-ensure)
               (lambda () 8765))
              ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore))
      (setq first (ai-code-mcp-agent-prepare-launch
                   'pi default-directory '("pi"))
            second (ai-code-mcp-agent-prepare-launch
                    'pi default-directory '("pi")))
      (let ((first-file (ai-code-test-mcp-agent--pi-launch-file first))
            (second-file (ai-code-test-mcp-agent--pi-launch-file second))
            (first-id (plist-get first :mcp-session-id))
            (second-id (plist-get second :mcp-session-id)))
        (unwind-protect
            (progn
              (should-not (equal first-id second-id))
              (should-not (equal first-file second-file))
              (should-not (equal (plist-get first :env-vars)
                                 (plist-get second :env-vars)))
              (should (ai-code-mcp-get-session-context first-id))
              (should (ai-code-mcp-get-session-context second-id))
              ;; Closing one instance must not affect the other.
              (funcall (plist-get first :cleanup-fn))
              (should-not (file-exists-p first-file))
              (should-not (ai-code-mcp-get-session-context first-id))
              (should (file-exists-p second-file))
              (should (ai-code-mcp-get-session-context second-id))
              (funcall (plist-get second :cleanup-fn))
              (should-not (file-exists-p second-file))
              (should-not (ai-code-mcp-get-session-context second-id)))
          (dolist (file (list first-file second-file))
            (when (file-exists-p file)
              (delete-file file)))
          (ai-code-mcp-unregister-session first-id)
          (ai-code-mcp-unregister-session second-id))))))

(provide 'test_ai-code-mcp-agent)

;;; test_ai-code-mcp-agent.el ends here
