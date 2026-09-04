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

(ert-deftest ai-code-test-mcp-agent-antigravity-writes-private-workspace-config ()
  "Antigravity should receive a private workspace config with its bearer token."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--antigravity-config-states
          (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-" t))
         (config-file
          (expand-file-name ".agents/mcp_config.json" working-dir))
         (secret (make-string 64 ?b))
         launch)
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
          (should (equal '("agy" "--continue")
                         (plist-get launch :argv)))
          (should (file-exists-p config-file))
          (should (zerop (logand (file-modes config-file) #o077)))
          (let* ((config (ai-code-test-mcp-agent--json-file config-file))
                 (server (alist-get
                          'emacs_tools
                          (alist-get 'mcpServers config))))
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
            (should (plist-member context :modern-protocol-enabled))
            (should-not (plist-get context :modern-protocol-enabled)))
          (funcall (plist-get launch :cleanup-fn))
          (setq launch nil)
          (should-not (file-exists-p config-file)))
      (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
        (funcall cleanup-fn))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-restores-existing-config ()
  "Antigravity cleanup should restore an existing MCP config byte for byte."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--antigravity-config-states
          (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-existing-" t))
         (config-dir (expand-file-name ".agents" working-dir))
         (config-file (expand-file-name "mcp_config.json" config-dir))
         (original
          (concat
           "{\n"
           "  \"mcpServers\": {\n"
           "    \"user_server\": {\"command\": \"keep-me\"},\n"
           "    \"emacs_tools\": {\"serverUrl\": \"https://original.invalid\"}\n"
           "  },\n"
           "  \"keep\": true\n"
           "}\n"))
         launch)
    (make-directory config-dir)
    (with-temp-file config-file
      (insert original))
    (set-file-modes config-file #o640)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore)
                  ((symbol-function 'ai-code-mcp--random-secret)
                   (lambda () (make-string 64 ?c)))
                  ((symbol-function 'ai-code-mcp-agent--make-session-id)
                   (lambda (_backend) "antigravity-existing-session")))
          (setq launch
                (ai-code-mcp-agent-prepare-launch
                 'antigravity working-dir '("agy")))
          (let* ((config (ai-code-test-mcp-agent--json-file config-file))
                 (servers (alist-get 'mcpServers config)))
            (should (equal "keep-me"
                           (alist-get 'command
                                      (alist-get 'user_server servers))))
            (should (equal t (alist-get 'keep config)))
            (should (equal
                     (concat "Bearer " (make-string 64 ?c))
                     (alist-get
                      'Authorization
                      (alist-get 'headers
                                 (alist-get 'emacs_tools servers))))))
          (funcall (plist-get launch :cleanup-fn))
          (setq launch nil)
          (should (equal original
                         (ai-code-test-mcp-agent--file-contents config-file)))
          (should (= #o640 (logand (file-modes config-file) #o777))))
      (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
        (funcall cleanup-fn))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-rejects-invalid-config ()
  "Antigravity preparation should reject invalid JSON without side effects."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--antigravity-config-states
          (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-invalid-" t))
         (config-dir (expand-file-name ".agents" working-dir))
         (config-file (expand-file-name "mcp_config.json" config-dir))
         (server-start-count 0)
         launch
         outcome)
    (make-directory config-dir)
    (with-temp-file config-file
      (insert "{not-json\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda ()
                     (cl-incf server-start-count)
                     8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore))
          (setq outcome
                (condition-case err
                    (progn
                      (setq launch
                            (ai-code-mcp-agent-prepare-launch
                             'antigravity working-dir '("agy")))
                      :unexpected-success)
                  (error err)))
          (should (eq 'json-parse-error (car outcome)))
          (should (zerop server-start-count))
          (should (zerop (hash-table-count ai-code-mcp--sessions)))
          (should (equal "{not-json\n"
                         (ai-code-test-mcp-agent--file-contents config-file))))
      (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
        (funcall cleanup-fn))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-coordinates-config-leases ()
  "Concurrent Antigravity launches should restore config after the last lease."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--antigravity-config-states
          (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-leases-" t))
         (config-file
          (expand-file-name ".agents/mcp_config.json" working-dir))
         (session-ids '("antigravity-session-1" "antigravity-session-2"))
         (secrets (list (make-string 64 ?d) (make-string 64 ?e)))
         first-launch
         second-launch)
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
                (ai-code-mcp-agent-prepare-launch
                 'antigravity working-dir '("agy")))
          (setq second-launch
                (ai-code-mcp-agent-prepare-launch
                 'antigravity working-dir '("agy")))
          (should (string-match-p
                   (regexp-quote (concat "Bearer " (make-string 64 ?e)))
                   (ai-code-test-mcp-agent--file-contents config-file)))
          (funcall (plist-get first-launch :cleanup-fn))
          (setq first-launch nil)
          (should (file-exists-p config-file))
          (should (string-match-p
                   (regexp-quote (concat "Bearer " (make-string 64 ?e)))
                   (ai-code-test-mcp-agent--file-contents config-file)))
          (funcall (plist-get second-launch :cleanup-fn))
          (setq second-launch nil)
          (should-not (file-exists-p config-file)))
      (when-let ((cleanup-fn (plist-get first-launch :cleanup-fn)))
        (funcall cleanup-fn))
      (when-let ((cleanup-fn (plist-get second-launch :cleanup-fn)))
        (funcall cleanup-fn))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-rolls-back-failed-registration ()
  "A failed Antigravity registration should remove config and stop its server."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--antigravity-config-states
          (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp-http-server--server nil)
         (ai-code-mcp-http-server--port nil)
         (working-dir (make-temp-file "ai-code-mcp-antigravity-rollback-" t))
         (config-file
          (expand-file-name ".agents/mcp_config.json" working-dir))
         (stop-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop)
                   (lambda () (cl-incf stop-count)))
                  ((symbol-function 'ai-code-mcp-register-session)
                   (lambda (&rest _args) (error "Registration failed"))))
          (should-error
           (ai-code-mcp-agent-prepare-launch
            'antigravity working-dir '("agy"))
           :type 'error)
          (should-not (file-exists-p config-file))
          (should (zerop
                   (hash-table-count
                    ai-code-mcp-agent--antigravity-config-states)))
          (should (= 1 stop-count)))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-preserves-external-edit ()
  "Cleanup should not overwrite an Antigravity config changed by the user."
  (let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
         (ai-code-mcp-agent--antigravity-config-states
          (make-hash-table :test 'equal))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (working-dir (make-temp-file "ai-code-mcp-antigravity-edit-" t))
         (config-file
          (expand-file-name ".agents/mcp_config.json" working-dir))
         (user-content "{\"mcpServers\":{},\"userEdit\":true}\n")
         launch)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-mcp-builtins-setup) #'ignore)
                  ((symbol-function 'ai-code-mcp-http-server-ensure)
                   (lambda () 8765))
                  ((symbol-function 'ai-code-mcp-http-server-stop) #'ignore)
                  ((symbol-function 'display-warning) #'ignore))
          (setq launch
                (ai-code-mcp-agent-prepare-launch
                 'antigravity working-dir '("agy")))
          (with-temp-file config-file
            (insert user-content))
          (funcall (plist-get launch :cleanup-fn))
          (setq launch nil)
          (should (equal user-content
                         (ai-code-test-mcp-agent--file-contents config-file))))
      (when-let ((cleanup-fn (plist-get launch :cleanup-fn)))
        (funcall cleanup-fn))
      (delete-directory working-dir t))))

(ert-deftest ai-code-test-mcp-agent-antigravity-preserves-json-values ()
  "Merging the Antigravity server should preserve unrelated JSON values."
  (dolist (original
           '("{\"mcpServers\":{},\"preserve\":[1,false,null,{\"nested\":\"x\"}]}"
             "{\"empty\":{},\"text\":\"quoted \\\"// value\\\"\"}"))
    (let* ((state (list :path "/tmp/mcp_config.json"
                        :original-exists-p t
                        :original-content original))
           (before
            (ai-code-mcp-agent--parse-antigravity-config
             original (plist-get state :path)))
           (after
            (ai-code-mcp-agent--parse-antigravity-config
             (ai-code-mcp-agent--antigravity-config-content
              state "http://127.0.0.1:8765/mcp" "test-token")
             (plist-get state :path)))
           (missing (make-symbol "missing")))
      (dolist (key '("preserve" "empty" "text"))
        (let ((expected (gethash key before missing)))
          (unless (eq expected missing)
            (should
             (equal
              (json-serialize expected
                              :null-object :null
                              :false-object :json-false)
              (json-serialize (gethash key after missing)
                              :null-object :null
                              :false-object :json-false))))))
      (should
       (hash-table-p
        (gethash ai-code-mcp-agent--server-name
                 (gethash "mcpServers" after)))))))

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
