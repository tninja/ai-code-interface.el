;;; test_ai-code-mcp-http-server.el --- Tests for ai-code-mcp-http-server -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-mcp-http-server module.

;;; Code:

(require 'ert)
(require 'json)
(require 'cl-lib)
(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))
(require 'ai-code-mcp-server)
(require 'ai-code-mcp-http-server nil t)

(cl-defstruct ai-code-test-mcp-http-response
  status headers body)

(defun ai-code-test-mcp-http--parse-response (payload)
  "Parse raw HTTP response PAYLOAD independently of the server code."
  (unless (string-match "\\`HTTP/1\\.[01] \\([0-9]+\\)[^\r\n]*\r\n" payload)
    (ert-fail (format "Malformed HTTP response: %S" payload)))
  (let* ((status (string-to-number (match-string 1 payload)))
         (status-line-end (match-end 0))
         (header-end (string-match "\r\n\r\n" payload))
         (header-lines
          (split-string (substring payload status-line-end header-end)
                        "\r\n" t))
         (headers
          (mapcar
           (lambda (line)
             (unless (string-match "\\`\\([^:]+\\):[ 	]*\\(.*\\)\\'" line)
               (ert-fail (format "Malformed HTTP header: %S" line)))
             (cons (downcase (match-string 1 line)) (match-string 2 line)))
           header-lines))
         (body-start (+ header-end 4))
         (content-length
          (string-to-number (or (cdr (assoc "content-length" headers)) "0")))
         (body (substring payload body-start (+ body-start content-length))))
    (make-ai-code-test-mcp-http-response
     :status status :headers headers :body body)))

(defun ai-code-test-mcp-http--exchange (port method path headers body)
  "Send one HTTP request to PORT using METHOD, PATH, HEADERS, and BODY."
  (let* ((buffer (generate-new-buffer " *ai-code-mcp-http-response*"))
         (process
          (make-network-process
           :name (generate-new-buffer-name "ai-code-mcp-http-client")
           :buffer buffer
           :host "127.0.0.1"
           :service port
           :coding 'binary
           :noquery t
           :nowait nil))
         (deadline (+ (float-time) 2.0)))
    (unwind-protect
        (progn
          (set-process-sentinel process #'ignore)
          (process-send-string
           process
           (concat
            (format "%s %s HTTP/1.1\r\n" method path)
            (format "Host: 127.0.0.1:%d\r\n" port)
            (mapconcat (lambda (header)
                         (format "%s: %s" (car header) (cdr header)))
                       headers
                       "\r\n")
            (when headers "\r\n")
            (unless (seq-some
                     (lambda (header)
                       (string= "content-length" (downcase (car header))))
                     headers)
              (format "Content-Length: %d\r\n" (string-bytes body)))
            "Connection: close\r\n\r\n"
            body))
          (while (and (process-live-p process)
                      (< (float-time) deadline))
            (accept-process-output process 0.02))
          (when (process-live-p process)
            (ert-fail "Timed out waiting for MCP HTTP response"))
          (with-current-buffer buffer
            (ai-code-test-mcp-http--parse-response (buffer-string))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ai-code-test-mcp-http--legacy-initialize (port token)
  "Initialize a legacy MCP session on PORT authenticated by TOKEN."
  (ai-code-test-mcp-http--exchange
   port "POST" "/mcp"
   `(("Authorization" . ,(concat "Bearer " token))
     ("Content-Type" . "application/json")
     ("Accept" . "application/json, text/event-stream"))
   (json-encode
    '((jsonrpc . "2.0")
      (id . 1)
      (method . "initialize")
                                (params . ((protocolVersion . "1900-01-01")
                 (capabilities . ())
                 (clientInfo . ((name . "ert") (version . "1")))))))))

(defun ai-code-test-mcp-http--legacy-ready (port token)
  "Initialize and ready a legacy MCP session on PORT using TOKEN."
  (let* ((initialize-response
          (ai-code-test-mcp-http--legacy-initialize port token))
         (transport-session-id
          (cdr (assoc "mcp-session-id"
                      (ai-code-test-mcp-http-response-headers
                       initialize-response))))
         (headers
          `(("Authorization" . ,(concat "Bearer " token))
            ("Content-Type" . "application/json")
            ("Accept" . "application/json, text/event-stream")
            ("MCP-Session-Id" . ,transport-session-id)
            ("MCP-Protocol-Version" . "2025-11-25"))))
    (ai-code-test-mcp-http--exchange
     port "POST" "/mcp" headers
     (json-encode
      '((jsonrpc . "2.0") (method . "notifications/initialized"))))
    transport-session-id))

(defun ai-code-test-mcp-http--modern-headers (token method &optional name)
  "Return modern MCP HTTP headers for TOKEN, METHOD, and optional NAME."
  (append
   `(("Authorization" . ,(concat "Bearer " token))
     ("Content-Type" . "application/json")
     ("Accept" . "application/json, text/event-stream")
     ("MCP-Protocol-Version" . "2026-07-28")
     ("Mcp-Method" . ,method))
   (when name `(("Mcp-Name" . ,name)))))

(defun ai-code-test-mcp-http--modern-meta ()
  "Return required per-request metadata for modern MCP tests."
  `((_meta
     . ((io.modelcontextprotocol/protocolVersion . "2026-07-28")
        (io.modelcontextprotocol/clientInfo
         . ((name . "ert") (version . "1")))
        (io.modelcontextprotocol/clientCapabilities
         . ,(make-hash-table :test 'equal))))))

(cl-defmacro ai-code-test-mcp-http--with-server
    ((port token state &optional session-id) &rest body)
  "Run BODY with a real MCP server bound to PORT for TOKEN and STATE."
  (declare (indent 1))
  `(let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp-server-tools nil)
         (ai-code-mcp-http-server-port nil)
         (ai-code-mcp-http-server--server nil)
         (ai-code-mcp-http-server--port nil)
         (project-dir (make-temp-file "ai-code-mcp-http-fixture-" t))
         (source-buffer (generate-new-buffer " *ai-code-mcp-http-fixture*")))
     (unwind-protect
         (progn
           (ai-code-mcp-register-session
            ,(or session-id "modern-session") project-dir source-buffer
            (list :token ,token :state ,state))
           (let ((,port (ai-code-mcp-http-server-ensure)))
             ,@body))
       (ai-code-mcp-http-server-stop)
       (when (buffer-live-p source-buffer)
         (kill-buffer source-buffer))
       (delete-directory project-dir t))))

(ert-deftest ai-code-test-mcp-http-server-does-not-leave-visible-client-buffers ()
  "Accepted connections should not leave visible client buffers behind."
  (let ((ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (buffers-before (buffer-list))
        leaked-buffers)
    (unwind-protect
        (let ((port (ai-code-mcp-http-server-ensure)))
          (ai-code-test-mcp-http--exchange port "GET" "/mcp" nil "")
          (setq leaked-buffers
                (seq-filter
                 (lambda (buffer)
                   (and (not (memq buffer buffers-before))
                        (string-prefix-p "ai-code-mcp-http-server <"
                                         (buffer-name buffer))))
                 (buffer-list)))
          (should-not leaked-buffers))
      (ai-code-mcp-http-server-stop)
      (dolist (buffer leaked-buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ai-code-test-mcp-http-server-discovers-modern-protocol-over-socket ()
  "Modern clients should discover capabilities without a session handshake."
  (ai-code-test-mcp-http--with-server
      (port "modern-discover-token" 'pending)
    (let* ((response
            (ai-code-test-mcp-http--exchange
             port "POST" "/mcp"
             (ai-code-test-mcp-http--modern-headers
              "modern-discover-token" "server/discover")
             (json-encode
              `((jsonrpc . "2.0")
                (id . "discover-1")
                (method . "server/discover")
                (params . ,(ai-code-test-mcp-http--modern-meta))))))
           (payload
            (json-parse-string
             (ai-code-test-mcp-http-response-body response)
             :object-type 'alist :array-type 'list
             :false-object :json-false))
           (result (alist-get 'result payload)))
      (ert-info ((ai-code-test-mcp-http-response-body response))
        (should (= 200 (ai-code-test-mcp-http-response-status response))))
      (should (equal "complete" (alist-get 'resultType result)))
      (should (member "2026-07-28" (alist-get 'supportedVersions result)))
      (should (alist-get 'tools (alist-get 'capabilities result)))
      (should-not (assoc "mcp-session-id"
                         (ai-code-test-mcp-http-response-headers response))))))

(ert-deftest ai-code-test-mcp-http-server-validates-modern-mirrored-headers ()
  "Modern mirrored headers must agree with the JSON-RPC body."
  (ai-code-test-mcp-http--with-server
      (port "modern-mismatch-token" 'ready)
    (let* ((response
            (ai-code-test-mcp-http--exchange
             port "POST" "/mcp"
             (ai-code-test-mcp-http--modern-headers
              "modern-mismatch-token" "tools/list")
             (json-encode
              `((jsonrpc . "2.0")
                (id . 7)
                (method . "tools/call")
                (params . ((name . "project_info")
                           (arguments . ())
                           ,@(ai-code-test-mcp-http--modern-meta)))))))
           (payload
            (json-parse-string
             (ai-code-test-mcp-http-response-body response)
             :object-type 'alist))
           (error-object (alist-get 'error payload)))
      (should (= 400 (ai-code-test-mcp-http-response-status response)))
      (should (= -32020 (alist-get 'code error-object))))))

(ert-deftest ai-code-test-mcp-http-server-requires-modern-request-metadata ()
  "Every modern request should carry protocol and capability metadata."
  (ai-code-test-mcp-http--with-server
      (port "modern-meta-token" 'ready)
    (let* ((response
            (ai-code-test-mcp-http--exchange
             port "POST" "/mcp"
             (ai-code-test-mcp-http--modern-headers
              "modern-meta-token" "tools/list")
             (json-encode
              `((jsonrpc . "2.0")
                (id . 3)
                (method . "tools/list")
                (params . ,(make-hash-table :test 'equal))))))
           (payload
            (json-parse-string
             (ai-code-test-mcp-http-response-body response)
             :object-type 'alist))
           (error-object (alist-get 'error payload)))
      (should (= 400 (ai-code-test-mcp-http-response-status response)))
      (should (= -32602 (alist-get 'code error-object))))))

(ert-deftest ai-code-test-mcp-http-server-rejects-unsupported-modern-version ()
  "Modern requests should receive the unsupported-version protocol error."
  (ai-code-test-mcp-http--with-server
      (port "modern-version-token" 'ready)
    (let* ((headers
            '(("Authorization" . "Bearer modern-version-token")
              ("Content-Type" . "application/json")
              ("Accept" . "application/json, text/event-stream")
              ("MCP-Protocol-Version" . "1900-01-01")
              ("Mcp-Method" . "tools/list")))
           (response
            (ai-code-test-mcp-http--exchange
             port "POST" "/mcp" headers
             (json-encode
              `((jsonrpc . "2.0")
                (id . 4)
                (method . "tools/list")
                (params
                 . ((_meta
                     . ((io.modelcontextprotocol/protocolVersion
                         . "1900-01-01")
                        (io.modelcontextprotocol/clientCapabilities
                         . ,(make-hash-table :test 'equal))))))))))
           (payload
            (json-parse-string
             (ai-code-test-mcp-http-response-body response)
             :object-type 'alist))
           (error-object (alist-get 'error payload)))
      (should (= 400 (ai-code-test-mcp-http-response-status response)))
      (should (= -32022 (alist-get 'code error-object))))))

(ert-deftest ai-code-test-mcp-http-server-modern-tool-call-is-structured ()
  "A modern tool call should be stateless and return a complete result."
  (ai-code-test-mcp-http--with-server
      (port "modern-tool-token" 'ready)
    (with-temp-file (expand-file-name "main.el" project-dir)
      (insert "(message \"hello\")\n"))
    (let* ((response
            (ai-code-test-mcp-http--exchange
             port "POST" "/mcp"
             (ai-code-test-mcp-http--modern-headers
              "modern-tool-token" "tools/call" "project_info")
             (json-encode
              `((jsonrpc . "2.0")
                (id . 8)
                (method . "tools/call")
                (params . ((name . "project_info")
                           ,@(ai-code-test-mcp-http--modern-meta)))))))
           (payload
            (json-parse-string
             (ai-code-test-mcp-http-response-body response)
             :object-type 'alist :false-object :json-false))
           (result (alist-get 'result payload)))
      (ert-info ((ai-code-test-mcp-http-response-body response))
        (should (= 200 (ai-code-test-mcp-http-response-status response))))
      (should (equal "complete" (alist-get 'resultType result)))
      (should (alist-get 'structuredContent result))
      (should (string-match-p
               (regexp-quote project-dir)
               (alist-get 'text (aref (alist-get 'content result) 0))))
      (ert-info ((ai-code-test-mcp-http-response-body response))
        (should (eq :json-false (alist-get 'isError result)))))))

(ert-deftest ai-code-test-mcp-http-server-rejects-missing-bearer ()
  "The real HTTP endpoint should reject requests without a bearer token."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-auth*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "auth-session" "/tmp/" source-buffer
           '(:token "auth-test-token" :state pending))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   (json-encode
                    '((jsonrpc . "2.0")
                      (id . 1)
                      (method . "initialize")
                      (params . ((protocolVersion . "2025-11-25")
                                 (capabilities . ())
                                 (clientInfo . ((name . "ert")
                                                (version . "1")))))))))
            (should (= 401 (ai-code-test-mcp-http-response-status response)))
            (should (string-empty-p
                     (ai-code-test-mcp-http-response-body response)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))))))

(ert-deftest ai-code-test-mcp-http-server-rejects-expired-bearer ()
  "An expired per-launch bearer token should no longer authenticate."
  (ai-code-test-mcp-http--with-server
      (port "expired-token" 'ready "expired-session")
    (let ((context (ai-code-mcp-get-session-context "expired-session")))
      (puthash "expired-session"
               (plist-put context :expires-at
                          (time-subtract (current-time) (seconds-to-time 1)))
               ai-code-mcp--sessions))
    (let ((response
           (ai-code-test-mcp-http--exchange
            port "POST" "/mcp"
            (ai-code-test-mcp-http--modern-headers
             "expired-token" "server/discover")
            (json-encode
             `((jsonrpc . "2.0")
               (id . 1)
               (method . "server/discover")
               (params . ,(ai-code-test-mcp-http--modern-meta)))))))
      (should (= 401 (ai-code-test-mcp-http-response-status response))))))

(ert-deftest ai-code-test-mcp-http-server-initializes-legacy-session-over-socket ()
  "A valid bearer should initialize a 2025 MCP HTTP session."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-initialize*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "initialize-session" "/tmp/" source-buffer
           '(:token "initialize-test-token" :state pending))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (body
                  (json-encode
                   '((jsonrpc . "2.0")
                     (id . 1)
                     (method . "initialize")
                     (params . ((protocolVersion . "1900-01-01")
                                (capabilities . ())
                                (clientInfo . ((name . "ert")
                                               (version . "1"))))))))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer initialize-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   body))
                 (headers (ai-code-test-mcp-http-response-headers response))
                 (payload
                  (json-parse-string
                   (ai-code-test-mcp-http-response-body response)
                   :object-type 'alist))
                 (result (alist-get 'result payload))
                 (transport-session-id (cdr (assoc "mcp-session-id" headers))))
            (should (= 200 (ai-code-test-mcp-http-response-status response)))
            (should (equal "2025-11-25"
                           (alist-get 'protocolVersion result)))
            (should (and (stringp transport-session-id)
                         (not (string-empty-p transport-session-id))))
            (should-not (equal "initialize-session" transport-session-id))
            (should (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'"
                                    transport-session-id))
            (let ((context (ai-code-mcp-get-session-context
                            "initialize-session")))
              (should (equal transport-session-id
                             (plist-get context :transport-session-id)))
              (should (equal "2025-11-25"
                             (plist-get context :protocol-version)))
              (should (eq 'initializing (plist-get context :state))))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-initialized-notification-makes-session-ready ()
  "The initialized notification should complete the legacy lifecycle."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-ready*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "ready-session" "/tmp/" source-buffer
           '(:token "ready-test-token" :state pending))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (common-headers
                  '(("Authorization" . "Bearer ready-test-token")
                    ("Content-Type" . "application/json")
                    ("Accept" . "application/json, text/event-stream")))
                 (initialize-response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp" common-headers
                   (json-encode
                    '((jsonrpc . "2.0")
                      (id . 1)
                      (method . "initialize")
                      (params . ((protocolVersion . "2025-11-25")
                                 (capabilities . ())
                                 (clientInfo . ((name . "ert")
                                                (version . "1")))))))))
                 (transport-session-id
                  (cdr (assoc
                        "mcp-session-id"
                        (ai-code-test-mcp-http-response-headers
                         initialize-response))))
                 (notification-headers
                  (append common-headers
                          `(("MCP-Session-Id" . ,transport-session-id)
                            ("MCP-Protocol-Version" . "2025-11-25"))))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp" notification-headers
                   (json-encode
                    '((jsonrpc . "2.0")
                      (method . "notifications/initialized"))))))
            (should (= 202 (ai-code-test-mcp-http-response-status response)))
            (should (string-empty-p
                     (ai-code-test-mcp-http-response-body response)))
            (should (eq 'ready
                        (plist-get
                         (ai-code-mcp-get-session-context "ready-session")
                         :state)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-tools-before-ready ()
  "Legacy tool requests should fail before initialization completes."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-pending*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "pending-session" "/tmp/" source-buffer
           '(:token "pending-test-token" :state pending))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer pending-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   (json-encode
                    '((jsonrpc . "2.0")
                      (id . 9)
                      (method . "tools/list")))))
                 (payload
                  (json-parse-string
                   (ai-code-test-mcp-http-response-body response)
                   :object-type 'alist))
                 (error-object (alist-get 'error payload)))
            (should (= 400 (ai-code-test-mcp-http-response-status response)))
            (should (= 9 (alist-get 'id payload)))
            (should (= -32600 (alist-get 'code error-object)))
            (should (eq 'pending
                        (plist-get
                         (ai-code-mcp-get-session-context "pending-session")
                         :state)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-missing-legacy-session-header ()
  "Ready legacy requests should require their transport session header."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-session-header*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "header-session" "/tmp/" source-buffer
           '(:token "header-test-token" :state pending))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (_transport-session-id
                  (ai-code-test-mcp-http--legacy-ready
                   port "header-test-token"))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer header-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream")
                     ("MCP-Protocol-Version" . "2025-11-25"))
                   (json-encode
                    '((jsonrpc . "2.0")
                      (id . 2)
                      (method . "tools/list"))))))
            (should (= 400 (ai-code-test-mcp-http-response-status response)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-untrusted-origin ()
  "The localhost endpoint should reject browser origins before dispatch."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-origin*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "origin-session" "/tmp/" source-buffer
           '(:token "origin-test-token" :state ready))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer origin-test-token")
                     ("Origin" . "https://attacker.example")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   (json-encode
                    '((jsonrpc . "2.0") (id . 3) (method . "ping"))))))
            (should (= 403 (ai-code-test-mcp-http-response-status response)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-unsupported-content-type ()
  "The endpoint should reject non-JSON request bodies before parsing."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-media*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "media-session" "/tmp/" source-buffer
           '(:token "media-test-token" :state ready))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer media-test-token")
                     ("Content-Type" . "text/plain")
                     ("Accept" . "application/json, text/event-stream"))
                   "not-json")))
            (should (= 415 (ai-code-test-mcp-http-response-status response)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-incompatible-accept-header ()
  "The endpoint should require JSON and event-stream response support."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-accept*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "accept-session" "/tmp/" source-buffer
           '(:token "accept-test-token" :state ready))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer accept-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/xml"))
                   (json-encode
                    '((jsonrpc . "2.0") (id . 4) (method . "ping"))))))
            (should (= 406 (ai-code-test-mcp-http-response-status response)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-oversized-body ()
  "The endpoint should reject requests above its configured byte limit."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (ai-code-mcp-http-server-max-request-bytes 16)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-size*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "size-session" "/tmp/" source-buffer
           '(:token "size-test-token" :state ready))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer size-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   "12345678901234567")))
            (should (= 413 (ai-code-test-mcp-http-response-status response)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-invalid-content-length ()
  "Malformed Content-Length input should receive a bounded bad request."
  (ai-code-test-mcp-http--with-server
      (port "length-test-token" 'ready "length-session")
    (let ((response
           (ai-code-test-mcp-http--exchange
            port "POST" "/mcp"
            '(("Authorization" . "Bearer length-test-token")
              ("Content-Type" . "application/json")
              ("Accept" . "application/json, text/event-stream")
              ("Content-Length" . "-1"))
            "{}")))
      (should (= 400 (ai-code-test-mcp-http-response-status response))))))

(ert-deftest ai-code-test-mcp-http-server-rejects-oversized-headers ()
  "The unauthenticated request header buffer should have a fixed upper bound."
  (ai-code-test-mcp-http--with-server
      (port "header-size-token" 'ready "header-size-session")
    (let* ((ai-code-mcp-http-server-max-header-bytes 128)
           (response
            (ai-code-test-mcp-http--exchange
             port "POST" "/mcp"
             `(("Authorization" . "Bearer header-size-token")
               ("Content-Type" . "application/json")
               ("Accept" . "application/json, text/event-stream")
               ("X-Fill" . ,(make-string 256 ?x)))
             "{}")))
      (should (= 431 (ai-code-test-mcp-http-response-status response))))))

(ert-deftest ai-code-test-mcp-http-server-rate-limits-authenticated-session ()
  "The endpoint should cap request bursts per authenticated session."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (ai-code-mcp-http-server-rate-limit 2)
        (ai-code-mcp-http-server-rate-window-seconds 60)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-rate*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "rate-session" "/tmp/" source-buffer
           '(:token "rate-test-token" :state ready))
          (let ((port (ai-code-mcp-http-server-ensure))
                responses)
            (dotimes (index 3)
              (push
               (ai-code-test-mcp-http--exchange
                port "POST" "/mcp"
                '(("Authorization" . "Bearer rate-test-token")
                  ("Content-Type" . "application/json")
                  ("Accept" . "application/json, text/event-stream"))
                (json-encode
                 `((jsonrpc . "2.0")
                   (id . ,(1+ index))
                   (method . "ping"))))
               responses))
            (setq responses (nreverse responses))
            (should (equal '(200 200 429)
                           (mapcar #'ai-code-test-mcp-http-response-status
                                   responses)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-returns-json-rpc-parse-error ()
  "Malformed JSON should produce a standard parse error without HTTP 500."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-json*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "json-session" "/tmp/" source-buffer
           '(:token "json-test-token" :state ready))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer json-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   "{not-json"))
                 (payload
                  (json-parse-string
                   (ai-code-test-mcp-http-response-body response)
                   :object-type 'alist))
                 (error-object (alist-get 'error payload)))
            (should (= 200 (ai-code-test-mcp-http-response-status response)))
            (should (= -32700 (alist-get 'code error-object)))
            (should (equal "Parse error" (alist-get 'message error-object)))
            (should (eq :null (alist-get 'id payload)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-unknown-method-keeps-zero-id ()
  "Unknown methods should return method-not-found and preserve id zero."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-method*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "method-session" "/tmp/" source-buffer
           '(:token "method-test-token" :state ready))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer method-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   (json-encode
                    '((jsonrpc . "2.0")
                      (id . 0)
                      (method . "unknown/method")))))
                 (payload
                  (json-parse-string
                   (ai-code-test-mcp-http-response-body response)
                   :object-type 'alist))
                 (error-object (alist-get 'error payload)))
            (should (= 200 (ai-code-test-mcp-http-response-status response)))
            (should (= 0 (alist-get 'id payload)))
            (should (= -32601 (alist-get 'code error-object)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-rejects-invalid-json-rpc-envelope ()
  "A malformed JSON-RPC envelope should return invalid-request."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-envelope*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "envelope-session" "/tmp/" source-buffer
           '(:token "envelope-test-token" :state ready))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer envelope-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   (json-encode '((id . 5) (method . "ping")))))
                 (payload
                  (json-parse-string
                   (ai-code-test-mcp-http-response-body response)
                   :object-type 'alist))
                 (error-object (alist-get 'error payload)))
            (should (= 200 (ai-code-test-mcp-http-response-status response)))
            (should (= 5 (alist-get 'id payload)))
            (should (= -32600 (alist-get 'code error-object)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest ai-code-test-mcp-http-server-maps-invalid-tool-params ()
  "Malformed tool calls should return JSON-RPC invalid-params."
  (let ((ai-code-mcp--sessions (make-hash-table :test 'equal))
        (ai-code-mcp-server-tools nil)
        (ai-code-mcp-http-server-port nil)
        (ai-code-mcp-http-server--server nil)
        (ai-code-mcp-http-server--port nil)
        (source-buffer (generate-new-buffer " *ai-code-mcp-http-params*")))
    (unwind-protect
        (progn
          (ai-code-mcp-register-session
           "params-session" "/tmp/" source-buffer
           '(:token "params-test-token" :state ready))
          (ai-code-mcp-make-tool
           :function #'identity
           :name "echo_name"
           :description "Echo one name."
           :args '((:name "name" :type string :description "Name.")))
          (let* ((port (ai-code-mcp-http-server-ensure))
                 (response
                  (ai-code-test-mcp-http--exchange
                   port "POST" "/mcp"
                   '(("Authorization" . "Bearer params-test-token")
                     ("Content-Type" . "application/json")
                     ("Accept" . "application/json, text/event-stream"))
                   (json-encode
                    `((jsonrpc . "2.0")
                      (id . 6)
                      (method . "tools/call")
                      (params . ((name . "echo_name")
                                 (arguments . ,(make-hash-table
                                                :test 'equal))))))))
                 (payload
                  (json-parse-string
                   (ai-code-test-mcp-http-response-body response)
                   :object-type 'alist))
                 (error-object (alist-get 'error payload)))
            (should (= 200 (ai-code-test-mcp-http-response-status response)))
            (should (= -32602 (alist-get 'code error-object)))
            (should (string-match-p "name"
                                    (alist-get 'message error-object)))))
      (ai-code-mcp-http-server-stop)
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(provide 'test_ai-code-mcp-http-server)

;;; test_ai-code-mcp-http-server.el ends here
