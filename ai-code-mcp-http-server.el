;;; ai-code-mcp-http-server.el --- HTTP transport for ai-code MCP tools -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Minimal local HTTP transport for `ai-code-mcp-server'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(require 'ai-code-mcp-server)

(defgroup ai-code-mcp-http-server nil
  "HTTP transport for AI Code MCP tools."
  :group 'tools
  :prefix "ai-code-mcp-http-server-")

(defcustom ai-code-mcp-http-server-port nil
  "Port used by the local MCP HTTP server.
When nil, an available port is selected automatically."
  :type '(choice (const :tag "Auto-select" nil)
                 integer)
  :group 'ai-code-mcp-http-server)

(defcustom ai-code-mcp-http-server-max-request-bytes (* 1024 1024)
  "Maximum accepted MCP HTTP request body size in bytes."
  :type 'natnum
  :group 'ai-code-mcp-http-server)

(defcustom ai-code-mcp-http-server-max-header-bytes (* 64 1024)
  "Maximum accepted MCP HTTP request header size in bytes."
  :type 'natnum
  :group 'ai-code-mcp-http-server)

(defcustom ai-code-mcp-http-server-rate-limit 120
  "Maximum authenticated requests allowed per session rate window."
  :type 'natnum
  :group 'ai-code-mcp-http-server)

(defcustom ai-code-mcp-http-server-rate-window-seconds 60
  "Length of the authenticated request rate window in seconds."
  :type 'positive-integer
  :group 'ai-code-mcp-http-server)

(defvar ai-code-mcp-http-server--server nil
  "Server process for the local MCP HTTP transport.")

(defvar ai-code-mcp-http-server--port nil
  "Port for `ai-code-mcp-http-server--server'.")

(defun ai-code-mcp-http-server-live-p ()
  "Return non-nil when the local MCP HTTP server is running."
  (and ai-code-mcp-http-server--server
       (process-live-p ai-code-mcp-http-server--server)
       ai-code-mcp-http-server--port))

(defun ai-code-mcp-http-server-ensure ()
  "Start the local MCP HTTP server when needed and return its port."
  (unless (ai-code-mcp-http-server-live-p)
    (ai-code-mcp-http-server--start))
  ai-code-mcp-http-server--port)

(defun ai-code-mcp-http-server-stop ()
  "Stop the local MCP HTTP server."
  (interactive)
  (when (process-live-p ai-code-mcp-http-server--server)
    (delete-process ai-code-mcp-http-server--server))
  (setq ai-code-mcp-http-server--server nil
        ai-code-mcp-http-server--port nil))

(defun ai-code-mcp-http-server--start ()
  "Start the local MCP HTTP server."
  (let ((server (make-network-process
                 :name "ai-code-mcp-http-server"
                 :server t
                 :host "127.0.0.1"
                 :service (or ai-code-mcp-http-server-port 0)
                 :noquery t
                 ;; Emacs derives a buffer named after this process for every
                 ;; accepted connection unless the server carries a non-default
                 ;; filter.  Installing the real filter here keeps those
                 ;; `ai-code-mcp-http-server <127.0.0.1:PORT>' buffers, one per
                 ;; request, out of the user's buffer list.
                 :filter #'ai-code-mcp-http-server--filter
                 :log #'ai-code-mcp-http-server--accept)))
    (setq ai-code-mcp-http-server--server server
          ai-code-mcp-http-server--port (process-contact server :service))))

(defun ai-code-mcp-http-server--accept (_server client _message)
  "Initialize accepted CLIENT process."
  (set-process-query-on-exit-flag client nil)
  (ai-code-mcp-http-server--detach-client-buffer client)
  (set-process-coding-system client 'binary 'binary)
  (process-put client :data "")
  (set-process-filter client #'ai-code-mcp-http-server--filter)
  (set-process-sentinel client #'ai-code-mcp-http-server--client-sentinel))

(defun ai-code-mcp-http-server--detach-client-buffer (client)
  "Detach and kill any buffer Emacs attached to the accepted CLIENT.
Request bytes live in the process plist, so a client never needs a
buffer.  Detaching without killing would strand one buffer per request."
  (let ((buffer (process-buffer client)))
    (set-process-buffer client nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun ai-code-mcp-http-server--client-sentinel (process _event)
  "Clean up PROCESS buffer after the client disconnects."
  (when (and (not (process-live-p process))
             (buffer-live-p (process-buffer process)))
    (kill-buffer (process-buffer process))))

(defun ai-code-mcp-http-server--filter (process chunk)
  "Accumulate CHUNK for PROCESS and handle a full request."
  (condition-case nil
      (let* ((data (concat (or (process-get process :data) "") chunk))
             (header-end (string-match "\r\n\r\n" data)))
        (cond
         ((and (null header-end)
               (> (string-bytes data)
                  ai-code-mcp-http-server-max-header-bytes))
          (ai-code-mcp-http-server--send-response
           process 431 "text/plain" "Request Header Fields Too Large"))
         ((and header-end
               (> header-end ai-code-mcp-http-server-max-header-bytes))
          (ai-code-mcp-http-server--send-response
           process 431 "text/plain" "Request Header Fields Too Large"))
         ((and header-end
               (> (string-bytes data)
                  (+ header-end 4
                     ai-code-mcp-http-server-max-request-bytes)))
          (ai-code-mcp-http-server--send-response
           process 413 "text/plain" "Payload Too Large"))
         (t
          (process-put process :data data)
          (when-let ((request
                      (ai-code-mcp-http-server--parse-request data)))
            (process-put process :data nil)
            (ai-code-mcp-http-server--handle-request process request)))))
    (error
     (ai-code-mcp-http-server--send-response
      process 400 "text/plain" "Bad Request"))))

(defun ai-code-mcp-http-server--content-length (headers)
  "Return the validated Content-Length from HEADERS, or `invalid'."
  (let ((values
         (mapcar #'cdr
                 (seq-filter
                  (lambda (header)
                    (equal (car header) "content-length"))
                  headers))))
    (cond
     ((null values) 0)
     ((not (= (length values) 1)) 'invalid)
     ((not (string-match-p "\\`[0-9]+\\'" (car values))) 'invalid)
     (t (string-to-number (car values))))))

(defun ai-code-mcp-http-server--parse-request (data)
  "Parse DATA when it includes a full HTTP request."
  (when (string-match "\r\n\r\n" data)
    (let* ((separator-start (match-beginning 0))
           (body-start (match-end 0))
           (header-text (substring data 0 separator-start))
           (lines (split-string header-text "\r\n" t))
           (request-line (car lines))
           (headers (delq nil (mapcar #'ai-code-mcp-http-server--parse-header
                                      (cdr lines))))
           (content-length
            (ai-code-mcp-http-server--content-length headers)))
      (pcase-let ((`(,method ,path)
                   (ai-code-mcp-http-server--parse-request-line request-line)))
        (cond
         ((eq content-length 'invalid)
          (list :method method :path path :headers headers :bad-request t))
         ((> content-length ai-code-mcp-http-server-max-request-bytes)
          (list :method method :path path :headers headers :too-large t))
         ((<= (+ body-start content-length) (string-bytes data))
          (list :method method
                :path path
                :headers headers
                :body (substring data body-start
                                 (+ body-start content-length)))))))))

(defun ai-code-mcp-http-server--parse-request-line (line)
  "Parse HTTP request LINE."
  (let ((parts (split-string line " " t)))
    (unless (>= (length parts) 2)
      (error "Malformed request line"))
    (list (nth 0 parts) (nth 1 parts))))

(defun ai-code-mcp-http-server--parse-header (line)
  "Parse HTTP header LINE into a cons cell."
  (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
    (cons (downcase (match-string 1 line))
          (match-string 2 line))))

(defun ai-code-mcp-http-server--handle-request (process request)
  "Handle REQUEST received on PROCESS."
  (condition-case err
      (let ((session (ai-code-mcp-http-server--authenticate request)))
        (cond
         ((plist-get request :bad-request)
          (ai-code-mcp-http-server--send-response
           process 400 "text/plain" "Bad Request"))
         ((plist-get request :too-large)
          (ai-code-mcp-http-server--send-response
           process 413 "text/plain" "Payload Too Large"))
         ((not (equal (plist-get request :path) "/mcp"))
          (ai-code-mcp-http-server--send-response
           process 404 "text/plain" "Not Found"))
         ((not (ai-code-mcp-http-server--origin-allowed-p request))
          (ai-code-mcp-http-server--send-response
           process 403 "text/plain" "Forbidden"))
         ((not session)
          (ai-code-mcp-http-server--send-response
           process 401 "text/plain" ""))
         ((ai-code-mcp-http-server--rate-limited-p (car session))
          (ai-code-mcp-http-server--send-response
           process 429 "text/plain" "Too Many Requests"
           `(("Retry-After" . ,(number-to-string
                                ai-code-mcp-http-server-rate-window-seconds)))))
         ((and (equal (plist-get request :method) "POST")
               (not (ai-code-mcp-http-server--json-content-type-p request)))
          (ai-code-mcp-http-server--send-response
           process 415 "text/plain" "Unsupported Media Type"))
         ((and (equal (plist-get request :method) "POST")
               (not (ai-code-mcp-http-server--accepts-mcp-responses-p request)))
          (ai-code-mcp-http-server--send-response
           process 406 "text/plain" "Not Acceptable"))
         ((equal (plist-get request :method) "POST")
          (let ((ai-code-mcp--current-session-id (car session)))
            (ai-code-mcp-http-server--handle-post process request)))
         (t
          (ai-code-mcp-http-server--send-response
           process 405 "text/plain" "Method Not Allowed"))))
    (json-parse-error
     (ai-code-mcp-http-server--send-json-error
      process nil -32700 "Parse error"
      (if (ai-code-mcp-http-server--modern-request-headers-p request)
          400
        200)))
    (ai-code-mcp-protocol-error
     (ai-code-mcp-http-server--send-json-error
      process
      (ai-code-mcp-http-server--request-id request)
      (nth 1 err)
      (nth 2 err)
      200
      (nth 3 err)))
    (error
     (display-warning
      'ai-code-mcp-http-server
      (format "MCP HTTP request failed: %s" (error-message-string err)))
     (ai-code-mcp-http-server--send-json-error
      process
      (ai-code-mcp-http-server--request-id request)
      -32603
      "Internal error"))))

(defun ai-code-mcp-http-server--origin-allowed-p (request)
  "Return non-nil when REQUEST has no Origin or a loopback Origin."
  (let ((origin (cdr (assoc "origin" (plist-get request :headers))))
        (case-fold-search t))
    (or (null origin)
        (string-match-p
         "\\`https?://\\(?:localhost\\|127\\.0\\.0\\.1\\|\\[::1\\]\\)\\(?::[0-9]+\\)?\\'"
         origin))))

(defun ai-code-mcp-http-server--json-content-type-p (request)
  "Return non-nil when REQUEST declares an application/json body."
  (let ((content-type
         (cdr (assoc "content-type" (plist-get request :headers))))
        (case-fold-search t))
    (and content-type
         (string-match-p
          "\\`application/json\\(?:[ 	]*;.*\\)?\\'"
          content-type))))

(defun ai-code-mcp-http-server--rate-limited-p (session-id)
  "Record a request for SESSION-ID and return non-nil when over limit."
  (let* ((context (ai-code-mcp-get-session-context session-id))
         (now (float-time))
         (window-start (plist-get context :rate-window-start))
         (count (or (plist-get context :rate-count) 0)))
    (when (or (null window-start)
              (>= (- now window-start)
                  ai-code-mcp-http-server-rate-window-seconds))
      (setq window-start now
            count 0))
    (if (>= count ai-code-mcp-http-server-rate-limit)
        t
      (setq context (plist-put context :rate-window-start window-start))
      (setq context (plist-put context :rate-count (1+ count)))
      (puthash session-id context ai-code-mcp--sessions)
      nil)))

(defun ai-code-mcp-http-server--accepts-mcp-responses-p (request)
  "Return non-nil when REQUEST accepts JSON and event-stream responses."
  (let* ((accept (cdr (assoc "accept" (plist-get request :headers))))
         (types
          (mapcar (lambda (value)
                    (downcase
                     (string-trim (car (split-string value ";" t)))))
                  (split-string (or accept "") "," t))))
    (and (member "application/json" types)
         (member "text/event-stream" types))))

(defun ai-code-mcp-http-server--authenticate (request)
  "Return the MCP session authenticated by REQUEST, or nil."
  (let* ((authorization
          (cdr (assoc "authorization" (plist-get request :headers))))
         (case-fold-search t)
         (token (and authorization
                     (string-match "\\`Bearer[ 	]+\\(.+\\)\\'" authorization)
                     (match-string 1 authorization))))
    (ai-code-mcp-find-session-by-token token)))

(defun ai-code-mcp-http-server--handle-post (process request)
  "Handle POST REQUEST on PROCESS."
  (let* ((json-object
          (ai-code-mcp-http-server--parse-json
           (plist-get request :body)))
         (id (alist-get 'id json-object))
         (method (alist-get 'method json-object))
         (params (alist-get 'params json-object))
         (context (ai-code-mcp-get-session-context))
         (modern (ai-code-mcp-http-server--modern-request-p
                  request method params))
         (envelope-error
          (ai-code-mcp-http-server--json-rpc-envelope-error json-object))
         (transport-error
          (unless envelope-error
            (if modern
                (ai-code-mcp-http-server--modern-transport-error
                 method params request)
              (ai-code-mcp-http-server--legacy-transport-error
               method request context)))))
    (cond
     (envelope-error
      (ai-code-mcp-http-server--send-json-error
       process id -32600 envelope-error (if modern 400 200)))
     (transport-error
      (ai-code-mcp-http-server--send-json-error
       process id
       (or (plist-get transport-error :code) -32600)
       (or (plist-get transport-error :message) transport-error)
       400
       (plist-get transport-error :data)))
     ((not (ai-code-mcp-http-server--method-allowed-p
            method context modern))
      (ai-code-mcp-http-server--send-json-error
       process id -32600 "MCP session is not ready" 400))
     (t
      (let* ((transport-session-id
              (when (equal method "initialize")
                (ai-code-mcp-http-server--begin-legacy-session params)))
             (_completed
              (when (equal method "notifications/initialized")
                (ai-code-mcp-http-server--complete-legacy-session request)))
             (response-headers
              (when transport-session-id
                `(("MCP-Session-Id" . ,transport-session-id)))))
        (ai-code-mcp-http-server--dispatch-request
         process json-object modern response-headers))))))

(defun ai-code-mcp-http-server--parse-json (body)
  "Parse JSON request BODY into the server's canonical representation."
  (json-parse-string body
                     :object-type 'alist
                     :array-type 'array
                     :null-object :null
                     :false-object :json-false))

(defun ai-code-mcp-http-server--modern-request-headers-p (request)
  "Return non-nil when REQUEST headers select the modern protocol."
  (let ((version (cdr (assoc "mcp-protocol-version"
                             (plist-get request :headers)))))
    (and version
         (not (member version ai-code-mcp--legacy-protocol-versions)))))

(defun ai-code-mcp-http-server--modern-request-p (request method params)
  "Return non-nil when REQUEST, METHOD, and PARAMS use modern MCP."
  (let* ((meta (and (listp params) (alist-get '_meta params)))
         (body-version
          (and (listp meta)
               (alist-get 'io.modelcontextprotocol/protocolVersion meta))))
    (or (equal method "server/discover")
        (ai-code-mcp-http-server--modern-request-headers-p request)
        (equal body-version ai-code-mcp--modern-protocol-version))))

(defun ai-code-mcp-http-server--modern-transport-error
    (method params request)
  "Return a modern transport error for METHOD, PARAMS, and REQUEST."
  (let* ((headers (plist-get request :headers))
         (header-version (cdr (assoc "mcp-protocol-version" headers)))
         (header-method (cdr (assoc "mcp-method" headers)))
         (header-name (cdr (assoc "mcp-name" headers)))
         (meta (and (listp params) (alist-get '_meta params)))
         (version-entry
          (and (listp meta)
               (assq 'io.modelcontextprotocol/protocolVersion meta)))
         (capabilities-entry
          (and (listp meta)
               (assq 'io.modelcontextprotocol/clientCapabilities meta)))
         (body-version (cdr version-entry))
         (body-name (and (listp params)
                         (or (alist-get 'name params)
                             (alist-get 'uri params))))
         (requires-name
          (member method '("tools/call" "resources/read" "prompts/get"))))
    (cond
     ((or (null version-entry) (not (stringp body-version))
          (null capabilities-entry))
      (list :code -32602
            :message "Missing required per-request MCP metadata"))
     ((not (equal header-version body-version))
      (list :code -32020
            :message "Header mismatch: MCP-Protocol-Version"))
     ((not (equal body-version ai-code-mcp--modern-protocol-version))
      (list :code -32022
            :message "Unsupported protocol version"
            :data `((supported . [,ai-code-mcp--modern-protocol-version])
                    (requested . ,body-version))))
     ((not (equal header-method method))
      (list :code -32020
            :message "Header mismatch: Mcp-Method"))
     ((and requires-name
           (not (equal (ai-code-mcp-http-server--decode-header-value
                        header-name)
                       body-name)))
      (list :code -32020
            :message "Header mismatch: Mcp-Name")))))

(defun ai-code-mcp-http-server--decode-header-value (value)
  "Decode modern MCP header VALUE, including its Base64 sentinel form."
  (when value
    (if (string-match "\\`=\\?base64\\?\\(.+\\)\\?=\\'" value)
        (condition-case nil
            (decode-coding-string
             (base64-decode-string (match-string 1 value))
             'utf-8)
          (error nil))
      value)))

(defun ai-code-mcp-http-server--dispatch-request
    (process json-object modern response-headers)
  "Dispatch JSON-OBJECT and reply on PROCESS.
MODERN selects stateless protocol semantics and RESPONSE-HEADERS are appended
to a successful response."
  (let* ((id-entry (assq 'id json-object))
         (id (cdr id-entry))
         (method (alist-get 'method json-object))
         (params (alist-get 'params json-object))
         (ai-code-mcp--current-protocol-version
          (and modern ai-code-mcp--modern-protocol-version)))
    (if (null id-entry)
        (ai-code-mcp-http-server--send-accepted process)
      (condition-case err
          (ai-code-mcp-http-server--send-json
           process 200
           `((jsonrpc . "2.0")
             (id . ,id)
             (result . ,(ai-code-mcp-dispatch method params)))
           response-headers)
        (ai-code-mcp-protocol-error
         (let ((code (nth 1 err)))
           (ai-code-mcp-http-server--send-json-error
            process id code (nth 2 err)
            (cond
             ((not modern) 200)
             ((= code -32601) 404)
             (t 400))
            (nth 3 err))))
        (error
         (display-warning
          'ai-code-mcp-http-server
          (format "MCP dispatch failed: %s" (error-message-string err)))
         (ai-code-mcp-http-server--send-json-error
          process id -32603 "Internal error" 500))))))

(defun ai-code-mcp-http-server--json-rpc-envelope-error (json-object)
  "Return an error string when JSON-OBJECT is not a JSON-RPC request."
  (cond
   ((not (listp json-object)) "Invalid JSON-RPC request object")
   ((not (equal "2.0" (alist-get 'jsonrpc json-object)))
    "Invalid or missing jsonrpc version")
   ((not (stringp (alist-get 'method json-object)))
    "Invalid or missing JSON-RPC method")
   ((and (assq 'id json-object)
         (not (or (stringp (alist-get 'id json-object))
                  (integerp (alist-get 'id json-object)))))
    "JSON-RPC id must be a string or integer")
   ((and (assq 'params json-object)
         (not (listp (alist-get 'params json-object))))
    "JSON-RPC params must be an object")))

(defun ai-code-mcp-http-server--legacy-transport-error (method request context)
  "Return a legacy transport error for METHOD, REQUEST, and CONTEXT."
  (unless (equal method "initialize")
    (let* ((headers (plist-get request :headers))
           (expected-session-id (plist-get context :transport-session-id))
           (actual-session-id (cdr (assoc "mcp-session-id" headers)))
           (expected-version (plist-get context :protocol-version))
           (actual-version (cdr (assoc "mcp-protocol-version" headers))))
      (cond
       ((and expected-session-id
             (not (equal expected-session-id actual-session-id)))
        (list :code -32600
              :message "Missing or invalid MCP-Session-Id header"))
       ((and expected-version
             (not (equal expected-version actual-version)))
        (list :code -32600
              :message "Missing or invalid MCP-Protocol-Version header"))))))

(defun ai-code-mcp-http-server--method-allowed-p (method context modern)
  "Return non-nil when METHOD is valid for CONTEXT and protocol era MODERN."
  (if modern
      (or (member method '("server/discover" "tools/list" "ping"))
          (not (equal method "tools/call"))
          (eq (plist-get context :state) 'ready))
    (or (member method '("initialize" "notifications/initialized" "ping"))
        (eq (plist-get context :state) 'ready))))

(defun ai-code-mcp-http-server--begin-legacy-session (params)
  "Record a legacy MCP session initialized with PARAMS."
  (let* ((session-id ai-code-mcp--current-session-id)
         (protocol-version
          (ai-code-mcp-negotiate-legacy-version
           (alist-get 'protocolVersion params)))
         (client-info (alist-get 'clientInfo params))
         (transport-session-id (ai-code-mcp--random-secret)))
    (ai-code-mcp-begin-legacy-session
     session-id transport-session-id protocol-version client-info)
    transport-session-id))

(defun ai-code-mcp-http-server--complete-legacy-session (request)
  "Complete legacy MCP initialization described by REQUEST headers."
  (let ((headers (plist-get request :headers)))
    (ai-code-mcp-complete-legacy-session
     ai-code-mcp--current-session-id
     (cdr (assoc "mcp-session-id" headers))
     (cdr (assoc "mcp-protocol-version" headers)))))

(defun ai-code-mcp-http-server--request-id (request)
  "Extract the JSON-RPC request ID from REQUEST."
  (when-let ((body (plist-get request :body)))
    (condition-case nil
        (alist-get 'id
                   (ai-code-mcp-http-server--parse-json body))
      (error nil))))

(defun ai-code-mcp-http-server--send-json-error
    (process id code message &optional http-code data)
  "Send a JSON-RPC error on PROCESS.
ID, CODE, MESSAGE, HTTP-CODE, and DATA describe the error response."
  (ai-code-mcp-http-server--send-json
   process
   (or http-code 500)
   `((jsonrpc . "2.0")
     (id . ,id)
     (error . ((code . ,code)
               (message . ,message)
               ,@(when data `((data . ,data))))))))

(defun ai-code-mcp-http-server--send-json (process code payload &optional headers)
  "Send JSON PAYLOAD with HTTP CODE and optional HEADERS on PROCESS."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (ai-code-mcp-http-server--send-response
     process
     code
     "application/json"
     (json-encode payload)
     headers)))

(defun ai-code-mcp-http-server--send-accepted (process)
  "Send an empty HTTP 202 Accepted response on PROCESS."
  (ai-code-mcp-http-server--send-response process 202 "text/plain" ""))

(defun ai-code-mcp-http-server--send-response
    (process code content-type body &optional headers)
  "Send an HTTP response on PROCESS.
CODE, CONTENT-TYPE, BODY, and HEADERS describe the response."
  (let* ((payload (or body ""))
         (response (concat
                    (format "HTTP/1.1 %d %s\r\n"
                            code
                            (ai-code-mcp-http-server--reason code))
                    (format "Content-Type: %s\r\n" content-type)
                    (format "Content-Length: %d\r\n" (string-bytes payload))
                    (mapconcat (lambda (header)
                                 (format "%s: %s\r\n"
                                         (car header) (cdr header)))
                               headers
                               "")
                    "Connection: close\r\n\r\n"
                    payload)))
    (process-send-string process response)
    (delete-process process)))

(defun ai-code-mcp-http-server--reason (code)
  "Return the HTTP reason phrase for CODE."
  (alist-get code '((200 . "OK")
                    (202 . "Accepted")
                    (400 . "Bad Request")
                    (401 . "Unauthorized")
                    (403 . "Forbidden")
                    (404 . "Not Found")
                    (405 . "Method Not Allowed")
                    (406 . "Not Acceptable")
                    (413 . "Payload Too Large")
                    (415 . "Unsupported Media Type")
                    (429 . "Too Many Requests")
                    (431 . "Request Header Fields Too Large")
                    (500 . "Internal Server Error"))
             "OK"))

(provide 'ai-code-mcp-http-server)

;;; ai-code-mcp-http-server.el ends here
