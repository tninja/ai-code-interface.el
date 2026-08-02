;;; ai-code-mcp-http-server.el --- HTTP transport for ai-code MCP tools -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Minimal local HTTP transport for `ai-code-mcp-server'.

;;; Code:

(require 'cl-lib)
(require 'json)

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

(defcustom ai-code-mcp-http-server-max-body-bytes (* 1024 1024)
  "Maximum accepted MCP HTTP request body size in bytes."
  :type 'natnum
  :group 'ai-code-mcp-http-server)

(define-error 'ai-code-mcp-http-server-request-too-large
  "MCP HTTP request body is too large")

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
                 :log #'ai-code-mcp-http-server--accept)))
    (setq ai-code-mcp-http-server--server server
          ai-code-mcp-http-server--port (process-contact server :service))))

(defun ai-code-mcp-http-server--accept (_server client _message)
  "Initialize accepted CLIENT process."
  (set-process-query-on-exit-flag client nil)
  (set-process-buffer client (generate-new-buffer " *ai-code-mcp-http-client*"))
  (set-process-coding-system client 'binary 'binary)
  (process-put client :data "")
  (set-process-filter client #'ai-code-mcp-http-server--filter)
  (set-process-sentinel client #'ai-code-mcp-http-server--client-sentinel))

(defun ai-code-mcp-http-server--client-sentinel (process _event)
  "Clean up PROCESS buffer after the client disconnects."
  (when (and (not (process-live-p process))
             (buffer-live-p (process-buffer process)))
    (kill-buffer (process-buffer process))))

(defun ai-code-mcp-http-server--filter (process chunk)
  "Accumulate CHUNK for PROCESS and handle a full request."
  (let ((data (concat (or (process-get process :data) "") chunk)))
    (condition-case err
        (let ((request (ai-code-mcp-http-server--parse-request data)))
          (process-put process :data data)
          (when request
            (process-put process :data nil)
            (ai-code-mcp-http-server--handle-request process request)))
      (ai-code-mcp-http-server-request-too-large
       (process-put process :data nil)
       (ai-code-mcp-http-server--send-response
        process 413 "text/plain" (error-message-string err))))))

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
           (content-length (string-to-number
                            (or (cdr (assoc "content-length" headers))
                                "0"))))
      (when (or (< content-length 0)
                (> content-length ai-code-mcp-http-server-max-body-bytes))
        (signal 'ai-code-mcp-http-server-request-too-large
                (list (format "Request body exceeds %d bytes"
                              ai-code-mcp-http-server-max-body-bytes))))
      (when (<= (+ body-start content-length) (string-bytes data))
        (pcase-let ((`(,method ,path)
                     (ai-code-mcp-http-server--parse-request-line request-line)))
          (list :method method
                :path path
                :headers headers
                :body (substring data body-start (+ body-start content-length))))))))

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
      (pcase (plist-get request :method)
        ("POST"
         (ai-code-mcp-http-server--handle-post process request))
        (_
         (ai-code-mcp-http-server--send-response process 404 "text/plain" "Not Found")))
    (error
     (ai-code-mcp-http-server--send-json-error
      process
      (ai-code-mcp-http-server--request-id request)
      -32603
      (format "Internal error: %s" (error-message-string err))))))

(defun ai-code-mcp-http-server--json-content-type-p (request)
  "Return non-nil when REQUEST declares JSON content."
  (when-let* ((value (cdr (assoc "content-type" (plist-get request :headers)))))
    (string-match-p "\\`application/json\\(?:[ \t]*;\\|\\'\\)" (downcase value))))

(defun ai-code-mcp-http-server--valid-json-rpc-envelope-p (json-object)
  "Return non-nil when JSON-OBJECT is a valid JSON-RPC request envelope."
  (and (equal (alist-get 'jsonrpc json-object) "2.0")
       (stringp (alist-get 'method json-object))))

(defun ai-code-mcp-http-server--handle-post (process request)
  "Handle POST REQUEST on PROCESS."
  (let* ((path (plist-get request :path))
         (body (or (plist-get request :body) ""))
         (session-id (ai-code-mcp-http-server--session-id-from-path path)))
    (cond
     ((not session-id)
      (ai-code-mcp-http-server--send-response process 404 "text/plain" "Not Found"))
     ((not (ai-code-mcp-get-session-context session-id))
      (ai-code-mcp-http-server--send-response process 404 "text/plain" "Unknown MCP session"))
     ((not (ai-code-mcp-http-server--json-content-type-p request))
      (ai-code-mcp-http-server--send-response
       process 415 "text/plain" "Content-Type must be application/json"))
     ((> (string-bytes body) ai-code-mcp-http-server-max-body-bytes)
      (ai-code-mcp-http-server--send-response process 413 "text/plain" "Request body too large"))
     (t
      (condition-case err
          (let* ((json-object (json-parse-string body :object-type 'alist))
                 (id (alist-get 'id json-object)))
            (if (not (ai-code-mcp-http-server--valid-json-rpc-envelope-p json-object))
                (ai-code-mcp-http-server--send-json-error
                 process id -32600 "Invalid JSON-RPC request" 400)
              (let ((response
                     (ai-code-mcp-http-server--json-rpc-response
                      path body json-object session-id)))
                (if (null response)
                    (ai-code-mcp-http-server--send-accepted process)
                  (ai-code-mcp-http-server--send-json process 200 response)))))
        (json-parse-error
         (ai-code-mcp-http-server--send-json-error
          process nil -32700
          (format "Parse error: %s" (error-message-string err))
          400)))))))

(defun ai-code-mcp-http-server--json-rpc-response (path body &optional json-object session-id)
  "Return a JSON-RPC response alist for PATH and BODY.
JSON-OBJECT and SESSION-ID may be supplied when REQUEST was already validated.
Returns nil for notifications."
  (let* ((json-object (or json-object
                          (json-parse-string body :object-type 'alist)))
         (id (alist-get 'id json-object))
         (method (alist-get 'method json-object))
         (params (alist-get 'params json-object))
         (ai-code-mcp--current-session-id
          (or session-id (ai-code-mcp-http-server--session-id-from-path path))))
    (when id
      `((jsonrpc . "2.0")
        (id . ,id)
        (result . ,(ai-code-mcp-dispatch method params))))))

(defun ai-code-mcp-http-server--request-id (request)
  "Extract the JSON-RPC request ID from REQUEST."
  (when-let ((body (plist-get request :body)))
    (condition-case nil
        (alist-get 'id
                   (json-parse-string body :object-type 'alist))
      (error nil))))

(defun ai-code-mcp-http-server--session-id-from-path (path)
  "Extract session ID from exact MCP PATH."
  (when (and (stringp path)
             (string-match "\\`/mcp/\\([^/?]+\\)\\'" path))
    (match-string 1 path)))

(defun ai-code-mcp-http-server--send-json-error (process id code message &optional http-code)
  "Send a JSON-RPC error response with ID, CODE, and MESSAGE on PROCESS.
HTTP-CODE defaults to 500."
  (ai-code-mcp-http-server--send-json
   process
   (or http-code 500)
   `((jsonrpc . "2.0")
     (id . ,id)
     (error . ((code . ,code)
               (message . ,message))))))

(defun ai-code-mcp-http-server--send-json (process code payload)
  "Send JSON PAYLOAD with HTTP CODE on PROCESS."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (ai-code-mcp-http-server--send-response
     process
     code
     "application/json"
     (json-encode payload))))

(defun ai-code-mcp-http-server--send-accepted (process)
  "Send an empty HTTP 202 Accepted response on PROCESS."
  (ai-code-mcp-http-server--send-response process 202 "text/plain" ""))

(defun ai-code-mcp-http-server--send-response (process code content-type body)
  "Send an HTTP response with CODE, CONTENT-TYPE, and BODY on PROCESS."
  (let* ((payload (or body ""))
         (response (concat
                    (format "HTTP/1.1 %d %s\r\n"
                            code
                            (ai-code-mcp-http-server--reason code))
                    (format "Content-Type: %s\r\n" content-type)
                    (format "Content-Length: %d\r\n" (string-bytes payload))
                    "Connection: close\r\n\r\n"
                    payload)))
    (process-send-string process response)
    (delete-process process)))

(defun ai-code-mcp-http-server--reason (code)
  "Return the HTTP reason phrase for CODE."
  (alist-get code '((200 . "OK")
                    (202 . "Accepted")
                    (400 . "Bad Request")
                    (404 . "Not Found")
                    (413 . "Payload Too Large")
                    (415 . "Unsupported Media Type")
                    (500 . "Internal Server Error"))
             "OK"))

(provide 'ai-code-mcp-http-server)

;;; ai-code-mcp-http-server.el ends here
