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

(ert-deftest ai-code-test-mcp-http-server-tools-call-uses-session-context ()
  "HTTP MCP transport should dispatch `tools/call' with the session path."
  (should (fboundp 'ai-code-mcp-dispatch))
  (should (fboundp 'ai-code-mcp-http-server--json-rpc-response))
  (let* ((project-dir (make-temp-file "ai-code-mcp-http-" t))
         (buffer (generate-new-buffer " *ai-code-mcp-http-project*"))
         (ai-code-mcp--sessions (make-hash-table :test 'equal))
         (ai-code-mcp-server-tools nil)
         (session-id "session-http"))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "main.el" project-dir)
            (insert "(message \"hello\")\n"))
          (ai-code-mcp-builtins-setup)
          (ai-code-mcp-register-session session-id project-dir buffer)
          (let* ((result (ai-code-mcp-http-server--json-rpc-response
                          (format "/mcp/%s" session-id)
                          (json-encode
                           '((jsonrpc . "2.0")
                             (id . 1)
                             (method . "tools/call")
                             (params . ((name . "project_info")
                                        (arguments . ((dummy . :json-false)))))))))
                 (content (alist-get 'content (alist-get 'result result)))
                 (text (alist-get 'text (car content))))
            (should (string-match-p "Project:" text))
            (should (string-match-p (regexp-quote project-dir) text))
            (should (string-match-p "Files: 1" text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (ignore-errors
        (delete-directory project-dir t)))))

(ert-deftest ai-code-test-mcp-http-server-notification-returns-accepted ()
  "Notification requests should return HTTP 202 with an empty body."
  (let ((captured-response nil))
    (cl-letf (((symbol-function 'ai-code-mcp-http-server--send-response)
               (lambda (_process code content-type body)
                 (setq captured-response
                       (list :code code
                             :content-type content-type
                             :body body)))))
      (ai-code-mcp-http-server--handle-post
       nil
       (list :path "/mcp/session-http"
             :body (json-encode
                    '((jsonrpc . "2.0")
                      (method . "notifications/initialized")
                      (params . ((dummy . :json-false)))))))
      (should (equal 202 (plist-get captured-response :code)))
      (should (equal "" (plist-get captured-response :body))))))

(ert-deftest ai-code-test-mcp-http-server-errors-keep-request-id ()
  "JSON-RPC errors should preserve the originating request id."
  (let ((captured-payload nil)
        (captured-code nil))
    (cl-letf (((symbol-function 'ai-code-mcp-http-server--send-json)
               (lambda (_process code payload)
                 (setq captured-code code)
                 (setq captured-payload payload))))
      (ai-code-mcp-http-server--handle-request
       nil
       (list :method "POST"
             :path "/mcp/session-http"
             :body (json-encode
                    '((jsonrpc . "2.0")
                      (id . 17)
                      (method . "unknown/method")
                      (params . ((dummy . :json-false)))))))
      (should (equal 500 captured-code))
      (should (equal 17 (alist-get 'id captured-payload)))
      (should (equal -32603
                     (alist-get 'code
                                (alist-get 'error captured-payload)))))))

(provide 'test_ai-code-mcp-http-server)

;;; test_ai-code-mcp-http-server.el ends here
