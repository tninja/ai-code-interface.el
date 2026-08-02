;;; test_ai-code-mcp-http-hardening.el --- MCP HTTP validation tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for local MCP HTTP request validation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-mcp-http-server)

(defun ai-code-test-mcp-http--request (path body &optional content-type)
  "Build a POST request for PATH and BODY with CONTENT-TYPE."
  (list :method "POST"
        :path path
        :headers `(("content-type" . ,(or content-type "application/json")))
        :body body))

(ert-deftest ai-code-test-mcp-http-hardening-rejects-non-session-path ()
  "POST endpoints outside an exact MCP session path should be rejected."
  (let (response dispatched)
    (cl-letf (((symbol-function 'ai-code-mcp-dispatch)
               (lambda (&rest _args) (setq dispatched t)))
              ((symbol-function 'ai-code-mcp-http-server--send-response)
               (lambda (_process code content-type body)
                 (setq response (list code content-type body)))))
      (ai-code-mcp-http-server--handle-post
       'process
       (ai-code-test-mcp-http--request
        "/other/session" "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}")))
    (should (equal (car response) 404))
    (should-not dispatched)))

(ert-deftest ai-code-test-mcp-http-hardening-rejects-unregistered-session ()
  "An exact MCP path should still require a registered session."
  (let (response)
    (cl-letf (((symbol-function 'ai-code-mcp-get-session-context)
               (lambda (_session-id) nil))
              ((symbol-function 'ai-code-mcp-http-server--send-response)
               (lambda (_process code content-type body)
                 (setq response (list code content-type body)))))
      (ai-code-mcp-http-server--handle-post
       'process
       (ai-code-test-mcp-http--request
        "/mcp/missing" "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}")))
    (should (equal (car response) 404))))

(ert-deftest ai-code-test-mcp-http-hardening-requires-json-content-type ()
  "MCP POST requests should require JSON content."
  (let (response)
    (cl-letf (((symbol-function 'ai-code-mcp-get-session-context)
               (lambda (_session-id) '(:project-dir "/tmp")))
              ((symbol-function 'ai-code-mcp-http-server--send-response)
               (lambda (_process code content-type body)
                 (setq response (list code content-type body)))))
      (ai-code-mcp-http-server--handle-post
       'process
       (ai-code-test-mcp-http--request
        "/mcp/live" "{}" "text/plain")))
    (should (equal (car response) 415))))

(ert-deftest ai-code-test-mcp-http-hardening-rejects-invalid-jsonrpc-envelope ()
  "Requests should declare JSON-RPC 2.0 and a string method."
  (let (response)
    (cl-letf (((symbol-function 'ai-code-mcp-get-session-context)
               (lambda (_session-id) '(:project-dir "/tmp")))
              ((symbol-function 'ai-code-mcp-http-server--send-json-error)
               (lambda (_process id code message &optional http-code)
                 (setq response (list id code message http-code)))))
      (ai-code-mcp-http-server--handle-post
       'process
       (ai-code-test-mcp-http--request
        "/mcp/live" "{\"jsonrpc\":\"1.0\",\"id\":7,\"method\":42}")))
    (should (equal (nth 0 response) 7))
    (should (equal (nth 1 response) -32600))
    (should (equal (nth 3 response) 400))))

(ert-deftest ai-code-test-mcp-http-hardening-rejects-oversized-body ()
  "The local HTTP transport should cap request body size."
  (let ((ai-code-mcp-http-server-max-body-bytes 4)
        response)
    (cl-letf (((symbol-function 'ai-code-mcp-get-session-context)
               (lambda (_session-id) '(:project-dir "/tmp")))
              ((symbol-function 'ai-code-mcp-http-server--send-response)
               (lambda (_process code content-type body)
                 (setq response (list code content-type body)))))
      (ai-code-mcp-http-server--handle-post
       'process
       (ai-code-test-mcp-http--request "/mcp/live" "12345")))
    (should (equal (car response) 413))))

(provide 'test_ai-code-mcp-http-hardening)

;;; test_ai-code-mcp-http-hardening.el ends here
