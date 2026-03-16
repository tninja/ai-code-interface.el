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

(provide 'test_ai-code-mcp-agent)

;;; test_ai-code-mcp-agent.el ends here
