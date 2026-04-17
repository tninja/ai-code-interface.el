;;; ai-code-mcp-common.el --- Shared MCP helper functions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Shared helper functions used by optional MCP tool modules.

;;; Code:

(require 'subr-x)

(defvar ai-code-mcp-server-tool-setup-functions nil
  "Functions that register optional MCP tool groups.")

(defun ai-code-mcp--json-bool (value)
  "Return VALUE as a JSON boolean token."
  (if value t :json-false))

(defun ai-code-mcp--message-lines ()
  "Return the current `*Messages*' contents as a list of lines."
  (if-let ((buffer (get-buffer "*Messages*")))
      (with-current-buffer buffer
        (split-string (buffer-substring-no-properties (point-min) (point-max))
                      "\n"
                      t))
    '()))

(provide 'ai-code-mcp-common)

;;; ai-code-mcp-common.el ends here
