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

(defun ai-code-mcp--random-secret ()
  "Return a 256-bit secret encoded as lowercase hexadecimal."
  (cond
   ((executable-find "openssl")
    (with-temp-buffer
      (unless (zerop (call-process "openssl" nil t nil
                                   "rand" "-hex" "32"))
        (error "OpenSSL could not generate an MCP secret"))
      (let ((secret (string-trim (buffer-string))))
        (unless (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" secret)
          (error "OpenSSL returned an invalid MCP secret"))
        (downcase secret))))
   ((and (file-readable-p "/dev/urandom") (executable-find "head"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (unless (zerop (call-process "head" nil t nil
                                   "-c" "32" "/dev/urandom"))
        (error "Could not read operating-system entropy"))
      (unless (= (buffer-size) 32)
        (error "Could not read enough operating-system entropy"))
      (secure-hash 'sha256 (current-buffer))))
   (t
    (error "No secure random source is available for MCP authentication"))))

(provide 'ai-code-mcp-common)

;;; ai-code-mcp-common.el ends here
