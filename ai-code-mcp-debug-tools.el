;;; ai-code-mcp-debug-tools.el --- Optional MCP debugging tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional MCP debugging tools for inspecting Emacs variables,
;; functions, features, messages, and the most recent command error.

;;; Code:

(require 'json)
(require 'ai-code-mcp-common)
(require 'nadvice)
(require 'subr-x)

(declare-function ai-code-mcp-make-tool "ai-code-mcp-server")

(defgroup ai-code-mcp-debug-tools nil
  "Optional MCP debugging tools."
  :group 'ai-code-mcp-server
  :prefix "ai-code-mcp-debug-tools-")

(defcustom ai-code-mcp-debug-tools-enabled t
  "When non-nil, register optional MCP debugging tools."
  :type 'boolean
  :group 'ai-code-mcp-debug-tools)

(defvar ai-code-mcp--last-error-record nil
  "Most recent Emacs error snapshot recorded for MCP diagnostics tools.")

(defvar ai-code-mcp--error-capture-installed nil
  "Non-nil when MCP error capture advice has been installed.")

(defconst ai-code-mcp-debug-tools--specs
  '((:function ai-code-mcp-get-variable-binding-info
     :name "get_variable_binding_info"
     :description "Get current and default binding details for an Emacs variable."
     :args ((:name "variable_name"
             :type string
             :description "Emacs variable name to inspect.")
            (:name "buffer_name"
             :type string
             :description "Optional buffer name used for buffer-local inspection."
             :optional t)))
    (:function ai-code-mcp-get-variable-value
     :name "get_variable_value"
     :description "Get the printed representation of an Emacs variable value by name."
     :args ((:name "variable_name"
             :type string
             :description "Emacs variable name to inspect.")))
    (:function ai-code-mcp-get-function-info
     :name "get_function_info"
     :description "Get metadata about an Emacs function by name."
     :args ((:name "function_name"
             :type string
             :description "Emacs function name to inspect.")))
    (:function ai-code-mcp-get-last-error-backtrace
     :name "get_last_error_backtrace"
     :description "Get the most recently recorded Emacs error backtrace."
     :args nil)
    (:function ai-code-mcp-get-feature-load-state
     :name "get_feature_load_state"
     :description "Get load state details for an Emacs feature."
     :args ((:name "feature_name"
             :type string
             :description "Emacs feature name to inspect.")))
    (:function ai-code-mcp-get-recent-messages
     :name "get_recent_messages"
     :description "Get recent entries from the Emacs *Messages* buffer."
     :args ((:name "limit"
             :type integer
             :description "Maximum number of messages to return."
             :optional t))))
  "Optional MCP debugging tool specifications.")

(defun ai-code-mcp-debug-tools-setup ()
  "Register optional MCP debugging tools when enabled."
  (when ai-code-mcp-debug-tools-enabled
    (ai-code-mcp--ensure-error-capture)
    (dolist (tool ai-code-mcp-debug-tools--specs)
      (apply #'ai-code-mcp-make-tool tool))))

(defun ai-code-mcp--documentation-summary (documentation)
  "Return a trimmed summary line for DOCUMENTATION."
  (if (stringp documentation)
      (string-trim (car (split-string documentation "\n" t)))
    ""))

(defun ai-code-mcp--resolve-buffer (buffer-name)
  "Return BUFFER-NAME when it names a live buffer, or the current buffer."
  (if buffer-name
      (or (get-buffer buffer-name)
          (error "Buffer not found: %s" buffer-name))
    (current-buffer)))

(defun ai-code-mcp--backtrace-frame-summary (frame)
  "Return a readable summary string for backtrace FRAME."
  (let ((function (nth 1 frame))
        (arguments (nth 2 frame)))
    (if arguments
        (format "%S %S" function arguments)
      (format "%S" function))))

(defun ai-code-mcp--capture-backtrace-summaries ()
  "Return the current backtrace as a list of summary strings."
  (mapcar #'ai-code-mcp--backtrace-frame-summary
          (backtrace-frames)))

(defun ai-code-mcp--error-message (data)
  "Return a friendly error message string for DATA."
  (condition-case nil
      (error-message-string data)
    (error
     (format "%S" data))))

(defun ai-code-mcp--record-command-error (data context signal)
  "Record the command error DATA, CONTEXT, and SIGNAL for MCP tools."
  (let ((frames (ai-code-mcp--capture-backtrace-summaries)))
    (setq ai-code-mcp--last-error-record
          `((error_symbol . ,(symbol-name (car data)))
            (error_message . ,(ai-code-mcp--error-message data))
            (context . ,(format "%s" context))
            (signal . ,signal)
            (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
            (frame_count . ,(length frames))
            (frames . ,frames)))))

(defun ai-code-mcp--ensure-error-capture ()
  "Install error capture advice once for MCP debugging tools."
  (unless ai-code-mcp--error-capture-installed
    (advice-add 'command-error-default-function
                :before
                #'ai-code-mcp--record-command-error)
    (setq ai-code-mcp--error-capture-installed t)))

(defun ai-code-mcp--find-existing-variable-symbol (variable-name)
  "Return the interned symbol for VARIABLE-NAME, or nil when missing."
  (and (stringp variable-name)
       (intern-soft variable-name)))

(defun ai-code-mcp-get-variable-binding-info (variable-name &optional buffer-name)
  "Return JSON binding details for VARIABLE-NAME in BUFFER-NAME."
  (let ((symbol (ai-code-mcp--find-existing-variable-symbol variable-name)))
    (if (not symbol)
        (json-encode
         `((exists . :json-false)
           (variable_name . ,variable-name)
           (buffer_name . ,buffer-name)
           (bound . :json-false)
           (default_bound . :json-false)
           (buffer_local . :json-false)
           (current_value_repr . nil)
           (default_value_repr . nil)
           (documentation_summary . nil)))
      (with-current-buffer (ai-code-mcp--resolve-buffer buffer-name)
        (let ((bound (boundp symbol))
              (default-bound (default-boundp symbol))
              (buffer-local (local-variable-p symbol (current-buffer))))
          (json-encode
           `((exists . t)
             (variable_name . ,variable-name)
             (buffer_name . ,(buffer-name (current-buffer)))
             (bound . ,(ai-code-mcp--json-bool bound))
             (default_bound . ,(ai-code-mcp--json-bool default-bound))
             (buffer_local . ,(ai-code-mcp--json-bool buffer-local))
             (current_value_repr . ,(and bound
                                         (format "%S" (symbol-value symbol))))
             (default_value_repr . ,(and default-bound
                                         (format "%S" (default-value symbol))))
             (documentation_summary
              . ,(ai-code-mcp--documentation-summary
                  (documentation-property symbol
                                          'variable-documentation
                                          t))))))))))

(defun ai-code-mcp-get-variable-value (variable-name)
  "Return the printed representation of VARIABLE-NAME.
Return a friendly error string when VARIABLE-NAME does not name an
existing bound variable."
  (let ((symbol (ai-code-mcp--find-existing-variable-symbol variable-name)))
    (cond
     ((not symbol)
      (format "Variable not found: %s" variable-name))
     ((not (boundp symbol))
      (format "Variable is unbound: %s" variable-name))
     (t
      (format "%S" (symbol-value symbol))))))

(defun ai-code-mcp--find-existing-function-symbol (function-name)
  "Return the interned symbol for FUNCTION-NAME, or nil when missing."
  (and (stringp function-name)
       (intern-soft function-name)))

(defun ai-code-mcp--function-advised-p (symbol)
  "Return non-nil when SYMBOL has active advice."
  (let (advised)
    (advice-mapc (lambda (&rest _args)
                   (setq advised t))
                 symbol)
    advised))

(defun ai-code-mcp--function-root-definition (symbol)
  "Return SYMBOL's root definition with advice wrappers removed."
  (let ((definition (advice--symbol-function symbol)))
    (if (advice--p definition)
        (advice--cd*r definition)
      definition)))

(defun ai-code-mcp--function-kind (symbol)
  "Return a string describing SYMBOL's callable kind."
  (let ((root-definition (ai-code-mcp--function-root-definition symbol)))
    (cond
     ((autoloadp root-definition) "autoload")
     ((macrop symbol) "macro")
     (t
      (let ((resolved-definition (if (symbolp root-definition)
                                     (indirect-function root-definition)
                                   (indirect-function symbol))))
        (cond
         ((subrp resolved-definition) "subr")
         ((or (functionp resolved-definition)
              (and (consp resolved-definition)
                   (memq (car resolved-definition) '(lambda closure))))
          "lambda")
         (t "unknown")))))))

(defun ai-code-mcp-get-function-info (function-name)
  "Return JSON metadata describing FUNCTION-NAME."
  (let ((symbol (ai-code-mcp--find-existing-function-symbol function-name)))
    (if (not (and symbol (fboundp symbol)))
        (json-encode
         `((exists . :json-false)
           (function_name . ,function-name)
           (kind . nil)
           (interactive . :json-false)
           (advised . :json-false)
           (aliased_to . nil)
           (source_file . nil)
           (documentation_summary . nil)))
      (let* ((root-definition (ai-code-mcp--function-root-definition symbol))
             (aliased-to (and (symbolp root-definition)
                              (symbol-name root-definition))))
        (json-encode
         `((exists . t)
           (function_name . ,function-name)
           (kind . ,(ai-code-mcp--function-kind symbol))
           (interactive . ,(ai-code-mcp--json-bool (commandp symbol)))
           (advised . ,(ai-code-mcp--json-bool
                        (ai-code-mcp--function-advised-p symbol)))
           (aliased_to . ,aliased-to)
           (source_file . ,(symbol-file symbol 'defun))
           (documentation_summary
            . ,(ai-code-mcp--documentation-summary
                (documentation symbol t)))))))))

(defun ai-code-mcp--last-error-json-payload (record)
  "Return a JSON-ready payload for a recorded error RECORD."
  `((recorded . t)
    (error_symbol . ,(alist-get 'error_symbol record))
    (error_message . ,(alist-get 'error_message record))
    (context . ,(alist-get 'context record))
    (signal . ,(ai-code-mcp--json-bool (alist-get 'signal record)))
    (timestamp . ,(alist-get 'timestamp record))
    (frame_count . ,(alist-get 'frame_count record))
    (frames . ,(vconcat (alist-get 'frames record)))))

(defun ai-code-mcp--empty-last-error-json-payload ()
  "Return a JSON-ready payload when no error has been recorded."
  `((recorded . :json-false)
    (error_symbol . nil)
    (error_message . nil)
    (context . nil)
    (signal . :json-false)
    (timestamp . nil)
    (frame_count . 0)
    (frames . nil)))

(defun ai-code-mcp-get-last-error-backtrace ()
  "Return a JSON snapshot of the most recently recorded Emacs error."
  (json-encode
   (if ai-code-mcp--last-error-record
       (ai-code-mcp--last-error-json-payload ai-code-mcp--last-error-record)
     (ai-code-mcp--empty-last-error-json-payload))))

(defun ai-code-mcp--feature-providers (feature-symbol)
  "Return `load-history' files that provided FEATURE-SYMBOL."
  (let (providers)
    (dolist (entry load-history (delete-dups (nreverse providers)))
      (when (and (car entry)
                 (member `(provide . ,feature-symbol) (cdr entry)))
        (push (car entry) providers)))))

(defun ai-code-mcp--feature-library-matches (feature-name)
  "Return `load-path' files that look like FEATURE-NAME libraries."
  (let (matches)
    (dolist (directory load-path (delete-dups (nreverse matches)))
      (when (stringp directory)
        (dolist (suffix '(".el" ".elc" ".eln"))
          (let ((candidate (expand-file-name (concat feature-name suffix)
                                             directory)))
            (when (file-exists-p candidate)
              (push candidate matches))))))))

(defun ai-code-mcp--valid-feature-name-p (feature-name)
  "Return non-nil when FEATURE-NAME is a non-empty string."
  (and (stringp feature-name)
       (not (string-empty-p feature-name))))

(defun ai-code-mcp--invalid-feature-load-state-payload (feature-name)
  "Return a JSON payload for invalid FEATURE-NAME input."
  `((feature_name . ,feature-name)
    (loaded . :json-false)
    (error_message . "feature_name must be a non-empty string")
    (library_path . nil)
    (provided_by_files . nil)
    (load_path_matches . nil)))

(defun ai-code-mcp-get-feature-load-state (feature-name)
  "Return JSON load-state details for FEATURE-NAME."
  (if (not (ai-code-mcp--valid-feature-name-p feature-name))
      (json-encode
       (ai-code-mcp--invalid-feature-load-state-payload feature-name))
    (let* ((feature-symbol (intern-soft feature-name))
           (loaded (and feature-symbol (featurep feature-symbol)))
           (library-path (locate-library feature-name))
           (providers (and feature-symbol
                           (ai-code-mcp--feature-providers feature-symbol))))
      (json-encode
       `((feature_name . ,feature-name)
         (loaded . ,(ai-code-mcp--json-bool loaded))
         (error_message . nil)
         (library_path . ,library-path)
         (provided_by_files . ,(vconcat providers))
         (load_path_matches
          . ,(vconcat (ai-code-mcp--feature-library-matches feature-name))))))))

(defun ai-code-mcp-get-recent-messages (&optional limit)
  "Return a JSON payload for recent messages using LIMIT."
  (let* ((limit (or limit 50))
         (messages (ai-code-mcp--message-lines)))
    (unless (and (integerp limit) (> limit 0))
      (error "Argument limit must be a positive integer"))
    (setq messages (last messages (min limit (length messages))))
    (json-encode
     `((ok . t)
       (limit . ,limit)
       (messages . ,(vconcat messages))))))

(add-to-list 'ai-code-mcp-server-tool-setup-functions
             #'ai-code-mcp-debug-tools-setup)

(provide 'ai-code-mcp-debug-tools)

;;; ai-code-mcp-debug-tools.el ends here
