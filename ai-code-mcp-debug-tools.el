;;; ai-code-mcp-debug-tools.el --- Optional MCP debugging tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional MCP debugging tools for inspecting Emacs variables,
;; functions, features, messages, and the most recent command error.

;;; Code:

(require 'cl-lib)
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

(defcustom ai-code-mcp-debug-tools-enable-eval-elisp nil
  "When non-nil, register the `eval_elisp' MCP tool."
  :type 'boolean
  :group 'ai-code-mcp-debug-tools)

(defcustom ai-code-mcp-debug-tools-allow-effect-eval nil
  "When non-nil, allow `eval_elisp' to run in effect mode."
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

(defconst ai-code-mcp-debug-tools--eval-spec
  '(:function ai-code-mcp-eval-elisp
    :name "eval_elisp"
    :description "Evaluate a single Emacs Lisp form."
    :args ((:name "code"
            :type string
            :description "Single Emacs Lisp form to evaluate.")
           (:name "mode"
            :type string
            :description "Evaluation mode."
            :optional t)
           (:name "buffer_name"
            :type string
            :description "Optional buffer context."
            :optional t)
           (:name "file_path"
            :type string
            :description "Optional file buffer context."
            :optional t)
           (:name "capture_messages"
            :type boolean
            :description "When non-nil, capture new messages."
            :optional t)
           (:name "include_backtrace"
            :type boolean
            :description "When non-nil, include a backtrace on failure."
            :optional t)
           (:name "timeout_ms"
            :type integer
            :description "Maximum time budget for the evaluation."
            :optional t)))
  "Optional MCP eval tool specification.")

(defconst ai-code-mcp-debug-tools--always-denied-symbols
  '(append-to-file async-shell-command call-interactively call-process
    command-execute compile copy-file delete-file delete-frame
    delete-window eval funcall kill-buffer load load-file
    make-directory make-network-process make-process rename-file
    recompile require save-buffer save-buffers-kill-emacs shell-command
    start-process url-retrieve write-file write-region)
  "Symbols that `eval_elisp' rejects in every mode.")

(defconst ai-code-mcp-debug-tools--query-denied-symbols
  '(add-hook delete-region erase-buffer indent-region insert kill-region
    newline put remove-hook replace-buffer-contents set setf setq
    setq-local switch-to-buffer yank)
  "Additional symbols that `eval_elisp' rejects in query mode.")

(defun ai-code-mcp-debug-tools-setup ()
  "Register optional MCP debugging tools when enabled."
  (when ai-code-mcp-debug-tools-enabled
    (ai-code-mcp--ensure-error-capture)
    (dolist (tool ai-code-mcp-debug-tools--specs)
      (apply #'ai-code-mcp-make-tool tool))
    (when ai-code-mcp-debug-tools-enable-eval-elisp
      (apply #'ai-code-mcp-make-tool ai-code-mcp-debug-tools--eval-spec))))

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

(defun ai-code-mcp-debug-tools--bool-arg (value default)
  "Return boolean VALUE, falling back to DEFAULT when VALUE is omitted."
  (cond
   ((null value) default)
   ((eq value :json-false) nil)
   (t (not (null value)))))

(defun ai-code-mcp-debug-tools--selected-window ()
  "Return the selected window, falling back to the frame root window."
  (or (and (window-live-p (selected-window))
           (selected-window))
      (frame-root-window)))

(defun ai-code-mcp-debug-tools--resolve-eval-buffer (&optional buffer-name file-path)
  "Return the requested live buffer from BUFFER-NAME or FILE-PATH."
  (when (and buffer-name file-path)
    (error "Arguments buffer_name and file_path are mutually exclusive"))
  (cond
   (buffer-name
    (or (get-buffer buffer-name)
        (error "Buffer not found: %s" buffer-name)))
   (file-path
    (find-file-noselect (expand-file-name file-path) t))
   (t
    (window-buffer (ai-code-mcp-debug-tools--selected-window)))))

(defun ai-code-mcp-debug-tools--point-line-column (buffer point)
  "Return line and column for POINT in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      `((line . ,(line-number-at-pos))
        (column . ,(current-column))))))

(defun ai-code-mcp-debug-tools--modified-buffer-snapshot ()
  "Return an alist of live buffers and their modified states."
  (mapcar (lambda (buffer)
            (cons buffer
                  (with-current-buffer buffer
                    (buffer-modified-p))))
          (buffer-list)))

(defun ai-code-mcp-debug-tools--changed-buffers (before)
  "Return a list of buffers whose modified state changed after BEFORE."
  (let ((before-table (make-hash-table :test 'eq))
        changed)
    (dolist (entry before)
      (puthash (car entry) (cdr entry) before-table))
    (dolist (buffer (buffer-list) (nreverse changed))
      (let ((before-modified (gethash buffer before-table :missing))
            (after-modified (with-current-buffer buffer
                              (buffer-modified-p))))
        (when (or (eq before-modified :missing)
                  (not (eq before-modified after-modified)))
          (push `((buffer_name . ,(buffer-name buffer))
                  (file_path . ,(buffer-file-name buffer))
                  (modified . ,(ai-code-mcp--json-bool
                                after-modified)))
                changed))))))

(defun ai-code-mcp-debug-tools--context-summary (buffer)
  "Return a summary of BUFFER after an evaluation."
  (let ((position (ai-code-mcp-debug-tools--point-line-column
                   buffer
                   (with-current-buffer buffer (point)))))
    `((buffer_name . ,(buffer-name buffer))
      (file_path . ,(buffer-file-name buffer))
      (line . ,(alist-get 'line position))
      (column . ,(alist-get 'column position)))))

(defun ai-code-mcp-debug-tools--symbol-denied-p (form denied-symbols)
  "Return the first symbol in FORM that appears in DENIED-SYMBOLS."
  (cond
   ((symbolp form)
    (and (memq form denied-symbols) form))
   ((consp form)
    (let ((head (car form)))
      (cond
       ((and (symbolp head)
              (memq head denied-symbols))
        head)
       (t
        (or (ai-code-mcp-debug-tools--symbol-denied-p head denied-symbols)
            (cl-some
             (lambda (item)
               (ai-code-mcp-debug-tools--symbol-denied-p
                item denied-symbols))
             (cdr form)))))))
   ((vectorp form)
    (cl-some
     (lambda (item)
       (ai-code-mcp-debug-tools--symbol-denied-p item denied-symbols))
     (append form nil)))
   (t nil)))

(defun ai-code-mcp-debug-tools--parse-single-form (code)
  "Parse CODE and return exactly one top-level Emacs Lisp form."
  (let* ((read-result (read-from-string code))
         (form (car read-result))
         (position (cdr read-result))
         (rest (substring code position)))
    (unless (string-match-p "\\`[[:space:]\n\r\t]*\\'" rest)
      (error "Argument code must contain exactly one top-level form"))
    form))

(defun ai-code-mcp-debug-tools--evaluation-messages (before capture-messages)
  "Return messages added after BEFORE when CAPTURE-MESSAGES."
  (if capture-messages
      (nthcdr (length before) (ai-code-mcp--message-lines))
    '()))

(defun ai-code-mcp-debug-tools--error-alist (type message)
  "Return a JSON-ready error payload for TYPE and MESSAGE."
  `((type . ,type)
    (message . ,message)))

(defun ai-code-mcp-debug-tools--backtrace-string ()
  "Return the current backtrace as a string."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (backtrace))
    (buffer-string)))

(defun ai-code-mcp-debug-tools--encode-eval-result
    (mode target-buffer before-messages capture-messages timed-out
          value changed-buffers &optional error-object backtrace)
  "Return a JSON response for MODE in TARGET-BUFFER.
BEFORE-MESSAGES and CAPTURE-MESSAGES control message collection.
TIMED-OUT records timeout state, VALUE carries the result,
CHANGED-BUFFERS lists modified buffers, and ERROR-OBJECT or BACKTRACE
describe failures."
  (json-encode
   `((ok . ,(ai-code-mcp--json-bool (null error-object)))
     (mode . ,mode)
     (value_repr . ,(and (null error-object) (prin1-to-string value)))
     (value_type . ,(and (null error-object)
                         (symbol-name (type-of value))))
     (messages . ,(vconcat
                   (ai-code-mcp-debug-tools--evaluation-messages
                    before-messages
                    capture-messages)))
     (error . ,error-object)
     (backtrace . ,backtrace)
     (changed_buffers . ,(vconcat changed-buffers))
     (context_after . ,(ai-code-mcp-debug-tools--context-summary
                        target-buffer))
     (timed_out . ,(ai-code-mcp--json-bool timed-out)))))

(defun ai-code-mcp-debug-tools--run-eval (form mode target-buffer timeout-ms
                                               capture-messages
                                               include-backtrace)
  "Evaluate FORM in MODE within TARGET-BUFFER using TIMEOUT-MS.
CAPTURE-MESSAGES controls message collection, and INCLUDE-BACKTRACE
keeps the backtrace on failures."
  (let ((before-messages (ai-code-mcp--message-lines))
        (before-snapshot (ai-code-mcp-debug-tools--modified-buffer-snapshot))
        (value nil)
        (timed-out nil)
        (error-object nil)
        (backtrace nil))
    (condition-case err
        (catch 'ai-code-mcp-debug-tools-timeout
          (with-timeout ((/ (float timeout-ms) 1000.0)
                         (setq timed-out t)
                         (throw 'ai-code-mcp-debug-tools-timeout nil))
            (setq value
                  (if (string= mode "query")
                      (save-current-buffer
                        (with-current-buffer target-buffer
                          (save-excursion
                            (save-match-data
                              (save-restriction
                                (eval form t))))))
                    (save-current-buffer
                      (with-current-buffer target-buffer
                        (eval form t)))))))
      (error
       (setq error-object
             (ai-code-mcp-debug-tools--error-alist
              (symbol-name (car err))
              (error-message-string err)))
       (when include-backtrace
         (setq backtrace (ai-code-mcp-debug-tools--backtrace-string)))))
    (when timed-out
      (setq error-object
            (ai-code-mcp-debug-tools--error-alist
             "timeout"
             "Evaluation exceeded the configured timeout")))
    (ai-code-mcp-debug-tools--encode-eval-result
     mode
     target-buffer
     before-messages
     capture-messages
     timed-out
     value
     (ai-code-mcp-debug-tools--changed-buffers before-snapshot)
     error-object
     backtrace)))

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

(defun ai-code-mcp-eval-elisp (code &optional mode buffer-name file-path
                                    capture-messages include-backtrace
                                    timeout-ms)
  "Evaluate CODE as a single form using MODE and BUFFER-NAME.
Return a JSON payload for BUFFER-NAME, FILE-PATH,
CAPTURE-MESSAGES, INCLUDE-BACKTRACE, and TIMEOUT-MS."
  (let* ((mode (or mode "query"))
         (capture-messages (ai-code-mcp-debug-tools--bool-arg
                            capture-messages
                            t))
         (include-backtrace (ai-code-mcp-debug-tools--bool-arg
                             include-backtrace
                             nil))
         (timeout-ms (or timeout-ms 1000))
         (target-buffer (ai-code-mcp-debug-tools--resolve-eval-buffer
                         buffer-name
                         file-path))
         (parse-error nil)
         form
         always-denied
         query-denied)
    (unless (member mode '("query" "effect"))
      (error "Argument mode must be either query or effect"))
    (unless (and (integerp timeout-ms) (> timeout-ms 0))
      (error "Argument timeout_ms must be a positive integer"))
    (condition-case err
        (setq form (ai-code-mcp-debug-tools--parse-single-form code))
      (error
       (setq parse-error err)))
    (cond
     (parse-error
      (ai-code-mcp-debug-tools--encode-eval-result
       mode
       target-buffer
       (ai-code-mcp--message-lines)
       capture-messages
       nil
       nil
       '()
       (ai-code-mcp-debug-tools--error-alist
        (symbol-name (car parse-error))
        (error-message-string parse-error))
       (and include-backtrace
            (ai-code-mcp-debug-tools--backtrace-string))))
     (t
      (setq always-denied
            (ai-code-mcp-debug-tools--symbol-denied-p
             form
             ai-code-mcp-debug-tools--always-denied-symbols))
      (setq query-denied
            (and (string= mode "query")
                 (ai-code-mcp-debug-tools--symbol-denied-p
                  form
                  ai-code-mcp-debug-tools--query-denied-symbols)))
      (cond
       (always-denied
        (ai-code-mcp-debug-tools--encode-eval-result
         mode
         target-buffer
         (ai-code-mcp--message-lines)
         capture-messages
         nil
         nil
         '()
         (ai-code-mcp-debug-tools--error-alist
          "symbol_denied"
          (format "Symbol `%s' is not allowed in eval_elisp"
                  always-denied))
         nil))
       (query-denied
        (ai-code-mcp-debug-tools--encode-eval-result
         mode
         target-buffer
         (ai-code-mcp--message-lines)
         capture-messages
         nil
         nil
         '()
         (ai-code-mcp-debug-tools--error-alist
          "query_symbol_denied"
          (format "Symbol `%s' is not allowed in query mode"
                  query-denied))
         nil))
       ((and (string= mode "effect")
             (not ai-code-mcp-debug-tools-allow-effect-eval))
        (ai-code-mcp-debug-tools--encode-eval-result
         mode
         target-buffer
        (ai-code-mcp--message-lines)
        capture-messages
        nil
        nil
        '()
        (ai-code-mcp-debug-tools--error-alist
         "effect_mode_disabled"
         "Effect mode is disabled by configuration")
        nil))
       (t
        (ai-code-mcp-debug-tools--run-eval
         form
         mode
         target-buffer
         timeout-ms
         capture-messages
         include-backtrace)))))))

(add-to-list 'ai-code-mcp-server-tool-setup-functions
             #'ai-code-mcp-debug-tools-setup)

(provide 'ai-code-mcp-debug-tools)

;;; ai-code-mcp-debug-tools.el ends here
