;;; ai-code-mcp-editor-tools.el --- Optional editor-session MCP tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional Emacs-session MCP tools that expose editor-specific state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(declare-function ai-code-mcp-make-tool "ai-code-mcp-server")

(defvar ai-code-mcp-server-tool-setup-functions nil)

(defgroup ai-code-mcp-editor-tools nil
  "Optional editor-session MCP tools."
  :group 'ai-code-mcp-server
  :prefix "ai-code-mcp-editor-tools-")

(defcustom ai-code-mcp-editor-tools-enabled nil
  "When non-nil, register optional editor-session MCP tools."
  :type 'boolean
  :group 'ai-code-mcp-editor-tools)

(defcustom ai-code-mcp-editor-tools-allow-effect-eval nil
  "When non-nil, allow `eval_elisp' to run in effect mode."
  :type 'boolean
  :group 'ai-code-mcp-editor-tools)

(defconst ai-code-mcp-editor-tools--specs
  '((:function ai-code-mcp-editor-state
     :name "editor_state"
     :description "Get the current editor state."
     :args nil)
    (:function ai-code-mcp-visible-buffers
     :name "visible_buffers"
     :description "List buffers visible in the selected frame."
     :args nil)
    (:function ai-code-mcp-messages-tail
     :name "messages_tail"
     :description "Get recent Emacs messages."
     :args ((:name "limit"
             :type integer
             :description "Maximum number of messages to return."
             :optional t)))
    (:function ai-code-mcp-eval-elisp
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
             :optional t))))
  "Optional editor-session MCP tool specifications.")

(defconst ai-code-mcp-editor-tools--always-denied-symbols
  '(append-to-file async-shell-command call-interactively call-process
    command-execute compile copy-file delete-file delete-frame
    delete-window eval funcall kill-buffer load load-file
    make-directory make-network-process make-process rename-file
    recompile require save-buffer save-buffers-kill-emacs shell-command
    start-process url-retrieve write-file write-region)
  "Symbols that `eval_elisp' rejects in every mode.")

(defconst ai-code-mcp-editor-tools--query-denied-symbols
  '(add-hook delete-region erase-buffer indent-region insert kill-region
    newline put remove-hook replace-buffer-contents set setf setq
    setq-local switch-to-buffer yank)
  "Additional symbols that `eval_elisp' rejects in query mode.")

(defun ai-code-mcp-editor-tools-setup ()
  "Register optional editor-session MCP tools when enabled."
  (when ai-code-mcp-editor-tools-enabled
    (dolist (tool ai-code-mcp-editor-tools--specs)
      (apply #'ai-code-mcp-make-tool tool))))

(defun ai-code-mcp-editor-tools--json-bool (value)
  "Return VALUE as a JSON boolean token."
  (if value t :json-false))

(defun ai-code-mcp-editor-tools--bool-arg (value default)
  "Return boolean VALUE, falling back to DEFAULT when VALUE is omitted."
  (cond
   ((null value) default)
   ((eq value :json-false) nil)
   (t (not (null value)))))

(defun ai-code-mcp-editor-tools--selected-window ()
  "Return the selected window, falling back to the frame root window."
  (or (and (window-live-p (selected-window))
           (selected-window))
      (frame-root-window)))

(defun ai-code-mcp-editor-tools--resolve-buffer (&optional buffer-name file-path)
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
    (window-buffer (ai-code-mcp-editor-tools--selected-window)))))

(defun ai-code-mcp-editor-tools--point-line-column (buffer point)
  "Return line and column for POINT in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      `((line . ,(line-number-at-pos))
        (column . ,(current-column))))))

(defun ai-code-mcp-editor-tools--region-state (buffer point)
  "Return region metadata for BUFFER at POINT."
  (with-current-buffer buffer
    (let ((mark (mark t)))
      (if (and mark mark-active)
          (let* ((start (min point mark))
                 (end (max point mark))
                 (start-pos (ai-code-mcp-editor-tools--point-line-column
                             buffer start))
                 (end-pos (ai-code-mcp-editor-tools--point-line-column
                           buffer end)))
            `((region_active . t)
              (region . ((start . ,start-pos)
                         (end . ,end-pos)))))
        '((region_active . :json-false)
          (region . nil))))))

(defun ai-code-mcp-editor-tools--editor-state-data ()
  "Return an alist describing the selected window buffer."
  (let* ((window (ai-code-mcp-editor-tools--selected-window))
         (buffer (window-buffer window))
         (point (window-point window))
         (position (ai-code-mcp-editor-tools--point-line-column buffer point))
         (region-state (ai-code-mcp-editor-tools--region-state buffer point)))
    (with-current-buffer buffer
      `((ok . t)
        (buffer_name . ,(buffer-name buffer))
        (file_path . ,(buffer-file-name buffer))
        (major_mode . ,(symbol-name major-mode))
        (modified . ,(ai-code-mcp-editor-tools--json-bool
                      (buffer-modified-p)))
        (read_only . ,(ai-code-mcp-editor-tools--json-bool buffer-read-only))
        (narrowed . ,(ai-code-mcp-editor-tools--json-bool
                      (buffer-narrowed-p)))
        (point . ,point)
        (line . ,(alist-get 'line position))
        (column . ,(alist-get 'column position))
        (region_active . ,(alist-get 'region_active region-state))
        (region . ,(alist-get 'region region-state))
        (default_directory . ,default-directory)))))

(defun ai-code-mcp-editor-state ()
  "Return a JSON payload for the current editor state."
  (json-encode (ai-code-mcp-editor-tools--editor-state-data)))

(defun ai-code-mcp-editor-tools--visible-buffer-entry (window index)
  "Return a visible buffer entry for WINDOW at INDEX."
  (let* ((buffer (window-buffer window))
         (point (window-point window))
         (position (ai-code-mcp-editor-tools--point-line-column buffer point)))
    (with-current-buffer buffer
      `((index . ,index)
        (buffer_name . ,(buffer-name buffer))
        (file_path . ,(buffer-file-name buffer))
        (major_mode . ,(symbol-name major-mode))
        (modified . ,(ai-code-mcp-editor-tools--json-bool
                      (buffer-modified-p)))
        (line . ,(alist-get 'line position))
        (column . ,(alist-get 'column position))))))

(defun ai-code-mcp-visible-buffers ()
  "Return a JSON payload for visible buffers."
  (let* ((selected-window (ai-code-mcp-editor-tools--selected-window))
         (windows (window-list (selected-frame) 'no-minibuffer))
         (entries (cl-mapcar #'ai-code-mcp-editor-tools--visible-buffer-entry
                             windows
                             (number-sequence 0 (1- (length windows)))))
         (selected-index (or (cl-position selected-window windows) 0)))
    (json-encode
     `((ok . t)
       (selected_index . ,selected-index)
       (items . ,(vconcat entries))))))

(defun ai-code-mcp-editor-tools--message-lines ()
  "Return the current `*Messages*' contents as a list of lines."
  (if-let ((buffer (get-buffer "*Messages*")))
      (with-current-buffer buffer
        (split-string (buffer-substring-no-properties (point-min) (point-max))
                      "\n"
                      t))
    '()))

(defun ai-code-mcp-messages-tail (&optional limit)
  "Return a JSON payload for recent messages using LIMIT."
  (let* ((limit (or limit 50))
         (messages (ai-code-mcp-editor-tools--message-lines)))
    (unless (and (integerp limit) (> limit 0))
      (error "Argument limit must be a positive integer"))
    (setq messages (last messages (min limit (length messages))))
    (json-encode
     `((ok . t)
       (limit . ,limit)
       (messages . ,(vconcat messages))))))

(defun ai-code-mcp-editor-tools--modified-buffer-snapshot ()
  "Return an alist of live buffers and their modified states."
  (mapcar (lambda (buffer)
            (cons buffer
                  (with-current-buffer buffer
                    (buffer-modified-p))))
          (buffer-list)))

(defun ai-code-mcp-editor-tools--changed-buffers (before)
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
                  (modified . ,(ai-code-mcp-editor-tools--json-bool
                                after-modified)))
                changed))))))

(defun ai-code-mcp-editor-tools--context-summary (buffer)
  "Return a summary of BUFFER after an evaluation."
  (let ((position (ai-code-mcp-editor-tools--point-line-column
                   buffer
                   (with-current-buffer buffer (point)))))
    `((buffer_name . ,(buffer-name buffer))
      (file_path . ,(buffer-file-name buffer))
      (line . ,(alist-get 'line position))
      (column . ,(alist-get 'column position)))))

(defun ai-code-mcp-editor-tools--symbol-denied-p (form denied-symbols)
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
        (or (ai-code-mcp-editor-tools--symbol-denied-p head denied-symbols)
            (cl-some
             (lambda (item)
               (ai-code-mcp-editor-tools--symbol-denied-p
                item denied-symbols))
             (cdr form)))))))
   ((vectorp form)
    (cl-some
     (lambda (item)
       (ai-code-mcp-editor-tools--symbol-denied-p item denied-symbols))
     (append form nil)))
   (t nil)))

(defun ai-code-mcp-editor-tools--parse-single-form (code)
  "Parse CODE and return exactly one top-level Emacs Lisp form."
  (let* ((read-result (read-from-string code))
         (form (car read-result))
         (position (cdr read-result))
         (rest (substring code position)))
    (unless (string-match-p "\\`[[:space:]\n\r\t]*\\'" rest)
      (error "Argument code must contain exactly one top-level form"))
    form))

(defun ai-code-mcp-editor-tools--evaluation-messages (before capture-messages)
  "Return messages added after BEFORE when CAPTURE-MESSAGES."
  (if capture-messages
      (nthcdr (length before) (ai-code-mcp-editor-tools--message-lines))
    '()))

(defun ai-code-mcp-editor-tools--error-alist (type message)
  "Return a JSON-ready error payload for TYPE and MESSAGE."
  `((type . ,type)
    (message . ,message)))

(defun ai-code-mcp-editor-tools--backtrace-string ()
  "Return the current backtrace as a string."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (backtrace))
    (buffer-string)))

(defun ai-code-mcp-editor-tools--encode-eval-result
    (mode target-buffer before-messages capture-messages timed-out
          value changed-buffers &optional error-object backtrace)
  "Return a JSON response for MODE in TARGET-BUFFER.
BEFORE-MESSAGES and CAPTURE-MESSAGES control message collection.
TIMED-OUT records timeout state, VALUE carries the result,
CHANGED-BUFFERS lists modified buffers, and ERROR-OBJECT or BACKTRACE
describe failures."
  (json-encode
   `((ok . ,(ai-code-mcp-editor-tools--json-bool (null error-object)))
     (mode . ,mode)
     (value_repr . ,(and (null error-object) (prin1-to-string value)))
     (value_type . ,(and (null error-object)
                         (symbol-name (type-of value))))
     (messages . ,(vconcat
                   (ai-code-mcp-editor-tools--evaluation-messages
                    before-messages
                    capture-messages)))
     (error . ,error-object)
     (backtrace . ,backtrace)
     (changed_buffers . ,(vconcat changed-buffers))
     (context_after . ,(ai-code-mcp-editor-tools--context-summary
                        target-buffer))
     (timed_out . ,(ai-code-mcp-editor-tools--json-bool timed-out)))))

(defun ai-code-mcp-editor-tools--run-eval (form mode target-buffer timeout-ms
                                                capture-messages
                                                include-backtrace)
  "Evaluate FORM in MODE within TARGET-BUFFER using TIMEOUT-MS.
CAPTURE-MESSAGES controls message collection, and INCLUDE-BACKTRACE
keeps the backtrace on failures."
  (let ((before-messages (ai-code-mcp-editor-tools--message-lines))
        (before-snapshot (ai-code-mcp-editor-tools--modified-buffer-snapshot))
        (value nil)
        (timed-out nil)
        (error-object nil)
        (backtrace nil))
    (condition-case err
        (catch 'ai-code-mcp-editor-tools-timeout
          (with-timeout ((/ (float timeout-ms) 1000.0)
                         (setq timed-out t)
                         (throw 'ai-code-mcp-editor-tools-timeout nil))
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
             (ai-code-mcp-editor-tools--error-alist
              (symbol-name (car err))
              (error-message-string err)))
       (when include-backtrace
         (setq backtrace (ai-code-mcp-editor-tools--backtrace-string)))))
    (when timed-out
      (setq error-object
            (ai-code-mcp-editor-tools--error-alist
             "timeout"
             "Evaluation exceeded the configured timeout")))
    (ai-code-mcp-editor-tools--encode-eval-result
     mode
     target-buffer
     before-messages
     capture-messages
     timed-out
     value
     (ai-code-mcp-editor-tools--changed-buffers before-snapshot)
     error-object
     backtrace)))

(defun ai-code-mcp-eval-elisp (code &optional mode buffer-name file-path
                                    capture-messages include-backtrace
                                    timeout-ms)
  "Evaluate CODE as a single form using MODE and BUFFER-NAME.
Return a JSON payload for BUFFER-NAME, FILE-PATH,
CAPTURE-MESSAGES, INCLUDE-BACKTRACE, and TIMEOUT-MS."
  (let* ((mode (or mode "query"))
         (capture-messages (ai-code-mcp-editor-tools--bool-arg
                            capture-messages
                            t))
         (include-backtrace (ai-code-mcp-editor-tools--bool-arg
                             include-backtrace
                             nil))
         (timeout-ms (or timeout-ms 1000))
         (target-buffer (ai-code-mcp-editor-tools--resolve-buffer
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
        (setq form (ai-code-mcp-editor-tools--parse-single-form code))
      (error
       (setq parse-error err)))
    (cond
     (parse-error
      (ai-code-mcp-editor-tools--encode-eval-result
       mode
       target-buffer
       (ai-code-mcp-editor-tools--message-lines)
       capture-messages
       nil
       nil
       '()
       (ai-code-mcp-editor-tools--error-alist
        (symbol-name (car parse-error))
        (error-message-string parse-error))
       (and include-backtrace
            (ai-code-mcp-editor-tools--backtrace-string))))
     (t
      (setq always-denied
            (ai-code-mcp-editor-tools--symbol-denied-p
             form
             ai-code-mcp-editor-tools--always-denied-symbols))
      (setq query-denied
            (and (string= mode "query")
                 (ai-code-mcp-editor-tools--symbol-denied-p
                  form
                  ai-code-mcp-editor-tools--query-denied-symbols)))
      (cond
       (always-denied
        (ai-code-mcp-editor-tools--encode-eval-result
         mode
         target-buffer
         (ai-code-mcp-editor-tools--message-lines)
         capture-messages
         nil
         nil
         '()
         (ai-code-mcp-editor-tools--error-alist
          "symbol_denied"
          (format "Symbol `%s' is not allowed in eval_elisp"
                  always-denied))
         nil))
       (query-denied
        (ai-code-mcp-editor-tools--encode-eval-result
         mode
         target-buffer
         (ai-code-mcp-editor-tools--message-lines)
         capture-messages
         nil
         nil
         '()
         (ai-code-mcp-editor-tools--error-alist
          "query_symbol_denied"
          (format "Symbol `%s' is not allowed in query mode"
                  query-denied))
         nil))
       ((and (string= mode "effect")
             (not ai-code-mcp-editor-tools-allow-effect-eval))
        (ai-code-mcp-editor-tools--encode-eval-result
         mode
         target-buffer
         (ai-code-mcp-editor-tools--message-lines)
         capture-messages
         nil
         nil
         '()
         (ai-code-mcp-editor-tools--error-alist
          "effect_mode_disabled"
          "Effect mode is disabled by configuration")
         nil))
       (t
        (ai-code-mcp-editor-tools--run-eval
         form
         mode
         target-buffer
         timeout-ms
         capture-messages
         include-backtrace)))))))

(add-to-list 'ai-code-mcp-server-tool-setup-functions
             #'ai-code-mcp-editor-tools-setup)

(provide 'ai-code-mcp-editor-tools)

;;; ai-code-mcp-editor-tools.el ends here
