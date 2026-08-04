;;; ai-code-mcp-server.el --- MCP tools core for AI Code Interface -*- lexical-binding: t; -*-

;; Author: Yoav Orot, Kang Tu, Andrew Morrow, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This module provides a transport-agnostic MCP tools core for AI Code
;; Interface.  It handles tool registration, session context, method
;; dispatch, and a small built-in toolset that exposes common Emacs
;; project navigation, diagnostics, and code-intelligence capabilities.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ai-code-mcp-common)
(require 'imenu)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'xref)

(require 'flycheck nil t)
(require 'flymake nil t)

(require 'ai-code-input)

(declare-function treesit-available-p "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-parser-root-node "treesit" (parser))
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-node-start "treesit" (node))
(declare-function treesit-node-end "treesit" (node))
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-end-line "flycheck" (err))
(declare-function flycheck-error-end-column "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-checker "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(declare-function flycheck-running-p "flycheck")
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-backend "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-running-backends "flymake")
(declare-function url-filename "url-parse" (urlobj))
(declare-function url-generic-parse-url "url-parse" (url))
(declare-function url-host "url-parse" (urlobj))
(declare-function url-type "url-parse" (urlobj))

(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)

(defgroup ai-code-mcp-server nil
  "MCP tools core settings for AI Code Interface."
  :group 'ai-code
  :prefix "ai-code-mcp-")

(defcustom ai-code-mcp-server-tools nil
  "List of MCP tool specifications.
Each item is a plist with at least `:function', `:name', and `:description'."
  :type '(repeat sexp)
  :group 'ai-code-mcp-server)

(defcustom ai-code-mcp-default-tool-profile 'debug
  "Default MCP tool exposure profile.
The `core' profile exposes editor and project tools, `debug' additionally
exposes read-only Emacs inspection tools, and `full' also permits tools in the
`eval' category when their own explicit feature gate is enabled."
  :type '(choice (const core) (const debug) (const full))
  :group 'ai-code-mcp-server)

(defcustom ai-code-mcp-diagnostics-backend 'auto
  "Backend used by `ai-code-mcp-get-diagnostics'.
Use `auto' to prefer Flycheck and then Flymake when available."
  :type '(choice (const :tag "Automatic detection" auto)
                 (const :tag "Flycheck" flycheck)
                 (const :tag "Flymake" flymake))
  :group 'ai-code-mcp-server)

(defcustom ai-code-mcp-diagnostics-max-report-diagnostics 200
  "Maximum number of diagnostics listed in a `get_diagnostics' report.
When a report would exceed this many diagnostics, the observation envelope
lists only the first this-many and records the truncation in its summary, so a
large project cannot overflow the model context.  The summary always reports
the true totals.  Set to nil to disable truncation; otherwise the value must
be a non-negative integer."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'ai-code-mcp-server)

(defvar ai-code-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping MCP session ids to session metadata.")

(defvar ai-code-mcp--current-session-id nil
  "Dynamically bound MCP session id for the current tool invocation.")

(defvar ai-code-mcp--current-protocol-version nil
  "Dynamically bound MCP version for the current request.")

(defvar ai-code-mcp--diagnostics-baselines (make-hash-table :test 'equal)
  "Hash table mapping MCP session ids to recorded diagnostics baselines.
Each value is a hash table mapping diagnostic identity strings to counts.
The baseline lets `get_diagnostics' report only NEW diagnostics so the
agent can verify it introduced no regressions, without tracking the
baseline in the model context itself.")

(define-error 'ai-code-mcp-protocol-error "MCP protocol error")

(defun ai-code-mcp-signal-protocol-error (code message &optional data)
  "Signal an MCP protocol error with CODE, MESSAGE, and optional DATA."
  (signal 'ai-code-mcp-protocol-error (list code message data)))

(defconst ai-code-mcp--protocol-version "2025-11-25"
  "Preferred initialization-based MCP protocol version.")

(defconst ai-code-mcp--modern-protocol-version "2026-07-28"
  "Stateless MCP protocol version supported by the server.")

(defconst ai-code-mcp--legacy-protocol-versions
  '("2025-11-25" "2025-06-18" "2025-03-26")
  "Initialization-based MCP protocol versions supported by the server.")

(defconst ai-code-mcp--builtin-tool-specs
  '((:function ai-code-mcp-project-info
     :name "project_info"
     :description "Get information about the current project context."
     :args nil)
    (:function ai-code-mcp-editor-state
     :name "editor_state"
     :description "Get the current editor state."
     :args nil)
    (:function ai-code-mcp-visible-buffers
     :name "visible_buffers"
     :description "List buffers visible in the selected frame."
     :args nil)
    (:function ai-code-mcp-buffer-query
     :name "buffer_query"
     :description "Read contents from an Emacs buffer by line range."
     :args ((:name "buffer_name"
             :type string
             :description "Name of the buffer to read.")
            (:name "start_line"
             :type integer
             :description "1-based first line to read."
             :optional t)
            (:name "num_lines"
             :type integer
             :description "Number of lines to read from start_line."
             :optional t)))
    (:function ai-code-mcp-get-diagnostics
     :name "get_diagnostics"
     :description "Get language diagnostics as an observation envelope (status, summary, files, next_actions, artifacts) for a file or the active project.  Pass since=\"baseline\" to report only diagnostics that are new since diagnostics_baseline was called."
     :args ((:name "uri"
             :type string
             :description "Optional file URI to inspect."
             :optional t)
            (:name "since"
             :type string
             :description "Optional.  Use \"baseline\" to report only diagnostics new since the last diagnostics_baseline call."
             :optional t)))
    (:function ai-code-mcp-diagnostics-baseline
     :name "diagnostics_baseline"
     :description "Record current project diagnostics as a baseline.  After editing, call get_diagnostics with since=\"baseline\" to verify no new diagnostics were introduced."
     :args nil)
    (:function ai-code-mcp-get-project-files
     :name "get_project_files"
     :description "List regular files in the current project."
     :args nil)
    (:function ai-code-mcp-get-project-buffers
     :name "get_project_buffers"
     :description "List open buffers that belong to the current project."
     :args nil)
    (:function ai-code-mcp-notify-user
     :name "notify_user"
     :description "Show a notification to the Emacs user."
     :args ((:name "message_text"
              :type string
             :description "Notification text to show in Emacs.")))
    (:function ai-code-mcp-imenu-list-symbols
     :name "imenu_list_symbols"
     :description "List useful symbols in a file via imenu."
     :args ((:name "file_path"
             :type string
             :description "Path to the file to inspect.")))
    (:function ai-code-mcp-xref-find-references
     :name "xref_find_references"
     :description "Find references to an identifier in project context."
     :args ((:name "identifier"
             :type string
             :description "Identifier to resolve.")
            (:name "file_path"
             :type string
             :description "Path to the file that provides backend context.")))
    (:function ai-code-mcp-xref-find-definitions-at-point
     :name "xref_find_definitions_at_point"
     :description "Find definitions of the identifier at a file location."
     :args ((:name "file_path"
             :type string
             :description "Path to the file that provides backend context.")
            (:name "line"
             :type integer
             :description "1-based line number.")
            (:name "column"
             :type integer
             :description "0-based column number.")))
    (:function ai-code-mcp-treesit-info
     :name "treesit_info"
     :description "Return tree-sitter node information for a file location."
     :args ((:name "file_path"
             :type string
             :description "Path to the file to inspect.")
            (:name "line"
             :type integer
             :description "1-based line number."
             :optional t)
            (:name "column"
             :type integer
             :description "0-based column number."
             :optional t)
            (:name "whole_file"
             :type boolean
             :description "When non-nil, inspect the root node."
             :optional t))))
  "Built-in MCP tool specifications.

The default tool list includes:
- `project_info'
- `editor_state'
- `visible_buffers'
- `buffer_query'
- `get_diagnostics'
- `diagnostics_baseline'
- `get_project_files'
- `get_project_buffers'
- `notify_user'
- `imenu_list_symbols'
- `xref_find_references'
- `xref_find_definitions_at_point'
- `treesit_info'")

(defun ai-code-mcp-make-tool (&rest slots)
  "Create an MCP tool specification from SLOTS and register it.
Required keys are `:function', `:name', and `:description'."
  (let ((function (plist-get slots :function))
        (name (plist-get slots :name))
        (description (plist-get slots :description))
        (args (plist-get slots :args))
        (category (plist-get slots :category))
        (annotations (plist-get slots :annotations))
        (output-schema (plist-get slots :output-schema))
        spec)
    (unless function
      (error "Tool :function is required"))
    (unless name
      (error "Tool :name is required"))
    (unless description
      (error "Tool :description is required"))
    (setq spec (list :function function
                     :name name
                     :description description))
    (when args
      (setq spec (plist-put spec :args args)))
    (when category
      (setq spec (plist-put spec :category category)))
    (when annotations
      (setq spec (plist-put spec :annotations annotations)))
    (when output-schema
      (setq spec (plist-put spec :output-schema output-schema)))
    (setq ai-code-mcp-server-tools
          (append
           (seq-remove
            (lambda (tool)
              (equal (plist-get tool :name) name))
            ai-code-mcp-server-tools)
           (list spec)))
    spec))

(defun ai-code-mcp-register-session (session-id project-dir buffer &optional metadata)
  "Register MCP SESSION-ID with PROJECT-DIR, source BUFFER, and METADATA."
  (let ((context (list :project-dir project-dir
                       :buffer buffer
                       :source-buffer buffer
                       :state 'ready
                       :start-time (current-time))))
    (while metadata
      (setq context (plist-put context (pop metadata) (pop metadata))))
    (puthash session-id context ai-code-mcp--sessions)
    (ai-code-mcp-update-source-context session-id buffer)))

(defun ai-code-mcp-unregister-session (session-id)
  "Unregister MCP SESSION-ID and free its recorded diagnostics baseline."
  (remhash session-id ai-code-mcp--sessions)
  (remhash session-id ai-code-mcp--diagnostics-baselines))

(defun ai-code-mcp-get-session-context (&optional session-id)
  "Return session context for SESSION-ID or the current session."
  (gethash (or session-id ai-code-mcp--current-session-id)
           ai-code-mcp--sessions))

(defun ai-code-mcp-session-count ()
  "Return the number of registered MCP application sessions."
  (hash-table-count ai-code-mcp--sessions))

(defun ai-code-mcp-find-session-by-token (token)
  "Return the session id and context authenticated by TOKEN."
  (when (stringp token)
    (catch 'found
      (maphash
       (lambda (session-id context)
         (when (and (stringp (plist-get context :token))
                    (string= token (plist-get context :token))
                    (not (ai-code-mcp--session-expired-p context)))
           (throw 'found (cons session-id context))))
       ai-code-mcp--sessions)
      nil)))

(defun ai-code-mcp--session-expired-p (context)
  "Return non-nil when CONTEXT has passed its authentication expiry."
  (when-let ((expires-at (plist-get context :expires-at)))
    (time-less-p expires-at (current-time))))

(defun ai-code-mcp-update-source-context (session-id source-buffer)
  "Snapshot SOURCE-BUFFER as the prompt origin for SESSION-ID."
  (let ((context (ai-code-mcp-get-session-context session-id)))
    (unless context
      (error "Unknown MCP session: %s" session-id))
    (unless (buffer-live-p source-buffer)
      (error "MCP source buffer is not live"))
    (let ((snapshot
           (with-current-buffer source-buffer
             (let ((position (ai-code-mcp--point-line-column
                              source-buffer (point))))
               (list :buffer source-buffer
                     :point (point)
                     :line (alist-get 'line position)
                     :column (alist-get 'column position)
                     :modification-tick (buffer-chars-modified-tick)
                     :captured-at (current-time))))))
      (setq context (plist-put context :buffer source-buffer))
      (setq context (plist-put context :source-buffer source-buffer))
      (setq context (plist-put context :source-snapshot snapshot))
      (setq context (plist-put context :updated-at (current-time)))
      (puthash session-id context ai-code-mcp--sessions)
      context)))

(defun ai-code-mcp-attach-agent-buffer (session-id agent-buffer)
  "Attach AGENT-BUFFER to SESSION-ID without replacing its source."
  (let ((context (ai-code-mcp-get-session-context session-id)))
    (unless context
      (error "Unknown MCP session: %s" session-id))
    (setq context (plist-put context :agent-buffer agent-buffer))
    (setq context (plist-put context :state 'ready))
    (setq context (plist-put context :updated-at (current-time)))
    (puthash session-id context ai-code-mcp--sessions)
    context))

(defun ai-code-mcp-begin-legacy-session
    (session-id transport-session-id protocol-version client-info)
  "Begin legacy MCP initialization for SESSION-ID.
TRANSPORT-SESSION-ID identifies subsequent HTTP requests, while
PROTOCOL-VERSION and CLIENT-INFO record the negotiated client metadata."
  (let ((context (ai-code-mcp-get-session-context session-id)))
    (unless context
      (error "Unknown MCP session: %s" session-id))
    (setq context (plist-put context :transport-session-id transport-session-id))
    (setq context (plist-put context :protocol-version protocol-version))
    (setq context (plist-put context :client-info client-info))
    (setq context (plist-put context :state 'initializing))
    (setq context (plist-put context :updated-at (current-time)))
    (puthash session-id context ai-code-mcp--sessions)
    context))

(defun ai-code-mcp-complete-legacy-session
    (session-id transport-session-id protocol-version)
  "Complete legacy initialization for SESSION-ID.
TRANSPORT-SESSION-ID and PROTOCOL-VERSION must match the initialized session."
  (let ((context (ai-code-mcp-get-session-context session-id)))
    (unless (and context
                 (eq (plist-get context :state) 'initializing)
                 (equal transport-session-id
                        (plist-get context :transport-session-id))
                 (equal protocol-version (plist-get context :protocol-version)))
      (error "Invalid MCP initialized notification"))
    (setq context (plist-put context :state 'ready))
    (setq context (plist-put context :updated-at (current-time)))
    (puthash session-id context ai-code-mcp--sessions)
    context))

(defmacro ai-code-mcp-with-session-context (session-id &rest body)
  "Run BODY with SESSION-ID project context."
  (declare (indent 1))
  `(let* ((context (ai-code-mcp-get-session-context ,session-id))
          (project-dir (plist-get context :project-dir))
          (buffer (plist-get context :buffer)))
     (if (and buffer (buffer-live-p buffer))
         (with-current-buffer buffer
           (let ((default-directory (if project-dir
                                        (file-name-as-directory project-dir)
                                      default-directory)))
             ,@body))
       (let ((default-directory (if project-dir
                                    (file-name-as-directory project-dir)
                                  default-directory)))
         ,@body))))

(defun ai-code-mcp-dispatch (method &optional params)
  "Dispatch MCP METHOD using PARAMS."
  (pcase method
    ("initialize" (ai-code-mcp--initialize params))
    ("server/discover" (ai-code-mcp--discover))
    ("ping" '())
    ("tools/list" (ai-code-mcp--tools-list))
    ("tools/call" (ai-code-mcp--tools-call params))
    (_ (ai-code-mcp-signal-protocol-error
        -32601 (format "Method not found: %s" method)))))

(defun ai-code-mcp-builtins-setup ()
  "Register the built-in common Emacs MCP tools."
  (interactive)
  (dolist (tool ai-code-mcp--builtin-tool-specs)
    (apply #'ai-code-mcp-make-tool
           (append tool
                   (list :category 'core
                         :annotations
                         (ai-code-mcp--builtin-tool-annotations
                          (plist-get tool :name))))))
  (dolist (setup-fn ai-code-mcp-server-tool-setup-functions)
    (funcall setup-fn)))

(defun ai-code-mcp--builtin-tool-annotations (name)
  "Return effect annotations for built-in tool NAME."
  (let ((read-only (not (member name '("diagnostics_baseline"
                                       "notify_user")))))
    `((title . ,(capitalize (string-replace "_" " " name)))
      (readOnlyHint . ,(ai-code-mcp--json-bool read-only))
      (destructiveHint . :json-false)
      (idempotentHint . ,(ai-code-mcp--json-bool read-only))
      (openWorldHint . :json-false))))

(defun ai-code-mcp--ensure-builtins ()
  "Ensure built-in MCP tools are registered."
  (unless (ai-code-mcp--find-tool-spec "project_info")
    (ai-code-mcp-builtins-setup)))

(defun ai-code-mcp-project-info ()
  "Return a short textual description of the active project context."
  (let* ((project-dir (ai-code-mcp--project-directory))
         (active-buffer (current-buffer))
         (file-count (ai-code-mcp--count-project-files project-dir)))
    (format "Project: %s\nBuffer: %s\nFiles: %d"
            project-dir
            (if (buffer-live-p active-buffer)
                (buffer-name active-buffer)
              "No active buffer")
            file-count)))

(defun ai-code-mcp--selected-window ()
  "Return the selected window, falling back to the frame root window."
  (or (and (window-live-p (selected-window))
           (selected-window))
      (frame-root-window)))

(defun ai-code-mcp--point-line-column (buffer point)
  "Return line and column for POINT in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      `((line . ,(line-number-at-pos))
        (column . ,(current-column))))))

(defun ai-code-mcp--region-state (buffer point)
  "Return region metadata for BUFFER at POINT."
  (with-current-buffer buffer
    (let ((mark (mark t)))
      (if (and mark mark-active)
          (let* ((start (min point mark))
                 (end (max point mark))
                 (start-pos (ai-code-mcp--point-line-column buffer start))
                 (end-pos (ai-code-mcp--point-line-column buffer end)))
            `((region_active . t)
              (region . ((start . ,start-pos)
                         (end . ,end-pos)))))
        '((region_active . :json-false)
          (region . nil))))))

(defun ai-code-mcp--editor-state-data ()
  "Return an alist describing the active session source buffer."
  (let* ((context (ai-code-mcp-get-session-context))
         (source-buffer (plist-get context :buffer))
         (source-snapshot (plist-get context :source-snapshot))
         (window (ai-code-mcp--selected-window)))
    (if (and context (not (buffer-live-p source-buffer)))
        '((ok . :json-false)
          (status . "unavailable")
          (reason . "Prompt source buffer is no longer live"))
      (let* ((buffer (if (buffer-live-p source-buffer)
                         source-buffer
                       (window-buffer window)))
             (point (if (and (eq buffer source-buffer) source-snapshot)
                        (plist-get source-snapshot :point)
                      (window-point window)))
             (position (if (and (eq buffer source-buffer) source-snapshot)
                           `((line . ,(plist-get source-snapshot :line))
                             (column . ,(plist-get source-snapshot :column)))
                         (ai-code-mcp--point-line-column buffer point)))
             (region-point (with-current-buffer buffer
                             (min point (point-max))))
             (region-state (ai-code-mcp--region-state buffer region-point)))
        (with-current-buffer buffer
          `((ok . t)
            (buffer_name . ,(buffer-name buffer))
            (file_path . ,(buffer-file-name buffer))
            (major_mode . ,(symbol-name major-mode))
            (modified . ,(ai-code-mcp--json-bool
                          (buffer-modified-p)))
            (read_only . ,(ai-code-mcp--json-bool buffer-read-only))
            (narrowed . ,(ai-code-mcp--json-bool
                          (buffer-narrowed-p)))
            (point . ,point)
            (line . ,(alist-get 'line position))
            (column . ,(alist-get 'column position))
            (region_active . ,(alist-get 'region_active region-state))
            (region . ,(alist-get 'region region-state))
            (default_directory . ,default-directory)))))))

(defun ai-code-mcp-editor-state ()
  "Return a JSON payload for the current editor state."
  (json-encode (ai-code-mcp--editor-state-data)))

(defun ai-code-mcp--visible-buffer-entry (window index)
  "Return a visible buffer entry for WINDOW at INDEX."
  (let* ((buffer (window-buffer window))
         (point (window-point window))
         (position (ai-code-mcp--point-line-column buffer point)))
    (with-current-buffer buffer
      `((index . ,index)
        (buffer_name . ,(buffer-name buffer))
        (file_path . ,(buffer-file-name buffer))
        (major_mode . ,(symbol-name major-mode))
        (modified . ,(ai-code-mcp--json-bool
                      (buffer-modified-p)))
        (line . ,(alist-get 'line position))
        (column . ,(alist-get 'column position))))))

(defun ai-code-mcp-visible-buffers ()
  "Return a JSON payload for visible buffers."
  (let* ((selected-window (ai-code-mcp--selected-window))
         (windows (window-list (selected-frame) 'no-minibuffer))
         (entries (cl-mapcar #'ai-code-mcp--visible-buffer-entry
                             windows
                             (number-sequence 0 (1- (length windows)))))
         (selected-index (or (cl-position selected-window windows) 0)))
    (json-encode
     `((ok . t)
       (selected_index . ,selected-index)
       (items . ,(vconcat entries))))))

(defun ai-code-mcp--validate-buffer-query-range (start-line num-lines)
  "Validate optional buffer query range arguments START-LINE and NUM-LINES."
  (when (or (and start-line (not num-lines))
            (and num-lines (not start-line)))
    (error "Arguments start_line and num_lines must both be provided or both be omitted"))
  (when (and start-line
             (or (< start-line 1)
                 (< num-lines 1)))
    (error "Arguments start_line and num_lines must be positive integers")))

(defun ai-code-mcp--drop-trailing-newline (text)
  "Return TEXT without a single trailing newline."
  (if (string-suffix-p "\n" text)
      (substring text 0 -1)
    text))

(defun ai-code-mcp-buffer-query (buffer-name &optional start-line num-lines)
  "Return contents from BUFFER-NAME.
When START-LINE and NUM-LINES are non-nil, return only that line range."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (format "Error: Buffer not found: %s" buffer-name)
      (ai-code-mcp--validate-buffer-query-range start-line num-lines)
      (with-current-buffer buffer
        (save-excursion
          (if (not start-line)
              (buffer-substring-no-properties (point-min) (point-max))
            (goto-char (point-min))
            (forward-line (1- start-line))
            (let ((start-pos (point)))
              (forward-line num-lines)
              (ai-code-mcp--drop-trailing-newline
               (buffer-substring-no-properties start-pos (point))))))))))

(defun ai-code-mcp-get-diagnostics (&optional uri since)
  "Return a JSON diagnostics observation envelope for URI or the project.
The envelope includes coverage and freshness metadata so an unavailable or
still-running diagnostics backend cannot be mistaken for a clean result.
When SINCE is the string \"baseline\", report only diagnostics that are new
relative to the baseline recorded by `ai-code-mcp-diagnostics-baseline'."
  (let* ((observation (if uri
                          (ai-code-mcp--diagnostics-for-uri uri)
                        (ai-code-mcp--diagnostics-for-project)))
         (entries (plist-get observation :entries))
         (delta (and (stringp since) (string-equal since "baseline"))))
    (json-encode
     (cond
      ((and delta (not (ai-code-mcp--diagnostics-baseline-recorded-p)))
       (ai-code-mcp--diagnostics-no-baseline-envelope observation))
      (delta
       (ai-code-mcp--diagnostics-envelope
        (ai-code-mcp--diagnostics-new-since-baseline entries)
        'delta
        observation))
      (t
       (ai-code-mcp--diagnostics-envelope entries 'current observation))))))

(defun ai-code-mcp--diagnostics-summary (entries)
  "Return a one-line human summary string for diagnostics ENTRIES."
  (let ((file-count (length entries))
        (total 0)
        (errors 0)
        (warnings 0))
    (dolist (entry entries)
      (seq-doseq (diagnostic (alist-get 'diagnostics entry))
        (setq total (1+ total))
        (pcase (alist-get 'severity diagnostic)
          ("Error" (setq errors (1+ errors)))
          ("Warning" (setq warnings (1+ warnings))))))
    (if (zerop total)
        "No diagnostics found."
      (format "%d diagnostic%s across %d file%s (%d Error, %d Warning)."
              total (if (= total 1) "" "s")
              file-count (if (= file-count 1) "" "s")
              errors warnings))))

(defun ai-code-mcp--diagnostic-action-line (uri diagnostic)
  "Return a one-line fix suggestion for DIAGNOSTIC located via URI."
  (let* ((range (alist-get 'range diagnostic))
         (start (alist-get 'start range))
         (line (alist-get 'line start))
         (column (alist-get 'character start))
         (severity (alist-get 'severity diagnostic))
         (message (alist-get 'message diagnostic))
         (path (or (ignore-errors
                     (ai-code-mcp--display-path
                      (ai-code-mcp--uri-to-file-path uri)))
                   uri)))
    (format "Fix %s at %s:%s:%s - %s"
            (or severity "issue") path (or line "?") (or column "?")
            (or message ""))))

(defun ai-code-mcp--diagnostics-next-actions (entries)
  "Return a vector of fix suggestions for diagnostics ENTRIES."
  (let (actions)
    (dolist (entry entries)
      (let ((uri (alist-get 'uri entry)))
        (seq-doseq (diagnostic (alist-get 'diagnostics entry))
          (push (ai-code-mcp--diagnostic-action-line uri diagnostic) actions))))
    (vconcat (nreverse actions))))

(defun ai-code-mcp--cap-diagnostics-entries (entries limit)
  "Return ENTRIES truncated to at most LIMIT total diagnostics.
The return value is a cons (CAPPED-ENTRIES . SHOWN-COUNT).  Whole and partial
file entries beyond LIMIT are dropped so the listed diagnostics never exceed
LIMIT.  When LIMIT is nil, ENTRIES are returned unchanged."
  (if (null limit)
      (cons entries (ai-code-mcp--diagnostics-total-count entries))
    (let ((remaining (max limit 0))
          capped)
      (dolist (entry entries)
        (when (> remaining 0)
          (let* ((diagnostics (append (alist-get 'diagnostics entry) nil))
                 (take (min remaining (length diagnostics))))
            (when (> take 0)
              (push `((uri . ,(alist-get 'uri entry))
                      (diagnostics . ,(vconcat (seq-take diagnostics take))))
                    capped)
              (setq remaining (- remaining take))))))
      (cons (nreverse capped) (- (max limit 0) remaining)))))

(defun ai-code-mcp--diagnostics-truncation-note (shown total context)
  "Return a truncation note describing SHOWN of TOTAL diagnostics for CONTEXT.
In the `delta' context the caller is already filtering with since=\"baseline\",
so the note only points to per-file (uri) narrowing.  In the `current' context
it also offers since=\"baseline\" as a way to focus on newly introduced
diagnostics -- not as a way to page through the omitted ones."
  (let ((plural (if (= total 1) "" "s")))
    (if (eq context 'delta)
        (format " Listing %d of %d new diagnostic%s here;\
 request a specific file by uri to see the rest."
                shown total plural)
      (format " Listing %d of %d diagnostic%s here; request a specific\
 file by uri to see the rest, or use since=\"baseline\"\
 to focus on diagnostics you introduced."
              shown total plural))))

(defun ai-code-mcp--diagnostics-observation-count (observation predicate)
  "Count targets in OBSERVATION that satisfy PREDICATE."
  (cl-count-if predicate (plist-get observation :targets)))

(defun ai-code-mcp--diagnostics-observation-complete-p (observation)
  "Return non-nil when OBSERVATION covers every requested target.
A nil observation represents a compatibility caller that supplied diagnostic
entries directly and is therefore treated as complete."
  (or (null observation)
      (let ((targets (plist-get observation :targets)))
        (and targets
             (cl-every (lambda (target)
                         (eq (plist-get target :state) 'ready))
                       targets)))))

(defun ai-code-mcp--diagnostics-backend-counts (observation)
  "Return sorted backend coverage entries for OBSERVATION."
  (let ((counts (make-hash-table :test 'eq))
        pairs)
    (dolist (target (plist-get observation :targets))
      (when-let ((backend (plist-get target :backend)))
        (puthash backend (1+ (gethash backend counts 0)) counts)))
    (maphash (lambda (backend count)
               (push (cons backend count) pairs))
             counts)
    (vconcat
     (mapcar (lambda (pair)
               `((name . ,(symbol-name (car pair)))
                 (files . ,(cdr pair))))
             (sort pairs
                   (lambda (left right)
                     (string< (symbol-name (car left))
                              (symbol-name (car right)))))))))

(defun ai-code-mcp--diagnostics-coverage-data (observation)
  "Return JSON-ready diagnostics coverage metadata for OBSERVATION."
  (let* ((targets (plist-get observation :targets))
         (requested (length targets))
         (checked (ai-code-mcp--diagnostics-observation-count
                   observation
                   (lambda (target) (plist-get target :backend))))
         (unavailable (ai-code-mcp--diagnostics-observation-count
                       observation
                       (lambda (target)
                         (eq (plist-get target :state) 'unavailable))))
         (running (ai-code-mcp--diagnostics-observation-count
                   observation
                   (lambda (target)
                     (eq (plist-get target :state) 'running))))
         (complete (ai-code-mcp--diagnostics-observation-complete-p
                    observation))
         (unavailable-targets
          (delq nil
                (mapcar
                 (lambda (target)
                   (when (eq (plist-get target :state) 'unavailable)
                     `((uri . ,(plist-get target :uri))
                       (reason . ,(plist-get target :reason)))))
                 targets))))
    (append
     `((scope . ,(or (plist-get observation :scope) "supplied_entries"))
       (requested_files . ,requested)
       (checked_files . ,checked)
       (unavailable_files . ,unavailable)
       (running_files . ,running)
       (complete . ,(ai-code-mcp--json-bool complete))
       (backends . ,(ai-code-mcp--diagnostics-backend-counts observation))
       (unavailable . ,(vconcat unavailable-targets)))
     (when-let ((reason (plist-get observation :reason)))
       `((reason . ,reason))))))

(defun ai-code-mcp--diagnostics-freshness-data (observation)
  "Return JSON-ready snapshot metadata for diagnostics OBSERVATION."
  (let* ((targets (plist-get observation :targets))
         (modified (ai-code-mcp--diagnostics-observation-count
                    observation
                    (lambda (target) (plist-get target :modified))))
         (buffers
          (mapcar
           (lambda (target)
             `((uri . ,(plist-get target :uri))
               (backend . ,(if-let ((backend (plist-get target :backend)))
                               (symbol-name backend)
                             :null))
               (state . ,(symbol-name (plist-get target :state)))
               (modified . ,(ai-code-mcp--json-bool
                              (plist-get target :modified)))
               (modification_tick
                . ,(or (plist-get target :modification-tick) :null))))
           targets)))
    `((observed_at . ,(or (plist-get observation :observed-at)
                          (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
      (modified_files . ,modified)
      (buffers . ,(vconcat buffers)))))

(defun ai-code-mcp--diagnostics-coverage-summary (observation)
  "Return a concise coverage warning for OBSERVATION, or nil."
  (unless (ai-code-mcp--diagnostics-observation-complete-p observation)
    (let* ((targets (plist-get observation :targets))
           (requested (length targets))
           (checked (ai-code-mcp--diagnostics-observation-count
                     observation
                     (lambda (target) (plist-get target :backend))))
           (running (ai-code-mcp--diagnostics-observation-count
                     observation
                     (lambda (target)
                       (eq (plist-get target :state) 'running)))))
      (cond
       ((zerop requested)
        " Diagnostics unavailable: no open project buffers were in scope.")
       ((zerop checked)
        (format " Diagnostics unavailable: no active checker covered the %d requested file%s."
                requested (if (= requested 1) "" "s")))
       (t
        (format " Diagnostics coverage incomplete: checked %d of %d requested file%s%s."
                checked requested (if (= requested 1) "" "s")
                (if (> running 0)
                    (format "; %d checker%s still running"
                            running (if (= running 1) " is" "s are"))
                  "")))))))

(defun ai-code-mcp--diagnostics-coverage-actions (observation)
  "Return actions needed to complete diagnostics OBSERVATION coverage."
  (unless (ai-code-mcp--diagnostics-observation-complete-p observation)
    (let ((running (ai-code-mcp--diagnostics-observation-count
                    observation
                    (lambda (target)
                      (eq (plist-get target :state) 'running))))
          (unavailable (ai-code-mcp--diagnostics-observation-count
                        observation
                        (lambda (target)
                          (eq (plist-get target :state) 'unavailable)))))
      (delq nil
            (list
             (when (> unavailable 0)
               "Enable Flymake or Flycheck for each unavailable buffer, then run get_diagnostics again.")
             (when (> running 0)
               "Wait for running diagnostics backends to finish, then run get_diagnostics again.")
             (when (zerop (length (plist-get observation :targets)))
               "Open the project files to inspect and enable Flymake or Flycheck before retrying."))))))

(defun ai-code-mcp--diagnostics-envelope (entries &optional context observation)
  "Return a diagnostics observation envelope alist for ENTRIES.
CONTEXT is `current' (default) or `delta'.  In the `delta' context the
status and summary describe diagnostics that are new since the baseline
and express the done-condition the agent must reach (new == 0).
OBSERVATION supplies coverage and freshness metadata for the scan.

The listed `files' and `next_actions' are capped at
`ai-code-mcp-diagnostics-max-report-diagnostics' so a large project cannot
overflow the model context; the summary always reports the true totals and
notes any truncation."
  (let* ((total (ai-code-mcp--diagnostics-total-count entries))
         (has-issues (> total 0))
         (complete (ai-code-mcp--diagnostics-observation-complete-p
                    observation))
         (checked (ai-code-mcp--diagnostics-observation-count
                   observation
                   (lambda (target) (plist-get target :backend))))
         (capped-cell (ai-code-mcp--cap-diagnostics-entries
                       entries ai-code-mcp-diagnostics-max-report-diagnostics))
         (capped (car capped-cell))
         (shown (cdr capped-cell))
         (truncated (> total shown))
         (status (cond
                  (has-issues (if (eq context 'delta) "regression" "issues"))
                  (complete "clean")
                  ((zerop checked) "unavailable")
                  (t "incomplete")))
         (base-summary (cond
                        ((eq context 'delta)
                         (cond
                          (has-issues
                           (concat (ai-code-mcp--diagnostics-summary entries)
                                   " These are NEW versus the baseline;"
                                   " not done until new == 0."))
                          (complete
                           (concat "No new diagnostics versus the baseline;"
                                   " done-condition met (new == 0)."))
                          (t
                           (concat "No new diagnostics were observed versus"
                                   " the baseline, but coverage is incomplete;"
                                   " the done-condition is not established."))))
                        (t (ai-code-mcp--diagnostics-summary entries))))
         (coverage-summary
          (ai-code-mcp--diagnostics-coverage-summary observation))
         (summary (concat
                   base-summary
                   (or coverage-summary "")
                   (if truncated
                       (ai-code-mcp--diagnostics-truncation-note
                        shown total context)
                     "")))
         (actions (append
                   (append (ai-code-mcp--diagnostics-next-actions capped) nil)
                   (ai-code-mcp--diagnostics-coverage-actions observation))))
    `((status . ,status)
      (summary . ,summary)
      (files . ,(vconcat capped))
      (next_actions . ,(vconcat actions))
      (coverage . ,(ai-code-mcp--diagnostics-coverage-data observation))
      (freshness . ,(ai-code-mcp--diagnostics-freshness-data observation))
      (artifacts . ,(vconcat nil)))))

(defun ai-code-mcp--diagnostics-baseline-key ()
  "Return the baseline storage key for the active MCP session."
  (or ai-code-mcp--current-session-id "default"))

(defun ai-code-mcp--diagnostics-baseline-recorded-p ()
  "Return non-nil when a diagnostics baseline exists for the active session."
  (and (gethash (ai-code-mcp--diagnostics-baseline-key)
                ai-code-mcp--diagnostics-baselines)
       t))

(defun ai-code-mcp--diagnostics-total-count (entries)
  "Return the total number of diagnostics across ENTRIES."
  (let ((total 0))
    (dolist (entry entries total)
      (setq total (+ total (length (alist-get 'diagnostics entry)))))))

(defun ai-code-mcp--diagnostics-no-baseline-envelope (&optional observation)
  "Return an envelope explaining that no diagnostics baseline was recorded.
This avoids reporting pre-existing diagnostics as regressions when the agent
requests `since=\"baseline\"' without first calling `diagnostics_baseline'.
OBSERVATION supplies metadata about the attempted diagnostics scan."
  `((status . "no_baseline")
    (summary . ,(concat "No diagnostics baseline recorded for this session;"
                        " current diagnostics are not reported as regressions."))
    (files . ,(vconcat nil))
    (next_actions . ,(vector
                      (concat "Call the diagnostics_baseline MCP tool, then"
                              " re-run get_diagnostics with since=\"baseline\".")))
    (coverage . ,(ai-code-mcp--diagnostics-coverage-data observation))
    (freshness . ,(ai-code-mcp--diagnostics-freshness-data observation))
    (artifacts . ,(vconcat nil))))

(defun ai-code-mcp--diagnostic-identity (uri diagnostic)
  "Return a stable identity string for DIAGNOSTIC located in URI.
Position is intentionally excluded so edits that shift line numbers do not
make a pre-existing diagnostic look new.  Multiplicity is tracked by the
baseline count table, so duplicate diagnostics beyond the recorded count
are still reported as new."
  (format "%s\0%s\0%s\0%s"
          (or uri "")
          (or (alist-get 'severity diagnostic) "")
          (or (alist-get 'source diagnostic) "")
          (or (alist-get 'message diagnostic) "")))

(defun ai-code-mcp--diagnostics-identity-counts (entries)
  "Return a hash table of identity counts for diagnostics ENTRIES."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries counts)
      (let ((uri (alist-get 'uri entry)))
        (seq-doseq (diagnostic (alist-get 'diagnostics entry))
          (let ((identity (ai-code-mcp--diagnostic-identity uri diagnostic)))
            (puthash identity (1+ (gethash identity counts 0)) counts)))))))

(defun ai-code-mcp--diagnostics-new-since-baseline (entries)
  "Return ENTRIES filtered to diagnostics absent from the session baseline.
When no baseline has been recorded, return ENTRIES unchanged."
  (let ((baseline (gethash (ai-code-mcp--diagnostics-baseline-key)
                           ai-code-mcp--diagnostics-baselines)))
    (if (null baseline)
        entries
      (let ((remaining (copy-hash-table baseline)))
        (delq nil
              (mapcar
               (lambda (entry)
                 (let* ((uri (alist-get 'uri entry))
                        (new (delq nil
                                   (mapcar
                                    (lambda (diagnostic)
                                      (let* ((identity
                                              (ai-code-mcp--diagnostic-identity
                                               uri diagnostic))
                                             (count (gethash identity
                                                             remaining 0)))
                                        (if (> count 0)
                                            (progn
                                              (puthash identity (1- count)
                                                       remaining)
                                              nil)
                                          diagnostic)))
                                    (append (alist-get 'diagnostics entry)
                                            nil)))))
                   (when new
                     `((uri . ,uri)
                       (diagnostics . ,(vconcat new))))))
               entries))))))

(defun ai-code-mcp--diagnostics-source-counts (entries)
  "Return an alist of (SOURCE . COUNT) for ENTRIES, ordered by descending count."
  (let ((counts (make-hash-table :test 'equal))
        pairs)
    (dolist (entry entries)
      (seq-doseq (diagnostic (alist-get 'diagnostics entry))
        (let ((source (or (alist-get 'source diagnostic) "unknown")))
          (puthash source (1+ (gethash source counts 0)) counts))))
    (maphash (lambda (source count) (push (cons source count) pairs)) counts)
    (sort pairs (lambda (a b) (> (cdr a) (cdr b))))))

(defun ai-code-mcp--diagnostics-top-sources-string (entries &optional top-n)
  "Return a human string naming the TOP-N diagnostic sources in ENTRIES, or nil.
TOP-N defaults to 3.  The string keeps a compact signal about what produced the
diagnostics without listing every diagnostic."
  (let ((pairs (seq-take (ai-code-mcp--diagnostics-source-counts entries)
                         (or top-n 3))))
    (when pairs
      (mapconcat (lambda (pair) (format "%s (%d)" (car pair) (cdr pair)))
                 pairs ", "))))

(defun ai-code-mcp-diagnostics-baseline ()
  "Record current project diagnostics as the session baseline.
Return a JSON observation envelope describing what was recorded.  Later
`get_diagnostics' calls with `since' set to \"baseline\" report only NEW
diagnostics relative to this snapshot, which lets the agent verify it did
not introduce new problems.  Refuse to record a baseline unless every open
project buffer in scope has an active, idle diagnostics backend."
  (let* ((observation (ai-code-mcp--diagnostics-for-project))
         (entries (plist-get observation :entries))
         (counts (ai-code-mcp--diagnostics-identity-counts entries))
         (count (ai-code-mcp--diagnostics-total-count entries))
         (sources (ai-code-mcp--diagnostics-top-sources-string entries))
         (summary (concat
                   (format "Recorded %d diagnostic%s as the baseline.\
 Edit, then call get_diagnostics with since=\"baseline\" and finish only when\
 status is \"clean\"."
                           count (if (= count 1) "" "s"))
                   (when sources (format " Top sources: %s." sources)))))
    (if (not (ai-code-mcp--diagnostics-observation-complete-p observation))
        (json-encode
         `((status . "baseline_unavailable")
           (summary . ,(concat
                        "Diagnostics baseline was not recorded because"
                        " coverage is incomplete."
                        (or (ai-code-mcp--diagnostics-coverage-summary
                             observation)
                            "")))
           (files . ,(vconcat nil))
           (next_actions
            . ,(vconcat
                (ai-code-mcp--diagnostics-coverage-actions observation)))
           (coverage . ,(ai-code-mcp--diagnostics-coverage-data observation))
           (freshness . ,(ai-code-mcp--diagnostics-freshness-data observation))
           (artifacts . ,(vconcat nil))))
      (puthash (ai-code-mcp--diagnostics-baseline-key) counts
               ai-code-mcp--diagnostics-baselines)
      (json-encode
       `((status . "baseline_recorded")
         (summary . ,summary)
         ;; The baseline is recorded server-side in `counts' (via `puthash'
         ;; above); do not echo the full diagnostics list back into the model
         ;; context.  Returning every project diagnostic here can produce a
         ;; payload far too large to fit in the model context, which defeats
         ;; keeping the baseline out of context in the first place.
         (files . ,(vconcat nil))
         (next_actions . ,(vector
                           (concat "Edit, then call get_diagnostics with"
                                   " since=\"baseline\" on the touched files and"
                                   " finish only when status is \"clean\".")))
         (coverage . ,(ai-code-mcp--diagnostics-coverage-data observation))
         (freshness . ,(ai-code-mcp--diagnostics-freshness-data observation))
         (artifacts . ,(vconcat nil)))))))

(defun ai-code-mcp--diagnostics-for-uri (uri)
  "Return a diagnostics observation for URI.
The observation records an unavailable target when the file has no open
buffer instead of incorrectly treating the missing diagnostics as clean."
  (let* ((file-path (ai-code-mcp--uri-to-file-path uri))
         (buffer (and file-path (get-file-buffer file-path)))
         (target
          (if buffer
              (ai-code-mcp--diagnostics-observe-buffer buffer)
            (list :uri uri
                  :backend nil
                  :state 'unavailable
                  :reason "buffer_not_open"
                  :modified nil
                  :modification-tick nil
                  :diagnostics (vconcat nil)))))
    (ai-code-mcp--make-diagnostics-observation
     "requested_uri" (list target))))

(defun ai-code-mcp--diagnostics-for-project ()
  "Return a diagnostics observation for open project file buffers."
  (let ((project-dir (ai-code-mcp--project-directory))
        targets)
    (dolist (buffer (buffer-list))
      (when-let ((file-path (buffer-file-name buffer)))
        (when (or (not project-dir)
                  (file-in-directory-p file-path project-dir))
          (push (ai-code-mcp--diagnostics-observe-buffer buffer) targets))))
    (ai-code-mcp--make-diagnostics-observation
     "open_project_buffers"
     (nreverse targets)
     (when (null targets) "no_open_project_buffers"))))

(defun ai-code-mcp--make-diagnostics-observation (scope targets
                                                        &optional reason)
  "Return a diagnostics observation for SCOPE and TARGETS.
REASON explains why a scope has no targets."
  (let (entries)
    (dolist (target targets)
      (when-let ((entry (ai-code-mcp--diagnostics-file-entry
                         (plist-get target :uri)
                         (plist-get target :diagnostics))))
        (push entry entries)))
    (list :scope scope
          :targets targets
          :entries (nreverse entries)
          :reason reason
          :observed-at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

(defun ai-code-mcp--diagnostics-observe-buffer (buffer)
  "Return diagnostics and coverage metadata observed from BUFFER."
  (let* ((backend (ai-code-mcp--diagnostics-backend-for-buffer buffer))
         (running (and backend
                       (ai-code-mcp--diagnostics-backend-running-p
                        buffer backend)))
         (state (cond ((null backend) 'unavailable)
                      (running 'running)
                      (t 'ready))))
    (with-current-buffer buffer
      (list :uri (ai-code-mcp--file-path-to-uri (buffer-file-name buffer))
            :backend backend
            :state state
            :reason (cond ((null backend) "no_active_backend")
                          (running "diagnostics_running"))
            :modified (buffer-modified-p)
            :modification-tick (buffer-chars-modified-tick)
            :diagnostics (if backend
                             (ai-code-mcp--buffer-diagnostics buffer backend)
                           (vconcat nil))))))

(defun ai-code-mcp--buffer-diagnostics (buffer &optional backend)
  "Return a vector of diagnostics for BUFFER using optional BACKEND."
  (vconcat
   (pcase (or backend (ai-code-mcp--diagnostics-backend-for-buffer buffer))
     ('flycheck (or (ai-code-mcp--flycheck-diagnostics buffer) '()))
     ('flymake (or (ai-code-mcp--flymake-diagnostics buffer) '()))
     (_ '()))))

(defun ai-code-mcp--diagnostics-backend-active-p (buffer backend)
  "Return non-nil when diagnostics BACKEND is active in BUFFER."
  (with-current-buffer buffer
    (pcase backend
      ('flycheck (and (featurep 'flycheck)
                      (bound-and-true-p flycheck-mode)))
      ('flymake (and (featurep 'flymake)
                     (bound-and-true-p flymake-mode)))
      (_ nil))))

(defun ai-code-mcp--diagnostics-backend-for-buffer (buffer)
  "Return the active diagnostics backend symbol to use for BUFFER."
  (let ((backend ai-code-mcp-diagnostics-backend))
    (if (eq backend 'auto)
        (cond
         ((ai-code-mcp--diagnostics-backend-active-p buffer 'flycheck)
          'flycheck)
         ((ai-code-mcp--diagnostics-backend-active-p buffer 'flymake)
          'flymake)
         (t nil))
      (and (ai-code-mcp--diagnostics-backend-active-p buffer backend)
           backend))))

(defun ai-code-mcp--diagnostics-backend-running-p (buffer backend)
  "Return non-nil when diagnostics BACKEND is still running in BUFFER."
  (with-current-buffer buffer
    (pcase backend
      ('flycheck
       (or (and (fboundp 'flycheck-running-p)
                (ignore-errors (flycheck-running-p)))
           (and (boundp 'flycheck-last-status-change)
                (eq flycheck-last-status-change 'running))))
      ('flymake
       (and (fboundp 'flymake-running-backends)
            (ignore-errors (flymake-running-backends))))
      (_ nil))))

(defun ai-code-mcp--flycheck-diagnostics (buffer)
  "Return Flycheck diagnostics for BUFFER."
  (when (featurep 'flycheck)
    (with-current-buffer buffer
      (when (bound-and-true-p flycheck-mode)
        (mapcar
         (lambda (diag)
           (ai-code-mcp--make-diagnostic
            (flycheck-error-line diag)
            (or (flycheck-error-column diag) 0)
            (or (flycheck-error-end-line diag)
                (flycheck-error-line diag))
            (or (flycheck-error-end-column diag)
                (flycheck-error-column diag)
                0)
            (flycheck-error-level diag)
            (format "%s" (or (flycheck-error-checker diag) "flycheck"))
            (flycheck-error-message diag)))
         flycheck-current-errors)))))

(defun ai-code-mcp--flymake-diagnostics (buffer)
  "Return Flymake diagnostics for BUFFER."
  (when (featurep 'flymake)
    (with-current-buffer buffer
      (when (bound-and-true-p flymake-mode)
        (mapcar
         (lambda (diag)
           (save-excursion
             (let* ((beg (flymake-diagnostic-beg diag))
                    (end (flymake-diagnostic-end diag))
                    (start-line (progn
                                  (goto-char beg)
                                  (line-number-at-pos)))
                    (start-column (current-column))
                    (end-line (progn
                                (goto-char end)
                                (line-number-at-pos)))
                    (end-column (current-column)))
               (ai-code-mcp--make-diagnostic
                start-line
                start-column
                end-line
                end-column
                (flymake-diagnostic-type diag)
                (format "%s"
                        (or (flymake-diagnostic-backend diag)
                            'flymake))
                (flymake-diagnostic-text diag)))))
         (flymake-diagnostics))))))

(defun ai-code-mcp--diagnostic-severity (severity)
  "Return string severity for SEVERITY."
  (pcase severity
    ((or 'error 'flymake-error :error) "Error")
    ((or 'warning 'flymake-warning :warning) "Warning")
    ('hint "Hint")
    (_ "Information")))

(defun ai-code-mcp--make-diagnostic (start-line start-column end-line end-column
                                                severity source message)
  "Return an MCP diagnostics entry.
Use START-LINE, START-COLUMN, END-LINE, END-COLUMN, SEVERITY,
SOURCE, and MESSAGE to describe the diagnostic payload."
  `((range . ((start . ((line . ,start-line)
                        (character . ,start-column)))
              (end . ((line . ,end-line)
                      (character . ,end-column)))))
    (severity . ,(ai-code-mcp--diagnostic-severity severity))
    (source . ,source)
    (message . ,message)))

(defun ai-code-mcp--diagnostics-file-entry (uri diagnostics)
  "Return a diagnostics payload for URI when DIAGNOSTICS is non-empty."
  (when (> (length diagnostics) 0)
    `((uri . ,uri)
      (diagnostics . ,diagnostics))))

(defun ai-code-mcp--local-file-uri-path (uri)
  "Return local file path for file URI, or nil.
Accepts local file URIs with no authority or with localhost authority."
  (let* ((parsed (url-generic-parse-url uri))
         (host (url-host parsed))
         (path (url-filename parsed)))
    (when (and (equal (url-type parsed) "file")
               (or (null host)
                   (string-empty-p host)
                   (string= host "localhost")))
      (url-unhex-string path))))

(defun ai-code-mcp--uri-to-file-path (uri)
  "Return file path for URI."
  (when uri
    (if (string-prefix-p "file://" uri)
        (or (ai-code-mcp--local-file-uri-path uri)
            (url-unhex-string (substring uri 7)))
      uri)))

(defun ai-code-mcp--file-path-to-uri (file-path)
  "Return canonical file URI for FILE-PATH."
  (url-encode-url (concat "file://" (expand-file-name file-path))))

(defun ai-code-mcp--project-files (project-dir)
  "Return absolute regular files inside PROJECT-DIR."
  (let* ((default-directory (file-name-as-directory project-dir))
         (project (project-current nil project-dir))
         (project-root default-directory))
    (or (ignore-errors
          (when (and project (fboundp 'project-files))
            (seq-filter
             #'file-regular-p
             (mapcar (lambda (file)
                       (if (file-name-absolute-p file)
                           file
                         (expand-file-name file project-root)))
                     (project-files project)))))
        (cl-labels
            ((collect-files (dir)
               (apply
                #'append
                (mapcar
                 (lambda (entry)
                   (cond
                    ((member entry '("." "..")) nil)
                    ((string-prefix-p "." entry) nil)
                    (t
                     (let ((path (expand-file-name entry dir)))
                       (cond
                        ((file-directory-p path)
                         (collect-files path))
                        ((file-regular-p path)
                         (list path))
                        (t nil))))))
                 (directory-files dir nil nil t)))))
          (collect-files project-root)))))

(defun ai-code-mcp-get-project-files ()
  "Return regular files in the current project as relative paths."
  (let ((project-dir (ai-code-mcp--project-directory)))
    (if (not (and project-dir (file-directory-p project-dir)))
        nil
      (mapcar #'ai-code-mcp--display-path
              (ai-code-mcp--project-files project-dir)))))

(defun ai-code-mcp-get-project-buffers ()
  "Return open buffers that belong to the current project."
  (let ((project-dir (ai-code-mcp--project-directory)))
    (delq nil
          (mapcar
           (lambda (buffer)
             (ai-code-mcp--project-buffer-entry buffer project-dir))
           (buffer-list)))))

(defun ai-code-mcp-imenu-list-symbols (file-path)
  "Return formatted imenu entries for FILE-PATH."
  (let* ((resolved-file (ai-code-mcp--require-file-path file-path))
         (buffer (ai-code-mcp--file-buffer resolved-file)))
    (with-current-buffer buffer
      (let ((imenu-auto-rescan t)
            (index (imenu--make-index-alist t)))
        (ai-code-mcp--imenu-entries index resolved-file)))))

(defun ai-code-mcp-xref-find-references (identifier file-path)
  "Return formatted xref references for IDENTIFIER using FILE-PATH context."
  (let ((buffer (ai-code-mcp--file-buffer
                 (ai-code-mcp--require-file-path file-path))))
    (with-current-buffer buffer
      (let ((backend (xref-find-backend)))
        (if (not backend)
            (format "No xref backend available for %s" file-path)
          (let ((items (xref-backend-references backend (format "%s" identifier))))
            (if (not items)
                (format "No references found for '%s'" identifier)
              (mapcar #'ai-code-mcp--format-xref-item items))))))))

(defun ai-code-mcp-xref-find-definitions-at-point (file-path line column)
  "Return formatted xref definitions for the identifier at FILE-PATH:LINE:COLUMN."
  (let ((buffer (ai-code-mcp--file-buffer
                 (ai-code-mcp--require-file-path file-path))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column column)
        (let ((backend (xref-find-backend)))
          (if (not backend)
              (format "No xref backend available for %s" file-path)
            (let ((identifier (xref-backend-identifier-at-point backend)))
              (if (not identifier)
                  (format "No identifier at %s:%d:%d" file-path line column)
                (let ((items (xref-backend-definitions backend identifier)))
                  (if (not items)
                      (format "No definitions found for '%s'" identifier)
                    (mapcar #'ai-code-mcp--format-xref-item items)))))))))))

(defun ai-code-mcp-treesit-info (file-path &optional line column whole-file)
  "Return tree-sitter information for FILE-PATH at LINE and COLUMN.
When WHOLE-FILE is non-nil, inspect the root node instead."
  (cond
   ((not (and (fboundp 'treesit-available-p)
              (treesit-available-p)))
    "Tree-sitter is not available in this Emacs build")
   (t
    (let ((buffer (ai-code-mcp--file-buffer
                   (ai-code-mcp--require-file-path file-path))))
      (with-current-buffer buffer
        (let* ((parsers (and (fboundp 'treesit-parser-list)
                             (treesit-parser-list)))
               (parser (car parsers)))
          (if (not parser)
              (format "No tree-sitter parser available for %s" file-path)
            (let* ((node (if whole-file
                             (treesit-parser-root-node parser)
                           (treesit-node-at
                            (ai-code-mcp--line-column-to-point
                             (or line 1)
                             (or column 0))
                            parser)))
                   (text (and node (treesit-node-text node t))))
              (if (not node)
                  "No tree-sitter node found"
                (format "Node Type: %s\nRange: %d-%d\nText: %s"
                        (treesit-node-type node)
                        (treesit-node-start node)
                        (treesit-node-end node)
                        (if text
                            (substring text 0 (min 80 (length text)))
                          "")))))))))))

(defun ai-code-mcp--initialize (&optional params)
  "Return the MCP initialize payload negotiated from PARAMS."
  (let ((protocol-version
         (ai-code-mcp-negotiate-legacy-version
          (alist-get 'protocolVersion params))))
    `((protocolVersion . ,protocol-version)
    (capabilities . ((tools . ((listChanged . :json-false)))))
    (serverInfo . ((name . "ai-code-mcp-tools")
                   (version . "0.1.0"))))))

(defun ai-code-mcp-negotiate-legacy-version (requested)
  "Return the supported legacy version negotiated for REQUESTED."
  (if (member requested ai-code-mcp--legacy-protocol-versions)
      requested
    ai-code-mcp--protocol-version))

(defun ai-code-mcp--discover ()
  "Return stateless MCP server discovery metadata."
  `((resultType . "complete")
    (supportedVersions . [,ai-code-mcp--modern-protocol-version])
    (capabilities . ((tools . ((listChanged . :json-false)))))
    (_meta . ,(ai-code-mcp--server-meta))
    (instructions
     . "Use these tools for live Emacs context and Emacs-side validation.")
    (ttlMs . 5000)
    (cacheScope . "private")))

(defun ai-code-mcp--modern-request-p ()
  "Return non-nil while dispatching the stateless MCP version."
  (equal ai-code-mcp--current-protocol-version
         ai-code-mcp--modern-protocol-version))

(defun ai-code-mcp--server-meta ()
  "Return per-response server identity metadata."
  '((io.modelcontextprotocol/serverInfo
     . ((name . "ai-code-mcp-tools") (version . "0.1.0")))))

(defun ai-code-mcp--complete-result (fields)
  "Add modern completion metadata to result FIELDS when required."
  (if (ai-code-mcp--modern-request-p)
      (append `((resultType . "complete"))
              fields
              `((_meta . ,(ai-code-mcp--server-meta))))
    fields))

(defun ai-code-mcp--tools-list ()
  "Return MCP tools/list response."
  (ai-code-mcp--ensure-builtins)
  (ai-code-mcp--complete-result
   `((tools . ,(mapcar
                #'ai-code-mcp--tool-to-mcp
                (sort (seq-filter #'ai-code-mcp--tool-available-p
                                  (copy-sequence ai-code-mcp-server-tools))
                      (lambda (left right)
                        (string< (plist-get left :name)
                                 (plist-get right :name))))))
     ,@(when (ai-code-mcp--modern-request-p)
         '((ttlMs . 5000) (cacheScope . "private"))))))

(defun ai-code-mcp--tools-call (params)
  "Return MCP tools/call response for PARAMS."
  (ai-code-mcp--ensure-builtins)
  (let* ((tool-name (alist-get 'name params))
         (arguments-entry (assq 'arguments params))
         (arguments (if arguments-entry (cdr arguments-entry) '()))
         (tool (progn
                 (unless (stringp tool-name)
                   (ai-code-mcp-signal-protocol-error
                    -32602 "Tool name must be a string"))
                 (ai-code-mcp--find-tool tool-name))))
    (condition-case err
        (ai-code-mcp--tool-success-result
         (ai-code-mcp--call-tool tool arguments))
      (ai-code-mcp-protocol-error
       (signal (car err) (cdr err)))
      (error
       (ai-code-mcp--tool-error-result err)))))

(defun ai-code-mcp--find-tool (tool-name)
  "Return the tool spec matching TOOL-NAME."
  (or (let ((tool (ai-code-mcp--find-tool-spec tool-name)))
        (and tool (ai-code-mcp--tool-available-p tool) tool))
      (ai-code-mcp-signal-protocol-error
       -32602 (format "Unknown tool: %s" tool-name))))

(defun ai-code-mcp--active-tool-profile ()
  "Return the tool profile for the active application session."
  (or (plist-get (ai-code-mcp-get-session-context) :tool-profile)
      ai-code-mcp-default-tool-profile))

(defun ai-code-mcp--tool-available-p (tool)
  "Return non-nil when TOOL belongs to the active exposure profile."
  (let ((category (or (plist-get tool :category) 'core)))
    (pcase (ai-code-mcp--active-tool-profile)
      ('core (eq category 'core))
      ('debug (memq category '(core debug)))
      ('full (memq category '(core debug eval)))
      (_ nil))))

(defun ai-code-mcp--find-tool-spec (tool-name)
  "Return the tool spec matching TOOL-NAME, or nil."
  (cl-find-if (lambda (tool)
                (equal (plist-get tool :name) tool-name))
              ai-code-mcp-server-tools))

(defun ai-code-mcp--call-tool (tool arguments)
  "Run TOOL with validated ARGUMENTS inside the active session context."
  (ai-code-mcp-with-session-context ai-code-mcp--current-session-id
    (apply (plist-get tool :function)
           (ai-code-mcp--validate-args arguments
                                       (plist-get tool :args)))))

(defun ai-code-mcp--validate-args (arguments arg-specs)
  "Return ordered ARGUMENTS validated against ARG-SPECS."
  (unless (and (listp arguments)
               (seq-every-p #'consp arguments))
    (ai-code-mcp-signal-protocol-error
     -32602 "Tool arguments must be a JSON object"))
  (let ((allowed-names (mapcar (lambda (spec)
                                 (plist-get spec :name))
                               arg-specs)))
    (dolist (entry arguments)
      (unless (member (if (symbolp (car entry))
                          (symbol-name (car entry))
                        (car entry))
                      allowed-names)
        (ai-code-mcp-signal-protocol-error
         -32602 (format "Unknown tool argument: %s" (car entry))))))
  (let (values)
    (dolist (spec arg-specs (nreverse values))
      (let* ((name (plist-get spec :name))
             (entry (or (assq (intern name) arguments)
                        (assoc name arguments))))
        (when (and (not (plist-get spec :optional))
                   (null entry))
          (ai-code-mcp-signal-protocol-error
           -32602 (format "Missing required argument: %s" name)))
        (when (and entry
                   (not (ai-code-mcp--argument-type-p
                         (cdr entry) (plist-get spec :type))))
          (ai-code-mcp-signal-protocol-error
           -32602
           (format "Argument %s must be %s"
                   name (plist-get spec :type))))
        (push (if (and entry (eq (cdr entry) :null))
                  nil
                (cdr entry))
              values)))))

(defun ai-code-mcp--argument-type-p (value type)
  "Return non-nil when VALUE satisfies JSON schema TYPE."
  (pcase type
    ('string (stringp value))
    ('integer (integerp value))
    ('number (numberp value))
    ('boolean (memq value '(t :json-false :false)))
    ('array (or (vectorp value) (listp value)))
    ('object (or (hash-table-p value) (listp value)))
    (_ nil)))

(defun ai-code-mcp--tool-success-result (value)
  "Return an MCP success result for tool VALUE."
  (let* ((structured (ai-code-mcp--structured-content value))
         (is-error (if (eq (alist-get 'ok structured) :json-false)
                       t
                     :json-false)))
    (ai-code-mcp--complete-result
     `((content . (((type . "text")
                    (text . ,(ai-code-mcp--format-result value)))))
       (structuredContent . ,structured)
       (isError . ,is-error)))))

(defun ai-code-mcp--tool-error-result (err)
  "Return an MCP semantic error result for ERR."
  (let* ((message (error-message-string err))
         (structured
          `((error . ((type . "tool_execution_error")
                      (message . ,message))))))
    (ai-code-mcp--complete-result
     `((content . (((type . "text") (text . ,message))))
       (structuredContent . ,structured)
       (isError . t)))))

(defun ai-code-mcp--structured-content (value)
  "Return object-shaped structured content for tool VALUE."
  (cond
   ((stringp value)
    (condition-case nil
        (let ((parsed (json-parse-string
                       value
                       :object-type 'alist
                       :array-type 'array
                       :null-object :null
                       :false-object :json-false)))
          (cond
           ((and (listp parsed) (seq-every-p #'consp parsed)) parsed)
           ((and (null parsed)
                 (string-match-p "\\`[[:space:]]*{" value))
            (ai-code-mcp--empty-object))
           (t `((value . ,parsed)))))
      (json-parse-error `((text . ,value)))))
   ((hash-table-p value) value)
   ((and (listp value) (seq-every-p #'consp value)) value)
   ((listp value) `((items . ,(vconcat value))))
   (t `((value . ,value)))))

(defun ai-code-mcp--tool-to-mcp (tool)
  "Convert TOOL spec into MCP tool metadata."
  `((name . ,(plist-get tool :name))
    (description . ,(plist-get tool :description))
    ,@(when-let ((annotations (plist-get tool :annotations)))
        `((annotations . ,annotations)))
    (inputSchema . ((type . "object")
                    (additionalProperties . :json-false)
                    (properties . ,(or (ai-code-mcp--args-to-schema
                                        (plist-get tool :args))
                                       (ai-code-mcp--empty-object)))
                    (required . ,(vconcat
                                  (ai-code-mcp--required-args
                                   (plist-get tool :args))))))
    ,@(when-let ((output-schema (plist-get tool :output-schema)))
        `((outputSchema . ,output-schema)))))

(defun ai-code-mcp--empty-object ()
  "Return an empty JSON object placeholder."
  (make-hash-table :test 'equal))

(defun ai-code-mcp--args-to-schema (arg-specs)
  "Convert ARG-SPECS into an alist keyed by argument symbols."
  (let (schema)
    (dolist (spec arg-specs (nreverse schema))
      (let ((name (intern (plist-get spec :name)))
            (type (plist-get spec :type))
            (description (plist-get spec :description)))
        (push
         (cons name
               (append
                `((type . ,(symbol-name type)))
                (when description
                  `((description . ,description)))))
         schema)))))

(defun ai-code-mcp--required-args (arg-specs)
  "Return required argument names from ARG-SPECS."
  (let (required)
    (dolist (spec arg-specs (nreverse required))
      (unless (plist-get spec :optional)
        (push (plist-get spec :name) required)))))

(defun ai-code-mcp--format-result (result)
  "Return RESULT converted to a tool response string."
  (cond
   ((stringp result) result)
   ((listp result)
    (mapconcat (lambda (item)
                 (if (stringp item)
                     item
                   (format "%S" item)))
               result
               "\n"))
   (t (format "%s" result))))

(defun ai-code-mcp--project-buffer-entry (buffer project-dir)
  "Return buffer metadata for BUFFER when it belongs to PROJECT-DIR."
  (when (ai-code-mcp--buffer-in-project-p buffer project-dir)
    (with-current-buffer buffer
      `((name . ,(buffer-name buffer))
        (mode . ,major-mode)
        (file . ,(buffer-file-name))
        (modified . ,(buffer-modified-p buffer))))))

(defun ai-code-mcp--buffer-in-project-p (buffer project-dir)
  "Return non-nil when BUFFER belongs to PROJECT-DIR."
  (and (file-directory-p project-dir)
       (with-current-buffer buffer
         (let ((file (buffer-file-name))
               (buffer-dir default-directory))
           (or (and file
                    (file-in-directory-p file project-dir))
               (and buffer-dir
                    (file-in-directory-p buffer-dir project-dir)))))))

(defun ai-code-mcp--project-directory ()
  "Return the best available project directory."
  (or (when-let ((context (ai-code-mcp-get-session-context)))
        (plist-get context :project-dir))
      (when-let ((project (project-current nil default-directory)))
        (expand-file-name (project-root project)))
      default-directory))

(defun ai-code-mcp--count-project-files (project-dir)
  "Count regular files inside PROJECT-DIR."
  (if (and project-dir (file-directory-p project-dir))
      (length (seq-filter #'file-regular-p
                          (directory-files-recursively project-dir ".*" t)))
    0))

(defun ai-code-mcp--display-path (file-path)
  "Return FILE-PATH relative to the active project when possible."
  (let* ((expanded-path (and file-path (expand-file-name file-path)))
         (project-dir (ai-code-mcp--project-directory))
         (project-root (and project-dir
                            (file-name-as-directory
                             (expand-file-name project-dir)))))
    (if (and expanded-path
             project-root
             (file-in-directory-p expanded-path project-root))
        (file-relative-name expanded-path project-root)
      expanded-path)))

(defun ai-code-mcp--require-file-path (file-path)
  "Return FILE-PATH as an absolute path or signal an error."
  (unless file-path
    (error "Argument file_path is required"))
  (expand-file-name file-path))

(defun ai-code-mcp--file-buffer (file-path)
  "Return a live buffer visiting FILE-PATH."
  (find-file-noselect file-path t))

(defun ai-code-mcp--imenu-entries (index file-path)
  "Return flattened imenu INDEX entries for FILE-PATH."
  (let (entries)
    (dolist (item index (nreverse entries))
      (when (consp item)
        (let ((name (car item))
              (payload (cdr item)))
          (if (ai-code--imenu-subalist-p payload)
              (setq entries
                    (append (nreverse (ai-code-mcp--imenu-entries payload file-path))
                            entries))
            (let* ((symbol (ai-code--normalize-imenu-symbol-name name payload))
                   (position (ai-code--imenu-item-position payload)))
              (when (and symbol position)
                (push (format "%s:%d: %s"
                              (ai-code-mcp--display-path file-path)
                              (line-number-at-pos position)
                              symbol)
                      entries)))))))))

(defun ai-code-mcp--format-xref-item (item)
  "Return a human-readable line for xref ITEM."
  (let* ((location (xref-item-location item))
         (group (ai-code-mcp--display-path
                 (xref-location-group location)))
         (marker (xref-location-marker location))
         (line (with-current-buffer (marker-buffer marker)
                 (save-excursion
                   (goto-char marker)
                   (line-number-at-pos))))
         (summary (xref-item-summary item)))
    (format "%s:%d: %s" group line summary)))

(defun ai-code-mcp--line-column-to-point (line column)
  "Convert LINE and COLUMN to point in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun ai-code-mcp-notify-user (message-text)
  "Show MESSAGE-TEXT to the Emacs user and beep."
  (message "%s" message-text)
  (beep)
  (format "Notified user: %s" message-text))

(require 'ai-code-mcp-debug-tools nil t)

(provide 'ai-code-mcp-server)

;;; ai-code-mcp-server.el ends here
