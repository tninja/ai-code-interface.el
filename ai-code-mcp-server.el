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
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-backend "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function url-filename "url-parse" (urlobj))
(declare-function url-generic-parse-url "url-parse" (url))
(declare-function url-host "url-parse" (urlobj))
(declare-function url-type "url-parse" (urlobj))

(defvar flycheck-current-errors)

(defgroup ai-code-mcp-server nil
  "MCP tools core settings for AI Code Interface."
  :group 'ai-code
  :prefix "ai-code-mcp-")

(defcustom ai-code-mcp-server-tools nil
  "List of MCP tool specifications.
Each item is a plist with at least `:function', `:name', and `:description'."
  :type '(repeat sexp)
  :group 'ai-code-mcp-server)

(defcustom ai-code-mcp-diagnostics-backend 'auto
  "Backend used by `ai-code-mcp-get-diagnostics'.
Use `auto' to prefer Flycheck and then Flymake when available."
  :type '(choice (const :tag "Automatic detection" auto)
                 (const :tag "Flycheck" flycheck)
                 (const :tag "Flymake" flymake))
  :group 'ai-code-mcp-server)

(defvar ai-code-mcp-server-tool-setup-functions nil
  "Functions that register optional MCP tool groups.")

(defvar ai-code-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping MCP session ids to session metadata.")

(defvar ai-code-mcp--current-session-id nil
  "Dynamically bound MCP session id for the current tool invocation.")

(defconst ai-code-mcp--protocol-version "2024-11-05"
  "Protocol version reported by the MCP core.")

(defconst ai-code-mcp--builtin-tool-specs
  '((:function ai-code-mcp-project-info
     :name "project_info"
     :description "Get information about the current project context."
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
     :description "Get language diagnostics for a file or the active project."
     :args ((:name "uri"
             :type string
             :description "Optional file URI to inspect."
             :optional t)))
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
- `buffer_query'
- `get_diagnostics'
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
    (setq ai-code-mcp-server-tools
          (append
           (seq-remove
            (lambda (tool)
              (equal (plist-get tool :name) name))
            ai-code-mcp-server-tools)
           (list spec)))
    spec))

(defun ai-code-mcp-register-session (session-id project-dir buffer)
  "Register MCP SESSION-ID with PROJECT-DIR and BUFFER."
  (puthash session-id
           (list :project-dir project-dir
                 :buffer buffer
                 :start-time (current-time))
           ai-code-mcp--sessions))

(defun ai-code-mcp-unregister-session (session-id)
  "Unregister MCP SESSION-ID."
  (remhash session-id ai-code-mcp--sessions))

(defun ai-code-mcp-get-session-context (&optional session-id)
  "Return session context for SESSION-ID or the current session."
  (gethash (or session-id ai-code-mcp--current-session-id)
           ai-code-mcp--sessions))

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
    ("initialize" (ai-code-mcp--initialize))
    ("tools/list" (ai-code-mcp--tools-list))
    ("tools/call" (ai-code-mcp--tools-call params))
    (_ (error "Unknown MCP method: %s" method))))

(defun ai-code-mcp-builtins-setup ()
  "Register the built-in common Emacs MCP tools."
  (interactive)
  (dolist (tool ai-code-mcp--builtin-tool-specs)
    (apply #'ai-code-mcp-make-tool tool))
  (dolist (setup-fn ai-code-mcp-server-tool-setup-functions)
    (funcall setup-fn)))

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

(defun ai-code-mcp-get-diagnostics (&optional uri)
  "Return JSON diagnostics for URI or the active project."
  (let ((diagnostics-by-file
         (if uri
             (ai-code-mcp--diagnostics-for-uri uri)
           (ai-code-mcp--diagnostics-for-project))))
    (if diagnostics-by-file
        (json-encode (vconcat diagnostics-by-file))
      "[]")))

(defun ai-code-mcp--diagnostics-for-uri (uri)
  "Return a list with diagnostics for URI, or nil when none exist."
  (when-let* ((file-path (ai-code-mcp--uri-to-file-path uri))
              (buffer (get-file-buffer file-path))
              (diagnostics (ai-code-mcp--buffer-diagnostics buffer)))
    (when-let ((entry (ai-code-mcp--diagnostics-file-entry uri diagnostics)))
      (list entry))))

(defun ai-code-mcp--diagnostics-for-project ()
  "Return project diagnostics for open file-visiting buffers."
  (let ((project-dir (ai-code-mcp--project-directory))
        diagnostics-by-file)
    (dolist (buffer (buffer-list) (nreverse diagnostics-by-file))
      (when-let ((file-path (buffer-file-name buffer)))
        (when (or (not project-dir)
                  (file-in-directory-p file-path project-dir))
          (when-let ((diagnostics (ai-code-mcp--buffer-diagnostics buffer)))
            (when-let ((entry (ai-code-mcp--diagnostics-file-entry
                               (ai-code-mcp--file-path-to-uri file-path)
                               diagnostics)))
              (push entry diagnostics-by-file))))))))

(defun ai-code-mcp--buffer-diagnostics (buffer)
  "Return a vector of diagnostics for BUFFER."
  (vconcat
   (pcase (ai-code-mcp--diagnostics-backend-for-buffer buffer)
     ('flycheck (or (ai-code-mcp--flycheck-diagnostics buffer) '()))
     ('flymake (or (ai-code-mcp--flymake-diagnostics buffer) '()))
     (_ '()))))

(defun ai-code-mcp--diagnostics-backend-for-buffer (buffer)
  "Return the diagnostics backend symbol to use for BUFFER."
  (let ((backend ai-code-mcp-diagnostics-backend))
    (if (eq backend 'auto)
        (cond
         ((with-current-buffer buffer
            (and (featurep 'flycheck)
                 (bound-and-true-p flycheck-mode)))
          'flycheck)
         ((with-current-buffer buffer
            (and (featurep 'flymake)
                 (bound-and-true-p flymake-mode)))
          'flymake)
         (t nil))
      backend)))

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

(defun ai-code-mcp--initialize ()
  "Return the MCP initialize payload."
  `((protocolVersion . ,ai-code-mcp--protocol-version)
    (capabilities . ((tools . ((listChanged . :json-false)))))
    (serverInfo . ((name . "ai-code-mcp-tools")
                   (version . "0.1.0")))))

(defun ai-code-mcp--tools-list ()
  "Return MCP tools/list response."
  (ai-code-mcp--ensure-builtins)
  `((tools . ,(mapcar #'ai-code-mcp--tool-to-mcp
                      ai-code-mcp-server-tools))))

(defun ai-code-mcp--tools-call (params)
  "Return MCP tools/call response for PARAMS."
  (ai-code-mcp--ensure-builtins)
  (let* ((tool-name (alist-get 'name params))
         (arguments (or (alist-get 'arguments params) '()))
         (tool (ai-code-mcp--find-tool tool-name))
         (result (ai-code-mcp--call-tool tool arguments)))
    `((content . (((type . "text")
                   (text . ,(ai-code-mcp--format-result result))))))))

(defun ai-code-mcp--find-tool (tool-name)
  "Return the tool spec matching TOOL-NAME."
  (or (ai-code-mcp--find-tool-spec tool-name)
      (error "Unknown tool: %s" tool-name)))

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
  (let (values)
    (dolist (spec arg-specs (nreverse values))
      (let* ((name (plist-get spec :name))
             (entry (assq (intern name) arguments)))
        (when (and (not (plist-get spec :optional))
                   (null entry))
          (error "Missing required argument: %s" name))
        (push (cdr entry) values)))))

(defun ai-code-mcp--tool-to-mcp (tool)
  "Convert TOOL spec into MCP tool metadata."
  `((name . ,(plist-get tool :name))
    (description . ,(plist-get tool :description))
    (inputSchema . ((type . "object")
                    (properties . ,(or (ai-code-mcp--args-to-schema
                                        (plist-get tool :args))
                                       (ai-code-mcp--empty-object)))
                    (required . ,(vconcat
                                  (ai-code-mcp--required-args
                                   (plist-get tool :args))))))))

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

(require 'ai-code-mcp-editor-tools nil t)

(provide 'ai-code-mcp-server)

;;; ai-code-mcp-server.el ends here
