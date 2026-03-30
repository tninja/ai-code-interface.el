;;; eca-util.el --- Minimal ECA util test stub -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Lightweight ECA utility stub for unit tests that do not need the real runtime.

;;; Code:

(require 'cl-lib)

(defvar eca--sessions nil
  "Minimal session store for the test stub.")

(defvar eca--stub-session nil
  "Current session object used by the test stub.")

(defun eca-session ()
  "Return the current stub session."
  eca--stub-session)

(defun eca-vals (map)
  "Return MAP values for alists and hash tables."
  (cond
   ((hash-table-p map)
    (let (values)
      (maphash (lambda (_key value) (push value values)) map)
      (nreverse values)))
   ((listp map)
    (mapcar #'cdr map))
   (t nil)))

(defun eca-info (format-string &rest args)
  "Format FORMAT-STRING with ARGS for the test stub."
  (apply #'format format-string args))

(defun eca--session-id (session)
  "Return SESSION id or a default stub value."
  (or (plist-get session :id) 1))

(defun eca--session-status (session)
  "Return SESSION status or a default stub value."
  (or (plist-get session :status) 'running))

(defun eca--session-workspace-folders (session)
  "Return SESSION workspace folders."
  (plist-get session :workspace-folders))

(defun eca-create-session (workspace-folders)
  "Create a new stub session for WORKSPACE-FOLDERS."
  (setq eca--stub-session (list :id 1 :status 'running :workspace-folders workspace-folders)))

(defun eca-delete-session (_session)
  "Delete the current stub session."
  (setq eca--stub-session nil)
  t)

(defun eca-assert-session-running (_session)
  "Pretend the current stub session is running."
  t)

(defun eca--session-add-workspace-folder (session folder)
  "Return SESSION with FOLDER adjoined to its workspace list."
  (plist-put session
             :workspace-folders
             (cl-adjoin folder (plist-get session :workspace-folders) :test #'string=)))

(defun eca-get (map key)
  "Get KEY from MAP for hash tables and plists."
  (cond
   ((hash-table-p map) (gethash key map))
   ((listp map) (plist-get map key))
   (t nil)))

(provide 'eca-util)

;;; eca-util.el ends here
