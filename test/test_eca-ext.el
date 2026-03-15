;;; test_eca-ext.el --- Tests for eca-ext.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for ECA extension helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'eca-ext)

(ert-deftest eca-ext-test-workspace-folder-for-file-matches-parent ()
  "Ensure workspace lookup returns the matching parent directory."
  (cl-letf (((symbol-function 'eca-session) (lambda () 'mock-session))
            ((symbol-function 'eca--session-workspace-folders)
             (lambda (_session) '("/repo" "/other"))))
    (should (equal (eca-workspace-folder-for-file "/repo/src/file.el")
                   "/repo"))
    (should-not (eca-workspace-folder-for-file "/elsewhere/file.el"))))

(ert-deftest eca-ext-test-workspace-provenance-builds-relative-info ()
  "Ensure provenance records workspace, relative path, and folder name."
  (cl-letf (((symbol-function 'eca-session) (lambda () 'mock-session))
            ((symbol-function 'eca--session-workspace-folders)
             (lambda (_session) '("/repo"))))
    (should (equal (eca-workspace-provenance "/repo/src/file.el")
                   '(:workspace "/repo"
                     :relative-path "src/file.el"
                     :folder-name "repo")))))

(ert-deftest eca-ext-test-share-and-clear-shared-context ()
  "Ensure shared context accumulates and clears paths."
  (let ((eca--shared-context nil))
    (eca-share-file-context "/tmp/a.txt")
    (eca-share-repo-map-context "/tmp/project")
    (should (equal (plist-get eca--shared-context :files)
                   '("/tmp/a.txt")))
    (should (equal (plist-get eca--shared-context :repo-maps)
                   '("/tmp/project")))
    (eca-clear-shared-context)
    (should-not eca--shared-context)))

(ert-deftest eca-ext-test-apply-shared-context-calls-context-helpers ()
  "Ensure shared context application delegates to per-session helpers."
  (let ((eca--shared-context '(:files ("/tmp/a.txt")
                                :repo-maps ("/tmp/project")))
        file-calls repo-map-calls workspace-adds)
    (cl-letf (((symbol-function 'file-exists-p) (lambda (path) (string= path "/tmp/a.txt")))
              ((symbol-function 'file-directory-p) (lambda (path) (string= path "/tmp/project")))
              ((symbol-function 'eca-list-workspace-folders) (lambda (_session) nil))
              ((symbol-function 'eca-add-workspace-folder)
               (lambda (root _session) (push root workspace-adds)))
              ((symbol-function 'eca-chat-add-file-context)
               (lambda (_session file-path) (push file-path file-calls)))
              ((symbol-function 'eca-chat-add-repo-map-context)
               (lambda (_session) (push t repo-map-calls)))
              ((symbol-function 'eca--session-id) (lambda (_session) 4))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (eca-apply-shared-context 'mock-session)
      (should (equal file-calls '("/tmp/a.txt")))
      (should (equal repo-map-calls '(t)))
      (should (equal workspace-adds '("/tmp/project"))))))

(provide 'test_eca-ext)

;;; test_eca-ext.el ends here
