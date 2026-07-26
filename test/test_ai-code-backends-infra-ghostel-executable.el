;;; test_ai-code-backends-infra-ghostel-executable.el --- Ghostel executable tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for Ghostel executable resolution.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)

(declare-function ghostel-exec "ghostel" (buffer program &optional args))

(defvar ai-code-backends-infra--launch-program)

(ert-deftest test-ai-code-backends-infra-ghostel-resolve-program-finds-bare-executable ()
  "Bare executable names should be resolved before Ghostel starts them."
  (let ((ai-code-backends-infra--launch-program "claude"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (when (string= program "claude")
                   "/opt/homebrew/bin/claude"))))
      (should
       (equal (ai-code-backends-infra-ghostel--resolve-program "claude")
              "/opt/homebrew/bin/claude")))))

(ert-deftest test-ai-code-backends-infra-ghostel-resolve-program-preserves-explicit-path ()
  "Explicit executable paths should not be searched again."
  (let ((ai-code-backends-infra--launch-program "./bin/claude")
        searched)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program)
                 (setq searched t)
                 nil)))
      (should
       (equal (ai-code-backends-infra-ghostel--resolve-program "./bin/claude")
              "./bin/claude"))
      (should-not searched))))

(ert-deftest test-ai-code-backends-infra-ghostel-resolve-program-finds-custom-ai-cli ()
  "The active AI CLI program should not depend on a hardcoded registry."
  (let ((ai-code-backends-infra--launch-program "claude-nightly"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (when (string= program "claude-nightly")
                   "/usr/local/bin/claude-nightly"))))
      (should
       (equal
        (ai-code-backends-infra-ghostel--resolve-program "claude-nightly")
        "/usr/local/bin/claude-nightly")))))

(ert-deftest test-ai-code-backends-infra-ghostel-resolve-program-preserves-generic-command ()
  "Commands outside the active AI CLI launch should remain unchanged."
  (let ((ai-code-backends-infra--launch-program "claude")
        searched)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program)
                 (setq searched t)
                 "/usr/bin/echo")))
      (should
       (equal (ai-code-backends-infra-ghostel--resolve-program "echo")
              "echo"))
      (should-not searched))))

(ert-deftest test-ai-code-backends-infra-ghostel-resolve-program-falls-back-when-missing ()
  "An unresolved active AI CLI should keep its original program name."
  (let ((ai-code-backends-infra--launch-program "agy"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
      (should
       (equal (ai-code-backends-infra-ghostel--resolve-program "agy")
              "agy")))))

(ert-deftest test-ai-code-backends-infra-start-cli-session-exposes-launch-program ()
  "Generic AI CLI startup should expose its program to terminal creation."
  (let (program-seen)
    (cl-letf (((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) (list :command "agy")))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda (&optional _arg) default-directory))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args)
                 (setq program-seen ai-code-backends-infra--launch-program))))
      (ai-code-backends-infra--start-cli-session
       (list :program "agy"
             :switches nil
             :label "Antigravity"
             :process-table (make-hash-table :test 'equal)
             :session-prefix "antigravity")
       nil))
    (should (equal program-seen "agy"))))

(ert-deftest test-ai-code-backends-infra-ghostel-start-process-resolves-launch-program ()
  "Ghostel startup should pass the resolved active AI CLI to `ghostel-exec'."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (ai-code-backends-infra--launch-program "agy")
          program-seen
          args-seen)
      (cl-letf (((symbol-function 'ai-code-backends-infra--configure-ghostel-buffer)
                 #'ignore)
                ((symbol-function 'ai-code-backends-infra-ghostel--native-editor-transport-p)
                 (lambda () nil))
                ((symbol-function 'ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums)
                 (lambda () nil))
                ((symbol-function 'executable-find)
                 (lambda (program)
                   (when (string= program "agy")
                     "/usr/local/bin/agy")))
                ((symbol-function 'ghostel-exec)
                 (lambda (_buffer program args)
                   (setq program-seen program
                         args-seen args))))
        (ai-code-backends-infra--start-ghostel-process
         buffer "agy --continue"))
      (should (equal program-seen "/usr/local/bin/agy"))
      (should (equal args-seen '("--continue"))))))

(provide 'test_ai-code-backends-infra-ghostel-executable)

;;; test_ai-code-backends-infra-ghostel-executable.el ends here
