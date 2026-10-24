;;; test_ai-code-backends-infra-startup-diagnostics.el --- Startup integration tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise startup reporting without changing launch or cleanup semantics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)
(require 'ai-code-startup-diagnostics)

(ert-deftest test-ai-code-startup-integration-reports-early-exit ()
  "Every terminal backend forwards launch metadata and cleans up once."
  (dolist (terminal '(vterm eat ghostel))
    (with-temp-buffer
      (let ((buffer (current-buffer))
            (ai-code-backends-infra-terminal-backend terminal)
            (argv '("/opt/AI Tools/codex" "--model" "model with spaces"))
            (session-key '("/tmp/project/" . "default"))
            (process-table (make-hash-table :test 'equal))
            (cleanup-count 0)
            reported launched)
        (let ((create (lambda (_name _directory command _environment)
                        (setq launched command)
                        (cons buffer 'failed-process))))
          (cl-letf (((symbol-function 'ai-code-editor-viewport-environment) #'identity)
                    ((symbol-function 'ai-code-backends-infra--terminal-ensure-backend) #'ignore)
                    ((symbol-function 'ai-code-backends-infra-vterm-create-session) create)
                    ((symbol-function 'ai-code-backends-infra-eat-create-session) create)
                    ((symbol-function 'ai-code-backends-infra-ghostel-create-session) create)
                    ((symbol-function 'sleep-for) #'ignore)
                    ((symbol-function 'process-live-p) (lambda (_process) nil))
                    ((symbol-function 'ai-code-startup-diagnostics-report)
                     (lambda (&rest args) (setq reported args))))
            (ai-code-backends-infra--create-new-session
             (buffer-name buffer) "/tmp/project/" argv nil
             session-key process-table "default" "codex" nil
             (lambda () (cl-incf cleanup-count)) nil nil nil nil)))
        (should (eq launched argv))
        (should (= cleanup-count 1))
        (should-not (gethash session-key process-table))
        (should (equal reported
                       (list buffer 'failed-process "codex" argv "/tmp/project/")))))))

(ert-deftest test-ai-code-startup-integration-preserves-setup-error ()
  "A created buffer must not cause an actionable setup error to be swallowed."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (cleanup-count 0)
          (process-table (make-hash-table :test 'equal))
          reported)
      (cl-letf (((symbol-function 'ai-code-editor-viewport-environment) #'identity)
                ((symbol-function 'ai-code-backends-infra--create-terminal-session)
                 (lambda (&rest _args)
                   (user-error "Ghostel requires ghostel-exec")))
                ((symbol-function 'ai-code-startup-diagnostics-report)
                 (lambda (&rest _args) (setq reported t))))
        (should
         (equal
          (should-error
           (ai-code-backends-infra--create-new-session
            (buffer-name buffer) "/tmp/project/" '("codex") nil
            'session-key process-table "default" "codex" nil
            (lambda () (cl-incf cleanup-count)) nil nil nil nil)
           :type 'user-error)
          '(user-error "Ghostel requires ghostel-exec"))))
      (should (= cleanup-count 1))
      (should-not reported)
      (should (= 0 (hash-table-count process-table))))))

(ert-deftest test-ai-code-startup-integration-retains-cleanup-error-protection ()
  "Cleanup errors must not replace the original terminal setup error."
  (let ((cleanup-count 0) warning)
    (cl-letf (((symbol-function 'ai-code-editor-viewport-environment) #'identity)
              ((symbol-function 'ai-code-backends-infra--create-terminal-session)
               (lambda (&rest _args) (user-error "Missing terminal dependency")))
              ((symbol-function 'display-warning)
               (lambda (&rest args) (setq warning args))))
      (should
       (equal
        (should-error
         (ai-code-backends-infra--create-new-session
          "*startup-error*" "/tmp/project/" '("codex") nil
          'session-key (make-hash-table :test 'equal) "default" "codex" nil
          (lambda () (cl-incf cleanup-count) (error "Cleanup failed"))
          nil nil nil nil)
         :type 'user-error)
        '(user-error "Missing terminal dependency")))
      (should (= cleanup-count 1))
      (should warning))))

(ert-deftest test-ai-code-startup-integration-keeps-remote-launch-argv ()
  "Selecting a remote directory must not expand a program against local home."
  (let ((program "~/bin/codex") launched)
    (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda (&optional _arg) "/ssh:host:/remote/project/"))
              ((symbol-function 'read-string)
               (lambda (_prompt initial-input &rest _args) initial-input))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest args) (setq launched args))))
      (ai-code-backends-infra--start-cli-session
       (list :program program :switches '("--quiet") :label "Codex"
             :session-prefix "codex")
       t))
    (should (equal (car launched) "/ssh:host:/remote/project/"))
    (should (equal (nth 3 launched) '("~/bin/codex" "--quiet")))))

(provide 'test_ai-code-backends-infra-startup-diagnostics)
;;; test_ai-code-backends-infra-startup-diagnostics.el ends here
