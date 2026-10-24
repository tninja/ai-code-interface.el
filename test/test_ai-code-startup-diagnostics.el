;;; test_ai-code-startup-diagnostics.el --- Startup metadata tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Startup reports contain metadata, never arguments or terminal output.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-startup-diagnostics)

(defun test-ai-code-startup-diagnostics--report (command &optional process backend)
  "Capture a report for COMMAND, PROCESS, and BACKEND."
  (with-temp-buffer
    (setq-local default-directory "/tmp/project/")
    (insert "private terminal output\n")
    (let ((buffer (current-buffer)) summary displayed)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (target &rest _args)
                   (setq displayed (eq target buffer))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq summary (apply #'format format-string args)))))
        (ai-code-startup-diagnostics-report
         buffer process (or backend "codex") command))
      (list :output (buffer-string) :summary summary :displayed displayed))))

(ert-deftest test-ai-code-startup-diagnostics-reports-launch-not-wrapper ()
  "Report the requested CLI and its failed process outcome."
  (cl-letf (((symbol-function 'processp) (lambda (_process) t))
            ((symbol-function 'process-status) (lambda (_process) 'exit))
            ((symbol-function 'process-exit-status) (lambda (_process) 127))
            ((symbol-function 'process-command)
             (lambda (_process) (ert-fail "Do not inspect shell wrappers"))))
    (let* ((result (test-ai-code-startup-diagnostics--report
                    '("/opt/bin/codex" "--flag") 'fake-process))
           (summary (plist-get result :summary)))
      (should (plist-get result :displayed))
      (dolist (field '("backend=codex" "executable=/opt/bin/codex"
                       "cwd=/tmp/project/" "status=exit" "exit=127"))
        (should (string-match-p (regexp-quote field) summary)))
      (should (string-match-p "Exit status: 127" (plist-get result :output))))))

(ert-deftest test-ai-code-startup-diagnostics-omits-all-arguments ()
  "Unknown credential forms and arbitrary prompt text are never logged."
  (let* ((arguments '("--api-key=inline-secret" "--token" "token-secret"
                      "--header" "Authorization: Bearer header-secret"
                      "--new-provider-option" "unrecognized-secret"
                      "private prompt\nsecond line"))
         (result (test-ai-code-startup-diagnostics--report
                  (cons "codex" arguments)))
         (output (plist-get result :output))
         (summary (plist-get result :summary)))
    (dolist (argument arguments)
      (should-not (string-match-p (regexp-quote argument) output))
      (should-not (string-match-p (regexp-quote argument) summary)))
    (should-not (string-match-p "private terminal output" summary))
    (should (string-prefix-p "private terminal output\n" output))))

(ert-deftest test-ai-code-startup-diagnostics-supports-legacy-command ()
  "Legacy commands skip environment assignments and preserve quoted paths."
  (let* ((result (test-ai-code-startup-diagnostics--report
                  "API_KEY=env-secret AWS_PROFILE=dev '/opt/AI Tools/codex' --token cli-secret"))
         (summary (plist-get result :summary)))
    (should (string-match-p "executable=/opt/AI Tools/codex" summary))
    (should-not (string-match-p "env-secret\\|cli-secret\\|AWS_PROFILE"
                                (plist-get result :output)))))

(ert-deftest test-ai-code-startup-diagnostics-handles-malformed-command ()
  "Unparseable commands should not prevent reporting or expose their text."
  (let ((result (test-ai-code-startup-diagnostics--report
                 "'malformed-secret")))
    (should (plist-get result :displayed))
    (should (string-match-p "executable=unknown" (plist-get result :summary)))
    (should-not (string-match-p "malformed-secret" (plist-get result :output)))))

(ert-deftest test-ai-code-startup-diagnostics-escapes-control-characters ()
  "Metadata cannot inject extra lines into the message log."
  (let* ((result (test-ai-code-startup-diagnostics--report
                  '("codex\nforged-executable") nil "codex\rforged-backend"))
         (summary (plist-get result :summary)))
    (should-not (string-match-p "[\n\r]" summary))
    (should (string-match-p (regexp-quote "codex\\nforged-executable") summary))
    (should (string-match-p "forged-backend" summary))))

(ert-deftest test-ai-code-startup-diagnostics-does-not-invent-child-exit-code ()
  "Open event pipes do not have a meaningful child exit code."
  (let ((process (make-pipe-process :name "startup-event-pipe" :noquery t)))
    (unwind-protect
        (let ((result (test-ai-code-startup-diagnostics--report '("codex") process)))
          (should (string-match-p "exit=unknown" (plist-get result :summary))))
      (delete-process process))))

(ert-deftest test-ai-code-startup-diagnostics-handles-killed-buffer ()
  "An explicit session directory survives a terminal buffer being killed."
  (let ((buffer (generate-new-buffer " *startup-killed*")) summary)
    (kill-buffer buffer)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq summary (apply #'format format-string args)))))
      (ai-code-startup-diagnostics-report
       buffer nil "codex" '("codex") "/ssh:host:/remote/project/"))
    (should (string-match-p "cwd=/ssh:host:/remote/project/" summary))))

(ert-deftest test-ai-code-startup-diagnostics-appending-is-best-effort ()
  "Read-only buffers and failed insertion must still be displayed."
  (with-temp-buffer
    (setq buffer-read-only t)
    (let ((buffer (current-buffer)) displayed summary)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (target &rest _args) (setq displayed (eq target buffer))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq summary (apply #'format format-string args)))))
        (ai-code-startup-diagnostics-report buffer nil "codex" '("codex"))
        (should (string-match-p "AI Code startup diagnostics" (buffer-string)))
        (let ((before-change-functions (list (lambda (&rest _) (error "Cannot insert")))))
          (ai-code-startup-diagnostics-report buffer nil "codex" '("codex")))
        (should displayed)
        (should (string-match-p "backend=codex" summary))))))

(provide 'test_ai-code-startup-diagnostics)
;;; test_ai-code-startup-diagnostics.el ends here
