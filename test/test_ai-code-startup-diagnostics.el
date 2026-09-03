;;; test_ai-code-startup-diagnostics.el --- Startup diagnostic tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for safe CLI startup failure diagnostics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-startup-diagnostics)

(defun test-ai-code-startup-diagnostics--report
    (command &optional backend process directory)
  "Report startup failure for COMMAND and return observable output.
BACKEND, PROCESS, and DIRECTORY customize the diagnostic context."
  (with-temp-buffer
    (setq-local default-directory (or directory "/tmp/project/"))
    (let ((buffer (current-buffer))
          captured-message
          displayed)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (target &rest _args)
                   (setq displayed (eq target buffer))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq captured-message
                         (apply #'format format-string args)))))
        (ai-code-startup-diagnostics-report
         buffer process (or backend "codex") command))
      (list :buffer-output (buffer-string)
            :message captured-message
            :displayed displayed))))

(ert-deftest test-ai-code-startup-diagnostics-report-redacts-secrets ()
  "Reporting should preserve useful context without exposing secret values."
  (let* ((secrets
          '("inline-secret"
            "separate-secret"
            "github-secret"
            "api-secret"
            "bearer-secret"
            "header-secret"
            "compact-header-secret"
            "environment-secret"
            "aws-secret"))
         (result
          (test-ai-code-startup-diagnostics--report
           '("codex"
             "--api-key=inline-secret"
             "--token" "separate-secret"
             "--github-token" "github-secret"
             "--api_key" "api-secret"
             "--header" "Authorization: Bearer bearer-secret"
             "-H" "X-API-Key: header-secret"
             "-HAuthorization: Bearer compact-header-secret"
             "OPENAI_API_KEY=environment-secret"
             "AWS_SECRET_ACCESS_KEY=aws-secret"
             "MONKEY=banana"
             "--model" "gpt-5")))
         (diagnostics (plist-get result :buffer-output))
         (summary (plist-get result :message)))
    (should (plist-get result :displayed))
    (should (string-match-p "Backend: \"codex\"" diagnostics))
    (should (string-match-p "Executable: \"codex\"" diagnostics))
    (should (string-match-p "<redacted>" diagnostics))
    (should (string-match-p "MONKEY=banana" diagnostics))
    (should (string-match-p "--model" diagnostics))
    (dolist (secret secrets)
      (should-not (string-match-p (regexp-quote secret) diagnostics))
      (should-not (string-match-p (regexp-quote secret) summary)))))

(ert-deftest test-ai-code-startup-diagnostics-report-includes-process-context ()
  "Reporting should include the requested executable and process outcome."
  (cl-letf (((symbol-function 'processp) (lambda (_process) t))
            ((symbol-function 'process-status) (lambda (_process) 'exit))
            ((symbol-function 'process-exit-status) (lambda (_process) 127)))
    (let* ((result
            (test-ai-code-startup-diagnostics--report
             "/usr/local/bin/codex --flag --token command-secret"
             "codex"
             'fake-process
             "/tmp/example/"))
           (diagnostics (plist-get result :buffer-output))
           (summary (plist-get result :message)))
      (should (string-match-p "Executable: \"/usr/local/bin/codex\""
                              diagnostics))
      (should (string-match-p "Working directory: \"/tmp/example/\""
                              diagnostics))
      (should (string-match-p "Process status: exit" diagnostics))
      (should (string-match-p "Exit status: 127" diagnostics))
      (should (string-match-p "backend=codex" summary))
      (should (string-match-p "executable=/usr/local/bin/codex" summary))
      (should (string-match-p "exit=127" summary))
      (should-not (string-match-p "command-secret" diagnostics))
      (should-not (string-match-p "command-secret" summary)))))

(ert-deftest test-ai-code-startup-diagnostics-report-redacts-nested-and-multiline-secrets ()
  "Nested, chained, and multiline secret forms should remain confidential."
  (let* ((secrets
          '("nested-secret"
            "separate-access-secret"
            "inline-access-secret"
            "key-id-secret"
            "chained-secret"
            "first-cli-line"
            "second-cli-line"
            "first-env-line"
            "second-env-line"))
         (result
          (test-ai-code-startup-diagnostics--report
           '("env"
             "--env=AWS_SECRET_ACCESS_KEY=nested-secret"
             "codex"
             "--access-key" "separate-access-secret"
             "--aws-access-key=inline-access-secret"
             "--aws-access-key-id" "key-id-secret"
             "--token" "--api-key" "chained-secret"
             "--api-key=first-cli-line\nsecond-cli-line"
             "PRIVATE_KEY=first-env-line\nsecond-env-line")))
         (diagnostics (plist-get result :buffer-output)))
    (should
     (string-match-p
      "--env=AWS_SECRET_ACCESS_KEY=<redacted>" diagnostics))
    (should (string-match-p "--api-key=<redacted>" diagnostics))
    (should (string-match-p "PRIVATE_KEY=<redacted>" diagnostics))
    (dolist (secret secrets)
      (should-not (string-match-p (regexp-quote secret) diagnostics)))))

(ert-deftest test-ai-code-startup-diagnostics-report-skips-environment-assignments ()
  "Environment assignments should not replace the executable in summaries."
  (let* ((result
          (test-ai-code-startup-diagnostics--report
           (concat "OPENAI_API_KEY=message-secret AWS_PROFILE=dev "
                   "/opt/bin/codex --flag")))
         (summary (plist-get result :message)))
    (should (string-match-p "executable=/opt/bin/codex" summary))
    (should-not (string-match-p "message-secret" summary))
    (should-not (string-match-p "OPENAI_API_KEY" summary))))

(ert-deftest test-ai-code-startup-diagnostics-report-preserves-quoted-boundaries ()
  "A quoted header should remain one argument and be fully redacted."
  (let* ((result
          (test-ai-code-startup-diagnostics--report
           "codex --header 'Authorization: Bearer header-secret' --model gpt-5"))
         (diagnostics (plist-get result :buffer-output)))
    (should (string-match-p "--header" diagnostics))
    (should (string-match-p "<redacted>" diagnostics))
    (should (string-match-p "--model" diagnostics))
    (should (string-match-p "gpt-5" diagnostics))
    (should-not (string-match-p "header-secret" diagnostics))))

(ert-deftest test-ai-code-startup-diagnostics-records-native-child-exit-status ()
  "A recorded native child status should override its event pipe status."
  (let ((process
         (make-pipe-process
          :name "ai-code-startup-diagnostics-native-exit"
          :noquery t)))
    (unwind-protect
        (progn
          (ai-code-startup-diagnostics-record-child-exit-status process 1)
          (let* ((result
                  (test-ai-code-startup-diagnostics--report
                   "codex" "codex" process))
                 (diagnostics (plist-get result :buffer-output)))
            (should (string-match-p "Exit status: 1" diagnostics))))
      (when (processp process)
        (delete-process process)))))

(ert-deftest test-ai-code-startup-diagnostics-report-fails-closed ()
  "Malformed commands and control characters should not forge diagnostics."
  (let* ((result
          (test-ai-code-startup-diagnostics--report
           "codex --model 'malformed-secret"
           "codex\nforged-backend"
           nil
           "/tmp/project\nforged-directory/"))
         (diagnostics (plist-get result :buffer-output))
         (summary (plist-get result :message)))
    (should (string-match-p "<unparseable-command>" diagnostics))
    (should-not (string-match-p "malformed-secret" diagnostics))
    (should-not (string-match-p "malformed-secret" summary))
    (should-not (string-match-p "\n" summary))
    (should
     (string-match-p (regexp-quote "codex\\nforged-backend") summary))
    (should
     (string-match-p
      (regexp-quote "/tmp/project\\nforged-directory/") summary))))

(provide 'test_ai-code-startup-diagnostics)

;;; test_ai-code-startup-diagnostics.el ends here
