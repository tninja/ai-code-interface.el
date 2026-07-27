;;; test_ai-code-backends-infra-startup-diagnostics.el --- Startup diagnostic tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for terminal startup failure diagnostics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)

(defun test-ai-code-backends-infra--render-startup-diagnostics (command)
  "Return startup failure details and rendered diagnostics for COMMAND."
  (with-temp-buffer
    (setq-local default-directory "/tmp/project/")
    (let ((details
           (ai-code-backends-infra--startup-failure-details
            (current-buffer) nil "codex" command)))
      (ai-code-backends-infra--append-startup-failure-diagnostics
       (current-buffer) details)
      (list details (buffer-string)))))

(ert-deftest test-ai-code-backends-infra--redact-startup-command-covers-secret-forms ()
  "Startup command diagnostics should redact common secret value forms."
  (should
   (equal
    (ai-code-backends-infra--redact-startup-command
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
       "--model" "gpt-5"))
    '("codex"
      "--api-key=<redacted>"
      "--token" "<redacted>"
      "--github-token" "<redacted>"
      "--api_key" "<redacted>"
      "--header" "<redacted>"
      "-H" "<redacted>"
      "-H<redacted>"
      "OPENAI_API_KEY=<redacted>"
      "AWS_SECRET_ACCESS_KEY=<redacted>"
      "MONKEY=banana"
      "--model" "gpt-5"))))

(ert-deftest test-ai-code-backends-infra--startup-failure-details-include-context ()
  "Startup failure details should include explicit backend and process context."
  (with-temp-buffer
    (setq-local default-directory "/tmp/example/")
    (insert "initial output\nfatal: missing credential\n")
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-command)
               (lambda (_process)
                 '("/bin/sh" "-c"
                   "stty sane && exec /usr/local/bin/codex --flag")))
              ((symbol-function 'process-status) (lambda (_process) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_process) 127)))
      (let ((details
             (ai-code-backends-infra--startup-failure-details
              (current-buffer) 'fake-process "codex"
              "/usr/local/bin/codex --flag")))
        (should (equal (plist-get details :backend) "codex"))
        (should (equal (plist-get details :executable) "/usr/local/bin/codex"))
        (should (equal (plist-get details :command)
                       '("/usr/local/bin/codex" "--flag")))
        (should (equal (plist-get details :cwd) "/tmp/example/"))
        (should (equal (plist-get details :status) 'exit))
        (should (= (plist-get details :exit-status) 127))
        (should-not (plist-member details :last-output))))))

(ert-deftest test-ai-code-backends-infra--startup-failure-executable-skips-environment-assignments ()
  "Startup summaries should not treat environment assignments as executables."
  (let* ((details
          (ai-code-backends-infra--startup-failure-details
           nil nil "codex"
           "OPENAI_API_KEY=message-secret AWS_PROFILE=dev /opt/bin/codex --flag"))
         (summary
          (ai-code-backends-infra--format-startup-failure details)))
    (should (equal (plist-get details :executable) "/opt/bin/codex"))
    (should (string-match-p "executable=/opt/bin/codex" summary))
    (should-not (string-match-p "message-secret" summary))
    (should-not (string-match-p "OPENAI_API_KEY" summary))))

(ert-deftest test-ai-code-backends-infra--startup-failure-summary-escapes-control-characters ()
  "Startup failure summaries should remain on one physical line."
  (let ((summary
         (ai-code-backends-infra--format-startup-failure
          '(:backend "codex\nforged-backend"
            :executable "codex\nforged-executable"
            :cwd "/tmp/project\nforged-directory"
            :status exit
            :exit-status 1))))
    (should-not (string-match-p "\n" summary))
    (should
     (string-match-p (regexp-quote "codex\\nforged-backend") summary))
    (should
     (string-match-p (regexp-quote "codex\\nforged-executable") summary))
    (should
     (string-match-p (regexp-quote "/tmp/project\\nforged-directory")
                     summary))))

(ert-deftest test-ai-code-backends-infra--startup-failure-preserves-quoted-argument-boundaries ()
  "Startup diagnostics should redact a quoted header value as one argument."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _args)
               "--header 'Authorization: Bearer header-secret' --model gpt-5")))
    (let* ((resolved
            (ai-code-backends-infra--resolve-start-command
             "codex" nil t "Codex"))
           (rendered
            (test-ai-code-backends-infra--render-startup-diagnostics
             (plist-get resolved :command)))
           (details (car rendered))
           (diagnostics (cadr rendered)))
      (should
       (equal (plist-get details :command)
              '("codex"
                "--header"
                "Authorization: Bearer header-secret"
                "--model"
                "gpt-5")))
      (should (string-match-p "<redacted>" diagnostics))
      (should-not (string-match-p "header-secret" diagnostics)))))

(ert-deftest test-ai-code-backends-infra--startup-failure-preserves-prompt-default-boundaries ()
  "Accepting default CLI args should preserve a header value as one argument."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt initial-input &rest _args)
               initial-input)))
    (let* ((resolved
            (ai-code-backends-infra--resolve-start-command
             "codex"
             '("--header" "Authorization: Bearer default-header-secret")
             t
             "Codex"))
           (rendered
            (test-ai-code-backends-infra--render-startup-diagnostics
             (plist-get resolved :command)))
           (details (car rendered))
           (diagnostics (cadr rendered)))
      (should
       (equal (plist-get details :command)
              '("codex"
                "--header"
                "Authorization: Bearer default-header-secret")))
      (should (string-match-p "<redacted>" diagnostics))
      (should-not
       (string-match-p "default-header-secret" diagnostics)))))

(ert-deftest test-ai-code-backends-infra--startup-failure-redacts-access-key-options ()
  "Startup diagnostics should redact access-key option values."
  (let ((diagnostics
         (cadr
          (test-ai-code-backends-infra--render-startup-diagnostics
           (concat "codex --access-key separate-access-secret "
                   "--aws-access-key=inline-access-secret "
                   "--aws-access-key-id key-id-secret")))))
    (should (string-match-p "<redacted>" diagnostics))
    (should-not (string-match-p "separate-access-secret" diagnostics))
    (should-not (string-match-p "inline-access-secret" diagnostics))
    (should-not (string-match-p "key-id-secret" diagnostics))))

(ert-deftest test-ai-code-backends-infra--startup-failure-redacts-multiline-secrets ()
  "Startup diagnostics should redact secret values containing newlines."
  (let ((diagnostics
         (cadr
          (test-ai-code-backends-infra--render-startup-diagnostics
           '("codex"
             "--api-key=first-cli-line\nsecond-cli-line"
             "PRIVATE_KEY=first-env-line\nsecond-env-line")))))
    (should (string-match-p "--api-key=<redacted>" diagnostics))
    (should (string-match-p "PRIVATE_KEY=<redacted>" diagnostics))
    (should-not (string-match-p "first-cli-line" diagnostics))
    (should-not (string-match-p "second-cli-line" diagnostics))
    (should-not (string-match-p "first-env-line" diagnostics))
    (should-not (string-match-p "second-env-line" diagnostics))))

(ert-deftest test-ai-code-backends-infra--startup-failure-redacts-nested-environment-secret ()
  "Startup diagnostics should redact secrets nested in environment options."
  (let ((diagnostics
         (cadr
          (test-ai-code-backends-infra--render-startup-diagnostics
           '("env"
             "--env=AWS_SECRET_ACCESS_KEY=nested-secret"
             "codex")))))
    (should
     (string-match-p
      "--env=AWS_SECRET_ACCESS_KEY=<redacted>"
      diagnostics))
    (should-not (string-match-p "nested-secret" diagnostics))))

(ert-deftest test-ai-code-backends-infra--startup-failure-preserves-chained-sensitive-options ()
  "Startup diagnostics should preserve chained options and redact their value."
  (let ((diagnostics
         (cadr
          (test-ai-code-backends-infra--render-startup-diagnostics
           "codex --token --api-key chained-secret --model gpt-5"))))
    (should (string-match-p "--token" diagnostics))
    (should (string-match-p "--api-key" diagnostics))
    (should (string-match-p "<redacted>" diagnostics))
    (should-not (string-match-p "chained-secret" diagnostics))))

(ert-deftest test-ai-code-backends-infra--startup-failure-rejects-unbalanced-command ()
  "Startup diagnostics should fail closed for an unbalanced launch command."
  (let* ((rendered
          (test-ai-code-backends-infra--render-startup-diagnostics
           "codex --model 'malformed-secret"))
         (details (car rendered))
         (diagnostics (cadr rendered)))
    (should (equal (plist-get details :command)
                   '("<unparseable-command>")))
    (should (string-match-p "<unparseable-command>" diagnostics))
    (should-not (string-match-p "malformed-secret" diagnostics))))

(ert-deftest test-ai-code-backends-infra--create-new-session-failure-is-actionable-and-safe ()
  "A real startup failure should show actionable details without logging output."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (session-key '("/tmp/project/" . "default"))
          (process-table (make-hash-table :test 'equal))
          displayed-buffer
          captured-message)
      (setq-local default-directory "/tmp/project/")
      (insert "spawn failed token=super-secret")
      (cl-letf (((symbol-function 'ai-code-editor-viewport-environment)
                 (lambda (env-vars) env-vars))
                ((symbol-function
                  'ai-code-backends-infra--create-terminal-session)
                 (lambda (&rest _args)
                   (cons buffer 'fake-process)))
                ((symbol-function 'sleep-for) (lambda (&rest _args)))
                ((symbol-function 'process-live-p) (lambda (_process) nil))
                ((symbol-function 'processp) (lambda (_process) t))
                ((symbol-function 'process-command)
                 (lambda (_process)
                   '("/bin/sh" "-c"
                     "stty sane && exec /opt/bin/codex --flag --github-token command-secret")))
                ((symbol-function 'process-status) (lambda (_process) 'exit))
                ((symbol-function 'process-exit-status) (lambda (_process) 126))
                ((symbol-function 'pop-to-buffer)
                 (lambda (target-buffer &rest _args)
                   (setq displayed-buffer target-buffer)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq captured-message
                         (apply #'format format-string args)))))
        (ai-code-backends-infra--create-new-session
         "*ai-code-startup-failure-handler*"
         "/tmp/project/"
         "/opt/bin/codex --flag --github-token command-secret"
         nil
         session-key
         process-table
         "default"
         "codex"
         nil nil nil nil nil nil))
      (should-not (gethash session-key process-table))
      (should (eq displayed-buffer buffer))
      (should (string-match-p "backend=codex" captured-message))
      (should (string-match-p "/opt/bin/codex" captured-message))
      (should-not (string-match-p "/bin/sh" captured-message))
      (should (string-match-p "/tmp/project/" captured-message))
      (should (string-match-p "126" captured-message))
      (should-not (string-match-p "spawn failed" captured-message))
      (should-not (string-match-p "super-secret" captured-message))
      (should-not (string-match-p "--flag" captured-message))
      (should (string-match-p "Command argv:" (buffer-string)))
      (should (string-match-p "--flag" (buffer-string)))
      (should (string-match-p "<redacted>" (buffer-string)))
      (should-not (string-match-p "/bin/sh" (buffer-string)))
      (should-not (string-match-p "stty sane" (buffer-string)))
      (should-not (string-match-p "command-secret" (buffer-string))))))

(provide 'test_ai-code-backends-infra-startup-diagnostics)

;;; test_ai-code-backends-infra-startup-diagnostics.el ends here
