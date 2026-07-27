;;; test_ai-code-backends-infra-startup-diagnostics.el --- Startup diagnostic tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for terminal startup failure diagnostics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)

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
       "OPENAI_API_KEY=environment-secret"
       "--model" "gpt-5"))
    '("codex"
      "--api-key=<redacted>"
      "--token" "<redacted>"
      "--github-token" "<redacted>"
      "--api_key" "<redacted>"
      "--header" "<redacted>"
      "-H" "<redacted>"
      "OPENAI_API_KEY=<redacted>"
      "--model" "gpt-5"))))

(ert-deftest test-ai-code-backends-infra--startup-failure-details-include-context ()
  "Startup failure details should include explicit backend and process context."
  (let ((buffer (generate-new-buffer " *ai-code-startup-failure*")))
    (unwind-protect
        (with-current-buffer buffer
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
                    buffer 'fake-process "codex"
                    "/usr/local/bin/codex --flag")))
              (should (equal (plist-get details :backend) "codex"))
              (should (equal (plist-get details :executable) "/usr/local/bin/codex"))
              (should (equal (plist-get details :command)
                             '("/usr/local/bin/codex" "--flag")))
              (should (equal (plist-get details :cwd) "/tmp/example/"))
              (should (equal (plist-get details :status) 'exit))
              (should (= (plist-get details :exit-status) 127))
              (should-not (plist-member details :last-output)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra--create-new-session-failure-is-actionable-and-safe ()
  "A real startup failure should show actionable details without logging output."
  (let ((buffer (generate-new-buffer " *ai-code-startup-failure-handler*"))
        (session-key '("/tmp/project/" . "default"))
        (process-table (make-hash-table :test 'equal))
        displayed-buffer
        captured-message)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local default-directory "/tmp/project/")
            (insert "spawn failed token=super-secret"))
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
          (with-current-buffer buffer
            (should (string-match-p "Command argv:" (buffer-string)))
            (should (string-match-p "--flag" (buffer-string)))
            (should (string-match-p "<redacted>" (buffer-string)))
            (should-not (string-match-p "/bin/sh" (buffer-string)))
            (should-not (string-match-p "stty sane" (buffer-string)))
            (should-not (string-match-p "command-secret" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'test_ai-code-backends-infra-startup-diagnostics)

;;; test_ai-code-backends-infra-startup-diagnostics.el ends here
