;;; test_ai-code-session-submit.el --- Final session submission regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise both sending entry points with actual session metadata and mock I/O.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-prompt-mode)
(require 'ai-code-backends-infra)

(defun ai-code-submit-test--exercise (route text prefix paste-backends &optional failure)
  "Exercise ROUTE with TEXT, PREFIX and PASTE-BACKENDS.
FAILURE selects an optional failing transport stage.  Return observed effects."
  (let* ((source (generate-new-buffer " *submit-source*"))
         (target (generate-new-buffer "*codex[submit-target]*"))
         (other (generate-new-buffer "*codex[submit-other]*"))
         (ai-code-session--sessions (make-hash-table :test 'equal))
         (ai-code-backends-infra--directory-buffer-map (make-hash-table :test 'equal))
         (ai-code-backends-infra--file-session-map (make-hash-table :test 'equal))
         (ai-code-backends-infra--last-accessed-buffer other)
         (ai-code-backends-infra-use-paste-backends paste-backends)
         (note-function (symbol-function 'ai-code-session-note-prompt))
         (session (ai-code-session-register
                   :buffer target :backend "codex" :repo-root "/tmp/"
                   :metadata '(:last-prompt "OLD TASK" :last-prompt-at 1.0)))
         events caught result
         (choices 0)
         (resolutions 0))
    (unwind-protect
        (progn
          (with-current-buffer target
            (setq-local ai-code-backends-infra--session-prefix prefix))
          (cl-letf (((symbol-function 'ai-code--prompt-choose-target-session)
                     (lambda () (cl-incf choices) target))
                    ((symbol-function 'ai-code-backends-infra--resolve-session-buffer)
                     (lambda (&rest _) (cl-incf resolutions) target))
                    ((symbol-function 'ai-code-mcp-agent-refresh-source-context)
                     (lambda (agent origin)
                       (push (list 'refresh agent origin) events)
                       (when (eq failure 'refresh) (error "Refresh failed"))))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (string &optional paste)
                       (push (list 'text (current-buffer) string paste) events)
                       (when (eq failure 'text) (error "Text failed"))))
                    ((symbol-function 'sit-for)
                     (lambda (seconds) (push (list 'wait seconds) events)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-return)
                     (lambda ()
                       ;; Until submission succeeds the dashboard retains its old task.
                       (push (list 'return (current-buffer)
                                   (plist-get (ai-code-session-metadata session) :last-prompt))
                             events)
                       (when (eq failure 'return) (error "Return failed"))
                       'submitted))
                    ((symbol-function 'ai-code-session-note-prompt)
                     (lambda (buffer prompt)
                       (push (list 'note buffer prompt) events)
                       (funcall note-function buffer prompt)))
                    ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                    ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                     (lambda (buffer) (push (list 'display buffer) events))))
            (with-current-buffer source
              (condition-case err
                  (setq result
                        (pcase route
                          ('visible (ai-code--send-prompt text))
                          ('cli (ai-code-backends-infra--send-line-to-session
                                 nil "Missing" text prefix "/tmp/"))))
                (error (setq caught err)))))
          (list :events (nreverse events) :error caught :result result
                :metadata (copy-sequence (ai-code-session-metadata session))
                :source source :target target :other other
                :last-accessed ai-code-backends-infra--last-accessed-buffer
                :choices choices :resolutions resolutions))
      (mapc #'kill-buffer (list source target other)))))

(ert-deftest ai-code-test-submit-visible-session-updates-dashboard ()
  "Visible-session delivery updates the actual recipient's dashboard metadata."
  (let* ((observed (ai-code-submit-test--exercise 'visible "NEW TASK" "codex" nil))
         (metadata (plist-get observed :metadata)))
    (should-not (plist-get observed :error))
    (should (equal (plist-get metadata :last-prompt) "NEW TASK"))
    (should (> (plist-get metadata :last-prompt-at) 1.0))
    (should (eq (plist-get observed :last-accessed) (plist-get observed :target)))))

(ert-deftest ai-code-test-submit-both-routes-preserve-transport-contract ()
  "Both routes keep paste policy, source context, one Return and one recording."
  (dolist (route '(visible cli))
    (dolist (case '(("single" "codex" ("codex") nil)
                    ("first\nsecond" "codex" ("codex") t)
                    ("first\nsecond" "claude" ("codex") nil)
                    ("first\nsecond" "codex" nil nil)))
      (pcase-let* ((`(,text ,prefix ,paste-backends ,paste) case)
                   (observed (ai-code-submit-test--exercise route text prefix paste-backends))
                   (target (plist-get observed :target))
                   (source (plist-get observed :source))
                   (events (plist-get observed :events)))
        (should-not (plist-get observed :error))
        (should (equal (car events) (list 'refresh target source)))
        (should (equal (cadr events) (list 'text target text paste)))
        (should (equal (nth 2 events) '(wait 0.5)))
        (should (equal (nth 3 events) (list 'return target "OLD TASK")))
        (should (equal (nth 4 events) (list 'note target text)))
        (should (= 1 (cl-count 'return events :key #'car)))
        (should (= 1 (cl-count 'note events :key #'car)))
        (should (equal (plist-get (plist-get observed :metadata) :last-prompt) text))
        (should (eq (plist-get observed :last-accessed) target))
        (if (eq route 'visible)
            (progn
              (should (= 1 (plist-get observed :choices)))
              (should (= 0 (plist-get observed :resolutions)))
              (should (equal (car (last events)) (list 'display target))))
          (should (= 1 (plist-get observed :resolutions)))
          (should (eq (plist-get observed :result) 'submitted))
          (should-not (assq 'display events)))))))

(ert-deftest ai-code-test-submit-failures-do-not-record-a-new-task ()
  "Refresh, text and Return errors must not report a successful new task."
  (dolist (route '(visible cli))
    (dolist (stage '(refresh text return))
      (let* ((observed (ai-code-submit-test--exercise route "NEW TASK" "codex" nil stage))
             (events (plist-get observed :events)))
        (should (plist-get observed :error))
        (should (equal (plist-get (plist-get observed :metadata) :last-prompt) "OLD TASK"))
        (should (= 1.0 (plist-get (plist-get observed :metadata) :last-prompt-at)))
        (should (eq (plist-get observed :last-accessed) (plist-get observed :other)))
        (should-not (assq 'note events))
        (should-not (assq 'display events))
        (unless (eq stage 'return)
          (should-not (assq 'return events)))))))

(ert-deftest ai-code-test-submit-visible-session-keeps-existing-window ()
  "Sending to an already visible session selects its window without redisplay."
  (with-temp-buffer
    (let ((target (current-buffer))
          (ai-code-backends-infra--last-accessed-buffer nil)
          selected)
      (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-send-string) #'ignore)
                ((symbol-function 'ai-code-backends-infra--terminal-send-return) #'ignore)
                ((symbol-function 'ai-code-mcp-agent-refresh-source-context) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'get-buffer-window) (lambda (_) 'existing-window))
                ((symbol-function 'select-window) (lambda (window &rest _) (setq selected window)))
                ((symbol-function 'ai-code-backends-infra--display-buffer-in-side-window)
                 (lambda (_) (ert-fail "Must reuse the existing window"))))
        (ai-code--send-prompt-to-session-buffer "Inspect" target)
        (should (eq selected 'existing-window))))))

(provide 'test_ai-code-session-submit)
;;; test_ai-code-session-submit.el ends here
