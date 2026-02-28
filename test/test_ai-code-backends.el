;;; test_ai-code-backends.el --- Tests for ai-code-backends.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-backends.el behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'magit)
  (provide 'magit))

(require 'ai-code-backends)

(ert-deftest ai-code-test-open-backend-agent-file-opens-path ()
  "Test that agent file opens from git root using backend config."
  (let* ((temp-dir (make-temp-file "ai-code-agent-" t))
         (backend-key 'test-backend)
         (ai-code-backends `((,backend-key
                              :label "Test Backend"
                              :agent-file "AGENTS.md")))
         (ai-code-selected-backend backend-key)
         (opened-path nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () temp-dir))
                  ((symbol-function 'find-file-other-window)
                   (lambda (path) (setq opened-path path))))
          (ai-code-open-backend-agent-file)
          (should (string= opened-path
                           (expand-file-name "AGENTS.md" temp-dir))))
      (when (and temp-dir (file-directory-p temp-dir))
        (delete-directory temp-dir t)))))

(ert-deftest ai-code-test-cli-send-command-nil-errors-noninteractive ()
  "Ensure nil COMMAND errors in noninteractive calls."
  (let ((ai-code--cli-send-fn (lambda (_command)
                                (ert-fail "Should not be called"))))
    (should-error (ai-code-cli-send-command nil)
                  :type 'user-error)))

(ert-deftest ai-code-test-cli-resume-preserves-prefix-arg ()
  "Ensure `current-prefix-arg' reaches backend resume when ARG is nil."
  (let* ((backend-key 'test-backend)
         (ai-code-backends `((,backend-key
                              :label "Test Backend"
                              :start ai-code-test-start
                              :switch ai-code-test-switch
                              :send ai-code-test-send
                              :resume ai-code-test-resume
                              :cli "test")))
         (saved-start ai-code--cli-start-fn)
         (saved-switch ai-code--cli-switch-fn)
         (saved-send ai-code--cli-send-fn)
         (saved-resume ai-code--cli-resume-fn)
         (saved-backend ai-code-selected-backend)
         (saved-cli (and (boundp 'ai-code-cli) ai-code-cli))
         (resume-arg nil))
    (cl-letf (((symbol-function 'ai-code-test-start) (lambda (&optional _arg)))
              ((symbol-function 'ai-code-test-switch) (lambda (&optional _arg)))
              ((symbol-function 'ai-code-test-send) (lambda (&optional _arg)))
              ((symbol-function 'ai-code-test-resume)
               (lambda (&optional arg)
                 (interactive "P")
                 (setq resume-arg arg))))
      (unwind-protect
          (progn
            (ai-code--apply-backend backend-key)
            (let ((current-prefix-arg '(4)))
              (ai-code-cli-resume nil))
            (should (equal resume-arg '(4))))
        (setq ai-code--cli-start-fn saved-start
              ai-code--cli-switch-fn saved-switch
              ai-code--cli-send-fn saved-send
              ai-code--cli-resume-fn saved-resume
              ai-code-selected-backend saved-backend
              ai-code-cli saved-cli)))))

(ert-deftest ai-code-test-agent-shell-backend-spec-contract ()
  "Ensure the agent-shell backend entry exposes required integration keys."
  (let ((spec (ai-code--backend-spec 'agent-shell)))
    (should spec)
    (should (eq (plist-get (cdr spec) :require) 'ai-code-agent-shell))
    (should (eq (plist-get (cdr spec) :start) 'ai-code-agent-shell))
    (should (eq (plist-get (cdr spec) :switch) 'ai-code-agent-shell-switch-to-buffer))
    (should (eq (plist-get (cdr spec) :send) 'ai-code-agent-shell-send-command))
    (should (eq (plist-get (cdr spec) :resume) 'ai-code-agent-shell-resume))))

(ert-deftest ai-code-test-backend-selection-keeps-repo-session-backend ()
  "Switching backend in one repo should not overwrite started backend in another repo."
  (let* ((ai-code-backends
          '((backend-a
             :label "Backend A"
             :start ai-code-test-backend-a-start
             :switch ai-code-test-backend-a-switch
             :send ai-code-test-backend-a-send
             :resume nil
             :cli "a")
            (backend-b
             :label "Backend B"
             :start ai-code-test-backend-b-start
             :switch ai-code-test-backend-b-switch
             :send ai-code-test-backend-b-send
             :resume nil
             :cli "b")))
         (ai-code-selected-backend 'backend-a)
         (ai-code--cli-start-fn #'ignore)
         (ai-code--cli-switch-fn #'ignore)
         (ai-code--cli-send-fn #'ignore)
         (ai-code--cli-resume-fn #'ignore)
         (repo-root "/repo-a/")
         (start-calls nil))
    (cl-letf (((symbol-function 'ai-code-test-backend-a-start)
               (lambda (&optional _arg)
                 (push 'backend-a start-calls)))
              ((symbol-function 'ai-code-test-backend-b-start)
               (lambda (&optional _arg)
                 (push 'backend-b start-calls)))
              ((symbol-function 'ai-code-test-backend-a-switch)
               (lambda (&optional _arg) nil))
              ((symbol-function 'ai-code-test-backend-b-switch)
               (lambda (&optional _arg) nil))
              ((symbol-function 'ai-code-test-backend-a-send)
               (lambda (&optional _arg) nil))
              ((symbol-function 'ai-code-test-backend-b-send)
               (lambda (&optional _arg) nil))
              ((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) repo-root))
              ((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (if (string= repo-root "/repo-a/")
                     "Backend A"
                   "Backend B")))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      ;; Start in repo A using Backend A.
      (ai-code-select-backend)
      (ai-code-cli-start)

      ;; Switch to repo B and use Backend B.
      (setq repo-root "/repo-b/")
      (ai-code-select-backend)
      (ai-code-cli-start)
      (should (string= (ai-code-current-backend-label) "Backend B"))

      ;; Returning to repo A should keep using Backend A.
      (setq repo-root "/repo-a/")
      (should (string= (ai-code-current-backend-label) "Backend A"))
      (ai-code-cli-start)

      (should (eq (car start-calls) 'backend-a))
      (should (equal (reverse start-calls)
                     '(backend-a backend-b backend-a))))))

(ert-deftest ai-code-test-install-skills-with-string-command ()
  "Backend with :install-skills string runs it via compile."
  (let* ((compiled-cmd nil)
         (ai-code-backends '((test-backend
                               :label "Test Backend"
                               :install-skills "npm install skills"
                               :cli "test")))
         (ai-code-selected-backend 'test-backend))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq compiled-cmd cmd)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (ai-code-install-backend-skills)
      (should (string= compiled-cmd "npm install skills")))))

(ert-deftest ai-code-test-install-skills-with-function-symbol ()
  "Backend with :install-skills as function symbol calls that function."
  (let* ((fn-called nil)
         (ai-code-backends '((test-backend
                               :label "Test Backend"
                               :install-skills ai-code-test--install-skills-fn
                               :cli "test")))
         (ai-code-selected-backend 'test-backend))
    (cl-letf (((symbol-function 'ai-code-test--install-skills-fn)
               (lambda () (setq fn-called t)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (ai-code-install-backend-skills)
      (should fn-called))))

(ert-deftest ai-code-test-install-skills-fallback-when-nil ()
  "Backend without :install-skills falls back to prompting AI via send-command."
  (let* ((sent-command nil)
         (ai-code-backends '((test-backend
                               :label "Test Backend"
                               :cli "test")))
         (ai-code-selected-backend 'test-backend))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional _initial _history _default &rest _rest)
                 "https://github.com/obra/superpowers"))
              ((symbol-function 'ai-code-cli-send-command)
               (lambda (cmd) (setq sent-command cmd)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (ai-code-install-backend-skills)
      (should (stringp sent-command))
      (should (string-match-p "superpowers" sent-command))
      (should (string-match-p "README" sent-command)))))

(ert-deftest ai-code-test-install-skills-no-backend-errors ()
  "Missing backend signals user-error."
  (let ((ai-code-selected-backend 'nonexistent-backend)
        (ai-code-backends nil))
    (should-error (ai-code-install-backend-skills)
                  :type 'user-error)))

(ert-deftest ai-code-test-claude-code-install-skills-sends-prompt ()
  "Claude Code install-skills function prompts for URL and sends a Claude-specific prompt."
  (let* ((sent-command nil)
         (prompted-url nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional _initial _history _default &rest _rest)
                 (setq prompted-url t)
                 "https://github.com/obra/superpowers"))
              ((symbol-function 'ai-code-cli-send-command)
               (lambda (cmd) (setq sent-command cmd)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (ai-code-claude-code-install-skills)
      (should prompted-url)
      (should (stringp sent-command))
      (should (string-match-p "superpowers" sent-command))
      (should (string-match-p "skill" sent-command)))))

(ert-deftest ai-code-test-install-skills-errors-on-non-callable-symbol ()
  "Backend with :install-skills as undefined symbol signals user-error."
  (let* ((ai-code-backends '((test-backend
                               :label "Test Backend"
                               :install-skills ai-code-test-nonexistent-install-fn
                               :cli "test")))
         (ai-code-selected-backend 'test-backend))
    (should-error (ai-code-install-backend-skills)
                  :type 'user-error)))

(ert-deftest ai-code-test-claude-code-backend-has-install-skills ()
  "Claude Code backend spec should have :install-skills set to the dedicated function."
  (let ((spec (ai-code--backend-spec 'claude-code)))
    (should spec)
    (should (eq (plist-get (cdr spec) :install-skills)
                'ai-code-claude-code-install-skills))))

(provide 'test_ai-code-backends)

;;; test_ai-code-backends.el ends here
