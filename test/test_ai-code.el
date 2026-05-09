;;; test_ai-code.el --- Tests for ai-code.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code.el behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(defun ai-code-test--maybe-prefer-packaged-transient ()
  "Prefer the newest packaged Transient when one is installed."
  (let* ((pattern (expand-file-name "transient-*" package-user-dir))
         (candidates (sort (file-expand-wildcards pattern) #'version<))
         (latest (car (last candidates))))
    (when latest
      (add-to-list 'load-path latest))))

(ai-code-test--maybe-prefer-packaged-transient)

(require 'transient)

(unless (fboundp 'transient-define-group)
  (error "AI Code tests require transient-define-group; please install transient >= 0.9.0"))

(require 'ai-code)

(ert-deftest ai-code-test-require-ai-code-loads-harness-module ()
  "Test that loading `ai-code` also loads the harness module."
  (should (featurep 'ai-code-harness)))

(ert-deftest ai-code-test-select-terminal-updates-terminal-backend-and-syncs-reflow-advice ()
  "Test that selecting a terminal backend updates infra state."
  (let ((ai-code-backends-infra-terminal-backend 'eat)
        sync-called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &optional _predicate require-match
                               _initial-input _hist def &rest _args)
                 (should (equal prompt "Select terminal: "))
                 (should require-match)
                 (should (equal collection '("eat" "vterm" "ghostel")))
                 (should (equal def "eat"))
                 "ghostel"))
              ((symbol-function 'ai-code-backends-infra--sync-reflow-filter-advice)
               (lambda ()
                 (setq sync-called t)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (ai-code-select-terminal)
      (should (eq ai-code-backends-infra-terminal-backend 'ghostel))
      (should sync-called))))

(ert-deftest ai-code-test-debug-emacs-runtime-uses-global-eval-flag-in-prompt ()
  "Debug Emacs runtime should describe the global eval flag state."
  (let (description-prompt
        confirm-read-args
        message-text
        sent-prompt)
    (let ((ai-code-mcp-debug-tools-enabled t)
          (ai-code-mcp-debug-tools-enable-eval-elisp t))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args)
                   (ert-fail "Should not ask whether eval_elisp is allowed.")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-text (apply #'format format-string args))))
                ((symbol-function 'ai-code-read-string)
               (lambda (prompt &optional initial-input _candidate-list)
                 (cond
                  ((string-match-p "Describe the Emacs runtime issue" prompt)
                   (setq description-prompt prompt)
                   "C-c x runs the wrong interactive command")
                  ((string-match-p "Confirm and edit Emacs runtime debug prompt" prompt)
                   (setq confirm-read-args (list prompt initial-input))
                   initial-input)
                  (t
                   (ert-fail (format "Unexpected prompt: %s" prompt))))))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq sent-prompt prompt))))
        (ai-code-debug-emacs-runtime)))
    (should (string-match-p "interactive function or a key binding"
                            description-prompt))
    (should (equal (car confirm-read-args)
                   "Confirm and edit Emacs runtime debug prompt: "))
    (should (string-match-p "Use the Emacs MCP tools available in this session"
                            (cadr confirm-read-args)))
    (should (string-match-p "eval_elisp is enabled" message-text))
    (should (string-match-p "Emacs can use it for debugging" message-text))
    (should (string-match-p "eval_elisp is enabled in your Emacs MCP config\\."
                            (cadr confirm-read-args)))
    (should-not (string-match-p "allowed for this debugging run"
                                (cadr confirm-read-args)))
    (should (string-match-p "C-c x runs the wrong interactive command"
                            sent-prompt))))

(ert-deftest ai-code-test-debug-emacs-runtime-warns-when-global-eval-flag-is-off ()
  "Debug Emacs runtime should recommend enabling the global eval flag."
  (let ((ai-code-mcp-debug-tools-enabled t)
        (ai-code-mcp-debug-tools-enable-eval-elisp nil)
        confirm-read-args
        description-prompt
        message-text
        sent-prompt)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _args)
                 (ert-fail "Should not ask whether eval_elisp is allowed.")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-text (apply #'format format-string args))))
              ((symbol-function 'ai-code-read-string)
               (lambda (prompt &optional initial-input _candidate-list)
                 (cond
                  ((string-match-p "Describe the Emacs runtime issue" prompt)
                   (setq description-prompt prompt)
                   "M-x foo fails")
                  ((string-match-p "Confirm and edit Emacs runtime debug prompt" prompt)
                   (setq confirm-read-args (list prompt initial-input))
                   initial-input)
                  (t
                   (ert-fail (format "Unexpected prompt: %s" prompt))))))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq sent-prompt prompt))))
      (ai-code-debug-emacs-runtime))
    (should (string-match-p "Describe the Emacs runtime issue"
                            description-prompt))
    (should (string-match-p "eval_elisp is disabled" message-text))
    (should (string-match-p "better to turn it on" message-text))
    (should (string-match-p "improve debugging capability" message-text))
    (should (string-match-p
             "eval_elisp is disabled in your Emacs MCP config"
             (cadr confirm-read-args)))
    (should (string-match-p "M-x foo fails" sent-prompt))))

(ert-deftest ai-code-test-debug-emacs-runtime-always-allows-enabled-eval ()
  "Debug Emacs runtime should not mention per-run eval consent."
  (let (confirm-read-args)
    (let ((ai-code-mcp-debug-tools-enabled t)
          (ai-code-mcp-debug-tools-enable-eval-elisp t))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args)
                   (ert-fail "Should not ask whether eval_elisp is allowed.")))
                ((symbol-function 'ai-code-read-string)
                 (lambda (prompt &optional initial-input _candidate-list)
                   (if (string-match-p "Confirm and edit Emacs runtime debug prompt" prompt)
                       (progn
                         (setq confirm-read-args (list prompt initial-input))
                         initial-input)
                     "C-c x runs the wrong interactive command")))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (&rest _args) nil)))
        (ai-code-debug-emacs-runtime)))
    (should (string-match-p
             "eval_elisp is enabled in your Emacs MCP config\\."
             (cadr confirm-read-args)))
    (should-not (string-match-p
                 "debugging run"
                 (cadr confirm-read-args)))))

(ert-deftest ai-code-test-debug-emacs-runtime-removes-stale-done-comment ()
  "The source should not keep the stale DONE note for the runtime debug menu item."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "ai-code.el" default-directory))
    (should-not
     (search-forward ";; DONE: add a menu item: Debug your emacs runtime." nil t))))

(ert-deftest ai-code-test-menu-ai-cli-session-includes-select-terminal-entry ()
  "Test that the AI CLI session menu exposes terminal backend selection."
  (let ((suffix (transient-get-suffix 'ai-code--menu-ai-cli-session "l")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'ai-code-select-terminal))))

(ert-deftest ai-code-test-menu-other-tools-includes-debug-emacs-runtime-entry ()
  "Test that the Other Tools menu exposes Emacs runtime debugging."
  (let ((suffix (transient-get-suffix 'ai-code--menu-other-tools "d")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'ai-code-debug-emacs-runtime))
    (should (equal (plist-get (cdr suffix) :description)
                   "Debug Emacs runtime"))))

(ert-deftest ai-code-test-menu-prefix-command-default-layout ()
  "Test that the default menu layout uses the original transient."
  (let ((ai-code-menu-layout 'default))
    (should (eq #'ai-code-menu-default
                (ai-code--menu-prefix-command)))))

(ert-deftest ai-code-test-menu-prefix-command-two-columns-layout ()
  "Test that the two-column menu layout uses the narrower transient."
  (let ((ai-code-menu-layout 'two-columns))
    (should (eq #'ai-code-menu-2-columns
                (ai-code--menu-prefix-command)))))

(ert-deftest ai-code-test-menu-includes-quickstart-entry ()
  "Test that the default menu exposes a quickstart entry."
  (should (transient-get-suffix 'ai-code--menu-other-tools
                                'ai-code-onboarding-open-quickstart)))

(ert-deftest ai-code-test-menu-calls-onboarding-gate-before-opening-transient ()
  "Test that `ai-code-menu' runs the onboarding gate before opening the menu."
  (let ((gate-called nil)
        (called-prefix nil)
        (ai-code-menu-layout 'default))
    (cl-letf (((symbol-function 'ai-code-onboarding-maybe-show-quickstart)
               (lambda ()
                 (setq gate-called t)))
              ((symbol-function 'call-interactively)
               (lambda (command &rest _args)
                 (setq called-prefix command))))
      (ai-code-menu)
      (should gate-called)
      (should (eq called-prefix #'ai-code-menu-default)))))

(ert-deftest ai-code-test-menu-keeps-source-buffer-selected-when-auto-showing-quickstart ()
  "Auto-showing quickstart should not change the source buffer for the menu."
  (let ((ai-code-menu-layout 'default)
        (ai-code-onboarding-auto-show t)
        (ai-code-onboarding-seen nil)
        selected-buffer
        source-buffer)
    (with-temp-buffer
      (setq source-buffer (current-buffer))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _args)
                   (set-buffer (get-buffer buffer))))
                ((symbol-function 'call-interactively)
                 (lambda (_command &rest _args)
                   (setq selected-buffer (current-buffer)))))
        (ai-code-menu)
        (should (eq selected-buffer source-buffer))))))

(ert-deftest ai-code-test-menu-prefix-command-fallback-to-default-layout ()
  "Test that unknown menu layout values still fall back to the default transient."
  (let ((ai-code-menu-layout 'unexpected-layout))
    (should (eq #'ai-code-menu-default
                (ai-code--menu-prefix-command)))))

(ert-deftest ai-code-test-menu-dispatches-to-selected-layout ()
  "Test that `ai-code-menu` dispatches to the configured transient command."
  (let ((ai-code-menu-layout 'two-columns)
        called-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq called-fn fn))))
      (ai-code-menu)
      (should (eq called-fn #'ai-code-menu-2-columns)))))

(ert-deftest ai-code-test-package-requires-transient-0-9 ()
  "Test that ai-code requires Transient 0.9 or newer."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "ai-code.el" default-directory))
    (should (re-search-forward
             "Package-Requires: ((emacs \"29\\.1\") (transient \"0\\.9\\.0\") (magit \"2\\.1\\.0\"))"
             nil t))))

(ert-deftest ai-code-test-menu-groups-define-four-sections ()
  "Test that menu sections are defined as reusable transient groups."
  (dolist (group '(ai-code--menu-ai-cli-session
                   ai-code--menu-actions-with-context
                   ai-code--menu-agile-development
                   ai-code--menu-other-tools))
    (should (get group 'transient--layout))))

(ert-deftest ai-code-test-menu-prefix-commands-are-interactive ()
  "Test that the main menu and layout-specific menus are defined as commands."
  (dolist (cmd '(ai-code-menu
                 ai-code-menu-default
                 ai-code-menu-2-columns))
    (should (fboundp cmd))
    (should (commandp cmd))))

(ert-deftest ai-code-test-menu-agile-development-includes-speech-to-text-input ()
  "Test that Agile Development menu exposes speech-to-text input."
  (let ((suffix (transient-get-suffix 'ai-code--menu-agile-development ":")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'ai-code-speech-to-text-input))
    (should (equal (plist-get (cdr suffix) :description)
                   "Speech to text input"))))

(provide 'test_ai-code)

;;; test_ai-code.el ends here
