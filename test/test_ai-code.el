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

(ert-deftest ai-code-test-autoloads-load-with-harness-custom-unbound ()
  "Test that loading autoloads works before harness custom is defined."
  (let* ((autoload-file (expand-file-name "ai-code-autoloads.el" default-directory))
         (symbols '(ai-code-auto-test-suffix
                    ai-code-test-after-code-change-suffix))
         (saved-states
          (mapcar (lambda (symbol)
                    (list symbol
                          (boundp symbol)
                          (when (boundp symbol)
                            (symbol-value symbol))))
                  symbols)))
    (unwind-protect
        (progn
          (mapc #'makunbound symbols)
          (should
           (eq 'loaded
               (condition-case nil
                   (progn
                     (load autoload-file nil t)
                     'loaded)
                 (error 'failed))))
          (should (boundp 'ai-code-test-after-code-change-suffix)))
      (dolist (state saved-states)
        (pcase-let ((`(,symbol ,was-bound ,value) state))
          (if was-bound
              (set symbol value)
            (makunbound symbol)))))))

(ert-deftest ai-code-test-set-auto-test-type-ask-me-clears-persistent-suffix ()
  "Test that setting auto test type to ask-me clears the persistent suffix."
  (let ((ai-code-auto-test-suffix "old")
        (ai-code-auto-test-type nil)
        (ai-code--tdd-test-pattern-instruction nil))
    (ai-code--apply-auto-test-type 'ask-me)
    (should (eq 'ask-me ai-code-auto-test-type))
    (should-not ai-code-auto-test-suffix)))

(ert-deftest ai-code-test-set-auto-test-type-off-clears-persistent-suffix ()
  "Test that turning off auto test type clears the persistent suffix."
  (let ((ai-code-auto-test-suffix "old")
        (ai-code-auto-test-type 'ask-me))
    (ai-code--apply-auto-test-type nil)
    (should-not ai-code-auto-test-type)
    (should-not ai-code-auto-test-suffix)))

(ert-deftest ai-code-test-resolve-test-after-change-suffix-includes-diagnostics-for-mcp-backend ()
  "Test that test-after-change suffix points to the diagnostics harness file."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-type 'test-after-change)
         (ai-code-auto-test-harness-cache-directory nil)
         (ai-code-selected-backend 'codex))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir))
                  ((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) temp-root)))
          (let ((suffix (ai-code--resolve-auto-test-suffix-for-send)))
            (should (string-match-p
                     (regexp-quote "@.ai.code.files/harness/test-after-change-diagnostics.v1.md")
                     suffix))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-resolve-test-after-change-suffix-omits-diagnostics-for-non-mcp-backend ()
  "Test that unsupported backends use the non-diagnostics harness variant."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-type 'test-after-change)
         (ai-code-auto-test-harness-cache-directory nil)
         (ai-code-selected-backend 'gemini))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir))
                  ((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) temp-root)))
          (let ((suffix (ai-code--resolve-auto-test-suffix-for-send)))
            (should (string-match-p
                     (regexp-quote "@.ai.code.files/harness/test-after-change.v1.md")
                     suffix))
            (should-not (string-match-p "diagnostics.v1.md" suffix))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-off ()
  "Test that off mode never resolves a send-time auto test type."
  (let ((ai-code-auto-test-type nil))
    (should-not (ai-code--resolve-auto-test-type-for-send))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-legacy-persistent-modes ()
  "Test that legacy persistent auto test modes still resolve at send time."
  (dolist (mode '(test-after-change tdd tdd-with-refactoring))
    (let ((ai-code-auto-test-type mode))
      (should (eq mode
                  (ai-code--resolve-auto-test-type-for-send))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me ()
  "Test that ask-me mode resolves by interactive per-send selection."
  (let ((ai-code-auto-test-type 'ask-me))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'tdd)))
      (should (eq 'tdd (ai-code--resolve-auto-test-type-for-send))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me-gptel-non-code-change ()
  "Test that ask-me mode skips selection when GPTel classifies non-code change."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change))
              ((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () (ert-fail "Should not ask test type for non-code prompts."))))
      (should (eq nil (ai-code--resolve-auto-test-type-for-send "Explain this function"))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me-gptel-code-change ()
  "Test that ask-me mode prompts user to select test type when GPTel classifies code change."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'code-change))
              ((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'test-after-change)))
      (should (eq 'test-after-change
                  (ai-code--resolve-auto-test-type-for-send "Refactor this function"))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me-gptel-unknown-fallback ()
  "Test that ask-me mode falls back to interactive selection when GPTel is unknown."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'unknown))
              ((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'test-after-change)))
      (should (eq 'test-after-change
                  (ai-code--resolve-auto-test-type-for-send "Please update code"))))))

(ert-deftest ai-code-test-read-auto-test-type-choice-allow-no-test ()
  "Test that ask choices support selecting no test run."
  (let ((ai-code--auto-test-type-ask-choices
         '(("Run tests after code change" . test-after-change)
           ("TDD Red + Green (write failing test, then make it pass)" . tdd)
           ("TDD Red + Green + Blue (refactor after Green)" . tdd-with-refactoring)
           ("Do not write or run tests" . no-test))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Do not write or run tests")))
      (should (eq 'no-test (ai-code--read-auto-test-type-choice))))))

(ert-deftest ai-code-test-read-auto-test-type-choice-allow-tdd-with-refactoring ()
  "Test that ask choices support selecting tdd-with-refactoring."
  (let ((ai-code--auto-test-type-ask-choices
         '(("Run tests after code change" . test-after-change)
           ("TDD Red + Green (write failing test, then make it pass)" . tdd)
           ("TDD Red + Green + Blue (refactor after Green)" . tdd-with-refactoring)
           ("Do not write or run tests" . no-test))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "TDD Red + Green + Blue (refactor after Green)")))
      (should (eq 'tdd-with-refactoring (ai-code--read-auto-test-type-choice))))))

(ert-deftest ai-code-test-resolve-auto-follow-up-suffix-for-send-off ()
  "Test that off mode never resolves a discussion follow-up suffix."
  (let ((ai-code-discussion-auto-follow-up-enabled nil)
        (this-command 'ai-code-ask-question))
    (should-not (ai-code--resolve-auto-follow-up-suffix-for-send "Explain this function"))))

(ert-deftest ai-code-test-resolve-auto-follow-up-suffix-for-send-ask-me-non-code-change ()
  "Test that ask-me mode can append next-step suggestions for discussion prompts."
  (let ((ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-use-gptel-classify-prompt t)
        (this-command 'ai-code-ask-question))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change))
              ((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda () t)))
      (should (string-match-p
               "3-4 numbered candidate next[[:space:]\n]+steps"
               (ai-code--resolve-auto-follow-up-suffix-for-send
                "Explain this function")))
      (should (string-match-p
               "At least 2 candidates must[[:space:]\n]+be AI-actionable items"
               (ai-code--resolve-auto-follow-up-suffix-for-send
                "Explain this function"))))))

(ert-deftest ai-code-test-resolve-auto-follow-up-suffix-for-send-ask-me-code-change-skips ()
  "Test that ask-me mode does not offer next-step suggestions for code-change prompts."
  (let ((ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-use-gptel-classify-prompt t)
        (this-command 'ai-code-ask-question)
        (asked nil))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'code-change))
              ((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda ()
                 (setq asked t)
                 t)))
      (should-not
       (ai-code--resolve-auto-follow-up-suffix-for-send
        "Please update the code"))
      (should-not asked))))

(ert-deftest ai-code-test-resolve-auto-follow-up-suffix-for-send-enabled-for-any-non-code-change-prompt ()
  "Test that the feature affects any prompt classified as non-code-change."
  (let ((ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change))
              ((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda () t)))
      (let ((this-command 'ai-code-ask-question))
        (should (string-match-p
                 "The user may also[[:space:]\n]+ignore these options"
                 (ai-code--resolve-auto-follow-up-suffix-for-send
                  "Explain this function"))))
      (let ((this-command 'ai-code-send-command))
        (should (string-match-p
                 "The user may also[[:space:]\n]+ignore these options"
                 (ai-code--resolve-auto-follow-up-suffix-for-send
                  "Summarize this design")))))))

(ert-deftest ai-code-test-write-prompt-appends-follow-up-suffix-for-discussion-prompts ()
  "Test that discussion prompts can append next-step suggestions."
  (let ((sent-command nil)
        (ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-use-gptel-classify-prompt t)
        (ai-code-use-prompt-suffix t)
        (ai-code-prompt-suffix "BASE SUFFIX")
        (this-command 'ai-code-ask-question))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change))
              ((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda () t))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
               (lambda () nil))
              ((symbol-function 'ai-code-cli-send-command)
               (lambda (command) (setq sent-command command)))
              ((symbol-function 'ai-code-cli-switch-to-buffer)
               (lambda (&rest _args) nil)))
      (ai-code--write-prompt-to-file-and-send "Explain this function")
      (should (string-match-p "BASE SUFFIX" sent-command))
      (should (string-match-p "3-4 numbered candidate next[[:space:]\n]+steps"
                              sent-command))
      (should (string-match-p
               "At least 2 candidates must[[:space:]\n]+be AI-actionable items"
               sent-command))
      (should (string-match-p
               "If the user replies with[[:space:]\n]+only a number"
               sent-command)))))

(ert-deftest ai-code-test-write-prompt-records-follow-up-suffix-in-prompt-file ()
  "Test that discussion follow-up suffix is also recorded in the prompt file."
  (let* ((temp-dir (make-temp-file "ai-code-prompt-" t))
         (prompt-file (expand-file-name ".ai.code.prompt.org" temp-dir))
         (ai-code-discussion-auto-follow-up-enabled t)
         (ai-code-use-gptel-classify-prompt t)
         (ai-code-use-prompt-suffix t)
         (ai-code-prompt-suffix "BASE SUFFIX")
         (this-command 'ai-code-ask-question))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
                   (lambda (_prompt-text) 'non-code-change))
                  ((symbol-function 'ai-code--read-auto-follow-up-choice)
                   (lambda () t))
                  ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
                   (lambda () prompt-file))
                  ((symbol-function 'ai-code-cli-send-command)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-cli-switch-to-buffer)
                   (lambda (&rest _args) nil)))
          (ai-code--write-prompt-to-file-and-send "Explain this function")
          (with-temp-buffer
            (insert-file-contents prompt-file)
            (let ((contents (buffer-string)))
              (should (string-match-p "Explain this function" contents))
              (should (string-match-p "BASE SUFFIX" contents))
              (should (string-match-p "3-4 numbered candidate next[[:space:]\n]+steps"
                                      contents))
              (should (string-match-p
                       "At least 2 candidates must[[:space:]\n]+be AI-actionable items"
                       contents)))))
      (delete-directory temp-dir t))))

(ert-deftest ai-code-test-write-prompt-appends-follow-up-suffix-for-send-command-non-code-change ()
  "Test that send-command also gets next-step suggestions when classified non-code-change."
  (let ((sent-command nil)
        (ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-use-gptel-classify-prompt t)
        (ai-code-use-prompt-suffix t)
        (this-command 'ai-code-send-command))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change))
              ((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda () t))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
               (lambda () nil))
              ((symbol-function 'ai-code-cli-send-command)
               (lambda (command) (setq sent-command command)))
              ((symbol-function 'ai-code-cli-switch-to-buffer)
               (lambda (&rest _args) nil)))
      (ai-code--write-prompt-to-file-and-send "Summarize this design")
      (should (string-match-p "3-4 numbered candidate next[[:space:]\n]+steps"
                              sent-command))
      (should (string-match-p
               "either a code change or tool usage"
               sent-command)))))

(ert-deftest ai-code-test-next-step-suggestion-suffix-requires-actionable-items ()
  "Test that numbered next-step suggestions require actionable AI items."
  (should (string-match-p
           "3-4 numbered candidate next[[:space:]\n]+steps"
           ai-code-next-step-suggestion-suffix))
  (should (string-match-p
           "At least 2 candidates must[[:space:]\n]+be AI-actionable items"
           ai-code-next-step-suggestion-suffix))
  (should (string-match-p
           "either a code change or tool usage"
           ai-code-next-step-suggestion-suffix)))

(ert-deftest ai-code-test-auto-test-type-ask-choices-use-explicit-red-green-blue-labels ()
  "Test that default ask choices use explicit staged TDD labels."
  (should (assoc "TDD Red + Green (write failing test, then make it pass)"
                 ai-code--auto-test-type-ask-choices))
  (should (assoc "TDD Red + Green + Blue (refactor after Green)"
                 ai-code--auto-test-type-ask-choices))
  (should-not (assoc "Test driven development: Write test first"
                     ai-code--auto-test-type-ask-choices))
  (should-not (assoc "Test driven development, follow up with refactoring"
                     ai-code--auto-test-type-ask-choices)))

(ert-deftest ai-code-test-auto-test-type-custom-options-are-ask-or-off ()
  "Test that persistent auto test type choices only expose ask-me and off."
  (should
   (equal
    '(choice (const :tag "Ask every time" ask-me)
             (const :tag "Off" nil))
    (get 'ai-code-auto-test-type 'custom-type))))

(ert-deftest ai-code-test-auto-test-type-persistent-choices-are-ask-or-off ()
  "Test that persistent auto test type choices are shared and limited."
  (should (equal '(("Ask every time" . ask-me)
                   ("Off" . nil))
                 ai-code--auto-test-type-persistent-choices)))

(ert-deftest ai-code-test-discussion-auto-follow-up-enabled-custom-option-is-boolean ()
  "Test that discussion auto follow-up setting is a boolean toggle."
  (should
   (equal
    'boolean
    (get 'ai-code-discussion-auto-follow-up-enabled 'custom-type))))

(ert-deftest ai-code-test-resolve-auto-test-suffix-for-send-ask-me-tdd-with-refactoring ()
  "Test that ask-me resolves to the repo-local TDD harness reference."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-harness-cache-directory nil)
         (ai-code-auto-test-type 'ask-me)
         (ai-code-selected-backend 'codex)
         (ai-code--tdd-test-pattern-instruction ""))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir))
                  ((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) temp-root))
                  ((symbol-function 'ai-code--read-auto-test-type-choice)
                   (lambda () 'tdd-with-refactoring)))
          (let ((suffix (ai-code--resolve-auto-test-suffix-for-send)))
            (should (string-match-p
                     (regexp-quote "@.ai.code.files/harness/tdd-with-refactoring-diagnostics.v1.md")
                     suffix))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-resolve-auto-test-suffix-for-send-ask-me-no-test ()
  "Test that ask-me can resolve to explicit no-test suffix."
  (let ((ai-code-auto-test-type 'ask-me))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'no-test)))
      (should (equal "Do not write or run any test."
                     (ai-code--resolve-auto-test-suffix-for-send))))))

(ert-deftest ai-code-test-write-prompt-ask-me-no-test-appends-explicit-no-test-instruction ()
  "Test that ask-me no-test choice appends explicit no-test instruction."
  (let ((sent-command nil)
        (ai-code-auto-test-type 'ask-me)
        (ai-code-use-prompt-suffix t)
        (ai-code-prompt-suffix "BASE SUFFIX")
        (ai-code-auto-test-suffix "SHOULD NOT APPEAR"))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'no-test))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
               (lambda () nil))
              ((symbol-function 'ai-code-cli-send-command)
               (lambda (command) (setq sent-command command)))
              ((symbol-function 'ai-code-cli-switch-to-buffer)
               (lambda (&rest _args) nil)))
      (ai-code--write-prompt-to-file-and-send "Implement feature")
      (should (string-match-p "BASE SUFFIX" sent-command))
      (should (string-match-p "Do not write or run any test\\." sent-command))
      (should-not (string-match-p "SHOULD NOT APPEAR" sent-command)))))

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
