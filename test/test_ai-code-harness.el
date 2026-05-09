;;; test_ai-code-harness.el --- Tests for ai-code-harness.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for harness generation and prompt suffix helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'ai-code-harness)

(defvar ai-code-mcp-agent-enabled-backends nil)
(defvar ai-code--tdd-run-test-after-each-stage-instruction)
(defvar ai-code-use-prompt-suffix)
(defvar ai-code-prompt-suffix)

(ert-deftest ai-code-test-resolve-tdd-suffix-includes-strict-stage-contract ()
  "Test that TDD suffix names Red and Green stages and forbids skipping."
  (let ((ai-code--tdd-test-pattern-instruction ""))
    (let ((suffix (ai-code--test-after-code-change--resolve-tdd-suffix)))
      (should (string-match-p "Do not skip stages" suffix))
      (should (string-match-p "Stage 1 - Red" suffix))
      (should (string-match-p "Stage 2 - Green" suffix))
      (should (string-match-p "Do not refactor during Green" suffix)))))

(ert-deftest ai-code-test-resolve-tdd-suffix-reuses-shared-each-stage-instruction ()
  "Test that TDD suffix can reuse shared each-stage instruction when available."
  (let ((ai-code--tdd-test-pattern-instruction "")
        (ai-code--tdd-run-test-after-each-stage-instruction
         " SHARED_EACH_STAGE_TEST_INSTRUCTION"))
    (should (string-match-p "SHARED_EACH_STAGE_TEST_INSTRUCTION"
                            (ai-code--test-after-code-change--resolve-tdd-suffix)))))

(ert-deftest ai-code-test-auto-test-harness-reference-suffix-tells-ai-to-use-local-harness ()
  "Test that harness reference prompt tells AI to read and use the harness."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-harness-cache-directory nil)
         (ai-code-mcp-agent-enabled-backends '(codex))
         (ai-code-selected-backend 'codex)
         (ai-code--tdd-test-pattern-instruction ""))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir))
                  ((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) temp-root)))
          (let ((suffix (ai-code--auto-test-harness-reference-suffix 'tdd-with-refactoring)))
            (should (string-match-p "Read the local harness file:" suffix))
            (should (string-match-p "Use its instructions for this work\\." suffix))
            (should (string-match-p
                     (regexp-quote "@.ai.code.files/harness/tdd-with-refactoring-diagnostics.v1.md")
                     suffix))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-resolve-tdd-suffix-includes-diagnostics-first-loop ()
  "Test that TDD suffix requires diagnostics checks before completion."
  (let ((ai-code--tdd-test-pattern-instruction "")
        (case-fold-search nil)
        (ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-selected-backend 'codex))
    (let ((suffix (ai-code--test-after-code-change--resolve-tdd-suffix)))
      (should (string-match-p "get_diagnostics" suffix))
      (should (string-match-p "get_diagnostics MCP tool" suffix))
      (should (string-match-p "baseline" suffix))
      (should (string-match-p "no new diagnostics" suffix)))))

(ert-deftest ai-code-test-resolve-tdd-suffix-omits-diagnostics-for-non-mcp-backend ()
  "Test that TDD suffix omits diagnostics for unsupported backends."
  (let ((ai-code--tdd-test-pattern-instruction "")
        (ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-selected-backend 'gemini))
    (let ((suffix (ai-code--test-after-code-change--resolve-tdd-suffix)))
      (should-not (string-match-p "get_diagnostics" suffix))
      (should-not (string-match-p "no new diagnostics" suffix)))))

(ert-deftest ai-code-test-maybe-append-diagnostics-harness-instruction-preserves-nil-suffix ()
  "Test that diagnostics harness logic preserves a nil suffix."
  (let ((ai-code-selected-backend 'codex)
        (ai-code-mcp-agent-enabled-backends '(codex)))
    (should-not (ai-code--maybe-append-diagnostics-harness-instruction nil))
    (should-not (ai-code--maybe-append-diagnostics-harness-instruction nil t))))

(ert-deftest ai-code-test-auto-test-harness-directory-defaults-to-ai-code-files-harness ()
  "Test that harness directory defaults to `.ai.code.files/harness/`."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-harness-cache-directory nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir)))
          (should (equal (expand-file-name "harness/" ai-files-dir)
                         (ai-code--auto-test-harness-directory))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-ensure-auto-test-harness-cache-directory-tolerates-unbound-custom ()
  "Test that harness directory creation falls back when the custom is unbound."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (expected-directory (expand-file-name "harness/" ai-files-dir))
         (was-bound (boundp 'ai-code-auto-test-harness-cache-directory))
         (original-value (when was-bound ai-code-auto-test-harness-cache-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir)))
          (makunbound 'ai-code-auto-test-harness-cache-directory)
          (should (equal expected-directory
                         (ai-code--ensure-auto-test-harness-cache-directory)))
          (should (file-directory-p expected-directory)))
      (if was-bound
          (setq ai-code-auto-test-harness-cache-directory original-value)
        (makunbound 'ai-code-auto-test-harness-cache-directory))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-auto-test-harness-prompt-path-uses-repo-relative-at-path ()
  "Test that harness prompt path becomes an `@` repo-relative path."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (harness-file (expand-file-name ".ai.code.files/harness/tdd.v1.md" temp-root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory harness-file) t)
          (with-temp-file harness-file
            (insert "harness"))
          (cl-letf (((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) temp-root)))
            (should (equal "@.ai.code.files/harness/tdd.v1.md"
                           (ai-code--auto-test-harness-prompt-path harness-file)))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-auto-test-harness-prompt-path-keeps-sibling-path-absolute ()
  "Test that sibling paths with a shared prefix are not treated as repo-local."
  (let* ((temp-root (make-temp-file "ai-code-harness-parent-" t))
         (git-root (directory-file-name (expand-file-name "repo/" temp-root)))
         (external-root (expand-file-name "repo-cache/" temp-root))
         (harness-file (expand-file-name "harness/tdd.v1.md" external-root)))
    (unwind-protect
        (progn
          (make-directory git-root t)
          (make-directory (file-name-directory harness-file) t)
          (with-temp-file harness-file
            (insert "harness"))
          (cl-letf (((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) git-root)))
            (should (equal harness-file
                           (ai-code--auto-test-harness-prompt-path harness-file)))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-auto-test-harness-cache-directory-docs-cover-non-repo-fallback ()
  "Test that the harness directory custom documents the non-repo fallback."
  (let ((doc (documentation-property 'ai-code-auto-test-harness-cache-directory
                                     'variable-documentation)))
    (should (string-match-p "Outside a Git repository" doc))
    (should (string-match-p "default-directory" doc))
    (should
     (equal
      '(choice
        (const :tag "Use default harness directory (.ai.code.files/harness in a repo, or harness under default-directory otherwise)"
               nil)
        directory)
      (get 'ai-code-auto-test-harness-cache-directory 'custom-type)))))

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
         (ai-code-mcp-agent-enabled-backends '(codex))
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
         (ai-code-mcp-agent-enabled-backends '(codex))
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

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me-simple-question-match-skips-gptel ()
  "Test that question-only prompt markers skip GPTel classification."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text)
                 (ert-fail "Should not call GPTel for question-only prompt markers.")))
              ((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda ()
                 (ert-fail "Should not ask test type for question-only prompts."))))
      (should-not
       (ai-code--resolve-auto-test-type-for-send
        "Explain this function\nNote: This is a question only - please do not modify the code.")))))

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

(ert-deftest ai-code-test-read-auto-follow-up-choice-uses-y-or-n-p ()
  "Test that follow-up choice reads a y/n decision."
  (let ((asked-prompt nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (setq asked-prompt prompt)
                 t))
              ((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (ert-fail "Should not use completing-read for follow-up y/n choice."))))
      (should (eq t (ai-code--read-auto-follow-up-choice)))
      (should (string-match-p
               "\\`Discussion follow-up suggestions\\(?: (y/n)\\)?\\? \\'"
               asked-prompt)))))

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
               (lambda (&rest _args) t)))
      (should (string-match-p
               "3-4 numbered candidate next[[:space:]\n]+steps"
               (ai-code--resolve-auto-follow-up-suffix-for-send
                "Explain this function")))
      (should (string-match-p
               "At least 2 candidates must[[:space:]\n]+be AI-actionable items"
               (ai-code--resolve-auto-follow-up-suffix-for-send
                "Explain this function"))))))

(ert-deftest ai-code-test-resolve-auto-follow-up-suffix-calls-choice-reader-without-args ()
  "Test that follow-up choice reader is called without extra arguments."
  (let ((ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-next-step-suggestion-suffix "FOLLOW-UP")
        (ai-code-use-gptel-classify-prompt nil))
    (cl-letf (((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda () t)))
      (should (equal "FOLLOW-UP"
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
               (lambda (&rest _args)
                 (setq asked t)
                 t)))
      (should-not
       (ai-code--resolve-auto-follow-up-suffix-for-send
        "Please update the code"))
      (should-not asked))))

(ert-deftest ai-code-test-resolve-auto-follow-up-suffix-for-send-simple-code-change-match-skips-gptel ()
  "Test that code-change prompt markers skip GPTel classification."
  (let ((ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-use-gptel-classify-prompt t)
        (asked nil))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text)
                 (ert-fail "Should not call GPTel for code-change prompt markers.")))
              ((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda (&rest _args)
                 (setq asked t)
                 t)))
      (should-not
       (ai-code--resolve-auto-follow-up-suffix-for-send
        "Refactor this function\nNote: Please make the code change described above."))
      (should-not asked))))

(ert-deftest ai-code-test-resolve-auto-follow-up-suffix-for-send-enabled-for-any-non-code-change-prompt ()
  "Test that the feature affects any prompt classified as non-code-change."
  (let ((ai-code-discussion-auto-follow-up-enabled t)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change))
              ((symbol-function 'ai-code--read-auto-follow-up-choice)
               (lambda (&rest _args) t)))
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
               (lambda (&rest _args) t))
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
                   (lambda (&rest _args) t))
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
               (lambda (&rest _args) t))
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

(ert-deftest ai-code-test-classify-prompt-for-send-simple-explain-match-skips-gptel ()
  "Test that explain prompts use simple non-code-change heuristics first."
  (let ((ai-code-use-gptel-classify-prompt t)
        (ai-code-auto-test-type 'ask-me))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text)
                 (ert-fail "Should not call GPTel for explain prompts."))))
      (should (eq 'non-code-change
                  (ai-code--classify-prompt-for-send
                   "Please explain the following file:\nFile: ai-code.el"))))))

(ert-deftest ai-code-test-classify-prompt-for-send-mixed-explain-edit-falls-back-to-gptel ()
  "Test that mixed explain/edit prompts still fall back to GPTel."
  (let ((ai-code-use-gptel-classify-prompt t)
        (ai-code-auto-test-type 'ask-me)
        (captured-prompt nil))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (prompt-text)
                 (setq captured-prompt prompt-text)
                 'code-change)))
      (should (eq 'code-change
                  (ai-code--classify-prompt-for-send
                   "Please explain why this fails, then update the implementation.")))
      (should (equal "Please explain why this fails, then update the implementation."
                     captured-prompt)))))

(ert-deftest ai-code-test-gptel-classifier-prompt-treats-document-edits-as-non-code-change ()
  "Test that GPTel instructions reserve CODE_CHANGE for program code edits."
  (let ((captured-prompt nil)
        (original-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'gptel)
                     t
                   (funcall original-require feature filename noerror))))
              ((symbol-function 'ai-code-call-gptel-sync)
               (lambda (prompt)
                 (setq captured-prompt prompt)
                 "NOT_CODE_CHANGE")))
      (should (eq 'non-code-change
                  (ai-code--gptel-classify-prompt-code-change
                   "Please update the README and other docs.")))
      (should
       (string-match-p
        "Return CODE_CHANGE only for changes to program code or test code\\."
        captured-prompt))
      (should
       (string-match-p
        "Treat documentation changes and any other non-program-code actions as NOT_CODE_CHANGE\\."
        captured-prompt)))))

(ert-deftest ai-code-test-simple-classifier-reuses-shared-prompt-markers ()
  "Test that classifier markers reuse shared prompt-builder constants."
  (should (boundp 'ai-code-change--selected-region-note))
  (should (boundp 'ai-code-change--generic-note))
  (should (boundp 'ai-code-change--selected-files-note))
  (should (boundp 'ai-code-discussion--question-only-note))
  (should (boundp 'ai-code-discussion--selected-region-note))
  (should (boundp 'ai-code-discussion--explain-prompt-prefixes))
  (should (equal ai-code--code-change-prompt-markers
                 (mapcar #'downcase
                         (list ai-code-change--selected-region-note
                               ai-code-change--generic-note
                               ai-code-change--selected-files-note))))
  (should (equal ai-code--non-code-change-prompt-markers
                 (append
                  (mapcar #'downcase
                          (list ai-code-discussion--question-only-note
                                ai-code-discussion--selected-region-note))
                  (mapcar #'downcase
                          ai-code-discussion--explain-prompt-prefixes)))))

(ert-deftest ai-code-test-prompt-classification-docstrings-are-not-gptel-specific ()
  "Test that prompt classification docstrings are not GPTel-specific."
  (should-not
   (string-match-p "GPTel prompt classification"
                   (documentation 'ai-code--classify-prompt-for-send)))
  (dolist (fn '(ai-code--resolve-auto-test-type-for-send
                ai-code--resolve-ask-auto-test-type-for-send
                ai-code--resolve-auto-follow-up-suffix-for-send
                ai-code--resolve-auto-test-suffix-for-send))
    (should-not
     (string-match-p "optional GPTel prompt classification result"
                     (documentation fn)))))

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

(ert-deftest ai-code-test-discussion-auto-follow-up-enabled-default-is-on ()
  "Test that discussion auto follow-up defaults to enabled."
  (should (eq t (default-value 'ai-code-discussion-auto-follow-up-enabled))))

(ert-deftest ai-code-test-resolve-auto-test-suffix-for-send-ask-me-tdd-with-refactoring ()
  "Test that ask-me resolves to the repo-local TDD harness reference."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-harness-cache-directory nil)
         (ai-code-auto-test-type 'ask-me)
         (ai-code-mcp-agent-enabled-backends '(codex))
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
        (ai-code-discussion-auto-follow-up-enabled nil)
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

(provide 'test_ai-code-harness)

;;; test_ai-code-harness.el ends here
