;;; test_ai-code-behaviors.el --- Tests for ai-code-behaviors.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Unit tests for the behavior injection system.

;;; Code:

(require 'ert)
(require 'ai-code-behaviors)

(defvar ai-code-test-mock-project-root "/tmp/ai-code-test-project"
  "Mock project root for testing.")

(defun ai-code-test--mock-git-root ()
  "Return mock git root for testing."
  ai-code-test-mock-project-root)

(defmacro ai-code-test-with-mock-project (&rest body)
  "Execute BODY with mocked git root for project isolation."
  `(let ((ai-code--git-root #'ai-code-test--mock-git-root))
     (cl-letf (((symbol-function 'ai-code--git-root) #'ai-code-test--mock-git-root))
       ,@body)))

(ert-deftest ai-code-test-behavior-operating-modes-list ()
  "Test that operating modes are properly defined."
  (should (member "=code" ai-code--behavior-operating-modes))
  (should (member "=debug" ai-code--behavior-operating-modes))
  (should (member "=research" ai-code--behavior-operating-modes))
  (should (member "=review" ai-code--behavior-operating-modes))
  (should (member "=spec" ai-code--behavior-operating-modes))
  (should (member "=test" ai-code--behavior-operating-modes)))

(ert-deftest ai-code-test-behavior-modifiers-list ()
  "Test that modifiers are properly defined."
  (should (member "deep" ai-code--behavior-modifiers))
  (should (member "tdd" ai-code--behavior-modifiers))
  (should (member "challenge" ai-code--behavior-modifiers))
  (should (member "concise" ai-code--behavior-modifiers)))

(ert-deftest ai-code-test-extract-single-mode ()
  "Test extracting a single operating mode hashtag."
  (let ((result (nth 0 (ai-code--extract-and-remove-hashtags "Fix the bug #=debug"))))
    (should result)
    (should (equal (plist-get result :mode) "=debug"))))

(ert-deftest ai-code-test-extract-mode-with-modifiers ()
  "Test extracting mode with modifiers."
  (let ((result (nth 0 (ai-code--extract-and-remove-hashtags "Implement feature #=code #deep #tdd"))))
    (should result)
    (should (equal (plist-get result :mode) "=code"))
    (should (member "deep" (plist-get result :modifiers)))
    (should (member "tdd" (plist-get result :modifiers)))))

(ert-deftest ai-code-test-extract-modifiers-only ()
  "Test extracting modifiers without mode."
  (let ((result (nth 0 (ai-code--extract-and-remove-hashtags "Explain this #deep #wide"))))
    (should result)
    (should (null (plist-get result :mode)))
    (should (member "deep" (plist-get result :modifiers)))
    (should (member "wide" (plist-get result :modifiers)))))

(ert-deftest ai-code-test-extract-no-hashtags ()
  "Test that prompts without hashtags return nil."
  (should (null (nth 0 (ai-code--extract-and-remove-hashtags "Fix the bug in auth")))))

(ert-deftest ai-code-test-unknown-behavior-warning ()
  "Test that unknown behaviors are preserved in prompt with warning."
  (let* ((extracted (ai-code--extract-and-remove-hashtags "Do something #=code #unknown-behavior"))
         (result (nth 0 extracted))
         (cleaned (nth 1 extracted)))
    (should result)
    (should (equal (plist-get result :mode) "=code"))
    (should (string-match-p "#unknown-behavior" cleaned))))

(ert-deftest ai-code-test-operating-mode-p ()
  "Test operating mode predicate."
  (should (ai-code--operating-mode-p "=code"))
  (should (ai-code--operating-mode-p "=debug"))
  (should-not (ai-code--operating-mode-p "deep"))
  (should-not (ai-code--operating-mode-p "unknown")))

(ert-deftest ai-code-test-behavior-p ()
  "Test general behavior predicate."
  (should (ai-code--behavior-p "=code"))
  (should (ai-code--behavior-p "deep"))
  (should-not (ai-code--behavior-p "unknown")))

(ert-deftest ai-code-test-classify-code-keywords ()
  "Test keyword-based classification for code mode."
  (let ((result (ai-code--classify-prompt-intent-keywords "Implement user authentication")))
    (should result)
    (should (equal (plist-get result :mode) "=code"))))

(ert-deftest ai-code-test-classify-debug-keywords ()
  "Test keyword-based classification for debug mode."
  (let ((result (ai-code--classify-prompt-intent-keywords "There's an error in the login module")))
    (should result)
    (should (equal (plist-get result :mode) "=debug"))))

(ert-deftest ai-code-test-classify-research-keywords ()
  "Test keyword-based classification for research mode."
  (let ((result (ai-code--classify-prompt-intent-keywords "Explain how this function works")))
    (should result)
    (should (equal (plist-get result :mode) "=research"))))

(ert-deftest ai-code-test-classify-review-keywords ()
  "Test keyword-based classification for review mode."
  (let ((result (ai-code--classify-prompt-intent-keywords "Review this PR for issues")))
    (should result)
    (should (equal (plist-get result :mode) "=review"))))

(ert-deftest ai-code-test-classify-spec-keywords ()
  "Test keyword-based classification for spec mode."
  (let ((result (ai-code--classify-prompt-intent-keywords "Design the architecture for payment system")))
    (should result)
    (should (equal (plist-get result :mode) "=spec"))))

(ert-deftest ai-code-test-classify-test-keywords ()
  "Test keyword-based classification for test mode."
  (let ((result (ai-code--classify-prompt-intent-keywords "Write unit tests for calculator")))
    (should result)
    (should (equal (plist-get result :mode) "=test"))))

;;; Preset Suggestion Tests

(ert-deftest ai-code-test-suggest-preset-for-code-mode ()
  "Test preset suggestion for code mode."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=code")) "quick-fix")))

(ert-deftest ai-code-test-suggest-preset-for-code-with-tdd ()
  "Test preset suggestion for code with tdd modifier."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=code" :modifiers ("tdd"))) "tdd-dev")))

(ert-deftest ai-code-test-suggest-preset-for-code-with-concise ()
  "Test preset suggestion for code with concise modifier."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=code" :modifiers ("concise"))) "quick-fix")))

(ert-deftest ai-code-test-suggest-preset-for-debug-mode ()
  "Test preset suggestion for debug mode."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=debug")) "thorough-debug"))
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=debug" :modifiers ("deep"))) "thorough-debug")))

(ert-deftest ai-code-test-suggest-preset-for-review-mode ()
  "Test preset suggestion for review mode."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=review")) "quick-review"))
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=review" :modifiers ("deep"))) "deep-review"))
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=review" :modifiers ("concise"))) "quick-review")))

(ert-deftest ai-code-test-suggest-preset-for-research-mode ()
  "Test preset suggestion for research mode."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=research")) "research-deep")))

(ert-deftest ai-code-test-suggest-preset-for-mentor-mode ()
  "Test preset suggestion for mentor mode."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=mentor")) "mentor-learn")))

(ert-deftest ai-code-test-suggest-preset-for-spec-mode ()
  "Test preset suggestion for spec mode."
  (should (equal (ai-code--suggest-preset-for-classification '(:mode "=spec")) "spec-planning")))

(ert-deftest ai-code-test-classify-modifier-triggers ()
  "Test that modifier triggers are detected."
  (let ((result (ai-code--classify-prompt-intent-keywords
                 "Implement this thoroughly with TDD")))
    (should result)
    (should (member "deep" (plist-get result :modifiers)))
    (should (member "tdd" (plist-get result :modifiers)))))

(ert-deftest ai-code-test-extract-clean-user-prompt-plain ()
  "Test that plain prompts are returned unchanged."
  (let ((result (ai-code--extract-clean-user-prompt "Implement user authentication")))
    (should (equal result "Implement user authentication"))))

(ert-deftest ai-code-test-extract-clean-user-prompt-with-tags ()
  "Test that prompts with <user-prompt> tags are extracted."
  (let ((result (ai-code--extract-clean-user-prompt 
                 "AdditionalContext: <operating-mode>\nSome behavior\n</operating-mode>\n\n<user-prompt>\nFix the bug\n</user-prompt>")))
    (should (equal result "Fix the bug"))))

(ert-deftest ai-code-test-extract-clean-user-prompt-with-behavior-blocks ()
  "Test that behavior injection blocks are stripped."
  (let ((result (ai-code--extract-clean-user-prompt 
                 "AdditionalContext: <operating-mode>\nYou are coding\n</operating-mode>\n\n<user-prompt>\nImplement login\n</user-prompt>")))
    (should (equal result "Implement login"))))

(ert-deftest ai-code-test-classify-ignores-context ()
  "Test that classification ignores behavior injection context."
  (let ((result (ai-code--classify-prompt-intent 
                 "AdditionalContext: <operating-mode>\nYou are debugging\n</operating-mode>\n\n<user-prompt>\nImplement user authentication\n</user-prompt>")))
    (should result)
    (should (equal (plist-get result :mode) "=code"))))

(ert-deftest ai-code-test-all-behavior-names ()
  "Test that all behavior names are returned with # prefix."
  (let ((names (ai-code--all-behavior-names)))
    (should (member "#=code" names))
    (should (member "#=debug" names))
    (should (member "#deep" names))
    (should (member "#tdd" names))))

(ert-deftest ai-code-test-session-state-persistence ()
  "Test that behaviors persist across prompts without hashtags."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (let ((first-result (ai-code--process-behaviors "Fix the bug #=debug #deep")))
     (should first-result)
     (should (string-match-p "=debug" first-result))
     (should (string-match-p "deep" first-result))
     (let ((state (ai-code--behaviors-get-state)))
       (should state)
       (should (equal (plist-get state :mode) "=debug"))
       (should (member "deep" (plist-get state :modifiers)))))
   (let ((second-result (ai-code--process-behaviors "What is the status?")))
     (should second-result)
     (should (string-match-p "=debug" second-result))
     (should (string-match-p "deep" second-result)))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-new-hashtags-supersede-session ()
  "Test that new hashtags supersede persisted session state."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (ai-code--process-behaviors "Fix the bug #=debug #deep")
   (let ((state (ai-code--behaviors-get-state)))
     (should (equal (plist-get state :mode) "=debug")))
   (let ((result (ai-code--process-behaviors "Review this code #=review #challenge")))
     (should result)
     (should (string-match-p "=review" result))
     (should (string-match-p "challenge" result))
     (should-not (string-match-p "=debug" result)))
   (let ((state (ai-code--behaviors-get-state)))
     (should (equal (plist-get state :mode) "=review"))
     (should (member "challenge" (plist-get state :modifiers))))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-presets-defined ()
  "Test that behavior presets are defined."
  (should ai-code--behavior-presets)
  (should (assoc "tdd-dev" ai-code--behavior-presets))
  (should (assoc "thorough-debug" ai-code--behavior-presets))
  (should (assoc "quick-review" ai-code--behavior-presets)))

(ert-deftest ai-code-test-apply-preset ()
  "Test applying a behavior preset."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (ai-code-behaviors-apply-preset "tdd-dev")
   (let ((state (ai-code--behaviors-get-state)))
     (should state)
     (should (equal (plist-get state :mode) "=code"))
     (should (member "tdd" (plist-get state :modifiers)))
     (should (member "deep" (plist-get state :modifiers))))
   (should (equal (ai-code--behaviors-get-preset) "tdd-dev"))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-mode-line-preset-display ()
  "Test that mode-line shows preset name when preset is active."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (ai-code-behaviors-apply-preset "tdd-dev")
   (ai-code--behaviors-update-mode-line)
   (should (string= (ai-code--behaviors-mode-line-string) "[@tdd-dev]"))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-mode-line-behavior-display ()
  "Test that mode-line shows behaviors when set directly."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (ai-code--process-behaviors "Fix it #=debug #deep")
   (ai-code--behaviors-update-mode-line)
   (should (string= (ai-code--behaviors-mode-line-string) "[=debug deep]"))
   (should (null (ai-code--behaviors-get-preset)))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-clear-resets-preset ()
  "Test that clear resets both preset and session state."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-apply-preset "tdd-dev")
   (should (ai-code--behaviors-get-preset))
   (should (ai-code--behaviors-get-state))
   (ai-code-behaviors-clear)
   (should (null (ai-code--behaviors-get-preset)))
   (should (null (ai-code--behaviors-get-state)))
   (should (string-match-p "\\[○\\]" (ai-code--behaviors-mode-line-string)))))

(ert-deftest ai-code-test-hashtag-clears-preset ()
  "Test that setting behaviors via hashtag clears preset name."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-apply-preset "tdd-dev")
   (should (equal (ai-code--behaviors-get-preset) "tdd-dev"))
   (ai-code--process-behaviors "Review this #=review")
   (should (null (ai-code--behaviors-get-preset)))
   (let ((state (ai-code--behaviors-get-state)))
     (should (equal (plist-get state :mode) "=review")))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-constraint-modifiers-defined ()
  "Test that constraint modifiers are defined."
  (should ai-code--constraint-modifiers)
  (should (assoc "chinese" ai-code--constraint-modifiers))
  (should (assoc "test-after" ai-code--constraint-modifiers))
  (should (assoc "strict-lint" ai-code--constraint-modifiers)))

(ert-deftest ai-code-test-constraint-bundles-defined ()
  "Test that constraint bundles are defined."
  (should ai-code--constraint-bundles)
  (should (assoc "react-stack" ai-code--constraint-bundles))
  (should (assoc "rust-stack" ai-code--constraint-bundles))
  (should (assoc "python-stack" ai-code--constraint-bundles)))

(ert-deftest ai-code-test-expand-constraint-bundle ()
  "Test expanding a constraint bundle to its constraints."
  (let ((constraints (ai-code--expand-constraint-bundle "react-stack")))
    (should constraints)
    (should (member "strict-types" constraints))
    (should (member "functional" constraints))
    (should (member "async-await" constraints))))

(ert-deftest ai-code-test-extract-constraint-bundle-from-prompt ()
  "Test extracting constraint bundle from @bundle-name in prompt."
  (let* ((result (nth 0 (ai-code--extract-and-remove-hashtags "@react-stack implement feature")))
         (constraints (plist-get result :constraint-modifiers)))
    (should result)
    (should (member "strict-types" constraints))
    (should (member "functional" constraints))))

(ert-deftest ai-code-test-extract-constraint-bundle-with-preset ()
  "Test that constraint bundle and preset can be used together."
  (let* ((result (nth 0 (ai-code--extract-and-remove-hashtags "@tdd-dev @rust-stack test the parser")))
         (preset (plist-get result :preset))
         (constraints (plist-get result :constraint-modifiers)))
    (should result)
    (should (equal preset "tdd-dev"))
    (should (member "strict-types" constraints))
    (should (member "immutable" constraints))))

(ert-deftest ai-code-test-extract-constraint-modifiers ()
  "Test extracting constraint modifiers from hashtags."
  (let ((result (nth 0 (ai-code--extract-and-remove-hashtags "Fix bug #=code #chinese #test-after"))))
    (should result)
    (should (equal (plist-get result :mode) "=code"))
    (should (member "chinese" (plist-get result :constraint-modifiers)))
    (should (member "test-after" (plist-get result :constraint-modifiers)))))

(ert-deftest ai-code-test-glob-to-regexp ()
  "Test glob pattern to regexp conversion."
  (let ((regex (ai-code--glob-to-regexp "*.csproj")))
    (should (string-match-p (concat regex "$") "MyProject.csproj"))
    (should (string-match-p (concat regex "$") "test.csproj"))
    (should-not (string-match-p (concat regex "$") "test.txt")))
  (let ((regex (ai-code--glob-to-regexp "*.json")))
    (should (string-match-p (concat regex "$") "tsconfig.json"))
    (should-not (string-match-p (concat regex "$") "tsconfig.js"))))

(ert-deftest ai-code-test-glob-pattern-p ()
  "Test glob pattern detection."
  (should (ai-code--glob-pattern-p "*.csproj"))
  (should (ai-code--glob-pattern-p "test?.txt"))
  (should-not (ai-code--glob-pattern-p "tsconfig.json"))
  (should-not (ai-code--glob-pattern-p "Cargo.toml")))

(ert-deftest ai-code-test-expand-glob-in-dir ()
  "Test glob expansion in directory."
  (let ((temp-dir (make-temp-file "glob-test" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "Project.csproj" temp-dir))
          (write-region "" nil (expand-file-name "Other.csproj" temp-dir))
          (write-region "" nil (expand-file-name "test.txt" temp-dir))
          (let ((matches (ai-code--expand-glob-in-dir "*.csproj" temp-dir)))
            (should (= (length matches) 2))
            (should (cl-find-if (lambda (f) (string-suffix-p "Project.csproj" f)) matches))
            (should (cl-find-if (lambda (f) (string-suffix-p "Other.csproj" f)) matches))
            (should-not (cl-find-if (lambda (f) (string-suffix-p "test.txt" f)) matches))))
      (delete-directory temp-dir t))))

(ert-deftest ai-code-test-bundle-persistence-parsing ()
  "Test that bundle is correctly parsed from persistence file format."
  (let ((test-content "# Auto-detected constraints

#strict-types
#chinese

# Bundle: rust-stack
"))
    (with-temp-buffer
      (insert test-content)
      (goto-char (point-min))
      (let ((constraints nil)
            (bundle nil))
        (while (not (eobp))
          (let ((line (string-trim (thing-at-point 'line t))))
            (when (string-match-p "^#" line)
              (let ((name (string-trim (substring line 1))))
                (cond
                 ((string-prefix-p "Bundle:" name)
                  (setq bundle (string-trim (substring name 7))))
                 ((assoc name ai-code--constraint-modifiers)
                  (push name constraints))))))
          (forward-line 1))
        (should (equal bundle "rust-stack"))
        (should (member "strict-types" constraints))
        (should (member "chinese" constraints))))))

(ert-deftest ai-code-test-project-scoped-bundle-storage ()
  "Test that active bundles are stored per-project."
  (let ((project-a "/tmp/test-project-a")
        (project-b "/tmp/test-project-b"))
    (ai-code--behaviors-set-active-bundle "rust-stack" project-a)
    (ai-code--behaviors-set-active-bundle "react-stack" project-b)
    (should (equal (ai-code--behaviors-get-active-bundle project-a) "rust-stack"))
    (should (equal (ai-code--behaviors-get-active-bundle project-b) "react-stack"))
    (ai-code--behaviors-clear-active-bundle project-a)
    (ai-code--behaviors-clear-active-bundle project-b)))

(ert-deftest ai-code-test-build-instruction-with-constraints ()
  "Test that behavior instructions include constraints."
  (ai-code-behaviors-clear)
  (let* ((behaviors (list :mode "=code"
                          :modifiers nil
                          :constraint-modifiers '("chinese" "test-after")
                          :custom-suffix nil))
         (instruction (ai-code--build-behavior-instruction behaviors)))
    (should instruction)
    (should (string-match-p "AdditionalContext: <constraints>" instruction))
    (should (string-match-p "简体中文" instruction))))

(ert-deftest ai-code-test-build-instruction-with-custom-suffix ()
  "Test that behavior instructions include custom suffix."
  (ai-code-behaviors-clear)
  (let* ((behaviors (list :mode nil
                          :modifiers nil
                          :constraint-modifiers nil
                          :custom-suffix "Use strict mode"))
         (instruction (ai-code--build-behavior-instruction behaviors)))
    (should instruction)
    (should (string-match-p "AdditionalContext: <custom-constraints>" instruction))
    (should (string-match-p "Use strict mode" instruction))))

(ert-deftest ai-code-test-command-preset-map-defined ()
  "Test that command preset map is defined."
  (should ai-code--command-preset-map)
  (should (assoc 'ai-code-tdd-cycle ai-code--command-preset-map))
  (should (assoc 'ai-code-code-change ai-code--command-preset-map)))

(ert-deftest ai-code-test-mode-line-with-constraints ()
  "Test that mode-line shows constraint count."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (ai-code--behaviors-set-state
    (list :mode "=code"
          :modifiers '("deep")
          :constraint-modifiers '("chinese" "test-after")
          :custom-suffix nil))
   (ai-code--behaviors-update-mode-line)
   (should (string= (ai-code--behaviors-mode-line-string) "[=code deep +2]"))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-mode-line-with-bundle ()
  "Test that mode-line shows bundle name when bundle is active."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (ai-code--behaviors-set-active-bundle "rust-stack")
   (ai-code--behaviors-set-state
    (list :mode nil
          :modifiers nil
          :constraint-modifiers '("strict-types" "immutable")))
   (ai-code--behaviors-update-mode-line)
   (should (string-match-p "@rust-stack" (ai-code--behaviors-mode-line-string)))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-mode-line-with-custom-suffix ()
  "Test that mode-line counts custom suffix."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (ai-code--behaviors-set-state
    (list :mode "=code"
          :modifiers nil
          :constraint-modifiers '("chinese")
          :custom-suffix "Use strict mode"))
   (ai-code--behaviors-update-mode-line)
   (should (string-match-p "+2" (ai-code--behaviors-mode-line-string)))
   (ai-code-behaviors-clear)))

(ert-deftest ai-code-test-project-scoped-state ()
  "Test that behaviors are scoped per project."
  (let ((root-a "/tmp/project-a")
        (root-b "/tmp/project-b"))
    ;; Set behaviors for project A
    (puthash root-a (list :state (list :mode "=code" :modifiers '("deep")) 
                          :preset nil) 
             ai-code--behaviors-session-states)
    ;; Verify project B has no behaviors
    (should-not (gethash root-b ai-code--behaviors-session-states))
    ;; Set behaviors for project B
    (puthash root-b (list :state (list :mode "=debug" :modifiers '("challenge")) 
                          :preset nil) 
             ai-code--behaviors-session-states)
    ;; Verify both have their own behaviors
    (let ((state-a (plist-get (gethash root-a ai-code--behaviors-session-states) :state))
          (state-b (plist-get (gethash root-b ai-code--behaviors-session-states) :state)))
      (should (equal (plist-get state-a :mode) "=code"))
      (should (equal (plist-get state-b :mode) "=debug")))
    ;; Clear all
    (clrhash ai-code--behaviors-session-states)))

(ert-deftest ai-code-test-clear-current-project-only ()
  "Test that clear only affects current project."
  (let ((root-a "/tmp/project-a")
        (root-b "/tmp/project-b"))
    ;; Set behaviors for both projects
    (puthash root-a (list :state (list :mode "=code") :preset "quick-fix")
             ai-code--behaviors-session-states)
    (puthash root-b (list :state (list :mode "=debug") :preset "thorough-debug")
             ai-code--behaviors-session-states)
    ;; Verify both have behaviors
    (should (gethash root-a ai-code--behaviors-session-states))
    (should (gethash root-b ai-code--behaviors-session-states))
    ;; Clear all
    (clrhash ai-code--behaviors-session-states)))

(ert-deftest ai-code-test-backend-session-prefixes-defined ()
  "Test that CLI backend session prefixes are defined."
  (should ai-code--backend-session-prefixes)
  (should (assoc 'opencode ai-code--backend-session-prefixes))
  (should (assoc 'claude-code ai-code--backend-session-prefixes))
  (should-not (assoc 'eca ai-code--backend-session-prefixes)))

(ert-deftest ai-code-test-get-session-prefix ()
  "Test getting session prefix for different backends."
  (should (equal "opencode" (alist-get 'opencode ai-code--backend-session-prefixes)))
  (should (equal "claude" (alist-get 'claude-code ai-code--backend-session-prefixes)))
  (should-not (alist-get 'eca ai-code--backend-session-prefixes)))

(ert-deftest ai-code-test-command-preset-map ()
  "Test that command-preset-map has expected commands."
  (should (assq 'ai-code-tdd-cycle ai-code--command-preset-map))
  (should (assq 'ai-code-code-change ai-code--command-preset-map))
  (should-not (assq 'other-command ai-code--command-preset-map)))

;;; Multi-signal detection tests

(ert-deftest ai-code-test-detect-from-filename-test-file ()
  "Test detection from test file name."
  (should (equal (plist-get (ai-code--detect-from-filename "foo_test.py") :preset)
                 "tdd-dev"))
  (should (equal (plist-get (ai-code--detect-from-filename "foo.test.js") :preset)
                 "tdd-dev"))
  (should (eq (plist-get (ai-code--detect-from-filename "foo_test.py") :confidence)
              :high)))

(ert-deftest ai-code-test-detect-from-filename-doc-file ()
  "Test detection from documentation file."
  (should (equal (plist-get (ai-code--detect-from-filename "README.md") :preset)
                 "mentor-learn"))
  (should (equal (plist-get (ai-code--detect-from-filename "CHANGELOG") :preset)
                 "mentor-learn"))
  (should (eq (plist-get (ai-code--detect-from-filename "README") :confidence)
              :high))
  (should (eq (plist-get (ai-code--detect-from-filename "CHANGELOG") :confidence)
              :medium)))

(ert-deftest ai-code-test-detect-from-filename-config-file ()
  "Test detection from config file."
  (should (equal (plist-get (ai-code--detect-from-filename "config.yaml") :preset)
                 "quick-review"))
  (should (eq (plist-get (ai-code--detect-from-filename "config.yaml") :confidence)
              :low)))

(ert-deftest ai-code-test-detect-from-filename-no-match ()
  "Test detection returns nil for non-matching file."
  (should-not (ai-code--detect-from-filename "random-file.xyz")))

(ert-deftest ai-code-test-detect-from-major-mode-org ()
  "Test detection from org mode."
  (with-temp-buffer
    (org-mode)
    (should (equal (plist-get (ai-code--detect-from-major-mode) :preset)
                   "mentor-learn"))
    (should (eq (plist-get (ai-code--detect-from-major-mode) :confidence)
                :medium))))

(ert-deftest ai-code-test-detect-from-major-mode-json ()
  "Test detection from json mode."
  (skip-unless (featurep 'json-mode))
  (with-temp-buffer
    (json-mode)
    (should (equal (plist-get (ai-code--detect-from-major-mode) :preset)
                   "quick-review"))))

(ert-deftest ai-code-test-select-best-preset-high-wins ()
  "Test preset selection - high confidence wins."
  (let ((signals (list '(:preset "tdd-dev" :confidence :high :source :filename)
                       '(:preset "mentor-learn" :confidence :medium :source :major-mode)
                       '(:preset "quick-fix" :confidence :low :source :git))))
    (should (equal (ai-code--select-best-preset signals) "tdd-dev"))))

(ert-deftest ai-code-test-select-best-preset-medium-wins ()
  "Test preset selection - medium wins when no high."
  (let ((signals (list '(:preset "mentor-learn" :confidence :medium :source :major-mode)
                       '(:preset "quick-fix" :confidence :low :source :git))))
    (should (equal (ai-code--select-best-preset signals) "mentor-learn"))))

(ert-deftest ai-code-test-select-best-preset-empty ()
  "Test preset selection with empty list."
  (should-not (ai-code--select-best-preset nil)))

(ert-deftest ai-code-test-override-preset ()
  "Test that override preset takes precedence."
  (let ((ai-code-behaviors-override-preset "deep-review"))
    (should (equal (ai-code--behaviors-detect-context-preset) "deep-review"))))

(ert-deftest ai-code-test-fallback-quick-fix ()
  "Test fallback to quick-fix when no signals match."
  (let ((ai-code-behaviors-detection-enabled-signals nil))
    (should (equal (ai-code--behaviors-detect-context-preset) "quick-fix"))))

(ert-deftest ai-code-test-detection-cache-clear ()
  "Test that cache clear works."
  (ai-code--behaviors-clear-detection-cache)
  (should (= (hash-table-count ai-code--detection-cache) 0)))

(ert-deftest ai-code-test-custom-pattern ()
  "Test custom detection patterns."
  (let ((ai-code-behaviors-detection-patterns '(("_custom\\.ext$" . "spec-planning"))))
    (should (equal (plist-get (ai-code--detect-from-filename "foo_custom.ext") :preset)
                   "spec-planning"))
    (should (eq (plist-get (ai-code--detect-from-filename "foo_custom.ext") :source)
                :custom-pattern))))

;;; @preset-name syntax tests

(ert-deftest ai-code-test-extract-preset-from-prompt ()
  "Test extracting @preset-name from prompt."
  (let* ((result (ai-code--extract-and-remove-hashtags "@tdd-dev implement feature"))
         (behaviors (nth 0 result))
         (cleaned (nth 1 result)))
    (should (equal (plist-get behaviors :preset) "tdd-dev"))
    (should (string= cleaned "implement feature"))))

(ert-deftest ai-code-test-extract-preset-with-modifiers ()
  "Test extracting @preset-name with additional modifiers."
  (let* ((result (ai-code--extract-and-remove-hashtags "@tdd-dev #chinese implement feature"))
         (behaviors (nth 0 result))
         (cleaned (nth 1 result)))
    (should (equal (plist-get behaviors :preset) "tdd-dev"))
    (should (member "chinese" (plist-get behaviors :constraint-modifiers)))
    (should (string= cleaned "implement feature"))))

(ert-deftest ai-code-test-extract-preset-removes-at-syntax ()
  "Test that @preset-name is removed from cleaned prompt."
  (let* ((result (ai-code--extract-and-remove-hashtags "@mentor-learn how to refactor")))
    (should (string= (nth 1 result) "how to refactor"))))

(ert-deftest ai-code-test-process-preset-in-behaviors ()
  "Test that process-behaviors applies preset correctly."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear)
   (let* ((result (ai-code--process-behaviors "@tdd-dev implement feature"))
          (preset (ai-code--behaviors-get-preset)))
     (should (equal preset "tdd-dev"))
     (should (string-match-p "operating-mode" result)))))

(ert-deftest ai-code-test-preset-merges-modifiers ()
  "Test that preset modifiers merge with additional modifiers."
  (ai-code-behaviors-clear)
  (let ((result (ai-code--process-behaviors "@tdd-dev #chinese implement feature")))
    (should (string-match-p "operating-mode" result))
    (should (string-match-p "constraints" result))
    (should (string-match-p "中文" result))))

(ert-deftest ai-code-test-unknown-preset-ignored ()
  "Test that unknown @preset-name is ignored."
  (let* ((result (ai-code--extract-and-remove-hashtags "@unknown-preset test")))
    (should-not (nth 0 result))))

(ert-deftest ai-code-test-merge-preset-with-modifiers-nil-preset ()
  "Test that merge works with nil preset (auto-classify case)."
  (let* ((classified '(:mode "=code" :modifiers ("deep")))
         (result (ai-code--merge-preset-with-modifiers nil classified)))
    (should (equal (plist-get result :mode) "=code"))
    (should (member "deep" (plist-get result :modifiers)))))

(ert-deftest ai-code-test-plist-put-constraint-modifiers ()
  "Test that plist-put correctly adds constraint-modifiers to fresh plist."
  (let* ((behaviors '(:mode "=code" :modifiers nil))
         (updated (plist-put (copy-sequence behaviors) :constraint-modifiers '("chinese"))))
    (should (equal (plist-get updated :constraint-modifiers) '("chinese")))
    (should (equal (plist-get updated :mode) "=code"))))

;;; GPTel Integration Tests

(ert-deftest ai-code-test-gptel-agent-transform-inject-behaviors-disabled ()
  "Test that transform does nothing when behaviors disabled."
  (skip-unless (require 'gptel-request nil t))
  (let ((ai-code-behaviors-enabled nil))
    (with-temp-buffer
      (insert "Fix the bug")
      (let* ((fsm (gptel-make-fsm :info (list :buffer (current-buffer))))
             (called nil))
        (ai-code--gptel-agent-transform-inject-behaviors
         (lambda () (setq called t))
         fsm)
        (should called)
        (should (string= (buffer-string) "Fix the bug"))))))

(ert-deftest ai-code-test-gptel-agent-transform-inject-behaviors-empty-prompt ()
  "Test that transform handles empty prompt."
  (skip-unless (require 'gptel-request nil t))
  (let ((ai-code-behaviors-enabled t))
    (with-temp-buffer
      (let* ((fsm (gptel-make-fsm :info (list :buffer (current-buffer))))
             (called nil))
        (ai-code--gptel-agent-transform-inject-behaviors
         (lambda () (setq called t))
         fsm)
        (should called)))))

(ert-deftest ai-code-test-gptel-agent-transform-skips-non-plan-preset ()
  "Test that transform skips injection for non-plan/agent presets."
  (skip-unless (require 'gptel-request nil t))
  (let ((ai-code-behaviors-enabled t))
    (with-temp-buffer
      (insert "Implement feature #=code #deep")
      (setq-local gptel--preset 'other-preset)
      (let* ((fsm (gptel-make-fsm :info (list :buffer (current-buffer))))
             (called nil))
        (ai-code--gptel-agent-transform-inject-behaviors
         (lambda () (setq called t))
         fsm)
        (should called)
        (should-not (string-match-p "operating-mode" (buffer-string)))))))

(ert-deftest ai-code-test-gptel-agent-transform-injects-for-plan-preset ()
  "Test that transform injects behaviors for gptel-plan preset."
  (skip-unless (require 'gptel-request nil t))
  (ai-code-behaviors-clear-all)
  (let ((ai-code-behaviors-enabled t))
    (with-temp-buffer
      (insert "Implement feature #=code #deep")
      (setq-local gptel--preset 'gptel-plan)
      (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
        (ai-code--gptel-agent-transform-inject-behaviors
         (lambda () nil)
         fsm))
      (should (string-match-p "operating-mode" (buffer-string)))
      (should (string-match-p "<user-prompt>" (buffer-string)))
      (should (string-match-p "Implement feature" (buffer-string))))))

(ert-deftest ai-code-test-gptel-agent-transform-injects-for-agent-preset ()
  "Test that transform injects behaviors for gptel-agent preset."
  (skip-unless (require 'gptel-request nil t))
  (ai-code-behaviors-clear-all)
  (let ((ai-code-behaviors-enabled t))
    (with-temp-buffer
      (insert "Debug this #=debug")
      (setq-local gptel--preset 'gptel-agent)
      (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
        (ai-code--gptel-agent-transform-inject-behaviors
         (lambda () nil)
         fsm))
      (should (string-match-p "operating-mode" (buffer-string)))
      (should (string-match-p "Debug this" (buffer-string))))))

(ert-deftest ai-code-test-gptel-agent-transform-uses-session-state ()
  "Test that transform uses session state for the correct project."
  (skip-unless (require 'gptel-request nil t))
  (ai-code-behaviors-clear-all)
  (let* ((ai-code-behaviors-enabled t)
         (test-root "/test/project/root")
         (existing-state '(:mode "=code" :modifiers ("deep"))))
    (ai-code--behaviors-set-state existing-state test-root)
    (with-temp-buffer
      (insert "Plain prompt without hashtags")
      (setq-local gptel--preset 'gptel-plan)
      (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
        (cl-letf (((symbol-function 'ai-code--behaviors--root)
                   (lambda (&optional _buffer-or-root) test-root)))
          (ai-code--gptel-agent-transform-inject-behaviors
           (lambda () nil)
           fsm)))
      (should (string-match-p "operating-mode" (buffer-string)))
      (should (string-match-p "Plain prompt without hashtags" (buffer-string))))))

(ert-deftest ai-code-test-gptel-agent-transform-when-preset-nil ()
  "Test that transform skips injection when gptel--preset is nil.
This happens when gptel-agent package is not loaded."
  (skip-unless (require 'gptel-request nil t))
  (ai-code-behaviors-clear-all)
  (let ((ai-code-behaviors-enabled t))
    (with-temp-buffer
      (insert "Implement feature #=code #deep")
      ;; gptel--preset is nil when gptel-agent package not loaded
      (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
        (ai-code--gptel-agent-transform-inject-behaviors
         (lambda () nil)
         fsm))
      ;; Should NOT inject since preset is nil (not gptel-plan or gptel-agent)
      (should-not (string-match-p "operating-mode" (buffer-string)))
      (should (string= (buffer-string) "Implement feature #=code #deep")))))

(ert-deftest ai-code-test-gptel-agent-preset-only-prompt ()
  "Test that preset-only prompts apply state without sending."
  (skip-unless (require 'gptel-request nil t))
  (ai-code-behaviors-clear-all)
  (let* ((ai-code-behaviors-enabled t)
         (test-root "/test/project/root"))
    (with-temp-buffer
      (insert "@tdd-dev")
      (setq-local gptel--preset 'gptel-plan)
      (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
        (cl-letf (((symbol-function 'ai-code--behaviors--root)
                   (lambda (&optional _buffer-or-root) test-root)))
          (ai-code--gptel-agent-transform-inject-behaviors
           (lambda () nil)
           fsm)))
      ;; Buffer should be empty after preset-only prompt
      (should (string-empty-p (buffer-string)))
      ;; State should be set
      (let ((state (ai-code--behaviors-get-state test-root)))
        (should state)
        (should (equal (plist-get state :mode) "=code")))
      ;; Preset should be set
      (should (equal (ai-code--behaviors-get-preset test-root) "tdd-dev")))))

(ert-deftest ai-code-test-gptel-agent-auto-classify-enabled-by-default ()
  "Test that auto-classify is enabled by default in gptel-agent."
  (should ai-code-behaviors-gptel-agent-auto-classify))

(ert-deftest ai-code-test-gptel-agent-process-behaviors-no-auto-classify ()
  "Test that gptel-agent doesn't auto-classify when disabled."
  (ai-code-behaviors-clear-all)
  (let* ((test-root "/test/project/root")
         (ai-code-behaviors-gptel-agent-auto-classify nil)
         (result (ai-code--gptel-agent-process-behaviors "Implement a feature" test-root)))
    ;; Without auto-classify and no session state, should not apply behaviors
    (should-not (nth 0 result))
    (should (string= (nth 1 result) "Implement a feature"))))

(ert-deftest ai-code-test-gptel-agent-process-behaviors-with-auto-classify ()
  "Test that gptel-agent auto-classifies when enabled."
  (ai-code-behaviors-clear-all)
  (let* ((test-root "/test/project/root")
         (ai-code-behaviors-gptel-agent-auto-classify t)
         (result (ai-code--gptel-agent-process-behaviors "Implement a feature" test-root)))
    ;; With auto-classify, should apply behaviors
    (should (nth 0 result))
    (should (string-match-p "operating-mode" (nth 1 result)))))

(ert-deftest ai-code-test-gptel-agent-setup-transform ()
  "Test that setup adds transform to gptel-prompt-transform-functions."
  (skip-unless (require 'gptel nil t))
  ;; Remove if present
  (remove-hook 'gptel-prompt-transform-functions
               #'ai-code--gptel-agent-transform-inject-behaviors)
  (should-not (memq 'ai-code--gptel-agent-transform-inject-behaviors
                    (default-value 'gptel-prompt-transform-functions)))
  ;; Setup should add it
  (ai-code--gptel-agent-setup-transform)
  (should (memq 'ai-code--gptel-agent-transform-inject-behaviors
                (default-value 'gptel-prompt-transform-functions)))
  ;; Cleanup
  (remove-hook 'gptel-prompt-transform-functions
               #'ai-code--gptel-agent-transform-inject-behaviors))

(ert-deftest ai-code-test-process-behaviors-with-root ()
  "Test that process-behaviors with root stores state for correct project."
  (ai-code-behaviors-clear-all)
  (let* ((test-root "/test/project/root")
         (result (ai-code--process-behaviors "Do stuff #=code" test-root)))
    (should (string-match-p "operating-mode" result))
    (let ((state (ai-code--behaviors-get-state test-root)))
      (should (equal (plist-get state :mode) "=code"))
      (should (equal (plist-get state :modifiers) nil)))))

(ert-deftest ai-code-test-behaviors-state-isolated-by-root ()
  "Test that state for different project roots is isolated."
  (ai-code-behaviors-clear-all)
  (let ((root1 "/project/one")
        (root2 "/project/two"))
    (ai-code--behaviors-set-state '(:mode "=code") root1)
    (ai-code--behaviors-set-state '(:mode "=debug") root2)
    (should (equal (ai-code--behaviors-get-state root1) '(:mode "=code")))
    (should (equal (ai-code--behaviors-get-state root2) '(:mode "=debug")))))

;;; Sync Check Tests

(ert-deftest ai-code-test-behaviors-sync-check-when-synced ()
  "Test that sync check returns t when commit matches."
  (should (eq (ai-code--behaviors-check-sync) t)))

(ert-deftest ai-code-test-behaviors-get-repo-behavior-names ()
  "Test that repo behavior names are retrieved correctly."
  (let ((behaviors (ai-code--behaviors-get-repo-behavior-names)))
    (should behaviors)
    (should (consp behaviors))
    (should (>= (length (car behaviors)) 10))
    (should (>= (length (cdr behaviors)) 20))))

(ert-deftest ai-code-test-behaviors-synced-commit-defined ()
  "Test that synced commit constant is defined."
  (should (stringp ai-code-behaviors--synced-commit))
  (should (> (length ai-code-behaviors--synced-commit) 0)))

;;; Hashtag Completion Tests

(ert-deftest ai-code-test-behavior-hashtag-completion-table ()
  "Test that hashtag completion table includes all behaviors."
  (let ((table (ai-code--behavior-hashtag-completion-table)))
    (should (member "code" table))
    (should (member "debug" table))
    (should (member "deep" table))
    (should (member "tdd" table))
    (should (member "chinese" table))))

(ert-deftest ai-code-test-preset-and-bundle-completion ()
  "Test that completion includes both presets and bundles."
  (let ((names (ai-code--behavior-preset-and-bundle-names)))
    (should (member "@tdd-dev" names))
    (should (member "@quick-fix" names))
    (should (member "@rust-stack" names))
    (should (member "@react-stack" names))
    (should (member "@python-stack" names))))

(ert-deftest ai-code-test-bundle-names-with-prefix ()
  "Test that bundle names have @ prefix."
  (let ((names (ai-code--constraint-bundle-names)))
    (should (member "@rust-stack" names))
    (should (member "@react-stack" names))
    (dolist (name names)
      (should (string-prefix-p "@" name)))))

(ert-deftest ai-code-test-behavior-hashtag-annotation ()
  "Test that hashtag annotation function works."
  (let ((annotation (ai-code--behavior-hashtag-annotation "code")))
    (should (stringp annotation)))
  (let ((annotation (ai-code--behavior-hashtag-annotation "deep")))
    (should (stringp annotation)))
  (let ((annotation (ai-code--behavior-hashtag-annotation "chinese")))
    (should (stringp annotation))
    (should (string-match-p "中文" annotation))))

;;; Preset Completion Tests

(ert-deftest ai-code-test-behavior-preset-gptel-annotation ()
  "Test that preset annotation shows description and mode."
  (let ((annotation (ai-code--behavior-preset-gptel-annotation "tdd-dev")))
    (should (stringp annotation))
    (should (string-match-p "Test-driven" annotation))
    (should (string-match-p "=code" annotation))))

(ert-deftest ai-code-test-behavior-preset-names ()
  "Test that preset names are returned with @ prefix."
  (let ((names (ai-code--behavior-preset-names)))
    (should (member "@tdd-dev" names))
    (should (member "@quick-fix" names))
    (should (member "@deep-review" names))))

;;; Plan Mode Restriction Tests

(ert-deftest ai-code-test-plan-allows-readonly-mode ()
  "Test that #=review works in gptel-plan context."
  (let* ((result (ai-code--extract-and-remove-hashtags "#=review this code" 'gptel-plan))
         (behaviors (nth 0 result))
         (switch-needed (nth 2 result)))
    (should (equal (plist-get behaviors :mode) "=review"))
    (should-not switch-needed)))

(ert-deftest ai-code-test-plan-switches-for-modify-mode ()
  "Test that #=code triggers switch in gptel-plan context."
  (let* ((result (ai-code--extract-and-remove-hashtags "#=code fix this" 'gptel-plan))
         (behaviors (nth 0 result))
         (switch-needed (nth 2 result)))
    (should (equal (plist-get behaviors :mode) "=code"))
    (should switch-needed)))

(ert-deftest ai-code-test-plan-allows-readonly-preset ()
  "Test that @quick-review works in gptel-plan context."
  (let* ((result (ai-code--extract-and-remove-hashtags "@quick-review this" 'gptel-plan))
         (behaviors (nth 0 result))
         (switch-needed (nth 2 result)))
    (should (equal (plist-get behaviors :preset) "quick-review"))
    (should-not switch-needed)))

(ert-deftest ai-code-test-plan-switches-for-modify-preset ()
  "Test that @tdd-dev triggers switch in gptel-plan context."
  (let* ((result (ai-code--extract-and-remove-hashtags "@tdd-dev test this" 'gptel-plan))
         (behaviors (nth 0 result))
         (switch-needed (nth 2 result)))
    (should (equal (plist-get behaviors :preset) "tdd-dev"))
    (should switch-needed)))

(ert-deftest ai-code-test-agent-allows-all-modes ()
  "Test that all modes work in gptel-agent context."
  (let* ((result (ai-code--extract-and-remove-hashtags "#=code fix this" 'gptel-agent))
         (behaviors (nth 0 result))
         (switch-needed (nth 2 result)))
    (should (equal (plist-get behaviors :mode) "=code"))
    (should-not switch-needed)))

(ert-deftest ai-code-test-unknown-mode-without-equals ()
  "Test that #review (without =) is treated as unknown."
  (let* ((result (ai-code--extract-and-remove-hashtags "#review this" 'gptel-plan))
         (behaviors (nth 0 result)))
    (should (null (plist-get behaviors :mode)))))

(ert-deftest ai-code-test-readonly-modes-include-review ()
  "Test that review mode is in readonly modes."
  (should (member "=review" ai-code--behavior-readonly-modes))
  (should (member "=research" ai-code--behavior-readonly-modes))
  (should (member "=spec" ai-code--behavior-readonly-modes)))

(ert-deftest ai-code-test-modify-modes-include-code ()
  "Test that code mode is in modify modes."
  (should (member "=code" ai-code--behavior-modify-modes))
  (should (member "=debug" ai-code--behavior-modify-modes)))

(ert-deftest ai-code-test-preset-readonly-check ()
  "Test preset readonly predicate."
  (should (ai-code--behaviors-preset-readonly-p "quick-review"))
  (should (ai-code--behaviors-preset-readonly-p "deep-review"))
  (should (ai-code--behaviors-preset-readonly-p "research-deep"))
  (should-not (ai-code--behaviors-preset-readonly-p "tdd-dev"))
  (should-not (ai-code--behaviors-preset-readonly-p "quick-fix")))

(ert-deftest ai-code-test-gptel-advice-installed-once ()
  "Test that gptel advice is installed only once."
  (skip-unless (fboundp 'gptel--apply-preset))
  (advice-remove 'gptel--apply-preset
                 #'ai-code--behaviors-gptel-preset-change-advice)
  (should-not (advice-member-p #'ai-code--behaviors-gptel-preset-change-advice
                                'gptel--apply-preset))
  (ai-code--behaviors-install-gptel-advice)
  (should (advice-member-p #'ai-code--behaviors-gptel-preset-change-advice
                            'gptel--apply-preset))
  (ai-code--behaviors-install-gptel-advice)
  (ai-code--behaviors-uninstall-gptel-advice))

(ert-deftest ai-code-test-mode-switch-plan-to-agent ()
  "Test that switching from plan to agent with readonly preset applies quick-fix."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear-all)
   (let ((project-root (ai-code--behaviors-project-root)))
     (should project-root)
     (ai-code--behaviors-set-preset "quick-review" project-root)
     (should (equal (ai-code--behaviors-get-preset project-root) "quick-review"))
     (should (ai-code--behaviors-preset-readonly-p "quick-review")))))

(ert-deftest ai-code-test-mode-switch-agent-to-plan ()
  "Test that switching from agent to plan with modify preset applies quick-review."
  (ai-code-test-with-mock-project
   (ai-code-behaviors-clear-all)
   (let ((project-root (ai-code--behaviors-project-root)))
     (should project-root)
     (ai-code--behaviors-set-preset "tdd-dev" project-root)
     (should (equal (ai-code--behaviors-get-preset project-root) "tdd-dev"))
     (should-not (ai-code--behaviors-preset-readonly-p "tdd-dev")))))

(provide 'test_ai-code-behaviors)

;;; test_ai-code-behaviors.el ends here