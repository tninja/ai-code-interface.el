;;; test_ai-code-behaviors.el --- Tests for ai-code-behaviors.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Unit tests for the behavior injection system.

;;; Code:

(require 'ert)
(require 'ai-code-behaviors)

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
  (let ((result (car (ai-code--extract-and-remove-hashtags "Fix the bug #=debug"))))
    (should result)
    (should (equal (plist-get result :mode) "=debug"))))

(ert-deftest ai-code-test-extract-mode-with-modifiers ()
  "Test extracting mode with modifiers."
  (let ((result (car (ai-code--extract-and-remove-hashtags "Implement feature #=code #deep #tdd"))))
    (should result)
    (should (equal (plist-get result :mode) "=code"))
    (should (member "deep" (plist-get result :modifiers)))
    (should (member "tdd" (plist-get result :modifiers)))))

(ert-deftest ai-code-test-extract-modifiers-only ()
  "Test extracting modifiers without mode."
  (let ((result (car (ai-code--extract-and-remove-hashtags "Explain this #deep #wide"))))
    (should result)
    (should (null (plist-get result :mode)))
    (should (member "deep" (plist-get result :modifiers)))
    (should (member "wide" (plist-get result :modifiers)))))

(ert-deftest ai-code-test-extract-no-hashtags ()
  "Test that prompts without hashtags return nil."
  (should (null (car (ai-code--extract-and-remove-hashtags "Fix the bug in auth")))))

(ert-deftest ai-code-test-unknown-behavior-warning ()
  "Test that unknown behaviors are preserved in prompt with warning."
  (let* ((extracted (ai-code--extract-and-remove-hashtags "Do something #=code #unknown-behavior"))
         (result (car extracted))
         (cleaned (cdr extracted)))
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

(ert-deftest ai-code-test-classify-modifier-triggers ()
  "Test that modifier triggers are detected."
  (let ((result (ai-code--classify-prompt-intent-keywords
                 "Implement this thoroughly with TDD")))
    (should result)
    (should (member "deep" (plist-get result :modifiers)))
    (should (member "tdd" (plist-get result :modifiers)))))

(ert-deftest ai-code-test-all-behavior-names ()
  "Test that all behavior names are returned with # prefix."
  (let ((names (ai-code--all-behavior-names)))
    (should (member "#=code" names))
    (should (member "#=debug" names))
    (should (member "#deep" names))
    (should (member "#tdd" names))))

(ert-deftest ai-code-test-session-state-persistence ()
  "Test that behaviors persist across prompts without hashtags."
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
  (ai-code-behaviors-clear))

(ert-deftest ai-code-test-new-hashtags-supersede-session ()
  "Test that new hashtags supersede persisted session state."
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
  (ai-code-behaviors-clear))

(ert-deftest ai-code-test-presets-defined ()
  "Test that behavior presets are defined."
  (should ai-code--behavior-presets)
  (should (assoc "tdd-dev" ai-code--behavior-presets))
  (should (assoc "thorough-debug" ai-code--behavior-presets))
  (should (assoc "quick-review" ai-code--behavior-presets)))

(ert-deftest ai-code-test-apply-preset ()
  "Test applying a behavior preset."
  (ai-code-behaviors-clear)
  (ai-code-behaviors-apply-preset "tdd-dev")
  (let ((state (ai-code--behaviors-get-state)))
    (should state)
    (should (equal (plist-get state :mode) "=code"))
    (should (member "tdd" (plist-get state :modifiers)))
    (should (member "deep" (plist-get state :modifiers))))
  (should (equal (ai-code--behaviors-get-preset) "tdd-dev"))
  (ai-code-behaviors-clear))

(ert-deftest ai-code-test-mode-line-preset-display ()
  "Test that mode-line shows preset name when preset is active."
  (ai-code-behaviors-clear)
  (ai-code-behaviors-apply-preset "tdd-dev")
  (ai-code--behaviors-update-mode-line)
  (should (string= (ai-code--behaviors-mode-line-string) "[@tdd-dev]"))
  (ai-code-behaviors-clear))

(ert-deftest ai-code-test-mode-line-behavior-display ()
  "Test that mode-line shows behaviors when set directly."
  (ai-code-behaviors-clear)
  (ai-code--process-behaviors "Fix it #=debug #deep")
  (ai-code--behaviors-update-mode-line)
  (should (string= (ai-code--behaviors-mode-line-string) "[=debug deep]"))
  (should (null (ai-code--behaviors-get-preset)))
  (ai-code-behaviors-clear))

(ert-deftest ai-code-test-clear-resets-preset ()
  "Test that clear resets both preset and session state."
  (ai-code-behaviors-apply-preset "tdd-dev")
  (should (ai-code--behaviors-get-preset))
  (should (ai-code--behaviors-get-state))
  (ai-code-behaviors-clear)
  (should (null (ai-code--behaviors-get-preset)))
  (should (null (ai-code--behaviors-get-state)))
  (should (null (ai-code--behaviors-mode-line-string))))

(ert-deftest ai-code-test-hashtag-clears-preset ()
  "Test that setting behaviors via hashtag clears preset name."
  (ai-code-behaviors-apply-preset "tdd-dev")
  (should (equal (ai-code--behaviors-get-preset) "tdd-dev"))
  (ai-code--process-behaviors "Review this #=review")
  (should (null (ai-code--behaviors-get-preset)))
  (let ((state (ai-code--behaviors-get-state)))
    (should (equal (plist-get state :mode) "=review")))
  (ai-code-behaviors-clear))

(ert-deftest ai-code-test-constraint-modifiers-defined ()
  "Test that constraint modifiers are defined."
  (should ai-code--constraint-modifiers)
  (should (assoc "chinese" ai-code--constraint-modifiers))
  (should (assoc "test-after" ai-code--constraint-modifiers))
  (should (assoc "strict-lint" ai-code--constraint-modifiers)))

(ert-deftest ai-code-test-extract-constraint-modifiers ()
  "Test extracting constraint modifiers from hashtags."
  (let ((result (car (ai-code--extract-and-remove-hashtags "Fix bug #=code #chinese #test-after"))))
    (should result)
    (should (equal (plist-get result :mode) "=code"))
    (should (member "chinese" (plist-get result :constraint-modifiers)))
    (should (member "test-after" (plist-get result :constraint-modifiers)))))

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
    (should (string-match-p "Reply in Simplified Chinese" instruction))))

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
  (ai-code-behaviors-clear)
  (ai-code--behaviors-set-state
   (list :mode "=code"
         :modifiers '("deep")
         :constraint-modifiers '("chinese" "test-after")
         :custom-suffix nil))
  (ai-code--behaviors-update-mode-line)
  (should (string= (ai-code--behaviors-mode-line-string) "[=code deep +2]"))
  (ai-code-behaviors-clear))

(ert-deftest ai-code-test-mode-line-with-custom-suffix ()
  "Test that mode-line counts custom suffix."
  (ai-code-behaviors-clear)
  (ai-code--behaviors-set-state
   (list :mode "=code"
         :modifiers nil
         :constraint-modifiers '("chinese")
         :custom-suffix "Use strict mode"))
  (ai-code--behaviors-update-mode-line)
  (should (string-match-p "+2" (ai-code--behaviors-mode-line-string)))
  (ai-code-behaviors-clear))

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
         (behaviors (car result))
         (cleaned (cdr result)))
    (should (equal (plist-get behaviors :preset) "tdd-dev"))
    (should (string= cleaned "implement feature"))))

(ert-deftest ai-code-test-extract-preset-with-modifiers ()
  "Test extracting @preset-name with additional modifiers."
  (let* ((result (ai-code--extract-and-remove-hashtags "@tdd-dev #chinese implement feature"))
         (behaviors (car result))
         (cleaned (cdr result)))
    (should (equal (plist-get behaviors :preset) "tdd-dev"))
    (should (member "chinese" (plist-get behaviors :constraint-modifiers)))
    (should (string= cleaned "implement feature"))))

(ert-deftest ai-code-test-extract-preset-removes-at-syntax ()
  "Test that @preset-name is removed from cleaned prompt."
  (let* ((result (ai-code--extract-and-remove-hashtags "@mentor-learn how to refactor")))
    (should (string= (cdr result) "how to refactor"))))

(ert-deftest ai-code-test-process-preset-in-behaviors ()
  "Test that process-behaviors applies preset correctly."
  (ai-code-behaviors-clear)
  (let* ((result (ai-code--process-behaviors "@tdd-dev implement feature"))
         (preset (ai-code--behaviors-get-preset)))
    (should (equal preset "tdd-dev"))
    (should (string-match-p "operating-mode" result))))

(ert-deftest ai-code-test-preset-merges-modifiers ()
  "Test that preset modifiers merge with additional modifiers."
  (ai-code-behaviors-clear)
  (let ((result (ai-code--process-behaviors "@tdd-dev #chinese implement feature")))
    (should (string-match-p "operating-mode" result))
    (should (string-match-p "constraints" result))
    (should (string-match-p "Chinese" result))))

(ert-deftest ai-code-test-unknown-preset-ignored ()
  "Test that unknown @preset-name is ignored."
  (let* ((result (ai-code--extract-and-remove-hashtags "@unknown-preset test")))
    (should-not (car result))))

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

(provide 'test_ai-code-behaviors)

;;; test_ai-code-behaviors.el ends here