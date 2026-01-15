;;; test_ai-code-backends-infra-loading.el --- Tests for ai-code-backends-infra loading  -*- lexical-binding: t; -*-

;; Author: AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests to ensure ai-code-backends-infra.el is properly loaded when
;; ai-code.el is required, addressing the loading bug reported in issue.

;;; Code:

(require 'ert)
(require 'ai-code)

(ert-deftest test-ai-code-backends-infra-is-loaded-after-requiring-ai-code ()
  "Test that ai-code-backends-infra is loaded after requiring ai-code."
  ;; Check that ai-code-backends-infra is loaded
  (should (featurep 'ai-code-backends-infra))
  
  ;; Verify key functions from ai-code-backends-infra are defined
  (should (fboundp 'ai-code-backends-infra--session-working-directory))
  (should (fboundp 'ai-code-backends-infra--session-buffer-name))
  (should (fboundp 'ai-code-backends-infra--toggle-or-create-session))
  (should (fboundp 'ai-code-backends-infra--create-terminal-session)))

(ert-deftest test-codex-backend-functions-available-after-set-backend ()
  "Test that codex backend functions are available after setting backend."
  ;; Set backend to codex
  (ai-code-set-backend 'codex)
  
  ;; Verify that the backend aliases are set up correctly
  (should (fboundp 'ai-code-cli-start))
  (should (fboundp 'ai-code-cli-switch-to-buffer))
  (should (fboundp 'ai-code-cli-send-command))
  (should (fboundp 'ai-code-cli-resume))
  
  ;; Verify that the aliases point to actual functions
  (should (functionp (symbol-function 'ai-code-cli-start)))
  (should (eq (symbol-function 'ai-code-cli-start) (symbol-function 'ai-code-codex-cli)))
  
  ;; Verify codex-specific functions are defined
  (should (fboundp 'ai-code-codex-cli))
  (should (fboundp 'ai-code-codex-cli-switch-to-buffer))
  (should (fboundp 'ai-code-codex-cli-send-command))
  (should (fboundp 'ai-code-codex-cli-resume)))

(provide 'test_ai-code-backends-infra-loading)

;;; test_ai-code-backends-infra-loading.el ends here
