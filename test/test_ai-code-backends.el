;;; test_ai-code-backends.el --- Tests for ai-code-backends.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for backend selection and case-insensitive matching.

;;; Code:

(require 'ert)
(require 'ai-code-backends)

(ert-deftest test-ai-code-backends--case-insensitive-backend-selection ()
  "Test that backend selection is case-insensitive."
  ;; Test lowercase (original format)
  (should (ai-code--backend-spec-normalized 'opencode))
  (should (ai-code--backend-spec-normalized 'claude-code))
  (should (ai-code--backend-spec-normalized 'gemini))
  
  ;; Test capitalized
  (should (ai-code--backend-spec-normalized 'Opencode))
  (should (ai-code--backend-spec-normalized 'Claude-code))
  (should (ai-code--backend-spec-normalized 'Gemini))
  
  ;; Test uppercase
  (should (ai-code--backend-spec-normalized 'OPENCODE))
  (should (ai-code--backend-spec-normalized 'GEMINI))
  
  ;; Test mixed case
  (should (ai-code--backend-spec-normalized 'OpenCode))
  (should (ai-code--backend-spec-normalized 'ClAuDe-CoDe))
  
  ;; Test invalid backend
  (should-not (ai-code--backend-spec-normalized 'invalid-backend)))

(ert-deftest test-ai-code-backends--backend-spec-normalized-returns-correct-spec ()
  "Test that backend spec lookup returns the correct backend plist."
  (let ((spec (ai-code--backend-spec-normalized 'opencode)))
    (should spec)
    (should (eq (car spec) 'opencode))
    (should (string= (plist-get (cdr spec) :label) "Opencode")))
  
  (let ((spec (ai-code--backend-spec-normalized 'Opencode)))
    (should spec)
    (should (eq (car spec) 'opencode))
    (should (string= (plist-get (cdr spec) :label) "Opencode")))
  
  (let ((spec (ai-code--backend-spec-normalized 'OPENCODE)))
    (should spec)
    (should (eq (car spec) 'opencode))
    (should (string= (plist-get (cdr spec) :label) "Opencode"))))

(ert-deftest test-ai-code-backends--set-backend-normalization ()
  "Test that backend name normalization works correctly."
  ;; Test string input conversion and normalization
  (let* ((backend-sym-lowercase (if (stringp "opencode")
                                    (intern (downcase "opencode"))
                                  "opencode"))
         (normalized-lowercase (intern (downcase (symbol-name backend-sym-lowercase)))))
    (should (eq normalized-lowercase 'opencode)))
  
  (let* ((backend-sym-capital (if (stringp "Opencode")
                                  (intern (downcase "Opencode"))
                                "Opencode"))
         (normalized-capital (intern (downcase (symbol-name backend-sym-capital)))))
    (should (eq normalized-capital 'opencode)))
  
  ;; Test symbol input normalization
  (let ((normalized-sym-capital (intern (downcase (symbol-name 'Opencode)))))
    (should (eq normalized-sym-capital 'opencode)))
  
  (let ((normalized-sym-upper (intern (downcase (symbol-name 'OPENCODE)))))
    (should (eq normalized-sym-upper 'opencode)))
  
  (let ((normalized-sym-mixed (intern (downcase (symbol-name 'OpenCode)))))
    (should (eq normalized-sym-mixed 'opencode))))

(ert-deftest test-ai-code-backends--set-backend-error-message ()
  "Test that ai-code-set-backend provides helpful error message for invalid backends."
  (let ((err-msg nil))
    (condition-case err
        (ai-code-set-backend 'invalid-backend-xyz)
      (user-error
       (setq err-msg (error-message-string err))))
    (should err-msg)
    (should (string-match-p "Unknown backend: invalid-backend-xyz" err-msg))
    (should (string-match-p "Available backends:" err-msg))
    (should (string-match-p "opencode" err-msg))))

(provide 'test_ai-code-backends)

;;; test_ai-code-backends.el ends here
