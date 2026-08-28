;;; test_ai-code-backends-infra-platform.el --- Platform compatibility tests -*- lexical-binding: t; -*-

;; Author: AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for native Windows terminal backend compatibility behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra-platform)

;; The focused Windows CI job loads this test without the main infra module.
;; Declare the core variable here so lexical-binding treats test `let' bindings
;; as dynamic bindings, matching production where ai-code-backends-infra.el has
;; already defined it before the platform compatibility layer is activated.
(defvar ai-code-backends-infra-terminal-backend)

(ert-deftest test-ai-code-backends-infra-platform-non-windows-delegates ()
  "Non-Windows systems should call vterm startup unchanged."
  (let ((system-type 'gnu/linux)
        (ai-code-backends-infra-terminal-backend 'vterm)
        (ai-code-backends-infra-windows-vterm-fallback t)
        called)
    (should
     (eq
      (ai-code-backends-infra-platform--vterm-ensure-advice
       (lambda ()
         (setq called t)
         'ok))
      'ok))
    (should called)
    (should (eq ai-code-backends-infra-terminal-backend 'vterm))))

(ert-deftest test-ai-code-backends-infra-platform-windows-keeps-working-vterm ()
  "Native Windows should keep vterm when it starts successfully."
  (let ((system-type 'windows-nt)
        (ai-code-backends-infra-terminal-backend 'vterm)
        (ai-code-backends-infra-windows-vterm-fallback t)
        calls)
    (should
     (eq
      (ai-code-backends-infra-platform--vterm-ensure-advice
       (lambda ()
         (push ai-code-backends-infra-terminal-backend calls)
         'vterm-ready))
      'vterm-ready))
    (should (equal calls '(vterm)))
    (should (eq ai-code-backends-infra-terminal-backend 'vterm))))

(ert-deftest test-ai-code-backends-infra-platform-windows-falls-back-to-ghostel ()
  "Native Windows should retry failed vterm startup with Ghostel."
  (let ((system-type 'windows-nt)
        (ai-code-backends-infra-terminal-backend 'vterm)
        (ai-code-backends-infra-windows-vterm-fallback t)
        calls)
    (cl-letf (((symbol-function 'ai-code-backends-infra-ghostel-ensure-backend)
               (lambda ()
                 (push ai-code-backends-infra-terminal-backend calls)
                 'ghostel-ready)))
      (should
       (eq
        (ai-code-backends-infra-platform--vterm-ensure-advice
         (lambda ()
           (push ai-code-backends-infra-terminal-backend calls)
           (user-error "vterm unavailable")))
        'ghostel-ready)))
    (should (equal (nreverse calls) '(vterm ghostel)))
    (should (eq ai-code-backends-infra-terminal-backend 'ghostel))))

(ert-deftest test-ai-code-backends-infra-platform-windows-restores-after-failure ()
  "Restore the selected backend if both Windows terminal attempts fail."
  (let ((system-type 'windows-nt)
        (ai-code-backends-infra-terminal-backend 'vterm)
        (ai-code-backends-infra-windows-vterm-fallback t))
    (cl-letf (((symbol-function 'ai-code-backends-infra-ghostel-ensure-backend)
               (lambda ()
                 (user-error "Ghostel unavailable"))))
      (should-error
       (ai-code-backends-infra-platform--vterm-ensure-advice
        (lambda ()
          (user-error "vterm unavailable")))
       :type 'user-error))
    (should (eq ai-code-backends-infra-terminal-backend 'vterm))))

(ert-deftest test-ai-code-backends-infra-platform-windows-respects-explicit-backend ()
  "Do not replace an explicitly selected non-vterm backend on Windows."
  (let ((system-type 'windows-nt)
        (ai-code-backends-infra-terminal-backend 'eat)
        (ai-code-backends-infra-windows-vterm-fallback t)
        calls)
    (should
     (eq
      (ai-code-backends-infra-platform--vterm-ensure-advice
       (lambda ()
         (push ai-code-backends-infra-terminal-backend calls)
         'eat-ready))
      'eat-ready))
    (should (equal calls '(eat)))
    (should (eq ai-code-backends-infra-terminal-backend 'eat))))

(ert-deftest test-ai-code-backends-infra-platform-fallback-can-be-disabled ()
  "Allow users to disable the native Windows vterm fallback."
  (let ((system-type 'windows-nt)
        (ai-code-backends-infra-terminal-backend 'vterm)
        (ai-code-backends-infra-windows-vterm-fallback nil)
        calls)
    (should-error
     (ai-code-backends-infra-platform--vterm-ensure-advice
      (lambda ()
        (push ai-code-backends-infra-terminal-backend calls)
        (user-error "vterm unavailable")))
     :type 'user-error)
    (should (equal calls '(vterm)))
    (should (eq ai-code-backends-infra-terminal-backend 'vterm))))

(provide 'test_ai-code-backends-infra-platform)

;;; test_ai-code-backends-infra-platform.el ends here
