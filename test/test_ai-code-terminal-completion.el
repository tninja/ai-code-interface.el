;;; test_ai-code-terminal-completion.el --- Terminal completion tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Check that terminal completion sends input through the terminal adapter,
;; leaves the read-only terminal untouched, and refuses stale input.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-terminal-completion)

(defvar company-candidates)
(defvar company-selection)

(ert-deftest ai-code-terminal-completion-test-accepts-prompt-without-submitting ()
  "A matching prefix expands by terminal I/O without changing the buffer."
  (with-temp-buffer
    (insert "> explain the cur")
    (goto-char (point-max))
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain the cur")
          (company-candidates '("current code"))
          (company-selection 0)
          (buffer-read-only t)
          (sent nil)
          (index (list '("current code")
                       (let ((table (make-hash-table :test #'equal)))
                         (puthash "current code" "explain the current code"
                                  table)
                         table))))
      (cl-letf (((symbol-function 'ai-code-prompt-completion--ensure-index)
                 (lambda () index))
                ((symbol-function 'ai-code-backends-infra--terminal-send-backspace)
                 (lambda () (push 'backspace sent)))
                ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                 (lambda (string &optional paste)
                   (push (list string paste) sent)))
                ((symbol-function 'company-abort) (lambda () nil)))
        (ai-code-terminal-completion-accept))
      (should (equal (buffer-string) "> explain the cur"))
      (should (equal (car sent) '("explain the current code" t)))
      (should (= (length (cdr sent)) (length "explain the cur")))
      (should (string-empty-p ai-code-terminal-completion--input)))))

(ert-deftest ai-code-terminal-completion-test-stale-terminal-text-is-not-replaced ()
  "If the CLI redraws a different input, do not send any backspaces."
  (with-temp-buffer
    (insert "> changed by CLI")
    (goto-char (point-max))
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain the cur")
          (company-candidates '("current code"))
          (sent nil))
      (cl-letf (((symbol-function 'ai-code-backends-infra--terminal-send-backspace)
                 (lambda () (setq sent t)))
                ((symbol-function 'company-abort) (lambda () nil)))
        (ai-code-terminal-completion-accept))
      (should-not sent))))

(ert-deftest ai-code-terminal-completion-test-unknown-edit-discards-tracking ()
  "History and cursor operations never leave a stale tracked prefix."
  (with-temp-buffer
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain the cur")
          (this-command 'vterm-send-up)
          (company-candidates nil))
      (cl-letf (((symbol-function 'this-command-keys-vector)
                 (lambda () [up])))
        (ai-code-terminal-completion--post-command))
      (should (string-empty-p ai-code-terminal-completion--input)))))

(ert-deftest ai-code-terminal-completion-test-leaves-agent-native-menus-alone ()
  "Do not offer Company candidates inside @file or /command tokens."
  (dolist (input '("@src/file" "/help" "open @project"))
    (with-temp-buffer
      (insert input)
      (let ((ai-code-terminal-completion-mode t)
            (ai-code-terminal-completion--input input))
        (should-not (ai-code-terminal-completion--word))))))

(ert-deftest ai-code-terminal-completion-test-schedules-before-cli-redraw ()
  "Typing can schedule completion before the terminal echoes the key."
  (with-temp-buffer
    (insert "> explai")
    (goto-char (point-max))
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain"))
      (unwind-protect
          (progn
            (should-not (ai-code-terminal-completion--word))
            (ai-code-terminal-completion--schedule)
            (should (timerp ai-code-terminal-completion--timer)))
        (ai-code-terminal-completion--cancel-timer)))))

(ert-deftest ai-code-terminal-completion-test-no-multiline-candidates ()
  "Do not feed multi-line history entries into terminal input."
  (with-temp-buffer
    (insert "explain")
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain"))
      (cl-letf (((symbol-function 'ai-code-prompt-completion--ensure-index)
                 (lambda () (list '("explain the code" "explain\nmore") nil)))
                ((symbol-function 'ai-code-terminal-completion--dict)
                 (lambda (_) nil)))
        (should (equal (ai-code-terminal-completion--company 'candidates "explain")
                       '("explain the code")))))))

(ert-deftest ai-code-terminal-completion-test-company-opens-over-read-only-terminal ()
  "Company can show candidates while the terminal remains read-only."
  (skip-unless (require 'company nil t))
  (let ((buffer (generate-new-buffer " *terminal completion test*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "> explain")
          (goto-char (point-max))
          (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
          (ai-code-terminal-completion-mode 1)
          (setq ai-code-terminal-completion--input "explain"
                buffer-read-only t)
          (cl-letf (((symbol-function 'ai-code-prompt-completion--ensure-index)
                     (lambda () (list '("explain the current code")
                                      (make-hash-table :test #'equal))))
                    ((symbol-function 'ai-code-terminal-completion--dict)
                     (lambda (_) nil)))
            (let ((buffer-read-only nil))
              (should (company-auto-begin)))
            (should (equal company-candidates '("explain the current code")))
            (should (equal (buffer-string) "> explain"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (ai-code-terminal-completion-mode -1))
        (kill-buffer buffer)))))

(provide 'test_ai-code-terminal-completion)
;;; test_ai-code-terminal-completion.el ends here
