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
(defvar company-pseudo-tooltip-overlay)

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

(ert-deftest ai-code-terminal-completion-test-accept-trusts-tracked-input-despite-tui-redraw ()
  "A TUI redraw never echoes the suffix, yet Tab still expands it.
Gating acceptance on buffer text made Tab a no-op in vterm sessions;
resetting the tracked input on unknown commands is the staleness guard."
  (with-temp-buffer
    (insert "> changed by CLI")
    (goto-char (point-max))
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain the cur")
          (company-candidates '("current code"))
          (company-selection 0)
          (sent nil)
          (index (list nil (make-hash-table :test #'equal))))
      (cl-letf (((symbol-function 'ai-code-prompt-completion--ensure-index)
                 (lambda () index))
                ((symbol-function 'ai-code-backends-infra--terminal-send-backspace)
                 (lambda () (push 'backspace sent)))
                ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                 (lambda (string &optional paste)
                   (push (list string paste) sent)))
                ((symbol-function 'company-abort) (lambda () nil)))
        (ai-code-terminal-completion-accept))
      (should (equal (buffer-string) "> changed by CLI"))
      (should (equal (car sent) '("current code" t)))
      (should (= (length (cdr sent)) (length "cur")))
      (should (string-empty-p ai-code-terminal-completion--input)))))

(ert-deftest ai-code-terminal-completion-test-accept-refuses-multiline-candidate ()
  "A multi-line candidate never reaches terminal input."
  (with-temp-buffer
    (insert "> explain")
    (goto-char (point-max))
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain")
          (company-candidates '("explain\nmore"))
          (company-selection 0)
          (sent nil))
      (cl-letf (((symbol-function 'ai-code-prompt-completion--ensure-index)
                 (lambda () (list nil (make-hash-table :test #'equal))))
                ((symbol-function 'ai-code-backends-infra--terminal-send-backspace)
                 (lambda () (setq sent t)))
                ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'company-abort) (lambda () nil)))
        (ai-code-terminal-completion-accept))
      (should-not sent)
      (should (string-empty-p ai-code-terminal-completion--input)))))

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
        (should-not (ai-code-terminal-completion--tracked-word))))))

(ert-deftest ai-code-terminal-completion-test-schedules-before-cli-redraw ()
  "Typing schedules completion before the terminal echoes the key."
  (with-temp-buffer
    (insert "> explai")
    (goto-char (point-max))
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain"))
      (unwind-protect
          (progn
            (should (equal (ai-code-terminal-completion--tracked-word)
                           "explain"))
            (ai-code-terminal-completion--schedule)
            (should (timerp ai-code-terminal-completion--timer)))
        (ai-code-terminal-completion--cancel-timer)))))

(ert-deftest ai-code-terminal-completion-test-prefix-follows-tracked-input-in-tui ()
  "A TUI redraw never echoes the suffix, yet the prefix still completes."
  (with-temp-buffer
    (insert "+-- ask --+\n| > expla |")
    (goto-char (point-max))
    (let ((ai-code-terminal-completion-mode t)
          (ai-code-terminal-completion--input "explain"))
      (should (equal (ai-code-terminal-completion--company 'prefix)
                     "explain")))))

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
  "The timer displays a tooltip and installs Tab in a read-only terminal."
  (skip-unless (require 'company nil t))
  (let ((buffer (generate-new-buffer " *terminal completion test*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "| > expla |")
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
            (ai-code-terminal-completion--schedule)
            (let ((timer ai-code-terminal-completion--timer)
                  (this-command nil))
              (should (timerp timer))
              ;; Run the scheduled callback without depending on wall time.
              (cancel-timer timer)
              (apply (timer--function timer) (timer--args timer)))
            (should (equal company-candidates '("explain the current code")))
            (should (overlayp company-pseudo-tooltip-overlay))
            (should (eq (overlay-buffer company-pseudo-tooltip-overlay) buffer))
            (should (overlay-get company-pseudo-tooltip-overlay 'before-string))
            (should (eq (key-binding (kbd "TAB"))
                        #'ai-code-terminal-completion-accept))
            (should buffer-read-only)
            (should (equal (buffer-string) "| > expla |"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (ai-code-terminal-completion-mode -1))
        (kill-buffer buffer)))))

(provide 'test_ai-code-terminal-completion)
;;; test_ai-code-terminal-completion.el ends here
