;;; test_ai-code-backends-infra-etc.el --- Tests for ai-code-backends-infra-etc.el -*- lexical-binding: t; -*-

;; Author: AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for side-panel resizing helpers and keybindings.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)

(ert-deftest test-ai-code-backends-infra-etc-main-module-requires-etc ()
  "Main infra module should require ai-code-backends-infra-etc for auto activation."
  (with-temp-buffer
    (insert-file-contents "ai-code-backends-infra.el")
    (goto-char (point-min))
    (should (re-search-forward
             "^(require 'ai-code-backends-infra-etc)" nil t))))

(ert-deftest test-ai-code-backends-infra-etc-configure-session-buffer-binds-resize-keys ()
  "Session buffer configuration should bind C-. and C-, for panel resizing."
  (let ((buffer (generate-new-buffer "*ai-code-etc-bindings*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-link--linkify-session-region)
                   (lambda (&rest _args) nil)))
          (ai-code-backends-infra--configure-session-buffer buffer)
          (with-current-buffer buffer
            (should (eq (local-key-binding (kbd "C-."))
                        #'ai-code-backends-infra-etc-grow-panel))
            (should (eq (local-key-binding (kbd "C-,"))
                        #'ai-code-backends-infra-etc-shrink-panel))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-etc-grow-panel-increases-width-for-vertical-side ()
  "Grow command should increase width by step and sync terminal dimensions."
  (let ((ai-code-backends-infra-window-width 90)
        (ai-code-backends-infra-window-height 20)
        (ai-code-backends-infra-etc-resize-step 10)
        (target-window 'mock-window)
        (target-buffer (get-buffer-create " *ai-code-etc-grow*"))
        fit-called
        sync-called)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra-etc--active-side-window)
                   (lambda () target-window))
                  ((symbol-function 'window-parameter)
                   (lambda (_window parameter)
                     (when (eq parameter 'window-side) 'right)))
                  ((symbol-function 'window-buffer)
                   (lambda (_window) target-buffer))
                  ((symbol-function 'ai-code-backends-infra--fit-side-window-body-width)
                   (lambda (window)
                     (setq fit-called window)))
                  ((symbol-function 'ai-code-backends-infra--sync-terminal-dimensions)
                   (lambda (buffer window)
                     (setq sync-called (list buffer window)))))
          (ai-code-backends-infra-etc-grow-panel)
          (should (= ai-code-backends-infra-window-width 100))
          (should (eq fit-called target-window))
          (should (equal sync-called (list target-buffer target-window))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))))

(ert-deftest test-ai-code-backends-infra-etc-shrink-panel-decreases-height-for-horizontal-side ()
  "Shrink command should decrease height by step and sync terminal dimensions." 
  (let ((ai-code-backends-infra-window-width 90)
        (ai-code-backends-infra-window-height 20)
        (ai-code-backends-infra-etc-resize-step 10)
        (target-window 'mock-window)
        (target-buffer (get-buffer-create " *ai-code-etc-shrink*"))
        resize-called
        sync-called)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra-etc--active-side-window)
                   (lambda () target-window))
                  ((symbol-function 'window-parameter)
                   (lambda (_window parameter)
                     (when (eq parameter 'window-side) 'bottom)))
                  ((symbol-function 'window-body-height)
                   (lambda (_window) 20))
                  ((symbol-function 'window-buffer)
                   (lambda (_window) target-buffer))
                  ((symbol-function 'window-resize)
                   (lambda (window delta horizontal)
                     (setq resize-called (list window delta horizontal))))
                  ((symbol-function 'ai-code-backends-infra--sync-terminal-dimensions)
                   (lambda (buffer window)
                     (setq sync-called (list buffer window)))))
          (ai-code-backends-infra-etc-shrink-panel)
          (should (= ai-code-backends-infra-window-height 10))
          (should (equal resize-called (list target-window -10 nil)))
          (should (equal sync-called (list target-buffer target-window))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))))

(provide 'test_ai-code-backends-infra-etc)

;;; test_ai-code-backends-infra-etc.el ends here
