;;; test_ai-code-backends-infra-ghostel.el --- Ghostel lifecycle tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for Ghostel-specific lifecycle integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra-ghostel)

(defvar ghostel-command-finish-functions)
(defvar ghostel-command-start-functions)
(defvar ghostel-progress-function)
(defvar ghostel-set-title-function)
(defvar ghostel-kill-buffer-on-exit)
(defvar ai-code-backends-infra--session-directory)

(ert-deftest test-ai-code-backends-infra-ghostel-create-session-restores-ai-state ()
  "Ghostel session creation should restore AI Code local state after mode reset."
  (let* ((working-dir (file-name-as-directory
                       (expand-file-name default-directory)))
         (buffer-name " *ai-code-ghostel-reset-test*")
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--set-session-directory)
                   (lambda (target directory)
                     (with-current-buffer target
                       (setq-local ai-code-backends-infra--session-directory
                                   (file-name-as-directory
                                    (expand-file-name directory))))))
                  ((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                   (lambda () nil))
                  ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                   (lambda () nil))
                  ((symbol-function 'ghostel-exec)
                   (lambda (_buffer _program _args)
                     (kill-local-variable
                      'ai-code-backends-infra--session-terminal-backend)
                     (kill-local-variable
                      'ai-code-backends-infra--session-directory)
                     nil)))
          (setq buffer
                (car (ai-code-backends-infra-ghostel-create-session
                      buffer-name working-dir "codex" nil)))
          (with-current-buffer buffer
            (should (eq (and (boundp
                              'ai-code-backends-infra--session-terminal-backend)
                             ai-code-backends-infra--session-terminal-backend)
                        'ghostel))
            (should (equal ai-code-backends-infra--session-directory
                           working-dir))
            (should
             (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-lifecycle-hooks ()
  "Ghostel AI session configuration should install lifecycle hooks locally."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (setq-local ghostel-command-start-functions nil)
    (setq-local ghostel-command-finish-functions nil)
    (setq-local ghostel-progress-function #'ignore)
    (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
               (lambda () nil))
              ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
               (lambda () nil)))
      (ai-code-backends-infra--configure-ghostel-buffer))
    (should (memq #'ai-code-backends-infra-ghostel--command-start
                  ghostel-command-start-functions))
    (should (memq #'ai-code-backends-infra-ghostel--command-finish
                  ghostel-command-finish-functions))
    (should (eq ghostel-progress-function
                #'ai-code-backends-infra-ghostel--progress))
    (should (eq ai-code-backends-infra-ghostel--progress-function
                #'ignore))))

(ert-deftest test-ai-code-backends-infra-ghostel-command-lifecycle-updates-session-metadata ()
  "OSC 133 command hooks should write structured session metadata."
  (let ((buffer (generate-new-buffer "*ai-code-ghostel-lifecycle*"))
        (updates nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-update-metadata)
                   (lambda (target metadata)
                     (push (list target metadata) updates))))
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel))
          (ai-code-backends-infra-ghostel--command-start buffer)
          (ai-code-backends-infra-ghostel--command-finish buffer 2)
          (let ((start (cadr (nth 1 updates)))
                (finish (cadr (nth 0 updates))))
            (should (eq (plist-get start :ghostel-command-state) 'started))
            (should (eq (plist-get start :ghostel-status) 'running))
            (should (null (plist-get start :ghostel-command-exit-status)))
            (should (eq (plist-get finish :ghostel-command-state) 'finished))
            (should (= (plist-get finish :ghostel-command-exit-status) 2))
            (should (eq (plist-get finish :ghostel-status) 'error))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-command-hooks-ignore-non-ai-buffers ()
  "Global Ghostel command hooks should not touch unrelated Ghostel buffers."
  (let ((buffer (generate-new-buffer "*plain-ghostel*"))
        (updated nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-update-metadata)
                   (lambda (&rest _args)
                     (setq updated t))))
          (ai-code-backends-infra-ghostel--command-start buffer)
          (ai-code-backends-infra-ghostel--command-finish buffer 0)
          (should-not updated))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-progress-updates-session-and-delegates ()
  "OSC 9;4 progress should update metadata and preserve Ghostel's handler."
  (let ((updates nil)
        (delegated nil))
    (cl-letf (((symbol-function 'ai-code-session-update-metadata)
               (lambda (target metadata)
                 (push (list target metadata) updates))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (setq-local ai-code-backends-infra-ghostel--progress-function
                    (lambda (state progress)
                      (setq delegated (list state progress))))
        (ai-code-backends-infra-ghostel--progress 'set 42)
        (let ((metadata (cadar updates)))
          (should (eq (plist-get metadata :ghostel-progress-state) 'set))
          (should (= (plist-get metadata :ghostel-progress-value) 42))
          (should (eq (plist-get metadata :ghostel-status) 'running)))
        (should (equal delegated '(set 42)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-progress-remove-marks-idle ()
  "OSC 9;4 remove progress should mark the Ghostel status as idle."
  (let ((updates nil))
    (cl-letf (((symbol-function 'ai-code-session-update-metadata)
               (lambda (_target metadata)
                 (push metadata updates))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (ai-code-backends-infra-ghostel--progress 'remove nil)
        (should (eq (plist-get (car updates) :ghostel-progress-state) 'remove))
        (should (eq (plist-get (car updates) :ghostel-status) 'idle))))))

(provide 'test_ai-code-backends-infra-ghostel)
;;; test_ai-code-backends-infra-ghostel.el ends here
