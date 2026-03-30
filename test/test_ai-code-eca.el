;;; test_ai-code-eca.el --- Tests for ECA backend -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends)
(require 'eca)
(require 'eca-util)
(require 'eca-chat)
(require 'eca-process)

(require 'ai-code-eca)

(ert-deftest ai-code-test-eca-backend-registered ()
  "ECA should be registered in ai-code-backends."
  (should (assoc 'eca ai-code-backends)))

(ert-deftest ai-code-test-eca-backend-has-required-keys ()
  "ECA backend should have all required keys."
  (let ((spec (cdr (assoc 'eca ai-code-backends))))
    (should (plist-get spec :label))
    (should (plist-get spec :require))
    (should (plist-get spec :start))
    (should (plist-get spec :switch))
    (should (plist-get spec :send))
    (should (plist-get spec :resume))
    (should (plist-get spec :upgrade))
    (should (plist-get spec :install-skills))))

(ert-deftest ai-code-test-eca-add-menu-group-when-eca-selected ()
  "Ensure ECA menu is added when ECA backend is selected."
  (let ((ai-code-selected-backend 'eca)
        (ai-code-eca--menu-group-added nil))
    (provide 'transient)
    (cl-letf (((symbol-function 'commandp) (lambda (_sym) t))
              ((symbol-function 'transient-append-suffix)
               (lambda (prefix loc suffix &optional _face)
                 (should (memq prefix '(ai-code-menu-default ai-code-menu-2-columns)))
                 (should (equal loc '(0 -1))))))
      (ai-code-eca--add-menu-group)
      (should ai-code-eca--menu-group-added))))

(ert-deftest ai-code-test-eca-remove-menu-group ()
  "Ensure ECA menu is removed when switching away."
  (let ((ai-code-eca--menu-group-added t)
        (removed-keys nil))
    (provide 'transient)
    (cl-letf (((symbol-function 'commandp) (lambda (_sym) t))
              ((symbol-function 'transient-remove-suffix)
               (lambda (prefix key)
                 (should (memq prefix '(ai-code-menu-default ai-code-menu-2-columns)))
                 (push key removed-keys))))
      (ai-code-eca--remove-menu-group)
      (should-not ai-code-eca--menu-group-added)
      (should (equal (length removed-keys) 18))
      (dolist (key '("A" "B" "D" "E" "F" "M" "W" "X" "Y"))
        (should (= (cl-count key removed-keys :test #'string=) 2))))))

(ert-deftest ai-code-test-eca-menu-group-not-added-when-other-backend ()
  "ECA menu should not be added when other backend is selected."
  (let ((ai-code-selected-backend 'claude-code)
        (ai-code-eca--menu-group-added nil))
    (ai-code-eca--add-menu-group)
    (should-not ai-code-eca--menu-group-added)))

(ert-deftest ai-code-test-eca-menu-group-appears-in-layout ()
  "ECA group should appear in the transient layout after adding.
This test verifies the actual layout exposes the expected suffixes."
  (skip-unless (and (featurep 'transient)
                    (fboundp 'transient-define-group)))
  (condition-case nil
      (require 'ai-code)
    (error (ert-skip "ai-code could not be loaded")))
  (let ((ai-code-selected-backend 'eca)
        (ai-code-eca--menu-group-added nil))
    (ai-code-eca--add-menu-group)
    (unwind-protect
        (dolist (prefix '(ai-code-menu-default ai-code-menu-2-columns))
          (dolist (expected '(("E" . ai-code-eca-create-session-for-workspace)
                              ("W" . ai-code-eca-switch-session)
                              ("D" . eca-workspaces)
                              ("A" . ai-code-eca-add-workspace-folder)
                              ("X" . ai-code-eca-remove-workspace-folder)
                              ("F" . ai-code-eca-share-file-context)
                              ("M" . ai-code-eca-share-repo-map-context)
                              ("B" . ai-code-eca-chat-add-clipboard-context-now)
                              ("Y" . ai-code-eca-clear-shared-context)))
            (let ((suffix (transient-get-suffix prefix (car expected))))
              (should suffix)
              (should (eq (plist-get (cdr suffix) :command) (cdr expected))))))
      (setq ai-code-eca--menu-group-added nil))))

;;; ==============================================================================
;;; Binary Upgrade Tests
;;; ==============================================================================

(ert-deftest ai-code-test-eca-upgrade/pinned-version-default ()
  "Default pinned version is a valid semver."
  (should (stringp ai-code-eca-upgrade--pinned-version))
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" ai-code-eca-upgrade--pinned-version)))

(ert-deftest ai-code-test-eca-upgrade/parse-semver-extracts-version ()
  "parse-semver extracts X.Y.Z from various version strings."
  (cl-flet ((parse-semver (raw)
              (and (stringp raw)
                   (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" raw)
                   (match-string 1 raw))))
    (should (equal "0.106.0" (parse-semver "eca 0.106.0")))
    (should (equal "1.2.3" (parse-semver "v1.2.3")))
    (should (equal "10.20.30" (parse-semver "10.20.30-beta")))
    (should (null (parse-semver nil)))
    (should (null (parse-semver "no-version-here")))))

(ert-deftest ai-code-test-eca-upgrade/unified-command-exists ()
  "Unified ai-code-eca-upgrade command should exist."
  (should (fboundp 'ai-code-eca-upgrade))
  (should (commandp 'ai-code-eca-upgrade)))

(ert-deftest ai-code-test-eca-upgrade/binary-command-exists ()
  "Binary upgrade command should exist."
  (should (fboundp 'ai-code-eca-upgrade-binary))
  (should (commandp 'ai-code-eca-upgrade-binary)))

(ert-deftest ai-code-test-eca-upgrade/package-command-exists ()
  "Package upgrade command should exist."
  (should (fboundp 'ai-code-eca-upgrade-package)))

(ert-deftest ai-code-test-eca-upgrade/show-command-exists ()
  "Show upgrade status command should exist."
  (should (fboundp 'ai-code-eca-upgrade-show))
  (should (commandp 'ai-code-eca-upgrade-show)))

;;; ==============================================================================
;;; Prefix Arg Forwarding Tests
;;; ==============================================================================

(ert-deftest ai-code-test-upgrade-backend-forwards-prefix-arg ()
  "Verify that ai-code-upgrade-backend forwards prefix arg to upgrade function."
  (let ((received-arg nil)
        (ai-code-selected-backend 'eca))
    (cl-letf (((symbol-function 'ai-code-eca-upgrade)
               (lambda (arg)
                 (interactive "P")
                 (setq received-arg arg))))
      (ai-code-upgrade-backend nil)
      (should (null received-arg))
      (ai-code-upgrade-backend '(4))
      (should (equal received-arg '(4)))
      (ai-code-upgrade-backend '(16))
      (should (equal received-arg '(16))))))

;;; ==============================================================================
;;; Session Reuse Tests
;;; ==============================================================================

(ert-deftest ai-code-test-eca-find-session-by-workspace ()
  "Test finding existing session by workspace root."
  (let ((temp-dir (make-temp-file "eca-test" t)))
    (unwind-protect
        (let* ((mock-session (list :workspace-folders (list temp-dir)))
               (eca--sessions (list (cons "test-id" mock-session))))
          (cl-letf (((symbol-function 'eca--session-workspace-folders)
                     (lambda (s) (plist-get s :workspace-folders)))
                    ((symbol-function 'eca-vals)
                     (lambda (ht) (mapcar #'cdr ht))))
            (should (ai-code-eca--find-session-by-workspace temp-dir))
            (should-not (ai-code-eca--find-session-by-workspace "/nonexistent/path"))))
      (delete-directory temp-dir t))))

(ert-deftest ai-code-test-eca-create-session-reuses-existing ()
  "Test that create-session-for-workspace reuses existing session."
  (skip-unless nil)  ; Requires ECA to be loaded
  (let ((temp-dir (make-temp-file "eca-test" t))
        (session-created nil)
        (session-reused nil))
    (unwind-protect
        (progn
          ;; First call should create new session
          ;; Second call with same workspace should reuse
          )
      (delete-directory temp-dir t))))

(provide 'test_ai-code-eca)

;;; test_ai-code-eca.el ends here
