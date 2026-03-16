;;; test_ai-code-eca.el --- Tests for ECA backend -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

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
    (cl-letf (((symbol-function 'transient-append-suffix)
               (lambda (prefix loc suffix &optional _face)
                 (should (eq prefix 'ai-code-menu))
                 (should (equal loc '(0 -1))))))
      (ai-code-eca--add-menu-group)
      (should ai-code-eca--menu-group-added))))

(ert-deftest ai-code-test-eca-remove-menu-group ()
  "Ensure ECA menu is removed when switching away."
  (let ((ai-code-eca--menu-group-added t))
    (provide 'transient)
    (cl-letf (((symbol-function 'transient-remove-suffix)
               (lambda (prefix suffix)
                 (should (eq prefix 'ai-code-menu))
                 (should (equal suffix "?")))))
      (ai-code-eca--remove-menu-group)
      (should-not ai-code-eca--menu-group-added))))

(ert-deftest ai-code-test-eca-menu-group-not-added-when-other-backend ()
  "ECA menu should not be added when other backend is selected."
  (let ((ai-code-selected-backend 'claude-code)
        (ai-code-eca--menu-group-added nil))
    (ai-code-eca--add-menu-group)
    (should-not ai-code-eca--menu-group-added)))

(provide 'test_ai-code-eca)

;;; test_ai-code-eca.el ends here
