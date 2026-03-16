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
    (should (plist-get spec :resume))))

(ert-deftest ai-code-test-eca-add-menu-suffixes-when-eca-selected ()
  "Ensure ECA menu is added when ECA backend is selected."
  (let ((ai-code-selected-backend 'eca)
        (ai-code-eca--menu-suffixes-added nil))
    (provide 'transient)
    (cl-letf (((symbol-function 'transient-append-suffix)
               (lambda (prefix loc suffix &optional _face)
                 (should (eq prefix 'ai-code-menu))
                 (should (equal loc "N")))))
      (ai-code-eca--add-menu-suffixes)
      (should ai-code-eca--menu-suffixes-added))))

(ert-deftest ai-code-test-eca-remove-menu-suffixes ()
  "Ensure ECA menu is removed when switching away."
  (let ((ai-code-eca--menu-suffixes-added t))
    (provide 'transient)
    (cl-letf (((symbol-function 'transient-remove-suffix)
               (lambda (prefix suffix)
                 (should (eq prefix 'ai-code-menu))
                 (should (equal suffix "E")))))
      (ai-code-eca--remove-menu-suffixes)
      (should-not ai-code-eca--menu-suffixes-added))))

(ert-deftest ai-code-test-eca-menu-suffixes-not-added-when-other-backend ()
  "ECA menu should not be added when other backend is selected."
  (let ((ai-code-selected-backend 'claude-code)
        (ai-code-eca--menu-suffixes-added nil))
    (ai-code-eca--add-menu-suffixes)
    (should-not ai-code-eca--menu-suffixes-added)))

(provide 'test_ai-code-eca)

;;; test_ai-code-eca.el ends here
