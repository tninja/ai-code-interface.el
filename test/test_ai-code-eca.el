;;; test_ai-code-eca.el --- Tests for ECA backend -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(setq load-prefer-newer t)

(unless (featurep 'magit)
  (defun magit-toplevel (&optional _dir) nil)
  (defun magit-get-current-branch () nil)
  (defun magit-git-lines (&rest _args) nil)
  (provide 'magit))

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

(ert-deftest ai-code-test-eca-list-sessions-formats-plist-sessions ()
  "Ensure ECA session listings handle plist session metadata."
  (let (message-text)
    (cl-letf (((symbol-function 'eca-list-sessions)
               (lambda ()
                 (list (list :id 1
                             :status 'ready
                             :workspace-folders '("/repo" "/lib")
                             :chat-count 2))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (ai-code-eca-list-sessions)
      (should (string-match-p "Session 1" message-text))
      (should (string-match-p "/repo, /lib" message-text))
      (should (string-match-p "ready" message-text))
      (should (string-match-p "2 chats" message-text)))))

(provide 'test_ai-code-eca)

;;; test_ai-code-eca.el ends here
