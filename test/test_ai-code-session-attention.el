;;; test_ai-code-session-attention.el --- Tests for ai-code-session-attention.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for tracking AI sessions that need the user's attention.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tab-bar)
(require 'ai-code-session)
(require 'ai-code-session-attention)

(defmacro ai-code-test-attention--with-sessions (names &rest body)
  "Bind NAMES to fresh registered session buffers, then run BODY.
The selected window shows a scratch buffer, so the sessions start hidden."
  (declare (indent 1))
  `(let ((ai-code-session--sessions (make-hash-table :test 'equal))
         (ai-code-session--next-id 0)
         ,@(mapcar (lambda (name)
                     `(,name (generate-new-buffer
                              ,(format " *ai-code-attention-%s*" name))))
                   names))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (set-window-buffer (selected-window)
                              (get-buffer-create " *ai-code-attention-other*"))
           ,@(mapcar (lambda (name)
                       `(ai-code-session-register :buffer ,name
                                                  :backend "codex"
                                                  :repo-root "/tmp/repo/"))
                     names)
           ,@body)
       ,@(mapcar (lambda (name) `(kill-buffer ,name)) names))))

(defun ai-code-test-attention--state (buffer)
  "Return the attention state recorded for BUFFER."
  (plist-get (ai-code-session-metadata (ai-code-session-get buffer))
             :attention))

(ert-deftest ai-code-test-attention-mark-hidden-session-done ()
  "A hidden session that finishes becomes done."
  (ai-code-test-attention--with-sessions (a)
    (ai-code-session-attention-mark a 'done)
    (should (eq (ai-code-test-attention--state a) 'done))))

(ert-deftest ai-code-test-attention-mark-ignores-visible-session ()
  "A session the user is already looking at needs no attention."
  (ai-code-test-attention--with-sessions (a)
    (set-window-buffer (selected-window) a)
    (ai-code-session-attention-mark a 'done)
    (should-not (ai-code-test-attention--state a))))

(ert-deftest ai-code-test-attention-done-does-not-downgrade-waiting ()
  "An idle signal must not hide that the agent is waiting for input."
  (ai-code-test-attention--with-sessions (a)
    (ai-code-session-attention-mark a 'waiting)
    (ai-code-session-attention-mark a 'done)
    (should (eq (ai-code-test-attention--state a) 'waiting))))

(ert-deftest ai-code-test-attention-clear-kind-keeps-other-state ()
  "New output clears done, but keeps a waiting state."
  (ai-code-test-attention--with-sessions (a b)
    (ai-code-session-attention-mark a 'done)
    (ai-code-session-attention-mark b 'waiting)
    (ai-code-session-attention-clear a 'done)
    (ai-code-session-attention-clear b 'done)
    (should-not (ai-code-test-attention--state a))
    (should (eq (ai-code-test-attention--state b) 'waiting))))

(ert-deftest ai-code-test-attention-resolve-waiting-becomes-done-when-hidden ()
  "A resolved question on a hidden session leaves an unseen result."
  (ai-code-test-attention--with-sessions (a b c)
    (ai-code-session-attention-mark a 'waiting)
    (ai-code-session-attention-mark b 'done)
    (ai-code-session-attention-resolve-waiting a)
    (ai-code-session-attention-resolve-waiting b)
    (ai-code-session-attention-resolve-waiting c)
    (should (eq (ai-code-test-attention--state a) 'done))
    (should (eq (ai-code-test-attention--state b) 'done))
    (should-not (ai-code-test-attention--state c))))

(ert-deftest ai-code-test-attention-resolve-waiting-clears-visible-session ()
  "A resolved question on a session the user sees leaves no marker."
  (ai-code-test-attention--with-sessions (a)
    (ai-code-session-attention-mark a 'waiting)
    (set-window-buffer (selected-window) a)
    (ai-code-session-attention-resolve-waiting a)
    (should-not (ai-code-test-attention--state a))))

(ert-deftest ai-code-test-attention-clear-visible-marks-seen ()
  "Showing a session buffer clears its attention state."
  (ai-code-test-attention--with-sessions (a b)
    (ai-code-session-attention-mark a 'done)
    (ai-code-session-attention-mark b 'waiting)
    (set-window-buffer (selected-window) b)
    (ai-code-session-attention--clear-visible)
    (should (eq (ai-code-test-attention--state a) 'done))
    (should-not (ai-code-test-attention--state b))))

(ert-deftest ai-code-test-attention-display-state-priority ()
  "Waiting beats done, done beats working, and a quiet session is idle."
  (ai-code-test-attention--with-sessions (a)
    (let ((session (ai-code-session-get a)))
      (should (eq (ai-code-session-attention--display-state session) 'idle))
      (ai-code-session-attention-set-working a t)
      (should (eq (ai-code-session-attention--display-state session) 'working))
      (ai-code-session-attention-mark a 'done)
      (should (eq (ai-code-session-attention--display-state session) 'done))
      (ai-code-session-attention-mark a 'waiting)
      (should (eq (ai-code-session-attention--display-state session) 'waiting)))))

(ert-deftest ai-code-test-attention-set-working-redraws-only-on-change ()
  "Repeated output must not redraw the tab bar each time."
  (ai-code-test-attention--with-sessions (a)
    (let ((redraws 0))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&rest _) (setq redraws (1+ redraws)))))
        (ai-code-session-attention-set-working a t)
        (ai-code-session-attention-set-working a t)
        (ai-code-session-attention-set-working a nil)
        (ai-code-session-attention-set-working a nil))
      (should (= redraws 2)))))

(ert-deftest ai-code-test-attention-records-state-timestamps ()
  "State changes record when working started and when the session went quiet."
  (ai-code-test-attention--with-sessions (a)
    (cl-flet ((meta (key) (plist-get (ai-code-session-metadata (ai-code-session-get a)) key)))
      (ai-code-session-attention-set-working a t)
      (should (numberp (meta :working-since)))
      (ai-code-session-attention-set-working a nil)
      (should (numberp (meta :quiet-since)))
      (ai-code-session-update-metadata a '(:quiet-since nil))
      (ai-code-session-attention-mark a 'done)
      (ai-code-session-attention-clear a)
      (should (numberp (meta :quiet-since))))))

(ert-deftest ai-code-test-attention-tab-badges-every-ai-tab ()
  "Every tab showing an AI session has a badge; other tabs have none."
  (ai-code-test-attention--with-sessions (a b)
    (let ((tab-bar-tabs-function #'tab-bar-tabs))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) a)
            (tab-bar-new-tab)
            (switch-to-buffer b)
            (tab-bar-new-tab)
            (switch-to-buffer (get-buffer-create " *ai-code-attention-other*"))
            (ai-code-session-attention-set-working b t)
            (let ((tabs (funcall tab-bar-tabs-function)))
              (should (equal (substring-no-properties
                              (ai-code-session-attention--tab-name-format
                               "idle-tab" (nth 0 tabs) 1))
                             "○ idle-tab"))
              (should (equal (substring-no-properties
                              (ai-code-session-attention--tab-name-format
                               "busy-tab" (nth 1 tabs) 2))
                             "◐ busy-tab"))
              (should (equal (ai-code-session-attention--tab-name-format
                              "notes" (nth 2 tabs) 3)
                             "notes"))))
        (tab-bar-close-other-tabs)))))

(ert-deftest ai-code-test-attention-tab-badge-shows-most-important-session ()
  "A tab with several sessions shows the most important state."
  (ai-code-test-attention--with-sessions (a b)
    (let ((tab-bar-tabs-function #'tab-bar-tabs))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) a)
            (set-window-buffer (split-window) b)
            (tab-bar-new-tab)
            (delete-other-windows)
            (switch-to-buffer (get-buffer-create " *ai-code-attention-other*"))
            (ai-code-session-attention-set-working a t)
            (ai-code-session-attention-mark b 'done)
            (should (equal (substring-no-properties
                            (ai-code-session-attention--tab-name-format
                             "work" (nth 0 (funcall tab-bar-tabs-function)) 1))
                           "✓ work")))
        (tab-bar-close-other-tabs)))))

(ert-deftest ai-code-test-attention-tab-name-badges-owning-tab ()
  "Tabs showing a session that needs attention get a badge."
  (ai-code-test-attention--with-sessions (a)
    (let ((tab-bar-tabs-function #'tab-bar-tabs))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) a)
            (tab-bar-new-tab)
            (switch-to-buffer (get-buffer-create " *ai-code-attention-other*"))
            (ai-code-session-attention-mark a 'waiting)
            (let ((tabs (funcall tab-bar-tabs-function)))
              (should (equal (substring-no-properties
                              (ai-code-session-attention--tab-name-format
                               "work" (nth 0 tabs) 1))
                             "? work"))
              (should (equal (ai-code-session-attention--tab-name-format
                              "notes" (nth 1 tabs) 2)
                             "notes"))))
        (tab-bar-close-other-tabs)))))

(ert-deftest ai-code-test-attention-mode-installs-and-removes-display ()
  "The global mode adds and removes its tab-bar and window hooks."
  (let ((window-buffer-change-functions nil)
        (tab-bar-tab-name-format-functions (list #'tab-bar-tab-name-format-face)))
    (ai-code-session-attention-mode 1)
    (unwind-protect
        (progn
          (should (memq #'ai-code-session-attention--clear-visible
                        window-buffer-change-functions))
          (should (memq #'ai-code-session-attention--tab-name-format
                        tab-bar-tab-name-format-functions))
          (should (memq #'tab-bar-tab-name-format-face
                        tab-bar-tab-name-format-functions)))
      (ai-code-session-attention-mode -1))
    (should-not window-buffer-change-functions)
    (should (equal tab-bar-tab-name-format-functions
                   (list #'tab-bar-tab-name-format-face)))))
(provide 'test_ai-code-session-attention)

;;; test_ai-code-session-attention.el ends here
