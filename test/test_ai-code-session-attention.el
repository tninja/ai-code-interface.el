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

(ert-deftest ai-code-test-attention-clear-visible-marks-seen ()
  "Showing a session buffer clears its attention state."
  (ai-code-test-attention--with-sessions (a b)
    (ai-code-session-attention-mark a 'done)
    (ai-code-session-attention-mark b 'waiting)
    (set-window-buffer (selected-window) b)
    (ai-code-session-attention--clear-visible)
    (should (eq (ai-code-test-attention--state a) 'done))
    (should-not (ai-code-test-attention--state b))))

(ert-deftest ai-code-test-attention-pending-orders-waiting-first ()
  "Waiting sessions come first, then the longest-pending ones."
  (ai-code-test-attention--with-sessions (a b c)
    (ai-code-session-attention-mark a 'done)
    (ai-code-session-attention-mark b 'done)
    (ai-code-session-attention-mark c 'waiting)
    (ai-code-session-update-metadata a '(:attention-since 20.0))
    (ai-code-session-update-metadata b '(:attention-since 10.0))
    (should (equal (mapcar #'ai-code-session-buffer
                           (ai-code-session-attention--pending))
                   (list c b a)))))

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
