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
  "A hidden session that finishes becomes done and shows in the mode line."
  (ai-code-test-attention--with-sessions (a)
    (ai-code-session-attention-mark a 'done)
    (should (eq (ai-code-test-attention--state a) 'done))
    (should (equal (substring-no-properties
                    (ai-code-session-attention-mode-line-string))
                   " AI ✓1"))))

(ert-deftest ai-code-test-attention-mark-ignores-visible-session ()
  "A session the user is already looking at needs no attention."
  (ai-code-test-attention--with-sessions (a)
    (set-window-buffer (selected-window) a)
    (ai-code-session-attention-mark a 'done)
    (should-not (ai-code-test-attention--state a))
    (should (equal (ai-code-session-attention-mode-line-string) ""))))

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
                   (list c b a)))
    (should (equal (substring-no-properties
                    (ai-code-session-attention-mode-line-string))
                   " AI ?1 ✓2"))))

(ert-deftest ai-code-test-attention-next-switches-to-owning-tab ()
  "Jumping selects the tab and window that already show the session."
  (ai-code-test-attention--with-sessions (a)
    (let ((tab-bar-tabs-function #'tab-bar-tabs))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) a)
            (tab-bar-new-tab)
            (switch-to-buffer (get-buffer-create " *ai-code-attention-other*"))
            (ai-code-session-attention-mark a 'done)
            (ai-code-session-attention-next)
            (should (eq (window-buffer (selected-window)) a))
            (should (= 0 (tab-bar--current-tab-index)))
            (should-not (ai-code-test-attention--state a)))
        (tab-bar-close-other-tabs)))))

(ert-deftest ai-code-test-attention-next-displays-hidden-session ()
  "A session shown nowhere is displayed in the current tab."
  (ai-code-test-attention--with-sessions (a)
    (ai-code-session-attention-mark a 'done)
    (ai-code-session-attention-next)
    (should (eq (window-buffer (selected-window)) a))
    (should-not (ai-code-test-attention--state a))))

(ert-deftest ai-code-test-attention-next-with-prefix-returns ()
  "A prefix argument returns to the tab and window used before the jump."
  (ai-code-test-attention--with-sessions (a)
    (let ((tab-bar-tabs-function #'tab-bar-tabs)
          (other (get-buffer " *ai-code-attention-other*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) a)
            (tab-bar-new-tab)
            (switch-to-buffer other)
            (ai-code-session-attention-mark a 'done)
            (ai-code-session-attention-next)
            (ai-code-session-attention-next '(4))
            (should (= 1 (tab-bar--current-tab-index)))
            (should (eq (window-buffer (selected-window)) other)))
        (tab-bar-close-other-tabs)))))

(ert-deftest ai-code-test-attention-return-after-chain-goes-to-first-origin ()
  "After several jumps in a row, return to where the first jump started."
  (ai-code-test-attention--with-sessions (a b)
    (let ((other (get-buffer " *ai-code-attention-other*")))
      (ai-code-session-attention-mark a 'done)
      (ai-code-session-attention-mark b 'done)
      (let ((last-command nil))
        (ai-code-session-attention-next))
      (let ((last-command #'ai-code-session-attention-next))
        (ai-code-session-attention-next))
      (ai-code-session-attention-next '(4))
      (should (eq (window-buffer (selected-window)) other)))))

(ert-deftest ai-code-test-attention-tab-name-badges-owning-tab ()
  "Tabs showing a session that needs attention get a badge."
  (ai-code-test-attention--with-sessions (a)
    (let ((tab-bar-tabs-function #'tab-bar-tabs)
          (ai-code-session-attention-tab-bar-badge t))
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
  "The global mode adds and removes its mode-line and window hooks."
  (let ((global-mode-string (list ""))
        (window-buffer-change-functions nil)
        (ai-code-session-attention-mode-line-position 'global)
        (tab-bar-tab-name-format-functions (list #'tab-bar-tab-name-format-face)))
    (ai-code-session-attention-mode 1)
    (unwind-protect
        (progn
          (should (member ai-code-session-attention--mode-line-construct
                          global-mode-string))
          (should (memq #'ai-code-session-attention--clear-visible
                        window-buffer-change-functions))
          (should (memq #'ai-code-session-attention--tab-name-format
                        tab-bar-tab-name-format-functions))
          (should (memq #'tab-bar-tab-name-format-face
                        tab-bar-tab-name-format-functions)))
      (ai-code-session-attention-mode -1))
    (should-not (member ai-code-session-attention--mode-line-construct
                        global-mode-string))
    (should-not window-buffer-change-functions)
    (should (equal tab-bar-tab-name-format-functions
                   (list #'tab-bar-tab-name-format-face)))))

(defmacro ai-code-test-attention--with-default-mode-line (format &rest body)
  "Run BODY with the default `mode-line-format' set to FORMAT, then restore it."
  (declare (indent 1))
  `(let ((saved (default-value 'mode-line-format)))
     (unwind-protect
         (progn (setq-default mode-line-format ,format) ,@body)
       (setq-default mode-line-format saved))))

(ert-deftest ai-code-test-attention-mode-front-position-prepends-summary ()
  "The front position puts the summary first, outside other mode-line packages."
  (let ((global-mode-string (list ""))
        (window-buffer-change-functions nil)
        (ai-code-session-attention-mode-line-position 'front))
    (ai-code-test-attention--with-default-mode-line
        '("%e" (:eval (spaceline-ml-main)))
      (ai-code-session-attention-mode 1)
      (unwind-protect
          (progn
            (should (equal (default-value 'mode-line-format)
                           (list ai-code-session-attention--mode-line-construct
                                 "%e" '(:eval (spaceline-ml-main)))))
            (should-not (member ai-code-session-attention--mode-line-construct
                                global-mode-string)))
        (ai-code-session-attention-mode -1))
      (should (equal (default-value 'mode-line-format)
                     '("%e" (:eval (spaceline-ml-main))))))))

(ert-deftest ai-code-test-attention-mode-front-position-wraps-string-format ()
  "A string mode-line format still gets the summary in front."
  (let ((global-mode-string (list ""))
        (window-buffer-change-functions nil)
        (ai-code-session-attention-mode-line-position 'front))
    (ai-code-test-attention--with-default-mode-line "%b"
      (ai-code-session-attention-mode 1)
      (unwind-protect
          (should (equal (default-value 'mode-line-format)
                         (list ai-code-session-attention--mode-line-construct "%b")))
        (ai-code-session-attention-mode -1)))))

(ert-deftest ai-code-test-attention-mode-disable-cleans-both-positions ()
  "Disabling removes the summary even if the position changed meanwhile."
  (let ((global-mode-string (list ""))
        (window-buffer-change-functions nil)
        (ai-code-session-attention-mode-line-position 'global))
    (ai-code-test-attention--with-default-mode-line '("%e")
      (ai-code-session-attention-mode 1)
      (setq ai-code-session-attention-mode-line-position 'front)
      (ai-code-session-attention-mode -1)
      (should-not (member ai-code-session-attention--mode-line-construct
                          global-mode-string))
      (should (equal (default-value 'mode-line-format) '("%e"))))))

(ert-deftest ai-code-test-attention-mode-line-position-defaults-to-front ()
  "By default the summary goes where no mode-line package can hide it."
  (should (eq (eval (car (get 'ai-code-session-attention-mode-line-position
                              'standard-value)))
              'front)))

(ert-deftest ai-code-test-attention-front-covers-buffer-local-mode-lines ()
  "Buffers with their own mode line show the summary too, and get it back on disable."
  (let ((global-mode-string (list ""))
        (window-buffer-change-functions nil)
        (after-change-major-mode-hook nil)
        (ai-code-session-attention-mode-line-position 'front)
        (own (generate-new-buffer " *ai-code-attention-own-mode-line*"))
        (hidden (generate-new-buffer " *ai-code-attention-no-mode-line*")))
    (unwind-protect
        (ai-code-test-attention--with-default-mode-line '("%e")
          (with-current-buffer own (setq-local mode-line-format '("%b")))
          (with-current-buffer hidden (setq-local mode-line-format nil))
          (ai-code-session-attention-mode 1)
          (unwind-protect
              (progn
                (should (equal (buffer-local-value 'mode-line-format own)
                               (list ai-code-session-attention--mode-line-construct "%b")))
                (should-not (buffer-local-value 'mode-line-format hidden)))
            (ai-code-session-attention-mode -1))
          (should (equal (buffer-local-value 'mode-line-format own) '("%b")))
          (should-not after-change-major-mode-hook))
      (kill-buffer own)
      (kill-buffer hidden))))

(ert-deftest ai-code-test-attention-front-covers-later-buffer-local-mode-lines ()
  "A buffer that sets its own mode line after enabling also shows the summary."
  (let ((global-mode-string (list ""))
        (window-buffer-change-functions nil)
        (after-change-major-mode-hook nil)
        (ai-code-session-attention-mode-line-position 'front))
    (ai-code-test-attention--with-default-mode-line '("%e")
      (ai-code-session-attention-mode 1)
      (unwind-protect
          (with-temp-buffer
            (setq-local mode-line-format '("%b"))
            (run-hooks 'after-change-major-mode-hook)
            (should (equal mode-line-format
                           (list ai-code-session-attention--mode-line-construct "%b"))))
        (ai-code-session-attention-mode -1)))))

(provide 'test_ai-code-session-attention)

;;; test_ai-code-session-attention.el ends here
