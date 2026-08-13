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
(defvar ghostel-kitty-graphics-mediums)
(defvar ghostel-inhibit-redraw-functions)
(defvar ghostel-eval-cmds)
(defvar ai-code-session-link-inhibit-functions)
(defvar ai-code-session-link-image-preview-source-function)
(defvar ai-code-session-link-image-preview-transaction-function)
(defvar ai-code-backends-infra--session-directory)
(defvar ghostel--fake-cursor-overlay)
(defvar ghostel--cursor-char-pos)
(defvar ghostel--plain-link-detection-begin)
(defvar ghostel--plain-link-detection-end)
(defvar ghostel-link-map)
(defvar x-preedit-overlay)

(declare-function ai-code-ghostel-image-preview--cached-source
                  "ai-code-ghostel-image-preview" (link-text))
(declare-function ghostel-cursor-point "ghostel" ())

(ert-deftest test-ai-code-backends-infra-ghostel--native-editor-transport-p-requires-osc52-callback ()
  "An editor whitelist alone should not imply OSC 52;e support."
  (let ((ghostel-eval-cmds '(("message" message))))
    (should-not
     (ai-code-backends-infra-ghostel--native-editor-transport-p))
    (cl-letf (((symbol-function 'ghostel--osc52-eval)
               (lambda (_payload) nil)))
      (should
       (ai-code-backends-infra-ghostel--native-editor-transport-p)))))

(ert-deftest test-ai-code-backends-infra-ghostel--install-editor-transport-replaces-command ()
  "Ghostel sessions should whitelist only AI Code's token-checked callback."
  (with-temp-buffer
    (setq-local ghostel-eval-cmds
                '(("ai-code-editor-viewport" ignore)
                  ("message" message)))
    (let ((ai-code-editor-viewport-enabled t))
      (cl-letf (((symbol-function 'ghostel--osc52-eval)
                 (lambda (_payload) nil)))
        (ai-code-backends-infra-ghostel--install-editor-transport)
        (should
         (equal ghostel-eval-cmds
                '(("ai-code-editor-viewport"
                   ai-code-backends-infra-ghostel--handle-editor-request)
                  ("message" message))))
        (let ((commands ghostel-eval-cmds))
          (ai-code-backends-infra-ghostel--install-editor-transport)
          (should (eq ghostel-eval-cmds commands)))))))

(ert-deftest test-ai-code-backends-infra-ghostel--start-ghostel-process-falls-back-from-unsupported-native-transport ()
  "Ghostel should use an Emacs PTY when native callbacks are unavailable."
  (with-temp-buffer
    (let ((ai-code-editor-viewport-enabled t)
          native-pty-seen)
      (cl-progv '(ghostel-use-native-pty) '(t)
        (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                   (lambda () nil))
                  ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                   (lambda () nil))
                  ((symbol-function 'ai-code-backends-infra-ghostel--native-editor-transport-p)
                   (lambda () nil))
                  ((symbol-function 'ghostel-exec)
                   (lambda (_buffer _program _args)
                     (setq native-pty-seen
                           (symbol-value 'ghostel-use-native-pty))
                     nil)))
          (ai-code-backends-infra--start-ghostel-process
           (current-buffer) "codex")))
      (should-not native-pty-seen))))

(ert-deftest test-ai-code-backends-infra-ghostel--create-session-selects-editor-transport-before-exec ()
  "Ghostel's child should inherit the native editor callback transport."
  (with-temp-buffer
    (rename-buffer " *ai-code-ghostel-editor-transport*")
    (setq-local ghostel-eval-cmds '(("message" message)))
    (let ((buffer-name (buffer-name))
          (ai-code-editor-viewport-enabled t)
          (ai-code-editor-viewport--protocol-token "test-token")
          (process-environment nil)
          environment-seen)
      (cl-letf (((symbol-function 'ai-code-backends-infra--set-session-directory)
                 (lambda (&rest _args) nil))
                ((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                 (lambda () nil))
                ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                 (lambda () nil))
                ((symbol-function 'ghostel-exec)
                 (lambda (_buffer _program _args)
                   (setq environment-seen process-environment)
                   nil))
                ((symbol-function 'ghostel--osc52-eval)
                 (lambda (_payload) nil)))
        (ai-code-backends-infra-ghostel-create-session
         buffer-name
         default-directory
         "codex"
         nil))
      (should
       (member
        (concat "AI_CODE_EDITOR_VIEWPORT_FRAME_PREFIX="
                "\e]52;e;ai-code-editor-viewport test-token ")
               environment-seen)))))

(ert-deftest test-ai-code-backends-infra-ghostel--remote-session-preserves-editor-environment ()
  "A remote Ghostel child should retain its remote editor environment."
  (let* ((buffer-name " *ai-code-ghostel-remote-editor*")
         (environment '("EDITOR=vim" "TERM_PROGRAM=ghostel"))
         (process-environment '("PATH=/bin"))
         buffer
         environment-seen)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--set-session-directory)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                   (lambda () nil))
                  ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                   (lambda () nil))
                  ((symbol-function 'ai-code-backends-infra--configure-ghostel-buffer)
                   (lambda () nil))
                  ((symbol-function 'ai-code-backends-infra-ghostel--native-editor-transport-p)
                   (lambda () t))
                  ((symbol-function 'ai-code-editor-viewport-environment)
                   (lambda (&rest _args)
                     (ert-fail "Remote Ghostel should not inject a local editor")))
                  ((symbol-function 'ghostel-exec)
                   (lambda (seen-buffer _program _args)
                     (setq buffer seen-buffer
                           environment-seen process-environment)
                     nil)))
          (ai-code-backends-infra-ghostel-create-session
           buffer-name "/ssh:example:/tmp/project/" "codex" environment)
          (should (member "EDITOR=vim" environment-seen)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
    (cl-letf (((symbol-function 'display-images-p)
               (lambda (&optional _display) t))
              ((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
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
                #'ignore))
    (should
     (eq ai-code-editor-viewport-source-cursor-function
         #'ghostel-cursor-point))
    (should (bound-and-true-p ai-code-ghostel-image-preview-mode))
    (should
     (eq ai-code-session-link-image-preview-transaction-function
         #'ai-code-ghostel-image-preview--call-transaction))))

(ert-deftest test-ai-code-backends-infra-ghostel-skips-disabled-image-preview-lifecycle ()
  "Disabling local previews should leave their Ghostel lifecycle inactive."
  (let ((ai-code-session-link-ghostel-image-preview-enabled nil))
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (setq-local window-scroll-functions nil)
      (cl-letf (((symbol-function
                  'ai-code-backends-infra--configure-session-input-shortcuts)
                 #'ignore)
                ((symbol-function
                  'ai-code-backends-infra--install-navigation-cursor-sync)
                 #'ignore))
        (ai-code-backends-infra--configure-ghostel-buffer))
      (should-not (bound-and-true-p ai-code-ghostel-image-preview-mode))
      (should-not
       (memq #'ai-code-ghostel-image-preview--window-scroll
             window-scroll-functions)))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-ime-redraw-inhibition ()
  "Ghostel AI session configuration should enable IME redraw inhibition."
  (let (enabled)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                 (lambda () nil))
                ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                 (lambda () nil))
                ((symbol-function 'require)
                 (lambda (feature &optional _filename _noerror)
                   (when (eq feature 'ghostel-ime)
                     t)))
                ((symbol-function 'ghostel-ime-mode)
                 (lambda (arg)
                   (setq enabled arg))))
        (ai-code-backends-infra--configure-ghostel-buffer))
      (should (equal enabled 1)))))

(ert-deftest test-ai-code-backends-infra-ghostel-detects-native-preedit-overlay ()
  "Native GUI preedit overlays should inhibit Ghostel redraws."
  (with-temp-buffer
    (insert "prompt")
    (let ((overlay (make-overlay (point-min) (point-min) (current-buffer))))
      (unwind-protect
          (progn
            (overlay-put overlay 'after-string "拼")
            (setq-local ai-code-backends-infra--session-terminal-backend
                        'ghostel)
            (setq-local x-preedit-overlay overlay)
            (should
             (ai-code-backends-infra-ghostel--native-preedit-active-p
              (current-buffer))))
        (delete-overlay overlay)))))

(ert-deftest test-ai-code-backends-infra-ghostel-detects-unbound-preedit-overlay-at-point ()
  "Native preedit overlays at point may not be reachable from a symbol."
  (with-temp-buffer
    (insert "prompt")
    (goto-char (point-max))
    (let ((overlay (make-overlay (point) (point) (current-buffer))))
      (unwind-protect
          (progn
            (overlay-put overlay 'after-string "zhong")
            (setq-local ai-code-backends-infra--session-terminal-backend
                        'ghostel)
            (should
             (ai-code-backends-infra-ghostel--native-preedit-active-p
              (current-buffer))))
        (delete-overlay overlay)))))

(ert-deftest test-ai-code-backends-infra-ghostel-ignores-fake-cursor-overlay ()
  "Ghostel's fake cursor overlay should not inhibit redraws."
  (with-temp-buffer
    (insert "prompt")
    (goto-char (point-max))
    (let ((overlay (make-overlay (point) (point) (current-buffer))))
      (unwind-protect
          (progn
            (overlay-put overlay 'after-string " ")
            (setq-local ai-code-backends-infra--session-terminal-backend
                        'ghostel)
            (setq-local ghostel--fake-cursor-overlay overlay)
            (should-not
             (ai-code-backends-infra-ghostel--native-preedit-active-p
              (current-buffer))))
        (delete-overlay overlay)))))

(ert-deftest test-ai-code-backends-infra-ghostel-ignores-composition-at-point ()
  "Ordinary composed text at point should not inhibit Ghostel redraws."
  (with-temp-buffer
    (insert "zhong")
    (put-text-property (point-min) (point-max) 'composition t)
    (goto-char (point-max))
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (should-not
     (ai-code-backends-infra-ghostel--native-preedit-active-p
      (current-buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-native-preedit-redraw-inhibition ()
  "Ghostel AI session configuration should defer redraw during GUI preedit."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (setq-local ghostel-inhibit-redraw-functions nil)
    (setq-local ai-code-session-link-inhibit-functions nil)
    (setq-local pre-command-hook nil)
    (setq-local post-command-hook nil)
    (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
               (lambda () nil))
              ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
               (lambda () nil)))
      (ai-code-backends-infra--configure-ghostel-buffer))
    (should
     (memq #'ai-code-backends-infra-ghostel--redraw-inhibited-p
           ghostel-inhibit-redraw-functions))
    (should
     (memq #'ai-code-backends-infra-ghostel--redraw-inhibited-p
           ai-code-session-link-inhibit-functions))
    (should-not
     (memq #'ai-code-backends-infra-ghostel--note-input-activity
           pre-command-hook))
    (should-not
     (memq #'ai-code-backends-infra-ghostel--note-input-activity
           post-command-hook))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-recent-input-redraw-inhibition ()
  "Ghostel can optionally defer redraw briefly after input."
  (let ((ai-code-backends-infra-ghostel-inhibit-redraw-after-input t))
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (setq-local ghostel-inhibit-redraw-functions nil)
      (setq-local ai-code-session-link-inhibit-functions nil)
      (setq-local pre-command-hook nil)
      (setq-local post-command-hook nil)
      (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                 (lambda () nil))
                ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                 (lambda () nil)))
        (ai-code-backends-infra--configure-ghostel-buffer))
      (should
       (memq #'ai-code-backends-infra-ghostel--redraw-inhibited-p
             ghostel-inhibit-redraw-functions))
      (should
       (memq #'ai-code-backends-infra-ghostel--redraw-inhibited-p
             ai-code-session-link-inhibit-functions))
      (should
       (memq #'ai-code-backends-infra-ghostel--note-input-activity
             pre-command-hook))
      (should
       (memq #'ai-code-backends-infra-ghostel--note-input-activity
             post-command-hook)))))

(ert-deftest test-ai-code-backends-infra-ghostel-restores-preserved-link-spans ()
  "Ghostel redraws should restore cached links without repeated property churn."
  (let ((link-keymap (make-sparse-keymap))
        (add-count 0))
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (insert "Working (42s)\nOpen src/foo.el:1\n")
      (let (link-start link-end)
        (goto-char (point-min))
        (search-forward "src/foo.el:1")
        (setq link-start (match-beginning 0)
              link-end (match-end 0))
        (add-text-properties
         link-start link-end
         (list 'help-echo "fileref:/tmp/src/foo.el:1"
               'mouse-face 'highlight
               'keymap link-keymap
               'face 'link))
        (ai-code-backends-infra-ghostel--cache-preserved-link-spans
         (point-min) (point-max))
        (remove-text-properties
         link-start link-end
         '(help-echo nil mouse-face nil keymap nil face nil))
        (goto-char (point-min))
        (search-forward "42s")
        (replace-match "43s")
        (should-not (get-text-property link-start 'help-echo))
        (cl-letf (((symbol-function 'add-text-properties)
                   (let ((orig (symbol-function 'add-text-properties)))
                     (lambda (start end props &optional object)
                       (cl-incf add-count)
                       (funcall orig start end props object)))))
          (ai-code-backends-infra-ghostel--restore-preserved-link-spans
           (point-min) (point-max))
          (should (equal (get-text-property link-start 'help-echo)
                         "fileref:/tmp/src/foo.el:1"))
          (should (eq (get-text-property link-start 'keymap) link-keymap))
          (should (= add-count 1))
          (ai-code-backends-infra-ghostel--restore-preserved-link-spans
           (point-min) (point-max))
          (should (= add-count 1)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-schedule-link-detection-restores-first ()
  "Ghostel link detection should see cached links before rescanning."
  (let ((link-keymap (make-sparse-keymap))
        restored-before-scan)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (insert "Open src/foo.el:1\n")
      (let (link-start link-end)
        (goto-char (point-min))
        (search-forward "src/foo.el:1")
        (setq link-start (match-beginning 0)
              link-end (match-end 0))
        (add-text-properties
         link-start link-end
         (list 'help-echo "fileref:/tmp/src/foo.el:1"
               'mouse-face 'highlight
               'keymap link-keymap))
        (ai-code-backends-infra-ghostel--cache-preserved-link-spans
         (point-min) (point-max))
        (remove-text-properties
         link-start link-end
         '(help-echo nil mouse-face nil keymap nil))
        (ai-code-backends-infra-ghostel--around-schedule-link-detection
         (lambda (_begin _end)
           (setq restored-before-scan
                 (get-text-property link-start 'help-echo)))
         (point-min)
         (point-max))
        (should (equal restored-before-scan
                       "fileref:/tmp/src/foo.el:1"))))))

(ert-deftest test-ai-code-backends-infra-ghostel-records-recent-input-in-ai-session ()
  "Recent input tracking should only mark AI Code Ghostel sessions."
  (cl-letf (((symbol-function 'float-time)
             (lambda (&optional _time) 10.0)))
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (ai-code-backends-infra-ghostel--note-input-activity)
      (should (= ai-code-backends-infra-ghostel--last-input-activity-time
                 10.0)))
    (with-temp-buffer
      (ai-code-backends-infra-ghostel--note-input-activity)
      (should-not ai-code-backends-infra-ghostel--last-input-activity-time))))

(ert-deftest test-ai-code-backends-infra-ghostel-detects-recent-input-window ()
  "Recent Ghostel input should inhibit redraws for a short bounded window."
  (let ((ai-code-backends-infra-ghostel-inhibit-redraw-after-input t)
        (ai-code-backends-infra-ghostel-input-redraw-inhibit-delay 0.8))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) 10.0)))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (setq-local ai-code-backends-infra-ghostel--last-input-activity-time
                    9.5)
        (should
         (ai-code-backends-infra-ghostel--recent-input-active-p
          (current-buffer)))
        (setq-local ai-code-backends-infra-ghostel--last-input-activity-time
                    9.0)
        (should-not
         (ai-code-backends-infra-ghostel--recent-input-active-p
          (current-buffer))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra-ghostel--last-input-activity-time
                    9.5)
        (should-not
         (ai-code-backends-infra-ghostel--recent-input-active-p
          (current-buffer)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-redraw-inhibited-p-combines-input-states ()
  "Ghostel redraw inhibition should cover preedit and recent input."
  (with-temp-buffer
    (let (native recent)
      (cl-letf (((symbol-function
                  'ai-code-backends-infra-ghostel--native-preedit-active-p)
                 (lambda (&optional _buffer) native))
                ((symbol-function
                  'ai-code-backends-infra-ghostel--recent-input-active-p)
                 (lambda (&optional _buffer) recent)))
        (setq native t
              recent nil)
        (should
         (ai-code-backends-infra-ghostel--redraw-inhibited-p
          (current-buffer)))
        (setq native nil
              recent t)
        (should
         (ai-code-backends-infra-ghostel--redraw-inhibited-p
          (current-buffer)))
        (setq recent nil)
        (should-not
         (ai-code-backends-infra-ghostel--redraw-inhibited-p
          (current-buffer)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-image-mediums ()
  "Ghostel AI session configuration should enable local image mediums."
  (with-temp-buffer
    (let ((ai-code-backends-infra-ghostel-kitty-graphics-mediums
           '(file temp-file))
          (original-bound (boundp 'ghostel-kitty-graphics-mediums))
          (original-value (and (boundp 'ghostel-kitty-graphics-mediums)
                               ghostel-kitty-graphics-mediums)))
      (unwind-protect
          (progn
            (setq ghostel-kitty-graphics-mediums '(shared-mem))
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
            (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                       (lambda () nil))
                      ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                       (lambda () nil)))
              (ai-code-backends-infra--configure-ghostel-buffer))
            (should (equal ghostel-kitty-graphics-mediums
                           '(file temp-file shared-mem))))
        (if original-bound
            (setq ghostel-kitty-graphics-mediums original-value)
          (makunbound 'ghostel-kitty-graphics-mediums))))))

(ert-deftest test-ai-code-backends-infra-ghostel-remote-keeps-user-image-mediums ()
  "Remote Ghostel sessions should not add AI Code local image mediums."
  (with-temp-buffer
    (let ((ai-code-backends-infra-ghostel-kitty-graphics-mediums
           '(file temp-file))
          (original-bound (boundp 'ghostel-kitty-graphics-mediums))
          (original-value (and (boundp 'ghostel-kitty-graphics-mediums)
                               ghostel-kitty-graphics-mediums)))
      (unwind-protect
          (progn
            (setq ghostel-kitty-graphics-mediums '(shared-mem))
            (setq-local ai-code-backends-infra--session-directory
                        "/ssh:example:/repo/")
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
            (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                       (lambda () nil))
                      ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                       (lambda () nil)))
              (ai-code-backends-infra--configure-ghostel-buffer))
            (should (equal ghostel-kitty-graphics-mediums '(shared-mem))))
        (if original-bound
            (setq ghostel-kitty-graphics-mediums original-value)
          (makunbound 'ghostel-kitty-graphics-mediums))))))

(ert-deftest test-ai-code-backends-infra-ghostel-start-process-binds-image-mediums-before-exec ()
  "Ghostel startup should pass image mediums to `ghostel-exec' before init."
  (with-temp-buffer
    (let ((ai-code-backends-infra-ghostel-kitty-graphics-mediums
           '(file temp-file))
          (mediums-seen nil)
          (original-bound (boundp 'ghostel-kitty-graphics-mediums))
          (original-value (and (boundp 'ghostel-kitty-graphics-mediums)
                               ghostel-kitty-graphics-mediums)))
      (unwind-protect
          (progn
            (setq ghostel-kitty-graphics-mediums nil)
            (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                       (lambda () nil))
                      ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                       (lambda () nil))
                      ((symbol-function 'ghostel-exec)
                       (lambda (_buffer _program _args)
                         (setq mediums-seen ghostel-kitty-graphics-mediums)
                         nil)))
              (ai-code-backends-infra--start-ghostel-process
               (current-buffer) "codex --foo"))
            (should (equal mediums-seen '(file temp-file)))
            (should (equal ghostel-kitty-graphics-mediums '(file temp-file))))
        (if original-bound
            (setq ghostel-kitty-graphics-mediums original-value)
          (makunbound 'ghostel-kitty-graphics-mediums))))))

(ert-deftest test-ai-code-backends-infra-ghostel-normalize-dim-sgr-uses-ansi-gray ()
  "SGR dim text without a foreground should use standard ANSI gray."
  (with-temp-buffer
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    "\e[2mWorking\e[22m")
                   "\e[2;90mWorking\e[22;39m"))
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    "\e[2;4mWorking\e[22m")
                   "\e[2;4;90mWorking\e[22;39m"))
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    "\e[2;31mWorking\e[22m")
                   "\e[2;31mWorking\e[22m"))
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    "\e[38;2;1;2;3mWorking\e[39m")
                   "\e[38;2;1;2;3mWorking\e[39m"))
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    "\e[48;2;1;2;3mWorking\e[49m")
                   "\e[48;2;1;2;3mWorking\e[49m"))))

(ert-deftest test-ai-code-backends-infra-ghostel-normalize-dim-sgr-cross-chunk ()
  "Injected gray foreground should be reset when SGR dim ends later."
  (with-temp-buffer
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    "\e[2mWorking")
                   "\e[2;90mWorking"))
    (should ai-code-backends-infra-ghostel--dim-foreground-active)
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    " (43s)")
                   " (43s)"))
    (should (equal (ai-code-backends-infra-ghostel--normalize-dim-sgr
                    "\e[22m")
                   "\e[22;39m"))
    (should-not ai-code-backends-infra-ghostel--dim-foreground-active)))

(ert-deftest test-ai-code-backends-infra-ghostel-render-output-normalizes-dim-sgr ()
  "Ghostel session output should normalize dim SGR before rendering."
  (let (rendered linkified noted)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (cl-letf (((symbol-function 'ai-code-backends-infra--output-meaningful-p)
                 (lambda (_output) t))
                ((symbol-function 'ai-code-backends-infra--note-meaningful-output)
                 (lambda ()
                   (setq noted t)))
                ((symbol-function 'ai-code-session-link--schedule-linkify-recent-output)
                 (lambda (_buffer output &optional _delay)
                   (setq linkified output))))
        (ai-code-backends-infra-ghostel--render-output
         (current-buffer)
         (lambda (_process output)
           (setq rendered output))
         'mock-process
         "\e[2mDone src/foo.el:1\e[22m")
        (should (equal rendered "\e[2;90mDone src/foo.el:1\e[22;39m"))
        (should (equal linkified "\e[2;90mDone src/foo.el:1\e[22;39m"))
        (should noted)))))

(ert-deftest test-ai-code-backends-infra-ghostel-render-output-skips-linkify-for-working ()
  "Working redraw output should not churn session link underlines."
  (let (rendered linkified)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (cl-letf (((symbol-function 'ai-code-backends-infra--output-meaningful-p)
                 (lambda (_output) nil))
                ((symbol-function 'ai-code-session-link--schedule-linkify-recent-output)
                 (lambda (&rest _args)
                   (setq linkified t))))
        (ai-code-backends-infra-ghostel--render-output
         (current-buffer)
         (lambda (_process output)
           (setq rendered output))
         'mock-process
         "\e[2K\r\e[2mWorking (43s) - esc to interrupt\e[22m")
        (should
         (equal rendered
                "\e[2K\r\e[2;90mWorking (43s) - esc to interrupt\e[22;39m"))
        (should-not linkified)))))

(ert-deftest test-ai-code-backends-infra-ghostel--render-output-captures-ephemeral-wrapped-image ()
  "Wrapped images should survive deletion before Ghostel materializes output."
  (let* ((root (make-temp-file "ai-code-ghostel-ephemeral-image-" t))
         (dir (expand-file-name "ephemeral-screenshot-service" root))
         (image-file (expand-file-name
                      "Emacs Screenshot 2026-07-14 at 00.38.35.jpeg"
                      dir))
         (encoded-file
          (replace-regexp-in-string " " "%20" image-file t t))
         (split-index
          (1- (- (length encoded-file)
                 (length (file-name-nondirectory encoded-file)))))
         (first-output
          (concat "{\"screenshot\": {\"url\": \"file://"
                  (substring encoded-file 0 split-index)
                  "\n"))
         (second-output
          (concat (substring encoded-file split-index) "\"}}\n"))
         (buffer (generate-new-buffer " *ai-code-ghostel-ephemeral-image*"))
         scheduled)
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args)))
                    ((symbol-function
                      'ai-code-backends-infra--output-meaningful-p)
                     (lambda (_output) nil))
                    ((symbol-function 'run-at-time)
                     (lambda (delay _repeat function &rest args)
                       (push (list delay function args) scheduled)
                       'mock-timer)))
            (save-window-excursion
              (set-window-buffer (selected-window) buffer)
              (with-current-buffer buffer
                (setq-local ai-code-backends-infra--session-directory root)
                (setq-local ai-code-backends-infra--session-terminal-backend
                            'ghostel)
                (setq-local ai-code-ghostel-image-preview-mode t)
                (setq-local
                 ai-code-session-link-image-preview-position-function
                 #'ai-code-ghostel-image-preview--position-allowed-p)
                (setq-local
                 ai-code-session-link-image-preview-source-function
                 #'ai-code-ghostel-image-preview--cached-source)
                ;; A real Ghostel process filter updates its terminal model;
                ;; the Emacs buffer remains unchanged until a later redraw.
                (ai-code-backends-infra-ghostel--render-output
                 buffer #'ignore 'mock-process first-output)
                (ai-code-backends-infra-ghostel--render-output
                 buffer
                 (lambda (_process _output)
                   ;; Model a producer cleanup racing with Ghostel's filter.
                   (delete-file image-file))
                 'mock-process
                 second-output)
                (should (= (point-min) (point-max)))
                (should-not (file-exists-p image-file))
                ;; Materialize the terminal model only after the file vanished.
                (insert first-output second-output)
                (setq-local ghostel--cursor-char-pos (point-max))
                (set-window-point (selected-window) (point-max))
                (let ((retry
                       (cl-find-if
                        (lambda (entry)
                          (eq (nth 1 entry)
                              #'ai-code-ghostel-image-preview--linkify-visible))
                        scheduled)))
                  (should retry)
                  (apply (nth 1 retry) (nth 2 retry)))
                (let ((previews
                       (cl-remove-if-not
                        (lambda (overlay)
                          (overlay-get overlay
                                       'ai-code-session-image-preview))
                        (overlays-in (point-min) (point-max)))))
                  (should (= (length previews) 1))
                  (goto-char (point-min))
                  (search-forward "/Emacs%20Screenshot")
                  (should-not
                   (get-char-property (match-beginning 0) 'display))
                  (should-not
                   (get-char-property (match-beginning 0) 'invisible))
                  (should
                   (equal
                    (get-text-property (match-beginning 0)
                                       'ai-code-session-link)
                    (concat "file://" encoded-file))))))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel--render-output-retries-image-after-cursor-leaves ()
  "A completed image output line should preview after Ghostel's cursor leaves."
  (let* ((root (make-temp-file "ai-code-ghostel-cursor-row-image-" t))
         (image-file (expand-file-name "screenshot.png" root))
         (output (format "结果见 report.org:42，%s。\n" image-file))
         (buffer (generate-new-buffer " *ai-code-ghostel-cursor-row-image*"))
         scheduled)
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args)))
                    ((symbol-function
                      'ai-code-backends-infra--output-meaningful-p)
                     (lambda (_output) nil))
                    ((symbol-function 'run-at-time)
                     (lambda (delay _repeat function &rest args)
                       (push (list delay function args) scheduled)
                       'mock-timer)))
            (save-window-excursion
              (set-window-buffer (selected-window) buffer)
              (with-current-buffer buffer
                (setq-local ai-code-backends-infra--session-directory root)
                (setq-local ai-code-backends-infra--session-terminal-backend
                            'ghostel)
                (setq-local ai-code-ghostel-image-preview-mode t)
                (setq-local
                 ai-code-session-link-image-preview-position-function
                 #'ai-code-ghostel-image-preview--position-allowed-p)
                (ai-code-backends-infra-ghostel--render-output
                 buffer
                 (lambda (_process text)
                   (insert text)
                   ;; Ghostel can leave its terminal cursor on a completed
                   ;; output row until the input box is rendered separately.
                   (setq-local ghostel--cursor-char-pos (1- (point-max))))
                 'mock-process
                 output)
                (let ((generic
                       (cl-find-if
                        (lambda (entry)
                          (not
                           (eq (nth 1 entry)
                               #'ai-code-ghostel-image-preview--linkify-visible)))
                        scheduled)))
                  (should generic)
                  (apply (nth 1 generic) (nth 2 generic)))
                (goto-char (point-min))
                (search-forward image-file)
                (should (eq (get-text-property (match-beginning 0) 'face)
                            'link))
                (should-not
                 (cl-find-if
                  (lambda (overlay)
                    (overlay-get overlay 'ai-code-session-image-preview))
                  (overlays-in (point-min) (point-max))))
                ;; Codex renders its input box later without repeating the
                ;; image path in that process-output chunk.
                (goto-char (point-max))
                (insert "› \n")
                (setq-local ghostel--cursor-char-pos (1- (point-max)))
                (goto-char (point-min))
                (search-forward image-file)
                (should
                 (ai-code-ghostel-image-preview--position-allowed-p
                  (match-beginning 0) (match-end 0)))
                (set-window-point (selected-window) (point-max))
                (let ((retry
                       (cl-find-if
                        (lambda (entry)
                          (eq (nth 1 entry)
                              #'ai-code-ghostel-image-preview--linkify-visible))
                        scheduled)))
                  (should retry)
                  (apply (nth 1 retry) (nth 2 retry)))
                (should
                 (= 1
                    (length
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max))))))))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-queues-redraw-output ()
  "Ghostel redraw-like output should be batched before rendering."
  (let (rendered scheduled scheduled-delay cancelled noted linkified
        (schedule-count 0))
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat function &rest args)
                   (setq scheduled-delay delay)
                   (setq scheduled (cons function args))
                   (setq schedule-count (1+ schedule-count))
                   'mock-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer cancelled)))
                ((symbol-function 'ai-code-backends-infra--output-meaningful-p)
                 (lambda (_output) t))
                ((symbol-function 'ai-code-backends-infra--note-meaningful-output)
                 (lambda ()
                   (setq noted t)))
                ((symbol-function 'ai-code-session-link--schedule-linkify-recent-output)
                 (lambda (_buffer output &optional _delay)
                   (push output linkified))))
        (ai-code-backends-infra-ghostel--handle-process-output
         (current-buffer)
         (lambda (_process output)
           (push output rendered))
         'mock-process
         "\e[2K\rWorking (42s")
        (should-not rendered)
        (should (equal ai-code-backends-infra-ghostel--render-queue
                       "\e[2K\rWorking (42s"))
        (should (eq ai-code-backends-infra-ghostel--render-timer 'mock-timer))
        ;; A complete TUI status frame commonly arrives in a few chunks over
        ;; multiple display ticks.  One 10ms tick is too short to hide those
        ;; intermediate clears on a graphical terminal with an image preview.
        (should (= scheduled-delay 0.05))
        (ai-code-backends-infra-ghostel--handle-process-output
         (current-buffer)
         (lambda (_process output)
           (push output rendered))
         'mock-process
         " - esc to interrupt)")
        (should-not cancelled)
        (should (= schedule-count 1))
        (should (equal ai-code-backends-infra-ghostel--render-queue
                       "\e[2K\rWorking (42s - esc to interrupt)"))
        (apply (car scheduled) (cdr scheduled))
        (should (equal rendered
                       '("\e[2K\rWorking (42s - esc to interrupt)")))
        (should noted)
        (should-not linkified)
        (should-not ai-code-backends-infra-ghostel--render-queue)
        (should-not ai-code-backends-infra-ghostel--render-timer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-renders-plain-output-immediately ()
  "Plain Ghostel output should not wait for anti-flicker batching."
  (let (rendered timer-scheduled)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (setq timer-scheduled t)
                   'mock-timer))
                ((symbol-function 'ai-code-backends-infra--output-meaningful-p)
                 (lambda (_output) nil))
                ((symbol-function 'ai-code-session-link--schedule-linkify-recent-output)
                 (lambda (&rest _args) nil)))
        (ai-code-backends-infra-ghostel--handle-process-output
         (current-buffer)
         (lambda (_process output)
           (push output rendered))
         'mock-process
         "plain log line\n")
        (should (equal rendered '("plain log line\n")))
        (should-not timer-scheduled)
        (should-not ai-code-backends-infra-ghostel--render-queue)
        (should-not ai-code-backends-infra-ghostel--render-timer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-handle-output-keeps-working-chunks ()
  "Process output handling should keep Working chunks for Ghostel parsing."
  (let (rendered scheduled cancelled linkified)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (setq scheduled (cons function args))
                   'mock-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer cancelled)))
                ((symbol-function 'ai-code-backends-infra--output-meaningful-p)
                 (lambda (_output) nil))
                ((symbol-function 'ai-code-session-link--schedule-linkify-recent-output)
                 (lambda (&rest _args)
                   (setq linkified t))))
        (ai-code-backends-infra-ghostel--handle-process-output
         (current-buffer)
         (lambda (_process output)
           (push output rendered))
         'mock-process
         "\e[2K\r\e[2mWorking (42s)\e[22m")
        (ai-code-backends-infra-ghostel--handle-process-output
         (current-buffer)
         (lambda (_process output)
           (push output rendered))
         'mock-process
         "\e[2K\r\e[2;4mWorking\e[22;24m (42s)")
        (should-not cancelled)
        (should (equal ai-code-backends-infra-ghostel--render-queue
                       (concat
                        "\e[2K\r\e[2mWorking (42s)\e[22m"
                        "\e[2K\r\e[2;4mWorking\e[22;24m (42s)")))
        (apply (car scheduled) (cdr scheduled))
        (should (= (length rendered) 1))
        (should
         (equal
          (ai-code-session-link--recent-output-plain-text (car rendered))
          "Working (42s)Working (42s)"))
        (should-not linkified)))))

(ert-deftest test-ai-code-backends-infra-ghostel-wrap-process-filter-is-idempotent ()
  "Ghostel process output wrapping should not stack duplicate wrappers."
  (let ((buffer (generate-new-buffer " *ai-code-ghostel-filter-test*"))
        proc
        calls)
    (unwind-protect
        (progn
          (setq proc
                (make-pipe-process
                 :name "ai-code-ghostel-filter-test"
                 :buffer buffer
                 :noquery t
                 :filter (lambda (_process _output)
                           (setq calls (1+ (or calls 0))))))
          (ai-code-backends-infra-ghostel--wrap-process-filter buffer proc)
          (let ((wrapped (process-filter proc)))
            (ai-code-backends-infra-ghostel--wrap-process-filter buffer proc)
            (should (eq (process-filter proc) wrapped))
            (with-current-buffer buffer
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel))
            (cl-letf (((symbol-function 'ai-code-backends-infra--output-meaningful-p)
                       (lambda (_output) nil))
                      ((symbol-function
                        'ai-code-session-link--schedule-linkify-recent-output)
                       (lambda (&rest _args) nil)))
              (funcall wrapped proc "plain output"))
            (should (= calls 1))))
      (when (processp proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel--wrap-process-filter-intercepts-editor-before-render ()
  "Ghostel should remove terminal editor requests before rendering output."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (ai-code-backends-infra-ghostel-anti-flicker nil)
          proc
          intercepted
          rendered)
      (unwind-protect
          (progn
            (setq proc
                  (make-pipe-process
                   :name "ai-code-ghostel-editor-request"
                   :buffer buffer
                   :noquery t
                   :filter (lambda (_process output)
                             (setq rendered output))))
            (with-current-buffer buffer
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel))
            (ai-code-backends-infra-ghostel--wrap-process-filter buffer proc)
            (cl-letf (((symbol-function 'ai-code-editor-viewport-filter-output)
                       (lambda (process output)
                         (setq intercepted (list process output))
                         "visible"))
                      ((symbol-function 'ai-code-backends-infra--output-meaningful-p)
                       (lambda (_output) nil))
                      ((symbol-function
                        'ai-code-session-link--schedule-linkify-recent-output)
                       (lambda (&rest _args) nil)))
              (funcall (process-filter proc) proc "frame"))
            (should (equal intercepted (list proc "frame")))
            (should (equal rendered "visible")))
        (when (processp proc)
          (delete-process proc))))))

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

(ert-deftest test-ai-code-backends-infra-ghostel-linum-overlay-does-not-inhibit-redraw ()
  "Linum margin display overlays should not inhibit Ghostel redraws.
Regression test for issue #430: linum overlays with margin display specs
in `before-string' were misidentified as IME preedit overlays."
  (with-temp-buffer
    (insert "prompt line\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point) (1+ (point)) (current-buffer)))
          (margin-string (propertize " " 'display
                                     '((margin left-margin)
                                       #("57" 0 2 (face linum))))))
      (unwind-protect
          (progn
            (overlay-put overlay 'before-string margin-string)
            (setq-local ai-code-backends-infra--session-terminal-backend
                        'ghostel)
            (should-not
             (ai-code-backends-infra-ghostel--redraw-inhibited-p
              (current-buffer))))
        (delete-overlay overlay)))))

(provide 'test_ai-code-backends-infra-ghostel)
;;; test_ai-code-backends-infra-ghostel.el ends here
