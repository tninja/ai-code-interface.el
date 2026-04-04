;;; test_ai-code-input.el --- Tests for ai-code-input.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-input.el, focusing on filepath completion features.

;;; Code:

(require 'ert)
(require 'ai-code-input)
(require 'ai-code-file)
(require 'magit)
(require 'cl-lib)

;;; Tests for ai-code--comment-context-p

(ert-deftest ai-code-test-comment-context-p-inside-line-comment ()
  "Test that ai-code--comment-context-p detects point inside a line comment."
  (with-temp-buffer
    ;; Set up comment syntax for a language like Emacs Lisp
    (emacs-lisp-mode)
    (insert ";; This is a comment\n")
    (insert "regular code\n")
    ;; Move point inside the comment
    (goto-char (point-min))
    (forward-char 5)  ; Inside the comment
    (should (ai-code--comment-context-p))))

(ert-deftest ai-code-test-comment-context-p-outside-comment ()
  "Test that ai-code--comment-context-p returns nil when point is outside a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment\n")
    (insert "regular code\n")
    ;; Move point to regular code
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)  ; Inside regular code
    (should-not (ai-code--comment-context-p))))

(ert-deftest ai-code-test-comment-context-p-c-style-line-comment ()
  "Test comment detection in C-style line comments."
  (with-temp-buffer
    (c-mode)
    (insert "// This is a C comment\n")
    (insert "int x = 0;\n")
    ;; Move point inside the comment
    (goto-char (point-min))
    (forward-char 5)  ; Inside the comment
    (should (ai-code--comment-context-p))))

(ert-deftest ai-code-test-comment-context-p-block-comment ()
  "Test comment detection inside block comments."
  (with-temp-buffer
    (c-mode)
    (insert "/* This is a\n")
    (insert "   block comment */\n")
    (insert "int x = 0;\n")
    ;; Move point inside the block comment
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)  ; Inside the block comment
    (should (ai-code--comment-context-p))))

(ert-deftest ai-code-test-comment-context-p-after-comment ()
  "Test that point after a comment is not considered inside the comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Comment\n")
    (insert "code")
    ;; Move to end of first line (after comment)
    (goto-char (point-min))
    (end-of-line)
    ;; Should still be in comment (syntax-ppss includes newline)
    (should (ai-code--comment-context-p))))

;;; Tests for ai-code--any-ai-session-active-p

(ert-deftest ai-code-test-any-ai-session-active-p-with-session ()
  "Test that ai-code--any-ai-session-active-p returns non-nil when AI session exists."
  (let ((original-bound (fboundp 'ai-code-backends-infra--session-buffer-p))
        (original-fn (and (fboundp 'ai-code-backends-infra--session-buffer-p)
                          (symbol-function 'ai-code-backends-infra--session-buffer-p))))
    (unwind-protect
        (progn
          ;; Define the function so fboundp returns t
          (fset 'ai-code-backends-infra--session-buffer-p
                (lambda (buf)
                  (string-prefix-p "*ai-session" (buffer-name buf))))
          ;; Create a mock AI session buffer
          (let ((session-buf (get-buffer-create "*ai-session-test*")))
            (unwind-protect
                (should (ai-code--any-ai-session-active-p))
              (when (buffer-live-p session-buf)
                (kill-buffer session-buf)))))
      ;; Restore original state
      (if original-bound
          (fset 'ai-code-backends-infra--session-buffer-p original-fn)
        (fmakunbound 'ai-code-backends-infra--session-buffer-p)))))

(ert-deftest ai-code-test-any-ai-session-active-p-no-session ()
  "Test that ai-code--any-ai-session-active-p returns nil when no AI session exists."
  (cl-letf (((symbol-function 'ai-code-backends-infra--session-buffer-p)
             (lambda (buf) nil)))
    (should-not (ai-code--any-ai-session-active-p))))

(ert-deftest ai-code-test-any-ai-session-active-p-function-not-available ()
  "Test that ai-code--any-ai-session-active-p returns nil when function is not available."
  ;; Save the original function and temporarily unbind it
  (let ((original-fn (symbol-function 'ai-code-backends-infra--session-buffer-p)))
    (unwind-protect
        (progn
          (fmakunbound 'ai-code-backends-infra--session-buffer-p)
          (should-not (ai-code--any-ai-session-active-p)))
      ;; Restore the function
      (fset 'ai-code-backends-infra--session-buffer-p original-fn))))

(ert-deftest ai-code-test-any-ai-session-active-p-multiple-buffers ()
  "Test AI session detection with multiple buffers, only one being a session."
  (cl-letf (((symbol-function 'ai-code-backends-infra--session-buffer-p)
             (lambda (buf)
               (string= (buffer-name buf) "*ai-session-active*"))))
    (let ((session-buf (get-buffer-create "*ai-session-active*"))
          (regular-buf (get-buffer-create "*regular-buffer*")))
      (unwind-protect
          (should (ai-code--any-ai-session-active-p))
        (when (buffer-live-p session-buf) (kill-buffer session-buf))
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))))))

;;; Tests for ai-code--comment-filepath-capf

(ert-deftest ai-code-test-comment-filepath-capf-returns-candidates ()
  "Test that ai-code--comment-filepath-capf returns candidates inside comment with @."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      ;; Set buffer file name
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; Check @")
      (goto-char (point-max))
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) "/tmp/"))
                ((symbol-function 'ai-code--prompt-filepath-candidates)
                 (lambda () '("@file1.el" "@file2.el"))))
        
        (let* ((result (ai-code--comment-filepath-capf))
               (start (nth 0 result))
               (end (nth 1 result))
               (candidates (nth 2 result))
               (props (nthcdr 3 result)))
          ;; Should return completion table
          (should result)
          (should (= start (- (point) 1)))  ; start at @
          (should (= end (point)))          ; end at current point
          (should (equal candidates '("@file1.el" "@file2.el")))
          (should (eq (plist-get props :exclusive) 'no)))))))

(ert-deftest ai-code-test-comment-filepath-capf-outside-comment ()
  "Test that ai-code--comment-filepath-capf returns nil outside a comment."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert "(defun test () @")
      (goto-char (point-max))
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) "/tmp/")))
        
        ;; Should return nil because we're not in a comment
        (should-not (ai-code--comment-filepath-capf))))))

;;; ai-code--comment-filepath-capf does not check ai-code--any-ai-session-active-p,
;;; so no test for "no AI session" scenario is needed here.

(ert-deftest ai-code-test-comment-filepath-capf-disabled ()
  "Test that ai-code--comment-filepath-capf returns nil when disabled."
  (let ((ai-code-prompt-filepath-completion-enabled nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; Check @")
      (goto-char (point-max))
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) "/tmp/")))
        
        ;; Should return nil because feature is disabled
        (should-not (ai-code--comment-filepath-capf))))))

(ert-deftest ai-code-test-comment-filepath-capf-no-git-repo ()
  "Test that ai-code--comment-filepath-capf returns nil outside a git repository."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; Check @")
      (goto-char (point-max))
      
      ;; Mock dependencies - not in git repo
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) nil)))
        
        ;; Should return nil because not in a git repository
        (should-not (ai-code--comment-filepath-capf))))))

(ert-deftest ai-code-test-comment-filepath-capf-in-minibuffer ()
  "Test that ai-code--comment-filepath-capf returns nil in minibuffer."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    ;; Mock minibufferp to return true
    (cl-letf (((symbol-function 'minibufferp)
               (lambda (&optional buffer) t))
              ((symbol-function 'ai-code--any-ai-session-active-p)
               (lambda () t))
              ((symbol-function 'magit-toplevel)
               (lambda (&optional dir) "/tmp/")))
      (with-temp-buffer
        (emacs-lisp-mode)
        (setq buffer-file-name "/tmp/test.el")
        (insert ";; Check @")
        (goto-char (point-max))
        
        ;; Should return nil because in minibuffer
        (should-not (ai-code--comment-filepath-capf))))))

(ert-deftest ai-code-test-comment-filepath-capf-no-buffer-file ()
  "Test that ai-code--comment-filepath-capf returns nil when buffer has no file."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      ;; Don't set buffer-file-name
      (insert ";; Check @")
      (goto-char (point-max))
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) "/tmp/")))
        
        ;; Should return nil because buffer has no file
        (should-not (ai-code--comment-filepath-capf))))))

(ert-deftest ai-code-test-comment-filepath-capf-partial-path ()
  "Test ai-code--comment-filepath-capf with partial file path after @."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; Check @src/ma")
      (goto-char (point-max))
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) "/tmp/"))
                ((symbol-function 'ai-code--prompt-filepath-candidates)
                 (lambda () '("@src/main.el" "@src/main.js"))))
        
        (let* ((result (ai-code--comment-filepath-capf))
               (start (nth 0 result))
               (end (nth 1 result))
               (candidates (nth 2 result)))
          (should result)
          ;; Start should be at @ position
          (should (= start (- (point) (length "src/ma") 1)))
          (should (= end (point)))
          (should (equal candidates '("@src/main.el" "@src/main.js"))))))))

(ert-deftest ai-code-test-comment-filepath-capf-no-at-symbol ()
  "Test that ai-code--comment-filepath-capf returns nil without @ symbol."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; Check file")
      (goto-char (point-max))
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) "/tmp/")))
        
        ;; Should return nil because no @ symbol before point
        (should-not (ai-code--comment-filepath-capf))))))

;;; Tests for ai-code-prompt-filepath-completion-mode

(ert-deftest ai-code-test-filepath-completion-mode-default-enabled ()
  "Test that @ filepath completion starts enabled by default."
  (ai-code-prompt-filepath-completion-mode -1)
  (load-file (expand-file-name "ai-code-input.el" default-directory))
  (should ai-code-prompt-filepath-completion-enabled)
  (should ai-code-prompt-filepath-completion-mode)
  (should (memq 'ai-code--comment-auto-trigger-filepath-completion
                post-self-insert-hook))
  (should (memq 'ai-code--session-auto-trigger-filepath-completion
                post-self-insert-hook))
  (should (memq 'ai-code--comment-filepath-setup
                after-change-major-mode-hook)))

(ert-deftest ai-code-test-filepath-completion-mode-enable ()
  "Test that enabling the mode sets up hooks and variable correctly."
  (let ((ai-code-prompt-filepath-completion-mode nil))
    (unwind-protect
        (progn
          ;; Enable the mode
          (ai-code-prompt-filepath-completion-mode 1)
          
          ;; Check that the variable is set
          (should ai-code-prompt-filepath-completion-enabled)
          (should ai-code-prompt-filepath-completion-mode)
          
          ;; Check that hooks are added
          (should (memq 'ai-code--comment-auto-trigger-filepath-completion
                        post-self-insert-hook))
          (should (memq 'ai-code--comment-filepath-setup
                        after-change-major-mode-hook)))
      
      ;; Cleanup: disable the mode
      (ai-code-prompt-filepath-completion-mode -1))))

(ert-deftest ai-code-test-filepath-completion-mode-disable ()
  "Test that disabling the mode removes hooks and variable correctly."
  (let ((ai-code-prompt-filepath-completion-mode nil))
    (unwind-protect
        (progn
          ;; Enable then disable the mode
          (ai-code-prompt-filepath-completion-mode 1)
          (ai-code-prompt-filepath-completion-mode -1)
          
          ;; Check that the variable is unset
          (should-not ai-code-prompt-filepath-completion-enabled)
          (should-not ai-code-prompt-filepath-completion-mode)
          
          ;; Check that hooks are removed
          (should-not (memq 'ai-code--comment-auto-trigger-filepath-completion
                            post-self-insert-hook))
          (should-not (memq 'ai-code--comment-filepath-setup
                            after-change-major-mode-hook)))
      
      ;; Cleanup
      (ai-code-prompt-filepath-completion-mode -1))))

(ert-deftest ai-code-test-filepath-completion-mode-setup-in-buffers ()
  "Test that enabling mode sets up completion in existing buffers."
  (let ((ai-code-prompt-filepath-completion-mode nil)
        (test-buf (get-buffer-create "*test-comment-completion*")))
    (unwind-protect
        (progn
          ;; Create a test buffer
          (with-current-buffer test-buf
            ;; Clear any existing completion functions
            (setq-local completion-at-point-functions nil))
          
          ;; Enable the mode
          (ai-code-prompt-filepath-completion-mode 1)
          
          ;; Check that completion function was added to the buffer
          (with-current-buffer test-buf
            (should (memq 'ai-code--comment-filepath-capf
                          completion-at-point-functions))))
      
      ;; Cleanup
      (ai-code-prompt-filepath-completion-mode -1)
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest ai-code-test-filepath-completion-mode-cleanup-in-buffers ()
  "Test that disabling mode cleans up completion in all buffers."
  (let ((ai-code-prompt-filepath-completion-mode nil)
        (test-buf (get-buffer-create "*test-comment-cleanup*")))
    (unwind-protect
        (progn
          ;; Enable the mode first
          (ai-code-prompt-filepath-completion-mode 1)
          
          ;; Verify setup in buffer
          (with-current-buffer test-buf
            (should (memq 'ai-code--comment-filepath-capf
                          completion-at-point-functions)))
          
          ;; Disable the mode
          (ai-code-prompt-filepath-completion-mode -1)
          
          ;; Check that completion function was removed from the buffer
          (with-current-buffer test-buf
            (should-not (memq 'ai-code--comment-filepath-capf
                              completion-at-point-functions))))
      
      ;; Cleanup
      (ai-code-prompt-filepath-completion-mode -1)
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest ai-code-test-filepath-completion-mode-toggle ()
  "Test that toggling mode works correctly."
  (unwind-protect
      (progn
        ;; Start from a known disabled state
        (ai-code-prompt-filepath-completion-mode -1)
        (should-not ai-code-prompt-filepath-completion-mode)

        ;; First toggle should enable
        (ai-code-prompt-filepath-completion-mode 'toggle)
        (should ai-code-prompt-filepath-completion-mode)

        ;; Second toggle should disable
        (ai-code-prompt-filepath-completion-mode 'toggle)
        (should-not ai-code-prompt-filepath-completion-mode))

    ;; Cleanup
    (ai-code-prompt-filepath-completion-mode -1)))

(ert-deftest ai-code-test-filepath-completion-mode-after-major-mode-change ()
  "Test that completion setup works after major mode change."
  (let ((ai-code-prompt-filepath-completion-mode nil)
        (test-buf (get-buffer-create "*test-major-mode-change*")))
    (unwind-protect
        (progn
          ;; Enable the mode
          (ai-code-prompt-filepath-completion-mode 1)
          
          ;; Simulate major mode change in buffer
          (with-current-buffer test-buf
            (setq-local completion-at-point-functions nil)
            (run-hooks 'after-change-major-mode-hook)
            
            ;; Check that completion function was added
            (should (memq 'ai-code--comment-filepath-capf
                          completion-at-point-functions))))
      
      ;; Cleanup
      (ai-code-prompt-filepath-completion-mode -1)
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

;;; Tests for ai-code--comment-auto-trigger-filepath-completion

(ert-deftest ai-code-test-comment-auto-trigger-with-at ()
  "Test that auto-trigger works when @ is inserted in a comment."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (completing-read-called nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; ")

      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--prompt-filepath-candidates)
                 (lambda () '("@file1.el" "@file2.el")))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (setq completing-read-called t) "@file1.el")))

        ;; Insert @ and trigger auto-completion
        (insert "@")
        (ai-code--comment-auto-trigger-filepath-completion)

        ;; Should have called completing-read
        (should completing-read-called)))))

(ert-deftest ai-code-test-comment-auto-trigger-outside-comment ()
  "Test that auto-trigger doesn't work outside a comment."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (completion-called nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert "(defun test () ")
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'completion-at-point)
                 (lambda () (setq completion-called t))))
        
        ;; Insert @ and trigger auto-completion
        (insert "@")
        (ai-code--comment-auto-trigger-filepath-completion)
        
        ;; Should NOT have called completion-at-point (not in comment)
        (should-not completion-called)))))

(ert-deftest ai-code-test-comment-auto-trigger-no-ai-session ()
  "Test that auto-trigger doesn't call completing-read when no candidates available."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (completing-read-called nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; ")

      ;; Mock dependencies - no candidates available
      (cl-letf (((symbol-function 'ai-code--prompt-filepath-candidates)
                 (lambda () nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (setq completing-read-called t) "")))

        ;; Insert @ and trigger auto-completion
        (insert "@")
        (ai-code--comment-auto-trigger-filepath-completion)

        ;; Should NOT have called completing-read (no candidates)
        (should-not completing-read-called)))))

(ert-deftest ai-code-test-comment-auto-trigger-disabled ()
  "Test that auto-trigger doesn't work when feature is disabled."
  (let ((ai-code-prompt-filepath-completion-enabled nil)
        (completion-called nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; ")
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'completion-at-point)
                 (lambda () (setq completion-called t))))
        
        ;; Insert @ and trigger auto-completion
        (insert "@")
        (ai-code--comment-auto-trigger-filepath-completion)
        
        ;; Should NOT have called completion-at-point (feature disabled)
        (should-not completion-called)))))

(ert-deftest ai-code-test-comment-auto-trigger-without-at ()
  "Test that auto-trigger doesn't work without @ symbol."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (completion-called nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; Check")
      
      ;; Mock dependencies
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () t))
                ((symbol-function 'completion-at-point)
                 (lambda () (setq completion-called t))))
        
        ;; Trigger auto-completion without @ before point
        (ai-code--comment-auto-trigger-filepath-completion)
        
        ;; Should NOT have called completion-at-point (no @ before point)
        (should-not completion-called)))))

(ert-deftest ai-code-test-comment-auto-trigger-in-minibuffer ()
  "Test that auto-trigger doesn't work in minibuffer."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (completion-called nil))
    ;; Mock minibufferp to return true
    (cl-letf (((symbol-function 'minibufferp)
               (lambda (&optional buffer) t))
              ((symbol-function 'ai-code--any-ai-session-active-p)
               (lambda () t))
              ((symbol-function 'completion-at-point)
               (lambda () (setq completion-called t))))
      (with-temp-buffer
        (emacs-lisp-mode)
        (setq buffer-file-name "/tmp/test.el")
        (insert ";; @")
        
        ;; Trigger auto-completion
        (ai-code--comment-auto-trigger-filepath-completion)
        
        ;; Should NOT have called completion-at-point (in minibuffer)
        (should-not completion-called)))))

;;; Tests for # symbol completion feature

;; Tests for ai-code--imenu-subalist-p

(ert-deftest ai-code-test-imenu-subalist-p-with-subalist ()
  "Test that ai-code--imenu-subalist-p correctly identifies a sub-alist."
  (let ((subalist '(("Function1" . 100) ("Function2" . 200))))
    (should (ai-code--imenu-subalist-p subalist))))

(ert-deftest ai-code-test-imenu-subalist-p-with-non-list ()
  "Test that ai-code--imenu-subalist-p returns nil for non-list."
  (should-not (ai-code--imenu-subalist-p 42))
  (should-not (ai-code--imenu-subalist-p "string")))

(ert-deftest ai-code-test-imenu-subalist-p-with-empty-list ()
  "Test that ai-code--imenu-subalist-p returns nil for empty list."
  (should-not (ai-code--imenu-subalist-p '())))

(ert-deftest ai-code-test-imenu-subalist-p-with-invalid-entries ()
  "Test that ai-code--imenu-subalist-p returns nil when entries lack string keys."
  (should-not (ai-code--imenu-subalist-p '((1 . 100) (2 . 200)))))

;; Tests for ai-code--imenu-item-position

(ert-deftest ai-code-test-imenu-item-position-with-integer ()
  "Test that ai-code--imenu-item-position extracts integer position."
  (should (= 42 (ai-code--imenu-item-position 42))))

(ert-deftest ai-code-test-imenu-item-position-with-marker ()
  "Test that ai-code--imenu-item-position extracts marker position."
  (with-temp-buffer
    (insert "test content")
    (let ((marker (point-marker)))
      (should (markerp (ai-code--imenu-item-position marker))))))

(ert-deftest ai-code-test-imenu-item-position-with-cons ()
  "Test that ai-code--imenu-item-position extracts position from cons cell."
  (should (= 100 (ai-code--imenu-item-position '(100 . 200)))))

(ert-deftest ai-code-test-imenu-item-position-with-invalid ()
  "Test that ai-code--imenu-item-position returns nil for invalid input."
  (should-not (ai-code--imenu-item-position "invalid"))
  (should-not (ai-code--imenu-item-position nil)))

;; Tests for ai-code--extract-symbol-from-line

(ert-deftest ai-code-test-extract-symbol-from-line-python-def ()
  "Test extracting function name from Python def statement."
  (should (string= "my_function"
                   (ai-code--extract-symbol-from-line "def my_function():"))))

(ert-deftest ai-code-test-extract-symbol-from-line-python-class ()
  "Test extracting class name from Python class statement."
  (should (string= "MyClass"
                   (ai-code--extract-symbol-from-line "class MyClass:"))))

(ert-deftest ai-code-test-extract-symbol-from-line-javascript-function ()
  "Test extracting function name from JavaScript function."
  (should (string= "myFunc"
                   (ai-code--extract-symbol-from-line "function myFunc() {"))))

(ert-deftest ai-code-test-extract-symbol-from-line-async-function ()
  "Test extracting function name from async function."
  (should (string= "asyncFunc"
                   (ai-code--extract-symbol-from-line "async function asyncFunc() {"))))

(ert-deftest ai-code-test-extract-symbol-from-line-with-whitespace ()
  "Test extracting symbol with leading whitespace."
  (should (string= "indent_func"
                   (ai-code--extract-symbol-from-line "    def indent_func():"))))

(ert-deftest ai-code-test-extract-symbol-from-line-no-match ()
  "Test that extraction returns nil when no pattern matches."
  (should-not (ai-code--extract-symbol-from-line "just some text")))

;; Tests for ai-code--imenu-noise-name-p

(ert-deftest ai-code-test-imenu-noise-name-p-with-asterisks ()
  "Test that ai-code--imenu-noise-name-p detects names wrapped in asterisks."
  (should (ai-code--imenu-noise-name-p "*Rescan*"))
  (should (ai-code--imenu-noise-name-p "*Variables*")))

(ert-deftest ai-code-test-imenu-noise-name-p-with-numbers ()
  "Test that ai-code--imenu-noise-name-p detects pure numeric names."
  (should (ai-code--imenu-noise-name-p "123"))
  (should (ai-code--imenu-noise-name-p "42")))

(ert-deftest ai-code-test-imenu-noise-name-p-with-empty ()
  "Test that ai-code--imenu-noise-name-p detects empty or whitespace-only names."
  (should (ai-code--imenu-noise-name-p ""))
  (should (ai-code--imenu-noise-name-p "   ")))

(ert-deftest ai-code-test-imenu-noise-name-p-with-valid-names ()
  "Test that ai-code--imenu-noise-name-p returns nil for valid names."
  (should-not (ai-code--imenu-noise-name-p "myFunction"))
  (should-not (ai-code--imenu-noise-name-p "MyClass"))
  (should-not (ai-code--imenu-noise-name-p "my_func_123")))

(ert-deftest ai-code-test-imenu-noise-name-p-with-non-string ()
  "Test that ai-code--imenu-noise-name-p handles non-string input."
  (should (ai-code--imenu-noise-name-p nil))
  (should (ai-code--imenu-noise-name-p 42)))

;; Tests for ai-code--normalize-imenu-symbol-name

(ert-deftest ai-code-test-normalize-imenu-symbol-name-valid ()
  "Test that ai-code--normalize-imenu-symbol-name returns trimmed valid name."
  (should (string= "myFunc"
                   (ai-code--normalize-imenu-symbol-name "  myFunc  " nil))))

(ert-deftest ai-code-test-normalize-imenu-symbol-name-noise ()
  "Test that ai-code--normalize-imenu-symbol-name uses fallback for noise names."
  (with-temp-buffer
    (insert "def fallback_func():\n")
    (should (string= "fallback_func"
                     (ai-code--normalize-imenu-symbol-name "*Rescan*" (point-min))))))

(ert-deftest ai-code-test-normalize-imenu-symbol-name-empty ()
  "Test that ai-code--normalize-imenu-symbol-name handles empty names."
  (with-temp-buffer
    (insert "def empty_fallback():\n")
    (should (string= "empty_fallback"
                     (ai-code--normalize-imenu-symbol-name "" (point-min))))))

;; Tests for ai-code--flatten-imenu-index

(ert-deftest ai-code-test-flatten-imenu-index-simple ()
  "Test that ai-code--flatten-imenu-index flattens a simple index."
  (let ((index '(("func1" . 100) ("func2" . 200))))
    (let ((result (ai-code--flatten-imenu-index index)))
      (should (member "func1" result))
      (should (member "func2" result)))))

(ert-deftest ai-code-test-flatten-imenu-index-nested ()
  "Test that ai-code--flatten-imenu-index flattens nested sub-alists."
  (let ((index '(("Functions" ("func1" . 100) ("func2" . 200))
                 ("Classes" ("Class1" . 300)))))
    (let ((result (ai-code--flatten-imenu-index index)))
      (should (member "func1" result))
      (should (member "func2" result))
      (should (member "Class1" result)))))

(ert-deftest ai-code-test-flatten-imenu-index-filters-noise ()
  "Test that ai-code--flatten-imenu-index filters out noise names."
  (with-temp-buffer
    (insert "def real_func():\n")
    (let ((index `(("*Rescan*" . ,(point-min))
                   ("real_func" . ,(point-min)))))
      (let ((result (ai-code--flatten-imenu-index index)))
        (should (member "real_func" result))
        (should-not (member "*Rescan*" result))))))

(ert-deftest ai-code-test-flatten-imenu-index-empty ()
  "Test that ai-code--flatten-imenu-index handles empty index."
  (should (null (ai-code--flatten-imenu-index '()))))

;; Tests for ai-code--hash-completion-target-file

(ert-deftest ai-code-test-hash-completion-target-file-valid ()
  "Test that ai-code--hash-completion-target-file returns file path for valid @file#."
  (let* ((git-root (expand-file-name "test-repo/" (file-truename temporary-file-directory)))
         (test-file (expand-file-name "src/test.el" git-root)))
    (unwind-protect
        (progn
          ;; Setup: Create test file
          (make-directory (file-name-directory test-file) t)
          (with-temp-file test-file (insert "content"))
          
          (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
            (with-temp-buffer
              (insert "@src/test.el")
              (should (string= test-file
                               (ai-code--hash-completion-target-file (point)))))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file))
      (when (file-directory-p (file-name-directory test-file))
        (delete-directory (file-name-directory test-file)))
      (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-hash-completion-target-file-no-at ()
  "Test that ai-code--hash-completion-target-file returns nil without @ prefix."
  (let ((git-root (expand-file-name "test-repo/" temporary-file-directory)))
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
      (with-temp-buffer
        (insert "src/test.el")
        (should-not (ai-code--hash-completion-target-file (point)))))))

(ert-deftest ai-code-test-hash-completion-target-file-nonexistent ()
  "Test that ai-code--hash-completion-target-file returns nil for nonexistent file."
  (let ((git-root (expand-file-name "test-repo/" temporary-file-directory)))
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
      (with-temp-buffer
        (insert "@nonexistent/file.el")
        (should-not (ai-code--hash-completion-target-file (point)))))))

(ert-deftest ai-code-test-hash-completion-target-file-outside-repo ()
  "Test that ai-code--hash-completion-target-file returns nil for files outside repo."
  (let* ((git-root (expand-file-name "test-repo/" (file-truename temporary-file-directory)))
         (outside-file (expand-file-name "outside.el" (file-truename temporary-file-directory))))
    (unwind-protect
        (progn
          ;; Setup: Create files
          (make-directory git-root t)
          (with-temp-file outside-file (insert "content"))
          
          (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
            (with-temp-buffer
              (insert (format "@%s" outside-file))
              (should-not (ai-code--hash-completion-target-file (point))))))
      ;; Cleanup
      (when (file-exists-p outside-file) (delete-file outside-file))
      (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-hash-completion-target-file-no-git-repo ()
  "Test that ai-code--hash-completion-target-file returns nil outside git repo."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) nil)))
    (with-temp-buffer
      (insert "@src/test.el")
      (should-not (ai-code--hash-completion-target-file (point))))))

;; Tests for ai-code--file-symbol-candidates

(ert-deftest ai-code-test-file-symbol-candidates-elisp ()
  "Test that ai-code--file-symbol-candidates extracts symbols from Elisp file."
  (let ((test-file (expand-file-name "test-symbols.el" temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Create test file with functions
          (with-temp-file test-file
            (insert "(defun test-func-1 () \"doc\" nil)\n")
            (insert "(defun test-func-2 () \"doc\" nil)\n")
            (insert "(defvar test-var 42)\n"))
          
          (let ((symbols (ai-code--file-symbol-candidates test-file)))
            (should (member "test-func-1" symbols))
            (should (member "test-func-2" symbols))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file)))))

(ert-deftest ai-code-test-file-symbol-candidates-sorted ()
  "Test that ai-code--file-symbol-candidates returns sorted symbols."
  (let ((test-file (expand-file-name "test-sorted.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun zebra () nil)\n")
            (insert "(defun alpha () nil)\n")
            (insert "(defun beta () nil)\n"))
          
          (let ((symbols (ai-code--file-symbol-candidates test-file)))
            (should (equal symbols (sort (copy-sequence symbols) #'string<)))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file)))))

(ert-deftest ai-code-test-file-symbol-candidates-deduped ()
  "Test that ai-code--file-symbol-candidates removes duplicates."
  (let ((test-file (expand-file-name "test-dedup.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun duplicate () nil)\n")
            (insert "(defun duplicate () nil)\n"))
          
          (let ((symbols (ai-code--file-symbol-candidates test-file)))
            (should (= 1 (cl-count "duplicate" symbols :test #'string=)))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file)))))

;; Tests for ai-code--choose-symbol-from-file

(ert-deftest ai-code-test-choose-symbol-from-file-returns-selection ()
  "Test that ai-code--choose-symbol-from-file returns user selection."
  (let ((test-file (expand-file-name "test-choose.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun selected-func () nil)\n"))
          
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt candidates &rest args)
                       "selected-func")))
            (should (string= "selected-func"
                             (ai-code--choose-symbol-from-file test-file)))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file)))))

(ert-deftest ai-code-test-choose-symbol-from-file-no-candidates ()
  "Test that ai-code--choose-symbol-from-file returns nil with no candidates."
  (let ((test-file (expand-file-name "test-empty.txt" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file test-file (insert "no symbols here"))
          (should-not (ai-code--choose-symbol-from-file test-file)))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file)))))

(ert-deftest ai-code-test-choose-symbol-from-file-quit ()
  "Test that ai-code--choose-symbol-from-file handles quit gracefully."
  (let ((test-file (expand-file-name "test-quit.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun some-func () nil)\n"))
          
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt candidates &rest args)
                       (signal 'quit nil))))
            (should-not (ai-code--choose-symbol-from-file test-file))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file)))))

;; Tests for # completion auto-trigger in comments

(ert-deftest ai-code-test-comment-auto-trigger-with-hash ()
  "Test that auto-trigger completes symbols when # is inserted after @file."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (git-root (expand-file-name "test-repo/" (file-truename temporary-file-directory)))
        (test-file (expand-file-name "src/test.el" (expand-file-name "test-repo/" (file-truename temporary-file-directory)))))
    (unwind-protect
        (progn
          ;; Setup: Create test file
          (make-directory (file-name-directory test-file) t)
          (with-temp-file test-file
            (insert "(defun target-symbol () nil)\n"))
          
          (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root))
                    ((symbol-function 'completing-read)
                     (lambda (prompt candidates &rest args)
                       "target-symbol")))
            (with-temp-buffer
              (emacs-lisp-mode)
              (setq buffer-file-name "/tmp/test.el")
              (insert ";; @src/test.el#")
              
              ;; Trigger auto-completion
              (ai-code--comment-auto-trigger-filepath-completion)
              
              ;; Should have inserted the symbol
              (should (string-match-p "target-symbol" (buffer-string))))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file))
      (when (file-directory-p (file-name-directory test-file))
        (delete-directory (file-name-directory test-file)))
      (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-comment-auto-trigger-hash-no-file ()
  "Test that # auto-trigger does nothing without valid @file prefix."
  (let ((ai-code-prompt-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; #")
      
      (let ((original-content (buffer-string)))
        (ai-code--comment-auto-trigger-filepath-completion)
        ;; Content should be unchanged (no completion without @file)
        (should (string= original-content (buffer-string)))))))

(ert-deftest ai-code-test-comment-auto-trigger-hash-disabled ()
  "Test that # auto-trigger doesn't work when feature is disabled."
  (let ((ai-code-prompt-filepath-completion-enabled nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; @src/test.el#")
      
      (let ((original-content (buffer-string)))
        (ai-code--comment-auto-trigger-filepath-completion)
        ;; Should be unchanged when disabled
        (should (string= original-content (buffer-string)))))))

;; Tests for # completion in AI session buffers

(ert-deftest ai-code-test-session-auto-trigger-hash ()
  "Test that # auto-trigger works in AI session buffers."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (git-root (expand-file-name "test-repo/" (file-truename temporary-file-directory)))
        (test-file (expand-file-name "src/test.el" (expand-file-name "test-repo/" (file-truename temporary-file-directory))))
        (terminal-sent nil))
    (unwind-protect
        (progn
          ;; Setup: Create test file
          (make-directory (file-name-directory test-file) t)
          (with-temp-file test-file
            (insert "(defun session-symbol () nil)\n"))
          
          (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root))
                    ((symbol-function 'ai-code-backends-infra--session-buffer-p)
                     (lambda (buf) t))
                    ((symbol-function 'completing-read)
                     (lambda (prompt candidates &rest args)
                       "session-symbol"))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-backspace)
                     (lambda () (setq terminal-sent 'backspace)))
                    ((symbol-function 'ai-code-backends-infra--terminal-send-string)
                     (lambda (str) (setq terminal-sent str))))
            (with-temp-buffer
              (insert "@src/test.el")
              (goto-char (point-max))
              (insert "#")
              
              ;; Trigger auto-completion
              (ai-code--session-auto-trigger-filepath-completion)
              
              ;; Should have sent the symbol to terminal
              (should (string= "#session-symbol" terminal-sent)))))
      ;; Cleanup
      (when (file-exists-p test-file) (delete-file test-file))
      (when (file-directory-p (file-name-directory test-file))
        (delete-directory (file-name-directory test-file)))
      (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-session-auto-trigger-hash-not-session ()
  "Test that # auto-trigger doesn't work in non-session buffers."
  (let ((ai-code-prompt-filepath-completion-enabled t)
        (terminal-sent nil))
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) "/tmp/repo/"))
              ((symbol-function 'ai-code-backends-infra--session-buffer-p)
               (lambda (buf) nil))
              ((symbol-function 'ai-code-backends-infra--terminal-send-string)
               (lambda (str) (setq terminal-sent str))))
      (with-temp-buffer
        (insert "@src/test.el#")
        
        (ai-code--session-auto-trigger-filepath-completion)
        
        ;; Should not have sent anything (not a session buffer)
        (should-not terminal-sent)))))

(ert-deftest ai-code-test-find-project-file-matches-project-basename ()
  "Project file lookup should resolve basename-only links inside the repo."
  (let* ((root (make-temp-file "ai-code-find-project-file-" t))
         (file (expand-file-name "src/Foo.java" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "class Foo {}\n"))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir)
                       'mock-project))
                    ((symbol-function 'project-root)
                     (lambda (_project)
                       root))
                    ((symbol-function 'project-files)
                     (lambda (_project &optional _dirs)
                       '("src/Foo.java"))))
            (should (equal (ai-code--find-project-file "Foo.java")
                           file))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-test-find-project-file-prefers-existing-absolute-local-path ()
  "Absolute existing paths should resolve directly without prompting for repo matches."
  (let* ((project-root (make-temp-file "ai-code-find-project-file-project-" t))
         (project-file (expand-file-name "src/ai-code-behaviors.el" project-root))
         (local-root (make-temp-file "ai-code-find-project-file-local-" t))
         (local-file (expand-file-name "ai-code-behaviors.el" local-root))
         chooser-called)
    (unwind-protect
        (progn
          (make-directory (file-name-directory project-file) t)
          (with-temp-file project-file
            (insert "project copy\n"))
          (with-temp-file local-file
            (insert "local copy\n"))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir)
                       'mock-project))
                    ((symbol-function 'project-root)
                     (lambda (_project)
                       project-root))
                    ((symbol-function 'project-files)
                     (lambda (_project &optional _dirs)
                       '("src/ai-code-behaviors.el")))
                    ((symbol-function 'ai-code--read-session-link-candidate)
                     (lambda (&rest _args)
                       (setq chooser-called t)
                       project-file)))
            (should (equal (ai-code--find-project-file local-file)
                           local-file))
            (should-not chooser-called)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (when (file-directory-p project-root)
        (delete-directory project-root t)))))

(ert-deftest ai-code-test-session-navigate-link-at-point-opens-file-from-session-link-property ()
  "Session link navigation should open files using the clickable link text at point."
  (let (opened-file opened-buffer)
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (file)
                 (setq opened-file file)
                 (setq opened-buffer (get-buffer-create " *ai-code-opened-file*"))
                 (set-buffer opened-buffer)
                 opened-buffer))
              ((symbol-function 'ai-code--find-project-file)
               (lambda (_file)
                 "/tmp/src/Foo.java"))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (with-temp-buffer
        (let ((link-text "src/Foo.java:42:8"))
          (insert (format "See %s for details" link-text))
          (search-backward link-text)
          (let ((start (point))
                (end (+ (point) (length link-text))))
            (add-text-properties start end `(ai-code-session-link ,link-text))
            (goto-char (+ start 5))))
        (ai-code-session-navigate-link-at-point)
        (should (equal opened-file "/tmp/src/Foo.java"))))
    (when (buffer-live-p opened-buffer)
      (kill-buffer opened-buffer))))

(ert-deftest ai-code-test-session-navigate-link-at-point-opens-file-range-link-from-session-link-property ()
  "Session link navigation should open range-style file links using the first line."
  (let (opened-file opened-buffer)
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (file)
                 (setq opened-file file)
                 (setq opened-buffer (get-buffer-create " *ai-code-opened-range-file*"))
                 (set-buffer opened-buffer)
                 opened-buffer))
              ((symbol-function 'ai-code--find-project-file)
               (lambda (_file)
                 "/tmp/ai-code-backends-infra.el"))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (with-temp-buffer
        (let ((link-text "ai-code-backends-infra.el:479-499"))
          (insert (format "See %s for details" link-text))
          (search-backward link-text)
          (let ((start (point))
                (end (+ (point) (length link-text))))
            (add-text-properties start end `(ai-code-session-link ,link-text))
            (goto-char (+ start 5))))
        (ai-code-session-navigate-link-at-point)
        (should (equal opened-file "/tmp/ai-code-backends-infra.el"))))
    (when (buffer-live-p opened-buffer)
      (kill-buffer opened-buffer))))

(ert-deftest ai-code-test-parse-session-link-file-with-line-and-column ()
  "Session link parsing should keep file, line, and column metadata."
  (should (equal (ai-code--parse-session-link "src/Foo.java:42:8")
                 '(:file "src/Foo.java" :line-start 42 :column-start 8))))

(ert-deftest ai-code-test-parse-session-link-file-range ()
  "Session link parsing should keep the first line for range-style file links."
  (should (equal (ai-code--parse-session-link "ai-code-backends-infra.el:479-499")
                 '(:file "ai-code-backends-infra.el" :line-start 479))))

(ert-deftest ai-code-test-session-navigate-link-at-point-opens-url-from-session-link-property ()
  "Session link navigation should open web URLs from clickable link text."
  (let (opened-url)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &optional _new-window)
                 (setq opened-url url)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (with-temp-buffer
        (insert "Visit https://example.com/docs?q=1 now")
        (add-text-properties 7 35 '(ai-code-session-link "https://example.com/docs?q=1"))
        (goto-char 20)
        (ai-code-session-navigate-link-at-point)
        (should (equal opened-url "https://example.com/docs?q=1"))))))

(ert-deftest ai-code-test-speech-to-text-input-inserts-transcription-into-regular-buffer ()
  "Speech-to-text input should insert the transcribed text at point."
  (let ((original-require (symbol-function 'require))
        (whisper-run-calls 0)
        action-choice-called)
    (with-temp-buffer
      (insert "Prompt: ")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'whisper)
                       t
                     (funcall original-require feature filename noerror))))
                ((symbol-function 'read-char)
                 (lambda (&optional _prompt _inherit-input-method _seconds)
                   ?\r))
                ((symbol-function 'sit-for)
                 (lambda (&rest _args) nil))
                ((symbol-function 'message)
                  (lambda (&rest _args) nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (setq action-choice-called t)
                   "Insert to current buffer"))
                ((symbol-function 'whisper-run)
                 (lambda ()
                   (cl-incf whisper-run-calls)
                   (when (= whisper-run-calls 2)
                     (with-current-buffer (get-buffer-create "*whisper-stdout*")
                       (erase-buffer)
                       (insert "transcribed speech")
                       (run-hooks 'whisper-after-transcription-hook))))))
        (ai-code-speech-to-text-input)
        (should action-choice-called)
        (should (= whisper-run-calls 2))
        (should (equal (buffer-string) "Prompt: transcribed speech"))))))

(ert-deftest ai-code-test-speech-to-text-input-send-option-uses-transcription-as-initial-input ()
  "Speech-to-text input should allow editing transcription before sending."
  (let ((original-require (symbol-function 'require))
        (whisper-run-calls 0)
        action-choice-called
        read-string-initial-input
        sent-text)
    (with-temp-buffer
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'whisper)
                       t
                     (funcall original-require feature filename noerror))))
                ((symbol-function 'read-char)
                 (lambda (&optional _prompt _inherit-input-method _seconds)
                   ?\r))
                ((symbol-function 'sit-for)
                 (lambda (&rest _args) nil))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (setq action-choice-called t)
                   "Send to AI coding session"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial-input _candidate-list)
                   (setq read-string-initial-input initial-input)
                   "edited transcription"))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text)
                   (setq sent-text text)))
                ((symbol-function 'whisper-run)
                 (lambda ()
                   (cl-incf whisper-run-calls)
                   (when (= whisper-run-calls 2)
                     (with-current-buffer (get-buffer-create "*whisper-stdout*")
                       (erase-buffer)
                       (insert "transcribed speech")
                       (run-hooks 'whisper-after-transcription-hook))))))
        (ai-code-speech-to-text-input)
        (should action-choice-called)
        (should (= whisper-run-calls 2))
        (should (equal read-string-initial-input "transcribed speech"))
        (should (equal sent-text "edited transcription"))))))

(ert-deftest ai-code-test-speech-to-text-input-send-option-loads-prompt-mode-on-demand ()
  "Speech-to-text send action should load prompt-mode before sending."
  (let ((original-require (symbol-function 'require))
        (original-bound (fboundp 'ai-code--insert-prompt))
        (original-insert-prompt (when (fboundp 'ai-code--insert-prompt)
                                  (symbol-function 'ai-code--insert-prompt)))
        (whisper-run-calls 0)
        prompt-mode-required
        send-result)
    (unwind-protect
        (with-temp-buffer
          (when original-bound
            (fmakunbound 'ai-code--insert-prompt))
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional filename noerror)
                       (cond
                        ((eq feature 'whisper) t)
                        ((eq feature 'ai-code-prompt-mode)
                         (setq prompt-mode-required t)
                         (fset 'ai-code--insert-prompt
                               (lambda (text)
                                 (setq send-result text)))
                         t)
                        (t
                         (funcall original-require feature filename noerror)))))
                    ((symbol-function 'read-char)
                     (lambda (&optional _prompt _inherit-input-method _seconds)
                       ?\r))
                    ((symbol-function 'sit-for)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _args)
                       "Send to AI coding session"))
                    ((symbol-function 'ai-code-read-string)
                     (lambda (_prompt &optional initial-input _candidate-list)
                       (concat initial-input " edited")))
                    ((symbol-function 'whisper-run)
                     (lambda ()
                       (cl-incf whisper-run-calls)
                       (when (= whisper-run-calls 2)
                         (with-current-buffer (get-buffer-create "*whisper-stdout*")
                           (erase-buffer)
                           (insert "transcribed speech")
                           (run-hooks 'whisper-after-transcription-hook))))))
            (let ((result (condition-case err
                              (progn
                                (ai-code-speech-to-text-input)
                                :ok)
                            (error (car err)))))
              (should (eq result :ok))
              (should prompt-mode-required)
              (should (= whisper-run-calls 2))
              (should (equal send-result "transcribed speech edited")))))
      (if original-bound
          (fset 'ai-code--insert-prompt original-insert-prompt)
        (fmakunbound 'ai-code--insert-prompt)))))

(ert-deftest ai-code-test-speech-to-text-docstrings-describe-available-actions ()
  "Speech-to-text docstrings should describe the available actions."
  (let ((apply-doc (documentation 'ai-code--speech-to-text-apply-transcription))
        (input-doc (documentation 'ai-code-speech-to-text-input)))
    (should (string-match-p "apply a chosen speech action" apply-doc))
    (should (string-match-p "send to an AI coding session" apply-doc))
    (should (string-match-p "choose how to use the transcription" input-doc))
    (should (string-match-p "copy it to the clipboard" input-doc))))

(ert-deftest ai-code-test-speech-to-text-input-copy-option-copies-transcription ()
  "Speech-to-text input should copy the transcription when requested."
  (let ((original-require (symbol-function 'require))
        (whisper-run-calls 0)
        action-choice-called
        copied-text)
    (with-temp-buffer
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'whisper)
                       t
                     (funcall original-require feature filename noerror))))
                ((symbol-function 'read-char)
                 (lambda (&optional _prompt _inherit-input-method _seconds)
                   ?\r))
                ((symbol-function 'sit-for)
                 (lambda (&rest _args) nil))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (setq action-choice-called t)
                   "Copy to clipboard"))
                ((symbol-function 'kill-new)
                 (lambda (text &optional _replace)
                   (setq copied-text text)))
                ((symbol-function 'whisper-run)
                 (lambda ()
                   (cl-incf whisper-run-calls)
                   (when (= whisper-run-calls 2)
                     (with-current-buffer (get-buffer-create "*whisper-stdout*")
                       (erase-buffer)
                       (insert "transcribed speech")
                       (run-hooks 'whisper-after-transcription-hook))))))
        (ai-code-speech-to-text-input)
        (should action-choice-called)
        (should (= whisper-run-calls 2))
        (should (equal copied-text "transcribed speech"))))))

(provide 'test_ai-code-input)
;;; test_ai-code-input.el ends here
