;;; test_ai-code-input.el --- Tests for ai-code-input.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-input.el, focusing on filepath completion features.

;;; Code:

(require 'ert)
(require 'ai-code-input)
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

;;; Tests for ai-code--parse-session-link

(ert-deftest ai-code-test-parse-session-link-file-only ()
  "Test parsing a plain filename with extension."
  (let ((result (ai-code--parse-session-link "FileABC.java")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should-not (plist-get result :line-start))
    (should-not (plist-get result :line-end))
    (should-not (plist-get result :symbol))))

(ert-deftest ai-code-test-parse-session-link-file-with-path ()
  "Test parsing a filename with a directory path."
  (let ((result (ai-code--parse-session-link "src/main/Foo.java")))
    (should result)
    (should (equal (plist-get result :file) "src/main/Foo.java"))
    (should-not (plist-get result :line-start))))

(ert-deftest ai-code-test-parse-session-link-file-colon-line ()
  "Test parsing filename:line format."
  (let ((result (ai-code--parse-session-link "FileABC.java:42")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should (= (plist-get result :line-start) 42))
    (should-not (plist-get result :line-end))))

(ert-deftest ai-code-test-parse-session-link-file-colon-line-column ()
  "Test parsing filename:line:column format."
  (let ((result (ai-code--parse-session-link "FileABC.java:42:8")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should (= (plist-get result :line-start) 42))
    (should (= (plist-get result :column-start) 8))
    (should-not (plist-get result :line-end))))

(ert-deftest ai-code-test-parse-session-link-file-github-line ()
  "Test parsing filename:L42 (GitHub single line) format."
  (let ((result (ai-code--parse-session-link "FileABC.java:L42")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should (= (plist-get result :line-start) 42))
    (should-not (plist-get result :line-end))))

(ert-deftest ai-code-test-parse-session-link-file-github-range ()
  "Test parsing filename:L42-60 (GitHub line range) format."
  (let ((result (ai-code--parse-session-link "FileABC.java:L42-60")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should (= (plist-get result :line-start) 42))
    (should (= (plist-get result :line-end) 60))))

(ert-deftest ai-code-test-parse-session-link-file-github-anchor-range ()
  "Test parsing filename#L42-L60 anchor format."
  (let ((result (ai-code--parse-session-link "FileABC.java#L42-L60")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should (= (plist-get result :line-start) 42))
    (should (= (plist-get result :line-end) 60))))

(ert-deftest ai-code-test-parse-session-link-file-editor-line-column ()
  "Test parsing filename(42,8) format."
  (let ((result (ai-code--parse-session-link "FileABC.java(42,8)")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should (= (plist-get result :line-start) 42))
    (should (= (plist-get result :column-start) 8))
    (should-not (plist-get result :line-end))))

(ert-deftest ai-code-test-parse-session-link-file-editor-line ()
  "Test parsing filename(42) format."
  (let ((result (ai-code--parse-session-link "FileABC.java(42)")))
    (should result)
    (should (equal (plist-get result :file) "FileABC.java"))
    (should (= (plist-get result :line-start) 42))
    (should-not (plist-get result :column-start))
    (should-not (plist-get result :line-end))))

(ert-deftest ai-code-test-parse-session-link-symbol ()
  "Test parsing a plain symbol (no dots, slashes, or colons)."
  (let ((result (ai-code--parse-session-link "myFunction")))
    (should result)
    (should (equal (plist-get result :symbol) "myFunction"))
    (should-not (plist-get result :file))))

(ert-deftest ai-code-test-parse-session-link-symbol-class ()
  "Test parsing a class name symbol."
  (let ((result (ai-code--parse-session-link "MyClass")))
    (should result)
    (should (equal (plist-get result :symbol) "MyClass"))))

(ert-deftest ai-code-test-parse-session-link-symbol-elisp ()
  "Test parsing a hyphenated Emacs Lisp symbol."
  (let ((result (ai-code--parse-session-link "ai-code--find-project-file")))
    (should result)
    (should (equal (plist-get result :symbol) "ai-code--find-project-file"))))

(ert-deftest ai-code-test-parse-session-link-nil-returns-nil ()
  "Test that nil input returns nil."
  (should-not (ai-code--parse-session-link nil)))

(ert-deftest ai-code-test-parse-session-link-empty-returns-nil ()
  "Test that empty string input returns nil."
  (should-not (ai-code--parse-session-link "")))

(ert-deftest ai-code-test-parse-session-link-path-with-line ()
  "Test parsing a full path with line number."
  (let ((result (ai-code--parse-session-link "src/main/java/Example.java:100")))
    (should result)
    (should (equal (plist-get result :file) "src/main/java/Example.java"))
    (should (= (plist-get result :line-start) 100))
    (should-not (plist-get result :line-end))))

;;; Tests for ai-code--session-link-text-at-point

(ert-deftest ai-code-test-session-link-text-at-point-filename ()
  "Test extracting a plain filename at point."
  (with-temp-buffer
    (insert "See FileABC.java for details")
    (goto-char (point-min))
    (search-forward "FileABC")
    (should (equal (ai-code--session-link-text-at-point) "FileABC.java"))))

(ert-deftest ai-code-test-session-link-text-at-point-with-line ()
  "Test extracting filename:line at point."
  (with-temp-buffer
    (insert "Modified FileABC.java:42 today")
    (goto-char (point-min))
    (search-forward "FileABC")
    (should (equal (ai-code--session-link-text-at-point) "FileABC.java:42"))))

(ert-deftest ai-code-test-session-link-text-at-point-github-range ()
  "Test extracting filename:Lstart-end at point."
  (with-temp-buffer
    (insert "Check FileABC.java:L10-20 please")
    (goto-char (point-min))
    (search-forward "FileABC")
    (should (equal (ai-code--session-link-text-at-point) "FileABC.java:L10-20"))))

(ert-deftest ai-code-test-session-link-text-at-point-line-column ()
  "Test extracting filename:line:column at point."
  (with-temp-buffer
    (insert "Inspect FileABC.java:42:8 next")
    (goto-char (point-min))
    (search-forward "42")
    (should (equal (ai-code--session-link-text-at-point) "FileABC.java:42:8"))))

(ert-deftest ai-code-test-session-link-text-at-point-github-anchor-range ()
  "Test extracting filename#Lstart-end at point."
  (with-temp-buffer
    (insert "Review FileABC.java#L10-L20 soon")
    (goto-char (point-min))
    (search-forward "L10")
    (should (equal (ai-code--session-link-text-at-point) "FileABC.java#L10-L20"))))

(ert-deftest ai-code-test-session-link-text-at-point-symbol ()
  "Test extracting a symbol name at point."
  (with-temp-buffer
    (insert "The function myFunction handles this")
    (goto-char (point-min))
    (search-forward "myFunction")
    (backward-char 3)
    (should (equal (ai-code--session-link-text-at-point) "myFunction"))))

(ert-deftest ai-code-test-session-link-text-at-point-symbol-elisp ()
  "Test extracting a hyphenated Emacs Lisp symbol at point."
  (with-temp-buffer
    (insert "Try ai-code--find-project-file in this repo")
    (goto-char (point-min))
    (search-forward "find-project")
    (should (equal (ai-code--session-link-text-at-point)
                   "ai-code--find-project-file"))))

;;; Tests for ai-code--find-project-file

(ert-deftest ai-code-test-find-project-file-absolute-path ()
  "Test that an absolute path is returned as-is when the file exists."
  (let* ((tmpfile (make-temp-file "ai-code-test-" nil ".el")))
    (unwind-protect
        (should (equal (ai-code--find-project-file tmpfile) tmpfile))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest ai-code-test-find-project-file-absolute-nonexistent ()
  "Test that nil is returned for a non-existent absolute path."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _dir) nil)))
    (should-not (ai-code--find-project-file "/nonexistent/path/file.java"))))

(ert-deftest ai-code-test-find-project-file-relative-to-git-root ()
  "Test finding a file relative to the git root."
  (let* ((tmpdir (make-temp-file "ai-code-proj-" t))
         (subfile (expand-file-name "src/Foo.java" tmpdir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory subfile) t)
          (with-temp-file subfile (insert "// test"))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir) nil))
                    ((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) tmpdir)))
            (should (equal (ai-code--find-project-file "src/Foo.java") subfile))))
      (delete-directory tmpdir t))))

(ert-deftest ai-code-test-find-project-file-nil-returns-nil ()
  "Test that nil filename returns nil."
  (should-not (ai-code--find-project-file nil)))

(ert-deftest ai-code-test-find-project-file-strips-at-prefix ()
  "Test finding a file when the session link starts with @."
  (let* ((tmpdir (make-temp-file "ai-code-proj-at-" t))
         (subfile (expand-file-name "src/Foo.java" tmpdir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory subfile) t)
          (with-temp-file subfile (insert "// test"))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir) 'mock-project))
                    ((symbol-function 'project-root)
                     (lambda (_project) tmpdir))
                    ((symbol-function 'project-files)
                     (lambda (_project &optional _dirs) (list "src/Foo.java"))))
            (should (equal (ai-code--find-project-file "@src/Foo.java") subfile))))
      (delete-directory tmpdir t))))

(ert-deftest ai-code-test-session-link-refresh-region-adds-file-properties ()
  "File links should become mouse-clickable after refresh."
  (cl-letf (((symbol-function 'ai-code--project-file-candidates)
             (lambda (_filename) '("/tmp/src/Foo.java"))))
    (with-temp-buffer
      (insert "See src/Foo.java:42 for details")
      (ai-code--session-link-refresh-region (point-min) (point-max))
      (search-backward "src/Foo.java:42")
      (let ((link-start (point))
            (link-end (+ (point) (1- (length "src/Foo.java:42")))))
        (should (equal (get-text-property link-start 'ai-code-session-link)
                       "src/Foo.java:42"))
        (should (equal (get-text-property link-end 'ai-code-session-link)
                       "src/Foo.java:42"))
        (should (eq (get-text-property link-start 'mouse-face) 'highlight))
        (should (eq (get-text-property link-start 'font-lock-face) 'link))
        (should (get-text-property link-start 'keymap))))))

(ert-deftest ai-code-test-session-link-refresh-region-adds-github-anchor-properties ()
  "GitHub #L anchors should become mouse-clickable."
  (cl-letf (((symbol-function 'ai-code--project-file-candidates)
             (lambda (_filename) '("/tmp/src/Foo.java"))))
    (with-temp-buffer
      (insert "Check src/Foo.java#L42-L60 for context")
      (ai-code--session-link-refresh-region (point-min) (point-max))
      (search-backward "src/Foo.java#L42-L60")
      (should (equal (get-text-property (point) 'ai-code-session-link)
                     "src/Foo.java#L42-L60")))))

(ert-deftest ai-code-test-session-link-refresh-region-adds-symbol-properties ()
  "Resolvable symbols should become mouse-clickable."
  (cl-letf (((symbol-function 'ai-code--project-symbol-exists-p)
             (lambda (symbol)
               (member symbol '("MyClass" "myFunction" "ai-code--find-project-file")))))
    (with-temp-buffer
      (insert "Use MyClass and myFunction and ai-code--find-project-file here")
      (ai-code--session-link-refresh-region (point-min) (point-max))
      (search-backward "MyClass")
      (should (equal (get-text-property (point) 'ai-code-session-link)
                     "MyClass"))
      (search-forward "myFunction")
      (backward-char (length "myFunction"))
      (should (equal (get-text-property (point) 'ai-code-session-link)
                     "myFunction"))
      (search-forward "ai-code--find-project-file")
      (backward-char (length "ai-code--find-project-file"))
      (should (equal (get-text-property (point) 'ai-code-session-link)
                     "ai-code--find-project-file")))))

(ert-deftest ai-code-test-session-link-refresh-region-skips-unresolvable-file ()
  "Unresolvable file links should not become clickable."
  (cl-letf (((symbol-function 'ai-code--project-file-candidates)
             (lambda (_filename) nil)))
    (with-temp-buffer
      (insert "See missing/Foo.java:42 for details")
      (ai-code--session-link-refresh-region (point-min) (point-max))
      (search-backward "missing/Foo.java:42")
      (should-not (get-text-property (point) 'ai-code-session-link))
      (should-not (get-text-property (point) 'font-lock-face)))))

(ert-deftest ai-code-test-session-link-refresh-region-skips-unresolvable-symbol ()
  "Unresolvable symbols should not become clickable."
  (cl-letf (((symbol-function 'ai-code--project-symbol-exists-p)
             (lambda (_symbol) nil)))
    (with-temp-buffer
      (insert "Use imaginarySymbolName here")
      (ai-code--session-link-refresh-region (point-min) (point-max))
      (search-backward "imaginarySymbolName")
      (should-not (get-text-property (point) 'ai-code-session-link))
      (should-not (get-text-property (point) 'font-lock-face)))))

(ert-deftest ai-code-test-session-link-refresh-region-skips-plain-words ()
  "Plain prose should not become clickable session links."
  (with-temp-buffer
    (insert "open the file and check this output")
    (ai-code--session-link-refresh-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "output")
    (backward-char (length "output"))
    (should-not (get-text-property (point) 'ai-code-session-link))))

;;; Tests for ai-code-session-navigate-link-at-point

(ert-deftest ai-code-test-session-navigate-link-opens-file ()
  "Test that navigating to a file link opens it in another window."
  (let* ((tmpfile (make-temp-file "ai-code-nav-" nil ".el"))
         (opened-file nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile (insert ";; test\n"))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (f) (setq opened-file f)))
                    ((symbol-function 'ai-code--find-project-file)
                     (lambda (f) tmpfile)))
            (with-temp-buffer
              (insert (concat "See " (file-name-nondirectory tmpfile)))
              (goto-char (point-min))
              (search-forward (file-name-sans-extension
                               (file-name-nondirectory tmpfile)))
               (ai-code-session-navigate-link-at-point)
               (should (equal opened-file tmpfile)))))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest ai-code-test-session-navigate-link-opens-file-with-column ()
  "Test that navigating file:line:column moves to the right column."
  (let ((opened-file nil)
        (forward-line-calls nil)
        (move-to-column-calls nil))
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (f) (setq opened-file f)))
              ((symbol-function 'ai-code--find-project-file)
               (lambda (_f) "/tmp/Foo.java"))
              ((symbol-function 'forward-line)
               (lambda (n) (push n forward-line-calls)))
              ((symbol-function 'move-to-column)
               (lambda (n &optional _force) (push n move-to-column-calls))))
      (with-temp-buffer
        (insert "See src/Foo.java:42:8")
        (goto-char (point-min))
        (search-forward "42")
        (ai-code-session-navigate-link-at-point)
        (should (equal opened-file "/tmp/Foo.java"))
        (should (equal forward-line-calls '(41)))
        (should (equal move-to-column-calls '(7)))))))

(ert-deftest ai-code-test-session-navigate-link-no-link ()
  "Test that navigating when no link is at point shows a message."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (insert "   ")
        (goto-char (point-min))
        (ai-code-session-navigate-link-at-point)
        (should (cl-some (lambda (m) (string-match-p "No code link" m)) messages))))))

(ert-deftest ai-code-test-session-navigate-link-symbol-prefers-helm-gtags ()
  "Symbol navigation should use Helm-Gtags when available."
  (let ((called nil))
    (cl-letf (((symbol-function 'helm-gtags-dwim)
               (lambda () (setq called 'helm-gtags)))
              ((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt _dir) nil))
              ((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) default-directory))
              ((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (if (eq feature 'helm-gtags) t (require feature nil t)))))
      (with-temp-buffer
        (insert "MyClass")
        (goto-char (point-min))
        (ai-code-session-navigate-link-at-point)
        (should (eq called 'helm-gtags))))))

(ert-deftest ai-code-test-session-navigate-link-at-mouse ()
  "Mouse navigation should move point to the clicked link and navigate."
  (let ((called nil))
    (cl-letf (((symbol-function 'select-window) (lambda (_window) nil))
              ((symbol-function 'ai-code-session-navigate-link-at-point)
               (lambda () (setq called (point)))))
      (with-temp-buffer
        (insert "See Foo.java")
        (goto-char (point-min))
        (let ((target (+ (point-min) 5))
              (window (selected-window)))
          (ai-code-session-navigate-link-at-mouse
           (list 'mouse-1 (list window target '(0 . 0) 0)))
          (should (= called target)))))))

(provide 'test_ai-code-input)
;;; test_ai-code-input.el ends here
