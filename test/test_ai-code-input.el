;;; test_ai-code-input.el --- Tests for ai-code-input.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-input.el, focusing on comment filepath completion features.

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
  (cl-letf (((symbol-function 'ai-code-backends-infra--session-buffer-p)
             (lambda (buf)
               (string-prefix-p "*ai-session*" (buffer-name buf)))))
    ;; Create a mock AI session buffer
    (let ((session-buf (get-buffer-create "*ai-session-test*")))
      (unwind-protect
          (should (ai-code--any-ai-session-active-p))
        (when (buffer-live-p session-buf)
          (kill-buffer session-buf))))))

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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
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

(ert-deftest ai-code-test-comment-filepath-capf-no-ai-session ()
  "Test that ai-code--comment-filepath-capf returns nil when no AI session is active."
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; Check @")
      (goto-char (point-max))
      
      ;; Mock dependencies - no AI session active
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () nil))
                ((symbol-function 'magit-toplevel)
                 (lambda (&optional dir) "/tmp/")))
        
        ;; Should return nil because no AI session is active
        (should-not (ai-code--comment-filepath-capf))))))

(ert-deftest ai-code-test-comment-filepath-capf-disabled ()
  "Test that ai-code--comment-filepath-capf returns nil when disabled."
  (let ((ai-code-prompt-comment-filepath-completion-enabled nil))
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t))
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

;;; Tests for ai-code-prompt-comment-filepath-completion-mode

(ert-deftest ai-code-test-comment-completion-mode-enable ()
  "Test that enabling the mode sets up hooks and variable correctly."
  (let ((ai-code-prompt-comment-filepath-completion-mode nil))
    (unwind-protect
        (progn
          ;; Enable the mode
          (ai-code-prompt-comment-filepath-completion-mode 1)
          
          ;; Check that the variable is set
          (should ai-code-prompt-comment-filepath-completion-enabled)
          (should ai-code-prompt-comment-filepath-completion-mode)
          
          ;; Check that hooks are added
          (should (memq 'ai-code--comment-auto-trigger-filepath-completion
                        post-self-insert-hook))
          (should (memq 'ai-code--comment-filepath-setup
                        after-change-major-mode-hook)))
      
      ;; Cleanup: disable the mode
      (ai-code-prompt-comment-filepath-completion-mode -1))))

(ert-deftest ai-code-test-comment-completion-mode-disable ()
  "Test that disabling the mode removes hooks and variable correctly."
  (let ((ai-code-prompt-comment-filepath-completion-mode nil))
    (unwind-protect
        (progn
          ;; Enable then disable the mode
          (ai-code-prompt-comment-filepath-completion-mode 1)
          (ai-code-prompt-comment-filepath-completion-mode -1)
          
          ;; Check that the variable is unset
          (should-not ai-code-prompt-comment-filepath-completion-enabled)
          (should-not ai-code-prompt-comment-filepath-completion-mode)
          
          ;; Check that hooks are removed
          (should-not (memq 'ai-code--comment-auto-trigger-filepath-completion
                            post-self-insert-hook))
          (should-not (memq 'ai-code--comment-filepath-setup
                            after-change-major-mode-hook)))
      
      ;; Cleanup
      (ai-code-prompt-comment-filepath-completion-mode -1))))

(ert-deftest ai-code-test-comment-completion-mode-setup-in-buffers ()
  "Test that enabling mode sets up completion in existing buffers."
  (let ((ai-code-prompt-comment-filepath-completion-mode nil)
        (test-buf (get-buffer-create "*test-comment-completion*")))
    (unwind-protect
        (progn
          ;; Create a test buffer
          (with-current-buffer test-buf
            ;; Clear any existing completion functions
            (setq-local completion-at-point-functions nil))
          
          ;; Enable the mode
          (ai-code-prompt-comment-filepath-completion-mode 1)
          
          ;; Check that completion function was added to the buffer
          (with-current-buffer test-buf
            (should (memq 'ai-code--comment-filepath-capf
                          completion-at-point-functions))))
      
      ;; Cleanup
      (ai-code-prompt-comment-filepath-completion-mode -1)
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest ai-code-test-comment-completion-mode-cleanup-in-buffers ()
  "Test that disabling mode cleans up completion in all buffers."
  (let ((ai-code-prompt-comment-filepath-completion-mode nil)
        (test-buf (get-buffer-create "*test-comment-cleanup*")))
    (unwind-protect
        (progn
          ;; Enable the mode first
          (ai-code-prompt-comment-filepath-completion-mode 1)
          
          ;; Verify setup in buffer
          (with-current-buffer test-buf
            (should (memq 'ai-code--comment-filepath-capf
                          completion-at-point-functions)))
          
          ;; Disable the mode
          (ai-code-prompt-comment-filepath-completion-mode -1)
          
          ;; Check that completion function was removed from the buffer
          (with-current-buffer test-buf
            (should-not (memq 'ai-code--comment-filepath-capf
                              completion-at-point-functions))))
      
      ;; Cleanup
      (ai-code-prompt-comment-filepath-completion-mode -1)
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest ai-code-test-comment-completion-mode-toggle ()
  "Test that toggling mode works correctly."
  (let ((ai-code-prompt-comment-filepath-completion-mode nil))
    (unwind-protect
        (progn
          ;; First toggle should enable
          (ai-code-prompt-comment-filepath-completion-mode)
          (should ai-code-prompt-comment-filepath-completion-mode)
          
          ;; Second toggle should disable
          (ai-code-prompt-comment-filepath-completion-mode)
          (should-not ai-code-prompt-comment-filepath-completion-mode))
      
      ;; Cleanup
      (ai-code-prompt-comment-filepath-completion-mode -1))))

(ert-deftest ai-code-test-comment-completion-mode-after-major-mode-change ()
  "Test that completion setup works after major mode change."
  (let ((ai-code-prompt-comment-filepath-completion-mode nil)
        (test-buf (get-buffer-create "*test-major-mode-change*")))
    (unwind-protect
        (progn
          ;; Enable the mode
          (ai-code-prompt-comment-filepath-completion-mode 1)
          
          ;; Simulate major mode change in buffer
          (with-current-buffer test-buf
            (setq-local completion-at-point-functions nil)
            (run-hooks 'after-change-major-mode-hook)
            
            ;; Check that completion function was added
            (should (memq 'ai-code--comment-filepath-capf
                          completion-at-point-functions))))
      
      ;; Cleanup
      (ai-code-prompt-comment-filepath-completion-mode -1)
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

;;; Tests for ai-code--comment-auto-trigger-filepath-completion

(ert-deftest ai-code-test-comment-auto-trigger-with-at ()
  "Test that auto-trigger works when @ is inserted in a comment."
  (let ((ai-code-prompt-comment-filepath-completion-enabled t)
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
        
        ;; Should have called completion-at-point
        (should completion-called)))))

(ert-deftest ai-code-test-comment-auto-trigger-outside-comment ()
  "Test that auto-trigger doesn't work outside a comment."
  (let ((ai-code-prompt-comment-filepath-completion-enabled t)
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
  "Test that auto-trigger doesn't work when no AI session is active."
  (let ((ai-code-prompt-comment-filepath-completion-enabled t)
        (completion-called nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq buffer-file-name "/tmp/test.el")
      (insert ";; ")
      
      ;; Mock dependencies - no AI session
      (cl-letf (((symbol-function 'ai-code--any-ai-session-active-p)
                 (lambda () nil))
                ((symbol-function 'completion-at-point)
                 (lambda () (setq completion-called t))))
        
        ;; Insert @ and trigger auto-completion
        (insert "@")
        (ai-code--comment-auto-trigger-filepath-completion)
        
        ;; Should NOT have called completion-at-point (no AI session)
        (should-not completion-called)))))

(ert-deftest ai-code-test-comment-auto-trigger-disabled ()
  "Test that auto-trigger doesn't work when feature is disabled."
  (let ((ai-code-prompt-comment-filepath-completion-enabled nil)
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t)
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
  (let ((ai-code-prompt-comment-filepath-completion-enabled t)
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

(provide 'test_ai-code-input)
;;; test_ai-code-input.el ends here
