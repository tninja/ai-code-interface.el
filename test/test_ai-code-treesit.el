;;; test_ai-code-treesit.el --- Tests for Tree-sitter semantic scope and context -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for semantic scope discovery backed by Tree-sitter.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-utils)

(ert-deftest test-ai-code-treesit--available-p-nil-without-parser ()
  "Test `ai-code--treesit-available-p' returns nil when buffer has no treesit parser."
  (with-temp-buffer
    (should-not (ai-code--treesit-available-p))))

(ert-deftest test-ai-code-treesit--current-function-name-fallback ()
  "Test `ai-code--current-function-name' falls back to `which-function' when treesit unavailable."
  (with-temp-buffer
    (cl-letf (((symbol-function 'ai-code--treesit-available-p) (lambda (&optional _) nil))
              ((symbol-function 'which-function) (lambda () "fallback-func")))
      (should (equal (ai-code--current-function-name) "fallback-func")))))

(ert-deftest test-ai-code-treesit--current-function-name-treesit ()
  "Test `ai-code--current-function-name' uses treesit defun name when parser is available."
  (with-temp-buffer
    (let ((mock-node (list 'mock-node)))
      (cl-letf (((symbol-function 'ai-code--treesit-available-p) (lambda (&optional _) t))
                ((symbol-function 'ai-code--treesit-defun-at-point) (lambda (&optional _) mock-node))
                ((symbol-function 'ai-code--treesit-node-name) (lambda (_node) "ts-detected-method"))
                ((symbol-function 'which-function) (lambda () "wrong-which-func")))
        (should (equal (ai-code--current-function-name) "ts-detected-method"))))))

(ert-deftest test-ai-code-treesit--current-function-name-treesit-fallback-on-nil-name ()
  "Test `ai-code--current-function-name' falls back to which-function if treesit finds no node."
  (with-temp-buffer
    (cl-letf (((symbol-function 'ai-code--treesit-available-p) (lambda (&optional _) t))
              ((symbol-function 'ai-code--treesit-defun-at-point) (lambda (&optional _) nil))
              ((symbol-function 'which-function) (lambda () "fallback-when-no-node")))
      (should (equal (ai-code--current-function-name) "fallback-when-no-node")))))

(ert-deftest test-ai-code-treesit--enclosing-class-skeleton ()
  "Test `ai-code--current-scope-context' extracts class context when available."
  (with-temp-buffer
    (let ((mock-defun (list 'defun-node))
          (mock-class (list 'class-node)))
      (cl-letf (((symbol-function 'ai-code--treesit-available-p) (lambda (&optional _) t))
                ((symbol-function 'ai-code--treesit-defun-at-point) (lambda (&optional _) mock-defun))
                ((symbol-function 'ai-code--treesit-enclosing-class-node) (lambda (_node) mock-class))
                ((symbol-function 'ai-code--treesit-node-name) (lambda (n) (if (eq n mock-class) "UserService" "find_user")))
                ((symbol-function 'ai-code--treesit-node-header) (lambda (n) (if (eq n mock-class) "class UserService(BaseService):" "def find_user(self, user_id: int) -> User:"))))
        (let ((scope (ai-code--current-scope-context)))
          (should (equal (plist-get scope :function-name) "find_user"))
          (should (equal (plist-get scope :class-name) "UserService"))
          (should (equal (plist-get scope :class-header) "class UserService(BaseService):"))
          (should (equal (plist-get scope :function-header)
                         "def find_user(self, user_id: int) -> User:")))))))

(ert-deftest test-ai-code-treesit--enclosing-type-node-types ()
  "Recognize concrete grammar container nodes without treating roots as classes."
  (dolist (node-type '("class_definition" "class_declaration"
                       "struct_item" "impl_item" "interface_declaration"
                       "trait_item" "object_declaration"))
    (should (ai-code--treesit-enclosing-type-node-p node-type)))
  (should-not (ai-code--treesit-enclosing-type-node-p "module"))
  (should-not (ai-code--treesit-enclosing-type-node-p "module_root")))

(ert-deftest test-ai-code-treesit--python-scope-uses-real-ast-boundaries ()
  "Extract Python class and function headers without including their bodies."
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)
                    (fboundp 'python-ts-mode)))
  (with-temp-buffer
    (insert "class UserService(\n"
            "        BaseService,\n"
            "        AuditMixin):\n"
            "    \"\"\"Service docs must not be part of the header.\"\"\"\n"
            "    enabled = True\n\n"
            "    def find_user(\n"
            "            self,\n"
            "            user_id: int) -> str:\n"
            "        return str(user_id)\n")
    (python-ts-mode)
    (goto-char (point-min))
    (search-forward "user_id)")
    (backward-char 1)
    (let* ((scope (ai-code--current-scope-context))
           (formatted (ai-code--format-scope-context scope)))
      (should (equal (plist-get scope :function-name) "find_user"))
      (should (equal (plist-get scope :class-name) "UserService"))
      (should (equal (plist-get scope :class-header)
                     (concat "class UserService(\n"
                             "        BaseService,\n"
                             "        AuditMixin):")))
      (should (equal (plist-get scope :function-header)
                     (concat "def find_user(\n"
                             "            self,\n"
                             "            user_id: int) -> str:")))
      (let ((range (plist-get scope :range)))
        (should (equal (list (line-number-at-pos (car range))
                             (line-number-at-pos (cdr range)))
                       '(7 10))))
      (should (string-match-p "Enclosing class: UserService" formatted))
      (should (string-match-p "Function definition: def find_user" formatted))
      (should (string-match-p "Function range: lines 7-10" formatted))
      (should-not (string-match-p "Service docs" formatted))
      (should-not (string-match-p "return str" formatted)))))

(ert-deftest test-ai-code-treesit--python-whole-function-region-uses-function-scope ()
  "Resolve a whole-function region without treating its class as a function."
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)
                    (fboundp 'python-ts-mode)))
  (with-temp-buffer
    (insert "class Service:\n"
            "    def first(self):\n"
            "        value = 1\n"
            "        return value\n\n"
            "    def second(self):\n"
            "        return 2\n")
    (python-ts-mode)
    (goto-char (point-min))
    (search-forward "return value")
    (backward-char 1)
    (let* ((function-node (ai-code--treesit-defun-at-point))
           (start (treesit-node-start function-node))
           (end (treesit-node-end function-node))
           (region-scope (ai-code--scope-context-for-region start end))
           (scope-at-exclusive-end (ai-code--current-scope-context end)))
      (should (equal (plist-get region-scope :function-name) "first"))
      (should (equal (plist-get region-scope :class-name) "Service"))
      (should (equal (plist-get region-scope :function-header)
                     "def first(self):"))
      (should-not (plist-get scope-at-exclusive-end :function-name))
      (should (equal (plist-get scope-at-exclusive-end :class-name)
                     "Service")))))

(ert-deftest test-ai-code-treesit--python-cross-function-region-keeps-class-only ()
  "Omit single-function metadata when a region spans sibling methods."
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)
                    (fboundp 'python-ts-mode)))
  (with-temp-buffer
    (insert "class Service:\n"
            "    def first(self):\n"
            "        return 1\n\n"
            "    def second(self):\n"
            "        return 2\n")
    (python-ts-mode)
    (goto-char (point-min))
    (search-forward "return 1")
    (backward-char 1)
    (let ((start (treesit-node-start (ai-code--treesit-defun-at-point))))
      (search-forward "return 2")
      (backward-char 1)
      (let* ((end (treesit-node-end (ai-code--treesit-defun-at-point)))
             (scope (ai-code--scope-context-for-region start end)))
        (should-not (plist-get scope :function-name))
        (should-not (plist-get scope :function-header))
        (should-not (plist-get scope :range))
        (should (equal (plist-get scope :class-name) "Service"))
        (should (equal (plist-get scope :class-header)
                       "class Service:"))))))

(ert-deftest test-ai-code-treesit--region-scope-empty-for-whitespace ()
  "Return no semantic metadata for a whitespace-only region."
  (with-temp-buffer
    (insert "  \n\t")
    (cl-letf (((symbol-function 'ai-code--current-scope-context)
               (lambda (&optional _)
                 (ert-fail "Whitespace must not trigger scope lookup"))))
      (should
       (string-empty-p
        (ai-code--format-scope-context
         (ai-code--scope-context-for-region
          (point-min) (point-max))))))))

(ert-deftest test-ai-code-treesit--region-scope-fallback-compares-function-names ()
  "Use matching fallback function names when Tree-sitter ranges are absent."
  (with-temp-buffer
    (insert "a b")
    (cl-letf (((symbol-function 'ai-code--current-scope-context)
               (lambda (&optional _)
                 (list :function-name "fallback-function"
                       :class-name nil
                       :class-header nil
                       :class-range nil
                       :function-header nil
                       :range nil))))
      (let ((scope (ai-code--scope-context-for-region
                    (point-min) (point-max))))
        (should (equal (plist-get scope :function-name)
                       "fallback-function"))))))

(ert-deftest test-ai-code-treesit--region-scope-empty-across-different-classes ()
  "Return no semantic metadata when region endpoints belong to different classes."
  (with-temp-buffer
    (insert "a b")
    (cl-letf (((symbol-function 'ai-code--current-scope-context)
               (lambda (&optional pos)
                 (if (= pos (point-min))
                     (list :function-name "first"
                           :class-name "FirstClass"
                           :class-range '(1 . 1)
                           :range '(1 . 1))
                   (list :function-name "second"
                         :class-name "SecondClass"
                         :class-range '(3 . 3)
                         :range '(3 . 3))))))
      (should
       (string-empty-p
        (ai-code--format-scope-context
         (ai-code--scope-context-for-region
          (point-min) (point-max))))))))

(ert-deftest test-ai-code-treesit--scope-context-fallback-without-treesit ()
  "Test `ai-code--current-scope-context' provides standard which-function fallback."
  (with-temp-buffer
    (cl-letf (((symbol-function 'ai-code--treesit-available-p) (lambda (&optional _) nil))
              ((symbol-function 'which-function) (lambda () "standalone_func")))
      (let ((scope (ai-code--current-scope-context)))
        (should (equal (plist-get scope :function-name) "standalone_func"))
        (should-not (plist-get scope :class-name))
        (should-not (plist-get scope :class-header))
        (should-not (plist-get scope :function-header))))))

(provide 'test_ai-code-treesit)
;;; test_ai-code-treesit.el ends here
