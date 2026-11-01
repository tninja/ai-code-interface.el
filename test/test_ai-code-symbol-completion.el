;;; test_ai-code-symbol-completion.el --- Tests for Imenu symbol completion -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for @file#symbol discovery through Imenu.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-input)

(ert-deftest ai-code-test-imenu-symbol-records-qualified-and-source-order ()
  "Preserve Imenu hierarchy while ordering symbols by source position."
  (with-temp-buffer
    (insert "def alpha():\n"
            "    pass\n\n"
            "class Service:\n"
            "    def first(self):\n"
            "        pass\n"
            "    def second(self):\n"
            "        pass\n\n"
            "def zebra():\n"
            "    pass\n")
    (let ((alpha-pos (save-excursion
                       (goto-char (point-min))
                       (search-forward "def alpha")
                       (line-beginning-position)))
          (service-pos (save-excursion
                         (goto-char (point-min))
                         (search-forward "class Service")
                         (line-beginning-position)))
          (first-pos (save-excursion
                       (goto-char (point-min))
                       (search-forward "def first")
                       (line-beginning-position)))
          (second-pos (save-excursion
                        (goto-char (point-min))
                        (search-forward "def second")
                        (line-beginning-position)))
          (zebra-pos (save-excursion
                       (goto-char (point-min))
                       (search-forward "def zebra")
                       (line-beginning-position))))
      ;; Deliberately group and scramble the index.  Source positions should
      ;; determine the final order, category names should not qualify it, and
      ;; Imenu's special rescan item should be ignored.
      (cl-letf (((symbol-function 'imenu--make-index-alist)
                 (lambda (&optional _noerror)
                   `(,imenu--rescan-item
                     ("Class"
                      ("Service"
                       (" " . ,service-pos)
                       ("first" . ,first-pos)
                       ("second" . ,second-pos)))
                     ("Function"
                      ("zebra" . ,zebra-pos)
                      ("alpha" . ,alpha-pos))))))
        (let* ((records (ai-code--file-symbol-records--imenu
                         (current-buffer)))
               (qualified (mapcar (lambda (record)
                                    (plist-get record :qualified))
                                  records)))
          (should (equal qualified
                         '("alpha" "Service" "Service.first"
                           "Service.second" "zebra")))
          (should (equal (plist-get (nth 2 records) :header)
                         "def first(self):"))
          (should (= (plist-get (nth 2 records) :line) 5)))))))

(ert-deftest ai-code-test-flat-treesit-imenu-gets-qualified-from-position ()
  "Qualify flat Tree-sitter Imenu method entries from their positions."
  (with-temp-buffer
    (insert "class Bar:\n    def baz(self):\n        pass\n")
    (let ((class-pos (point-min))
          (method-pos (save-excursion
                        (goto-char (point-min))
                        (forward-line 1)
                        (point))))
      (cl-letf (((symbol-function 'imenu--make-index-alist)
                 (lambda (&optional _noerror)
                   `(("Class" ("Bar" . ,class-pos))
                     ("Function" ("baz" . ,method-pos)))))
                ((symbol-function 'ai-code--imenu-treesit-qualified-name-at-position)
                 (lambda (pos)
                   (if (= pos class-pos) "Bar" "Bar.baz"))))
        (should
         (equal
          (mapcar (lambda (record) (plist-get record :qualified))
                  (ai-code--file-symbol-records--imenu (current-buffer)))
          '("Bar" "Bar.baz")))))))

(ert-deftest ai-code-test-imenu-qualified-name-does-not-double-prefix ()
  "Do not duplicate a parent prefix when Imenu already qualified NAME."
  (should (equal (ai-code--imenu-qualified-name "Outer" "Outer.Inner")
                 "Outer.Inner")))

(ert-deftest ai-code-test-imenu-treesit-qualified-name-preserves-full-type-path ()
  "Recover a full enclosing type path from one Imenu symbol position."
  (with-temp-buffer
    (insert "method")
    (cl-letf (((symbol-function 'ai-code--treesit-available-p)
               (lambda (&optional _buffer) t))
              ((symbol-function 'ai-code--treesit-defun-at-point)
               (lambda (&optional _pos) 'run))
              ((symbol-function 'treesit-node-parent)
               (lambda (node)
                 (pcase node
                   ('run 'inner)
                   ('inner 'outer)
                   ('outer 'root)
                   (_ nil))))
              ((symbol-function 'ai-code--treesit-class-like-node-p)
               (lambda (node) (memq node '(inner outer))))
              ((symbol-function 'ai-code--treesit-node-name)
               (lambda (node)
                 (pcase node
                   ('run "run")
                   ('inner "Inner")
                   ('outer "Outer")
                   (_ nil)))))
      (should
       (equal (ai-code--imenu-treesit-qualified-name-at-position (point-min))
              "Outer.Inner.run")))))

(ert-deftest ai-code-test-imenu-symbol-records-preserve-nested-type-path ()
  "Build the complete qualified path for nested Imenu containers."
  (with-temp-buffer
    (insert "class Outer:\n"
            "    class Inner:\n"
            "        def run(self):\n"
            "            pass\n")
    (let ((outer-pos (point-min))
          (inner-pos (save-excursion
                       (goto-char (point-min))
                       (forward-line 1)
                       (point)))
          (run-pos (save-excursion
                     (goto-char (point-min))
                     (forward-line 2)
                     (point))))
      (cl-letf (((symbol-function 'imenu--make-index-alist)
                 (lambda (&optional _noerror)
                   `(("Class"
                      ("Outer"
                       (" " . ,outer-pos)
                       ("Inner"
                        (" " . ,inner-pos)
                        ("run" . ,run-pos))))))))
        (should
         (equal
          (mapcar (lambda (record) (plist-get record :qualified))
                  (ai-code--file-symbol-records--imenu (current-buffer)))
          '("Outer" "Outer.Inner" "Outer.Inner.run")))))))

(ert-deftest ai-code-test-imenu-symbol-header-prefers-semantic-scope ()
  "Use Tree-sitter semantic scope only as enrichment for an Imenu symbol."
  (with-temp-buffer
    (insert "def run(\n"
            "        value):\n"
            "    return value\n")
    (let ((run-pos (point-min)))
      (cl-letf (((symbol-function 'imenu--make-index-alist)
                 (lambda (&optional _noerror)
                   `(("Function" ("run" . ,run-pos)))))
                ((symbol-function 'ai-code--treesit-available-p)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'ai-code--current-scope-context)
                 (lambda (&optional _pos)
                   (list :function-name "run" :function-header "def run(\n        value):"))))
        (let ((record (car (ai-code--file-symbol-records--imenu
                            (current-buffer)))))
          (should (equal (plist-get record :qualified) "run"))
          (should (equal (plist-get record :header)
                         "def run(\n        value):")))))))

(ert-deftest ai-code-test-file-symbol-candidates-use-imenu-with-treesit-active ()
  "Do not bypass Imenu when a Tree-sitter parser is active."
  (let ((buffer (generate-new-buffer " *ai-code-imenu-symbol-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "def alpha():\n    pass\n")
          (let ((alpha-pos (point-min)))
            (cl-letf (((symbol-function 'find-file-noselect)
                       (lambda (&rest _args) buffer))
                      ((symbol-function 'ai-code--treesit-available-p)
                       (lambda (&optional _buffer) t))
                      ((symbol-function 'imenu--make-index-alist)
                       (lambda (&optional _noerror)
                         `(("Function" ("alpha" . ,alpha-pos))))))
              (should (equal (ai-code--file-symbol-candidates "ignored.py")
                             '("alpha"))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-choose-symbol-annotation-from-imenu-records ()
  "Show Imenu-derived source metadata through completion annotations."
  (let ((buffer (generate-new-buffer " *ai-code-imenu-annotation-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "def alpha():\n    pass\n")
          (let ((alpha-pos (point-min))
                captured-candidates
                captured-extra)
            (cl-letf (((symbol-function 'find-file-noselect)
                       (lambda (&rest _args) buffer))
                      ((symbol-function 'imenu--make-index-alist)
                       (lambda (&optional _noerror)
                         `(("Function" ("alpha" . ,alpha-pos)))))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt candidates &rest _args)
                         (setq captured-candidates (all-completions "" candidates)
                               captured-extra completion-extra-properties)
                         "alpha")))
              (should (equal (ai-code--choose-symbol-from-file "ignored.py")
                             "alpha"))
              (should (equal captured-candidates '("alpha")))
              (let ((annotation-function
                     (plist-get captured-extra :annotation-function)))
                (should annotation-function)
                (should (string-match-p
                         "def alpha()"
                         (funcall annotation-function "alpha")))
                (should (string-match-p
                         "line 1"
                         (funcall annotation-function "alpha")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai-code-test-imenu-extended-entry-keeps-its-symbol ()
  "An extended jump entry's arguments must not become nested symbols."
  (with-temp-buffer
    (insert "function original() {}\n")
    (cl-letf (((symbol-function 'imenu--make-index-alist)
               (lambda (&optional _)
                 `(("original" 1 ignore ("argument" . 5))))))
      (should (equal (ai-code--file-symbol-candidates--imenu (current-buffer))
                     '("original"))))))

(ert-deftest ai-code-test-imenu-opaque-jump-entries-keep-provider-order ()
  "Named custom jump entries remain selectable without numeric positions."
  (with-temp-buffer
    (cl-letf (((symbol-function 'imenu--make-index-alist)
               (lambda (&optional _)
                 '(("zeta" "opaque-z" ignore)
                   ("alpha" "opaque-a" ignore)))))
      (should (equal (ai-code--file-symbol-candidates--imenu (current-buffer))
                     '("zeta" "alpha")))
      (should-not (ai-code--choose-symbol-annotation-alist (current-buffer))))))

(ert-deftest ai-code-test-imenu-enrichment-cannot-replace-a-field-with-its-class ()
  "Scope metadata must preserve the Imenu symbol's identity and header."
  (with-temp-buffer
    (insert "class Service:\n    field = 1\n")
    (cl-letf (((symbol-function 'imenu--make-index-alist)
               (lambda (&optional _) '(("field" . 16))))
              ((symbol-function 'ai-code--treesit-available-p)
               (lambda (&optional _) t))
              ((symbol-function 'ai-code--imenu-treesit-qualified-name-at-position)
               (lambda (_) "Service"))
              ((symbol-function 'ai-code--current-scope-context)
               (lambda (&optional _)
                 '(:class-name "Service" :class-header "class Service:"))))
      (let ((record (car (ai-code--file-symbol-records--imenu (current-buffer)))))
        (should (equal (plist-get record :qualified) "field"))
        (should (equal (plist-get record :header) "field = 1"))))))

(ert-deftest ai-code-test-imenu-enrichment-failure-keeps-discovered-symbols ()
  "Optional Tree-sitter failure must not erase the Imenu result."
  (with-temp-buffer
    (insert "def first(): pass\ndef second(): pass\n")
    (cl-letf (((symbol-function 'imenu--make-index-alist)
               (lambda (&optional _) '(("first" . 1) ("second" . 19))))
              ((symbol-function 'ai-code--imenu-treesit-qualified-name-at-position)
               (lambda (_) (error "Metadata unavailable"))))
      (should (equal (ai-code--file-symbol-candidates--imenu (current-buffer))
                     '("first" "second"))))))

(ert-deftest ai-code-test-imenu-completion-widens-and-restores-source-buffer ()
  "File discovery and line annotations use the whole file, preserving narrowing."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun first () t)\n\n(defun second () t)\n")
    (goto-char (point-min))
    (forward-line 2)
    (narrow-to-region (point) (point-max))
    (let ((start (point-min)) (at (point))
          (records (ai-code--file-symbol-records--imenu (current-buffer))))
      (should (equal (mapcar (lambda (record) (plist-get record :qualified)) records)
                     '("first" "second")))
      (should (equal (mapcar (lambda (record) (plist-get record :line)) records)
                     '(1 3)))
      (should (buffer-narrowed-p))
      (should (= (point-min) start))
      (should (= (point) at)))))

(ert-deftest ai-code-test-imenu-completion-advertises-source-order ()
  "Completion metadata tells frontends to preserve definition order."
  (with-temp-buffer
    (insert "def zebra(): pass\ndef alpha(): pass\n")
    (let ((buffer (current-buffer)))
      (cl-letf (((symbol-function 'find-file-noselect) (lambda (&rest _) buffer))
                ((symbol-function 'imenu--make-index-alist)
                 (lambda (&optional _) '(("zebra" . 1) ("alpha" . 19))))
                ((symbol-function 'completing-read)
                 (lambda (_label collection &rest _)
                   (let ((metadata (completion-metadata "" collection nil)))
                     (should (eq (completion-metadata-get metadata 'display-sort-function)
                                 #'identity))
                     (should (equal (all-completions "" collection) '("zebra" "alpha"))))
                   "zebra")))
        (should (equal (ai-code--choose-symbol-from-file "ignored.py") "zebra"))))))

(ert-deftest ai-code-test-imenu-real-python-treesit-qualified-symbols ()
  "Keep qualified definitions using an actual Python Tree-sitter Imenu index."
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)
                    (fboundp 'python-ts-mode)))
  (with-temp-buffer
    (insert "class Outer:\n    class Inner:\n        def run(self):\n            pass\n\n"
            "def zebra():\n    pass\n")
    (python-ts-mode)
    (should (equal (ai-code--file-symbol-candidates--imenu (current-buffer))
                   '("Outer" "Outer.Inner" "Outer.Inner.run" "zebra")))))

(ert-deftest ai-code-test-imenu-symbol-records-rescan-only-after-edits ()
  "Reuse the Imenu index until the buffer text changes."
  (with-temp-buffer
    (insert "def alpha():\n    pass\n")
    (let ((scans 0))
      (setq-local imenu-create-index-function
                  (lambda ()
                    (cl-incf scans)
                    (list (cons "alpha" (point-min)))))
      (ai-code--file-symbol-records--imenu (current-buffer))
      (ai-code--file-symbol-records--imenu (current-buffer))
      (should (= scans 1))
      (goto-char (point-max))
      (insert "def beta():\n    pass\n")
      (ai-code--file-symbol-records--imenu (current-buffer))
      (should (= scans 2)))))

(provide 'test_ai-code-symbol-completion)
;;; test_ai-code-symbol-completion.el ends here
