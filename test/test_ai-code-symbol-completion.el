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
                   (list :function-header "def run(\n        value):"))))
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
                         (setq captured-candidates candidates
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

(provide 'test_ai-code-symbol-completion)
;;; test_ai-code-symbol-completion.el ends here
