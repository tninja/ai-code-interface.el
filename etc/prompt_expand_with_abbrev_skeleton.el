
;; expand prompt with abbrev + skeleton

(setq-default abbrev-mode t)

;;; expand in mini-buffer (C-c a <SPC>)
(setq enable-recursive-minibuffers t)

;;; skeletons

;;;; leetcode implement function
(define-skeleton leetcode/implement
  "Insert a note template (works in minibuffer too)."
  nil
  "Implement this function, given requirement, test cases and hints inside comments") ;; This skeleton currently inserts a single prompt string and will concatenate it with any additional skeleton elements you add (for example, extra text, `read-string` prompts, or bug-tracking identifiers`). Extend this template by adding more elements to the skeleton body as needed.

(define-abbrev-table 'global-abbrev-table
  '(("leetcode" "" (lambda () (interactive) (leetcode/implement)) :system t)))
