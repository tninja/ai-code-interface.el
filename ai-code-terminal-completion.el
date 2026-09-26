;;; ai-code-terminal-completion.el --- Company in AI Code terminals -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; An opt-in, agent-independent completion interface for vterm sessions.
;; The CLI owns its input; Emacs tracks only a contiguous suffix typed at the
;; terminal cursor.  Company displays candidates over the read-only terminal,
;; while accepting a candidate sends keystrokes through the terminal adapter.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-prompt-completion)

(declare-function ai-code-backends-infra--terminal-send-string
                  "ai-code-backends-infra" (string &optional paste))
(declare-function ai-code-backends-infra--terminal-send-backspace
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--terminal-send-return
                  "ai-code-backends-infra" ())
(declare-function company-mode "company" (&optional arg))
(declare-function company-auto-begin "company" ())
(declare-function company-abort "company" ())
(declare-function company-select-next "company" ())
(declare-function company-select-previous "company" ())
(declare-function cape-dict "cape" (&optional interactive))
(defvar company-active-map)
(defvar company-backends)
(defvar company-backend)
(defvar company-candidates)
(defvar company-idle-delay)
(defvar company-minimum-prefix-length)
(defvar company-frontends)
(defvar company-selection)
(defvar company-mode)
(defvar company-insertion-on-trigger)
(defvar ai-code-backends-infra--session-terminal-backend)
(defvar vterm-copy-mode)
(defvar ai-code-terminal-completion-mode)

(defgroup ai-code-terminal-completion nil
  "Completion inside AI Code terminal sessions."
  :group 'ai-code-backends-infra)

(defcustom ai-code-terminal-completion-delay 0.2
  "Idle seconds before showing Company candidates in a terminal."
  :type 'number :group 'ai-code-terminal-completion)

(defvar-local ai-code-terminal-completion--input "")
(defvar-local ai-code-terminal-completion--timer nil)

(defun ai-code-terminal-completion--cancel-timer ()
  "Cancel this session's pending completion timer."
  (when (timerp ai-code-terminal-completion--timer)
    (cancel-timer ai-code-terminal-completion--timer))
  (setq ai-code-terminal-completion--timer nil))

(defun ai-code-terminal-completion--reset ()
  "Forget input after an operation whose effect is not known."
  (ai-code-terminal-completion--cancel-timer)
  (setq ai-code-terminal-completion--input "")
  (when (and (featurep 'company) company-candidates)
    (company-abort)))

(defun ai-code-terminal-completion--tracked-word ()
  "Return the last tracked word outside native agent input tokens."
  (when (and ai-code-terminal-completion-mode
             (not (bound-and-true-p vterm-copy-mode))
             ;; Leave native slash commands and @file menus to the CLI.
             (not (string-match-p
                   "\\(?:\\`\\|[[:space:]]\\)[/@][^[:space:]]*\\'"
                   ai-code-terminal-completion--input))
             (string-match "\\([[:alpha:]]+\\)\\'"
                           ai-code-terminal-completion--input))
    (match-string 1 ai-code-terminal-completion--input)))

(defun ai-code-terminal-completion--word ()
  "Return the tracked word only if it is visible at the terminal cursor."
  (when-let* ((word (ai-code-terminal-completion--tracked-word)))
    (when (and (>= (point) (+ (point-min) (length word)))
               (string= word (buffer-substring-no-properties
                              (- (point) (length word)) (point))))
      word)))

(defun ai-code-terminal-completion--dict (prefix)
  "Collect English dictionary candidates matching PREFIX when Cape exists."
  (when (fboundp 'cape-dict)
    (with-temp-buffer
      (insert prefix)
      (when-let* ((capf (cape-dict))
                  (table (nth 2 capf)))
        (all-completions prefix table (plist-get (nthcdr 3 capf) :predicate))))))

(defun ai-code-terminal-completion--company (command &optional arg &rest _ignored)
  "Company backend for tracked terminal text; COMMAND and ARG are its API."
  (pcase command
    ('prefix (ai-code-terminal-completion--word))
    ('candidates
     (let* ((index (ai-code-prompt-completion--ensure-index))
            (prompts (cl-remove-if-not
                      (lambda (candidate)
                        (and (not (string-match-p "\n" candidate))
                             (string-prefix-p arg candidate t)))
                      (car index))))
       (delete-dups
        (append prompts (ai-code-terminal-completion--dict arg)))))
    ('annotation
     (when (gethash arg (nth 1 (ai-code-prompt-completion--ensure-index)))
       " prompt"))
    ('sorted t)
    ('no-cache t)))

(defun ai-code-terminal-completion--schedule ()
  "Schedule a popup for the current terminal window."
  (ai-code-terminal-completion--cancel-timer)
  (when (and (featurep 'company) company-candidates)
    (company-abort))
  (when-let* ((word (ai-code-terminal-completion--tracked-word)))
    (when (>= (length word) 3)
      (let ((buffer (current-buffer))
            (window (selected-window))
            (input ai-code-terminal-completion--input))
        (setq ai-code-terminal-completion--timer
              (run-with-timer
               ai-code-terminal-completion-delay nil
               (lambda ()
                 (when (and (buffer-live-p buffer) (window-live-p window)
                            (eq (window-buffer window) buffer)
                            (eq (selected-window) window))
                   (with-current-buffer buffer
                     (when (and ai-code-terminal-completion-mode
                                (equal ai-code-terminal-completion--input input)
                                (ai-code-terminal-completion--word))
                       ;; Vterm is read-only.  Company needs this binding for
                       ;; its eligibility check, but never owns an insertion.
                       (let ((buffer-read-only nil))
                         (company-auto-begin))))))))))))

(defun ai-code-terminal-completion--post-command ()
  "Track only plain keys that the terminal itself processed."
  (when ai-code-terminal-completion-mode
    (let* ((keys (this-command-keys-vector))
           (key (and (= (length keys) 1) (aref keys 0))))
      (cond
       ((and (memq this-command '(vterm--self-insert vterm-send-space))
             (characterp key) (<= 32 key) (<= key 126))
        (setq ai-code-terminal-completion--input
              (concat ai-code-terminal-completion--input (char-to-string key)))
        (ai-code-terminal-completion--schedule))
       ((eq this-command 'vterm-send-backspace)
        (if (string-empty-p ai-code-terminal-completion--input)
            (ai-code-terminal-completion--reset)
          (setq ai-code-terminal-completion--input
                (substring ai-code-terminal-completion--input 0 -1))
          (ai-code-terminal-completion--schedule)))
       ((memq this-command '(company-select-next company-select-previous
                             company-abort ai-code-terminal-completion-accept))
        nil)
       (t (ai-code-terminal-completion--reset))))))

(defun ai-code-terminal-completion-accept ()
  "Insert the selected Company candidate into the CLI without submitting."
  (interactive)
  (let* ((word (ai-code-terminal-completion--word))
         (candidate (and word company-candidates
                         (nth (or company-selection 0) company-candidates)))
         (full (and candidate
                    (gethash (substring-no-properties candidate)
                             (nth 1 (ai-code-prompt-completion--ensure-index)))))
         (input ai-code-terminal-completion--input)
         (replace-all (and full
                           (string-prefix-p input full t)
                           (>= (point) (+ (point-min) (length input)))
                           (string= input (buffer-substring-no-properties
                                           (- (point) (length input)) (point)))))
         (replacement (or (and replace-all full) candidate))
         (count (if replace-all (length input) (length (or word "")))))
    (if (and word candidate
             (not (string-match-p "\n" replacement)))
        (progn
          (company-abort)
          (ai-code-terminal-completion--cancel-timer)
          (dotimes (_ count)
            (ai-code-backends-infra--terminal-send-backspace))
          (ai-code-backends-infra--terminal-send-string replacement t)
          ;; The CLI may transform a paste; start fresh on the next key.
          (setq ai-code-terminal-completion--input ""))
      (ai-code-terminal-completion--reset))))

;; Company must leave the selection visible until this command reads it.
(put 'ai-code-terminal-completion-accept 'company-keep t)

(defun ai-code-terminal-completion--return ()
  "Dismiss completion and forward RET to the CLI."
  (interactive)
  (ai-code-terminal-completion--reset)
  (ai-code-backends-infra--terminal-send-return))

(defun ai-code-terminal-completion--finish (original candidate)
  "Protect the read-only terminal from Company insertion.
ORIGINAL is `company-finish'; CANDIDATE is forwarded to the CLI instead."
  (if (and ai-code-terminal-completion-mode
           (eq company-backend #'ai-code-terminal-completion--company))
      (let ((company-candidates (list candidate))
            (company-selection 0))
        (ai-code-terminal-completion-accept))
    (funcall original candidate)))

(defvar ai-code-terminal-completion--advice-installed nil)

;;;###autoload
(define-minor-mode ai-code-terminal-completion-mode
  "Automatically show Company completion while typing in a vterm session.
This opt-in mode supports any AI Code agent using vterm.  It follows only
plain printable keys and backspace.  Cursor movement, CLI history, paste,
and other edits drop the tracked input until typing starts again."
  :lighter " TermComp"
  (if ai-code-terminal-completion-mode
      (progn
        (unless (eq ai-code-backends-infra--session-terminal-backend 'vterm)
          (setq ai-code-terminal-completion-mode nil)
          (user-error "Terminal completion currently requires vterm"))
        (unless (require 'company nil t)
          (setq ai-code-terminal-completion-mode nil)
          (user-error "Install company to enable terminal completion"))
        (require 'cape nil t)
        (unless ai-code-terminal-completion--advice-installed
          (advice-add 'company-finish :around #'ai-code-terminal-completion--finish)
          (setq ai-code-terminal-completion--advice-installed t))
        (setq-local company-backends '(ai-code-terminal-completion--company)
                    company-idle-delay nil
                    company-minimum-prefix-length 3
                    company-insertion-on-trigger nil
                    company-frontends '(company-pseudo-tooltip-frontend
                                        company-echo-metadata-frontend))
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "TAB") #'ai-code-terminal-completion-accept)
          (define-key map (kbd "<tab>") #'ai-code-terminal-completion-accept)
          (define-key map (kbd "C-n") #'company-select-next)
          (define-key map (kbd "C-p") #'company-select-previous)
          (define-key map (kbd "<escape>") #'company-abort)
          (define-key map (kbd "RET") #'ai-code-terminal-completion--return)
          (setq-local company-active-map map))
        (add-hook 'post-command-hook #'ai-code-terminal-completion--post-command nil t)
        (company-mode 1))
    (remove-hook 'post-command-hook #'ai-code-terminal-completion--post-command t)
    (ai-code-terminal-completion--reset)
    (when (bound-and-true-p company-mode) (company-mode -1))))

(provide 'ai-code-terminal-completion)
;;; ai-code-terminal-completion.el ends here
