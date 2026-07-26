;;; ai-code-backends-infra-vterm-render-queue.el --- Efficient vterm render batching -*- lexical-binding: t; -*-

;; Author: Kang Tu, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Efficient anti-flicker batching for AI Code vterm sessions.  New queued
;; chunks are stored as a reverse list so enqueue is O(1), while legacy string
;; queues are still accepted during flush and migration.  The batch timer is
;; anchored to the first redraw chunk instead of being restarted continuously.

;;; Code:

(require 'cl-lib)

(declare-function ai-code-backends-infra--session-buffer-p
                  "ai-code-backends-infra" (buffer))
(declare-function ai-code-backends-infra--vterm-render-preserving-copy-mode-view
                  "ai-code-backends-infra-vterm" (render-fn))
(declare-function vterm--filter "vterm" (&rest args))

(defvar ai-code-backends-infra-vterm-anti-flicker)
(defvar ai-code-backends-infra-vterm-render-delay)
(defvar ai-code-backends-infra--vterm-redraw-regexp)
(defvar ai-code-backends-infra--vterm-render-queue)
(defvar ai-code-backends-infra--vterm-render-timer)
(defvar vterm-copy-mode)

(defun ai-code-backends-infra-vterm-render-queue--data ()
  "Return queued vterm chunks in arrival order as one string.
Accept the previous string queue representation so live buffers and existing
callers can transition without losing pending output."
  (pcase ai-code-backends-infra--vterm-render-queue
    ((pred stringp) ai-code-backends-infra--vterm-render-queue)
    ((pred consp)
     (apply #'concat (nreverse ai-code-backends-infra--vterm-render-queue)))
    (_ nil)))

(defun ai-code-backends-infra-vterm-render-queue--push (input)
  "Queue INPUT in O(1), normalizing a legacy string queue when necessary."
  (when (stringp ai-code-backends-infra--vterm-render-queue)
    (setq ai-code-backends-infra--vterm-render-queue
          (list ai-code-backends-infra--vterm-render-queue)))
  (push input ai-code-backends-infra--vterm-render-queue))

(defun ai-code-backends-infra-vterm-render-queue--render-queued-output
    (orig-fun buffer)
  "Render queued vterm output for BUFFER using ORIG-FUN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ai-code-backends-infra--vterm-render-timer nil)
      (when ai-code-backends-infra--vterm-render-queue
        (let ((data (ai-code-backends-infra-vterm-render-queue--data)))
          (setq ai-code-backends-infra--vterm-render-queue nil)
          (when-let* ((process (get-buffer-process buffer))
                      ((process-live-p process)))
            (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
             (lambda ()
               (funcall orig-fun process data)))))))))

(defun ai-code-backends-infra-vterm-render-queue--flush (&optional buffer)
  "Immediately render delayed vterm output queued for BUFFER."
  (when (or (null buffer) (buffer-live-p buffer))
    (with-current-buffer (or buffer (current-buffer))
      (when ai-code-backends-infra--vterm-render-timer
        (cancel-timer ai-code-backends-infra--vterm-render-timer))
      (setq ai-code-backends-infra--vterm-render-timer nil)
      (when ai-code-backends-infra--vterm-render-queue
        (let ((data (ai-code-backends-infra-vterm-render-queue--data)))
          (setq ai-code-backends-infra--vterm-render-queue nil)
          (when-let* ((process (get-buffer-process (current-buffer)))
                      ((process-live-p process)))
            (let ((ai-code-backends-infra-vterm-anti-flicker nil))
              (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
               (lambda ()
                 (vterm--filter process data))))))))))

(defun ai-code-backends-infra-vterm-render-queue--smart-renderer
    (orig-fun process input)
  "Render PROCESS INPUT immediately or batch redraw-heavy output via ORIG-FUN."
  (if (or (not ai-code-backends-infra-vterm-anti-flicker)
          (not (ai-code-backends-infra--session-buffer-p (process-buffer process))))
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      ;; Most model output is plain text.  Avoid redraw analysis entirely unless
      ;; the chunk contains terminal control characters or joins an active batch.
      (if (and (null ai-code-backends-infra--vterm-render-queue)
               (not (string-match-p "[\r\e]" input)))
          (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
           (lambda ()
             (funcall orig-fun process input)))
        (let* ((complex-redraw-detected
                (string-match-p ai-code-backends-infra--vterm-redraw-regexp input))
               (clear-count (1- (length (split-string input "\033\\[K"))))
               (cr-count (cl-count ?\15 input))
               (escape-count (cl-count ?\033 input))
               (input-length (length input))
               (escape-density (if (> input-length 0)
                                   (/ (float escape-count) input-length)
                                 0)))
          (if (or complex-redraw-detected
                  (>= cr-count 2)
                  (and (> escape-density 0.3) (>= clear-count 2))
                  ai-code-backends-infra--vterm-render-queue)
              (let ((buffer (current-buffer)))
                (ai-code-backends-infra-vterm-render-queue--push input)
                (unless ai-code-backends-infra--vterm-render-timer
                  (setq ai-code-backends-infra--vterm-render-timer
                        (run-at-time
                         ai-code-backends-infra-vterm-render-delay nil
                         #'ai-code-backends-infra--vterm-render-queued-output
                         orig-fun buffer))))
            (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
             (lambda ()
               (funcall orig-fun process input)))))))))

(defun ai-code-backends-infra-vterm-render-queue--flush-on-copy-mode-exit ()
  "Flush queued output when leaving `vterm-copy-mode'."
  (unless (bound-and-true-p vterm-copy-mode)
    (when ai-code-backends-infra--vterm-render-queue
      (when ai-code-backends-infra--vterm-render-timer
        (cancel-timer ai-code-backends-infra--vterm-render-timer))
      (setq ai-code-backends-infra--vterm-render-timer nil)
      (when-let* ((proc (get-buffer-process (current-buffer))))
        (let ((data (ai-code-backends-infra-vterm-render-queue--data)))
          (setq ai-code-backends-infra--vterm-render-queue nil)
          (vterm--filter proc data))))))

(defun ai-code-backends-infra-vterm-render-queue--add-override (target function)
  "Add FUNCTION as an override advice for TARGET unless already present."
  (unless (advice-member-p function target)
    (advice-add target :override function)))

(defun ai-code-backends-infra-vterm-render-queue-activate ()
  "Activate efficient render batching for vterm sessions."
  (ai-code-backends-infra-vterm-render-queue--add-override
   #'ai-code-backends-infra--vterm-render-queued-output
   #'ai-code-backends-infra-vterm-render-queue--render-queued-output)
  (ai-code-backends-infra-vterm-render-queue--add-override
   #'ai-code-backends-infra-vterm-flush-render-queue
   #'ai-code-backends-infra-vterm-render-queue--flush)
  (ai-code-backends-infra-vterm-render-queue--add-override
   #'ai-code-backends-infra--vterm-smart-renderer
   #'ai-code-backends-infra-vterm-render-queue--smart-renderer)
  (ai-code-backends-infra-vterm-render-queue--add-override
   #'ai-code-backends-infra--vterm-flush-on-copy-mode-exit
   #'ai-code-backends-infra-vterm-render-queue--flush-on-copy-mode-exit))

(provide 'ai-code-backends-infra-vterm-render-queue)

;;; ai-code-backends-infra-vterm-render-queue.el ends here
