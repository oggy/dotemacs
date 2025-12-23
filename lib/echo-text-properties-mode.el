;;; -*- lexical-binding: t -*-
;;;;
;;;; Echo text properties at point wherever you go.
;;;;
;;;; echo-text-properties-mode is a minor mode.  Run (echo-text-properties-mode)
;;;; to toggle.
;;;;

;;;; Commands

;;;###autoload
(defun echo-text-properties-mode:show-point ()
  "Echo and return point."
  (interactive)
  (unless (echo-text-properties-mode:in-minibuffer-p)
    (message "%S" (text-properties-at (point)))
    (point)))

(defun echo-text-properties-mode:in-minibuffer-p ()
  (if (functionp 'minibufferp)
      ;; GNU Emacs
      (minibufferp (current-buffer))
    ;; XEmacs
    (active-minibuffer-window)))

;;;; Mode

(defvar echo-text-properties-mode nil
  "Non-nil iff echo-text-properties-mode is on.

See the `echo-text-properties-mode' function for more
information.")

;;;###autoload
(define-minor-mode echo-text-properties-mode
  "Toggle, enable, or disable echo-text-properties-mode.

When echo-text-properties-mode is enabled, text properties at
point are echoed after every command.  Intended as a development
aid."
  :lighter " text-props"
  :after-hook (echo-text-properties-mode:after-hook))

(defun echo-text-properties-mode:after-hook ()
  (if echo-text-properties-mode
      (add-hook 'post-command-hook 'echo-text-properties-mode:show-point)
    (remove-hook 'post-command-hook 'echo-text-properties-mode:show-point)))

;;;; Provide

(provide 'echo-text-properties-mode)
