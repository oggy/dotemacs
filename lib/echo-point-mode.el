;;;;
;;;; Echo the value of point wherever you go.
;;;;
;;;; echo-point-mode is a minor mode.  Run (echo-point-mode) to
;;;; toggle.
;;;;

;;;; Commands

;;;###autoload
(defun echo-point-mode:show-point ()
  "Echo and return point."
  (interactive)
  (unless (echo-point-mode:in-minibuffer-p)
    (message "%i" (point))
    (point)))

(defun echo-point-mode:in-minibuffer-p ()
  (if (functionp 'minibufferp)
      ;; GNU Emacs
      (minibufferp (current-buffer))
    ;; XEmacs
    (active-minibuffer-window)))

;;;; Mode

(defvar echo-point-mode nil
  "Non-nil iff echo-point-mode is on.

See the `echo-point-mode' function for more information.")

;;;###autoload
(define-minor-mode echo-point-mode
  "Toggle, enable, or disable echo-point-mode.

When echo-point-mode is enabled, point is echoed after every
command.  Intended as a development aid."
  :lighter " point"
  :after-hook (echo-point-mode:after-hook))

(defun echo-point-mode:after-hook ()
  (if echo-point-mode
      (add-hook 'post-command-hook 'echo-point-mode:show-point)
    (remove-hook 'post-command-hook 'echo-point-mode:show-point)))

;;;; Provide

(provide 'echo-point-mode)
