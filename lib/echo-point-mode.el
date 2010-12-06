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
(defun echo-point-mode (&optional arg)
  "Toggle, enable, or disable echo-point-mode.

When echo-point-mode is enabled, point is echoed after every
command.  Intended as a development aid."
  (interactive)
  (let ((old-state echo-point-mode))
    (setq echo-point-mode
          (if (null arg) (not echo-point-mode)
            (> (prefix-numeric-value arg) 0)))
    (when (not (eq old-state echo-point-mode))
      (if echo-point-mode
          (echo-point-mode:enable)
        (echo-point-mode:disable)))))

(defun echo-point-mode:enable ()
  (add-hook 'post-command-hook 'echo-point-mode:show-point))

(defun echo-point-mode:disable ()
  (remove-hook 'post-command-hook 'echo-point-mode:show-point))

(add-minor-mode 'echo-point-mode nil)

;;;; Provide

(provide 'echo-point)
