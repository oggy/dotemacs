;;; -*- lexical-binding: t -*-
;;;;
;;;; Like wandering around with a metal detector, only it picks up the
;;;; text properties at point.
;;;;
;;;; property-detector-mode is a minor mode.  Run
;;;; (property-detector-mode) to toggle.
;;;;

;;;; Commands

;;;###autoload
(defun property-detector:show-properties ()
  "Show text properties at point in the echo area."
  (interactive)
  (unless (property-detector:in-minibuffer-p)
    (let ((properties (text-properties-at (point))))
      (message "%s" properties))))

(defun property-detector:in-minibuffer-p ()
  (if (functionp 'minibufferp)
      ;; GNU Emacs
      (minibufferp (current-buffer))
    ;; XEmacs
    (active-minibuffer-window)))

;;;; Mode

(defvar property-detector-mode nil
  "Non-nil iff property-detector-mode is on.

See the `property-detector-mode' function for more information.")

;;;###autoload
(defun property-detector-mode (&optional arg)
  "Toggle, enable, or disable property-detector-mode.

It's like wandering around with a metal detector, only it picks
up the text properties at point.

This is useful, for example, when examining font-locking issues."
  (interactive)
  (let ((old-state property-detector-mode))
    (setq property-detector-mode
          (if (null arg) (not property-detector-mode)
            (> (prefix-numeric-value arg) 0)))
    (when (not (eq old-state property-detector-mode))
      (if property-detector-mode
          (property-detector:enable)
        (property-detector:disable)))))

(defun property-detector:enable ()
  (add-hook 'post-command-hook 'property-detector:show-properties))

(defun property-detector:disable ()
  (remove-hook 'post-command-hook 'property-detector:show-properties))

(add-minor-mode 'property-detector-mode nil)

;;;; Provide

(provide 'property-detector)
