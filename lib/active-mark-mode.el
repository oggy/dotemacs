(defvar active-mark-mode-map (make-sparse-keymap)
  "Keymap enabled while active-mark-mode is enabled.")
(define-key active-mark-mode-map (kbd ">") 'active-mark-indent-region)
(define-key active-mark-mode-map (kbd "<") 'active-mark-outdent-region)

(define-minor-mode active-mark-mode
  "Minor mode enabled while the mark is active (transient mark mode only).

This mode is automatically toggled. Simply ensure this file is
loaded when emacs starts up."
  :init-value nil
  :keymap 'active-mark-mode-map
  :lighter " [A]")

(add-hook 'activate-mark-hook 'turn-on-active-mark-mode t)
(add-hook 'deactivate-mark-hook 'turn-off-active-mark-mode t)

(defun turn-on-active-mark-mode ()
  (active-mark-mode 1))

(defun turn-off-active-mark-mode ()
  (active-mark-mode 0))

(defun active-mark-indent-region (region-start region-end n)
  "Indent the region N times.

The region is indented by `tab-width` * N spaces. The lines
before (or after if none) the region is used to determine if tabs
should be used to indent. If there is no indentation on the line
before or after, or if the buffer is empty, indent with spaces."
  (interactive "r\np")
  (let* ((s (save-excursion
             (goto-char region-start)
             (beginning-of-line)
             (point-marker)))
         (e (save-excursion
              (goto-char region-end)
              (unless (bolp)
                (end-of-line)
                (forward-char))
              (if (= (point) s)
                  (forward-line))
              (point-marker)))
         (indent (make-string (* n tab-width) ? )))
    (save-excursion
      (goto-char s)
      (while (< (point) e)
        (insert indent)
        (forward-line)))))

;; Hack for cua-mode. When enabled, deactivate-mark-hook is not called on first
;; C-g.
(add-hook 'active-mark-mode 'active-mark-cua-hack)
(defun active-mark-cua-hack ()
  (if active-mark-mode
      (add-hook 'post-command-hook 'active-mark-mode-post-command-hook)
    (remove-hook 'post-command-hook 'active-mark-mode-post-command-hook)))

(defun active-mark-mode-post-command-hook ()
  (if (not mark-active)
      (active-mark-mode 0)))

(provide 'active-mark-mode)
