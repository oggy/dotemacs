(defvar g-last-region nil
  "Region used for g-reselect-region.

A list: (point mark command).")

(defun g-reselect-region ()
  "Reselect the region last used for a command.

If the region was deactivated by a keyboard-quit, that doesn't count."
  (interactive)
  (if g-last-region
      (progn
        (goto-char (nth 0 g-last-region))
        (set-mark (nth 1 g-last-region))
        (message "Region used for %s." (nth 2 g-last-region))
        (zmacs-activate-region))
    (error "No saved region.")))

(add-hook 'zmacs-deactivate-region-hook
          (lambda ()
            (unless (memq this-command '(keyboard-quit exit-minibuffer))
              (setq g-last-region
                    (list (point) (mark t) this-command)))))

(provide 'g-region)
