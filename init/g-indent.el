(defun g-indent-for-tab-command (&optional arg)
  "Indent the current line or region according to the major mode.

Some major modes rebind the TAB key.  To accommodate for this, this
function will check the `g-tab-command' property of the major mode
symbol for the name of the function the TAB key is usually bound to.

If a negative prefix argument is given, run `g-unprefix-region'
instead."
  (interactive "p")
  ;; TAB isn't bound to anything in any keymap--it seems to fallback
  ;; to some invisible thing beyond the current global keymap which
  ;; calls `minibuffer-complete'.  Make sure we don't clobber this.
  (if (minibufferp (current-buffer))
      (minibuffer-complete)
    (if (< arg 0)
        (call-interactively 'g-unprefix-region)
      (if (region-active-p)
          (call-interactively 'indent-region)
        (funcall (or (get major-mode 'g-tab-command)
                     'indent-for-tab-command))))))

(defun g-indent-to-column-with-spaces (col)
  "Insert spaces at point until at column COL.

If already past COL, do nothing."
  (interactive "NIndent to column: ")
  (when (< (current-column) col)
    (insert (make-string (- col (current-column)) ? ))))

(defun g-prefix-region (string)
  "Prefix the region with a given string."
  (interactive "MPrefix region with: ")
  (replace-regexp "^" string nil (region-beginning) (1- (region-end))))

(defun g-unprefix-region (prefix beg end)
  "Remove a prefix from each line of a region."
  (interactive "sRemove prefix regex: \nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((continue t))
        (while continue
          (when (looking-at (concat "^" prefix))
            (delete-region (point)
                           (+ (point) (- (match-end 0)
                                         (match-beginning 0)))))
          (setq continue (= (forward-line) 0)))))))

(defun g-toggle-tab-width (&optional arg)
  "Set the tab width to ARG (toggle between 4 and 8 if ARG is nil)."
  (interactive "P")
  (setq tab-width
        (if arg
            (prefix-numeric-value arg)
          (if (= tab-width 4) 8 4)))
  (message "tab-width set to %s" tab-width))

(provide 'g-indent)