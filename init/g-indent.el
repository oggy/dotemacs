(defun g-indent-to-column-with-spaces (col)
  "Insert spaces at point until at column COL.

If already past COL, do nothing."
  (interactive "NIndent to column: ")
  (when (< (current-column) col)
    (insert (make-string (- col (current-column)) ? ))))

(defun g-toggle-tab-width (&optional arg)
  "Set the tab width to ARG (toggle between 4 and 8 if ARG is nil)."
  (interactive "P")
  (setq tab-width
        (if arg
            (prefix-numeric-value arg)
          (if (= tab-width 4) 8 4)))
  (message "tab-width set to %s" tab-width))

(provide 'g-indent)
