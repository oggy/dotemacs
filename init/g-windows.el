(defun g-select-next-window ()
  (interactive)
  "Select the next window (see `next-window')."
  (select-window (next-window)))

(defun g-select-previous-window ()
  (interactive)
  "Select the previous window (see `previous-window')."
  (select-window (previous-window)))

(provide 'g-windows)
