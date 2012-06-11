;;;; Private

(defun vertical-move-line (count)
  (let ((goal-col (current-column))
	(start (point))
        (sign (/ count (abs count)))
	s e)
    (condition-case nil
        (progn
          (while (not (zerop count))
            (when (/= (forward-line sign) 0)
              (error))
            (setq s (save-excursion
                      (back-to-indentation)
                      (current-column))
                  e (save-excursion
                      (end-of-line)
                      (skip-syntax-backward "-" (point-at-bol))
                      (current-column)))
            (when (and (<= s goal-col)
                       (<= goal-col e)
                       (> e s))
              (setq count (- count sign))))
          (forward-char goal-col))
      (error
       (ding)
       (goto-char start)))))

;;;; Public

;;;###autoload
(defun vertical-next-line (&optional count)
  "Move forward COUNT lines which have text at the current column."
  (interactive "p")
  (vertical-move-line count))

;;;###autoload
(defun vertical-previous-line (&optional count)
  "Move backward COUNT lines which have text at the current column."
  (interactive "p")
  (vertical-move-line (- count)))

;;;; Provide

(provide 'vertical-motion)
