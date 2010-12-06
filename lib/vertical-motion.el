;;;; Private

(defun vertical-move-line (count next-line-func)
  (let ((goal-column (current-column))
	(start (point))
	s e)
    (condition-case nil
	(while (not (zerop count))
	  (funcall next-line-func 1)
	  (setq s (save-excursion
		    (back-to-indentation)
		    (current-column))
		e (save-excursion
		    (end-of-line)
		    (skip-syntax-backward "-" (point-at-bol))
		    (current-column)))
	  (when (and (<= s goal-column)
		     (<= goal-column e)
		     (> e s))
	    (setq count (1- count))))
      (error
       (ding)
       (goto-char start)))))

;;;; Public

;;;###autoload
(defun vertical-next-line (&optional count)
  "Move forward COUNT lines which have text at the current column."
  (interactive "p")
  (vertical-move-line count 'next-line))

;;;###autoload
(defun vertical-previous-line (&optional count)
  "Move backward COUNT lines which have text at the current column."
  (interactive "p")
  (vertical-move-line count 'previous-line))

;;;; Provide

(provide 'vertical-motion)
