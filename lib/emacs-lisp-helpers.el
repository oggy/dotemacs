;;;###autoload
(defun g-show-region (s e &optional buffer)
  "Dance around a region for few seconds."
  (let (ex tmp)
    (when (> s e) (setq tmp s s e e tmp))
    (if (markerp s) (setq s (marker-position s)))
    (if (markerp e) (setq e (marker-position e)))
    (message "%d -- %d (%d characters)" s e (- e s))
    (unwind-protect
        (progn
          (setq ex (make-overlay s e))
          (overlay-put ex 'face 'underline)
          (save-excursion
            (goto-char s)
            (sit-for 0.25)
            (goto-char e)
            (sit-for 0.25)
            (goto-char s)
            (sit-for 0.25)
            (goto-char e)
            (sit-for 0.25)
            (goto-char s)
            (sit-for 0.25)
            (goto-char e)
            (sit-for 0.25)
            (goto-char s)
            (sit-for 0.25)
            (goto-char e)
            (sit-for 0.25)
            )
          )
      (when ex
        (funcall 'delete-overlay ex)))))

(provide 'emacs-lisp-helpers)
