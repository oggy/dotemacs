(defun g-isearch-yank-symbol ()
  "Pull next identifier from buffer into search string."
  (interactive)
  (isearch-yank (function (lambda ()
                            (skip-syntax-forward "^w_")
                            (skip-syntax-forward "w_")))))

(define-key isearch-mode-map (kbd "C-S-w") 'g-isearch-yank-symbol)
(put 'g-isearch-yank-symbol 'isearch-command t)

(provide 'g-isearch)
