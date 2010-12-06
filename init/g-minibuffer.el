;;;; Minibuffer

(define-key minibuffer-local-completion-map (kbd " ") 'self-insert-command)
(defun g-define-minibuffer-local-completion-shortcut (key path)
  "Make KEY insert PATH in the minibuffer for filename completion.

KEY is of the form given to `kbd'."
  (let ((keyseq (read-kbd-macro key)))
    ;; We can't use read-file-name-map here because ffap makes us use
    ;; local-completion-map.  Makes sense--URLs aren't file names...
    (define-key minibuffer-local-completion-map keyseq
      (eval `(lambda ()
               (interactive)
               (delete-region
                ;; can't just use point-min here, as it causes an
                ;; error if it's in a prompt, which is readonly
                (save-excursion (goto-char (point-min)) (point))
                (point-max))
               (insert ,path))))))

(provide 'g-minibuffer)
