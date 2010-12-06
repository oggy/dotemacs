(g-define-mode-keys dired
  "C-q" 'g-kill-all-dired-buffers)

;; from http://www.emacswiki.org/cgi-bin/wiki.pl?DiredSortDirectoriesFirst
(defun g-dired-sort ()
  (save-excursion
    (let ((buffer-read-only nil))
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (set-buffer-modified-p nil))
;(add-hook 'dired-after-readin-hook 'sof/dired-sort)

(defun g-kill-all-dired-buffers ()
  (interactive)
  (mapc (lambda (cc) (kill-buffer (cdr cc))) dired-buffers))

(provide 'g-dired)
