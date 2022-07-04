;; Don't make font-lock-mode give up so easily.
(setq font-lock-maximum-size 400000)

;; Make find-file always start at ~.
(setq default-directory "~/")

;; On Aquamacs, this has a background and foreground color set which
;; overrides the default face in g-customizations.
(when aquamacsp
  (setq default-frame-alist nil))

;; Infer project name in frame titles.
(defun g-buffer-project-root (buffer)
  "Get the root path of the project of BUFFER's file, if any."
  (unless (string-match "\\`[* ]" (buffer-name buffer))
    (let* ((root (with-current-buffer buffer (projectile-project-root))))
      (replace-in-string root "/*\\'" ""))))

(defun g-frame-title-base ()
  (let ((titles (mapcar (lambda (root) (g-titleize (file-name-base root)))
                        (g-project-roots))))
    (string-join titles ", ")))

(defun g-set-frame-titles ()
  (let ((title (g-frame-title-base)))
    (setq frame-title-format
          (if (> (length (frame-list)) 1)
              (concat title " -- %b")
            title))))

(add-hook 'buffer-list-update-hook 'g-set-frame-titles)
(add-hook 'after-make-frame-functions 'g-set-frame-titles)
(add-hook 'after-delete-frame-functions 'g-set-frame-titles)

(provide 'g-overrides)
