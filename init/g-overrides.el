;; Don't make font-lock-mode give up so easily.
(setq font-lock-maximum-size 400000)

;; Make find-file always start at ~.
(setq default-directory "~/")

;; On Aquamacs, this has a background and foreground color set which
;; overrides the default face in g-customizations.
(when aquamacsp
  (setq default-frame-alist nil))

(provide 'g-overrides)
