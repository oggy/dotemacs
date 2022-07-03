(defun g-ensure-package (package)
  "Install the named package (a symbol) if needed."
  (unless (package-installed-p package)
    (package-install package)))

(g-ensure-package 'coffee-mode)
(g-ensure-package 'haml-mode)
(g-ensure-package 'julia-mode)
(g-ensure-package 'lua-mode)
(g-ensure-package 'markdown-mode)
(g-ensure-package 'sass-mode)
(g-ensure-package 'scala-mode)
(g-ensure-package 'yaml-mode)

(provide 'g-packages)