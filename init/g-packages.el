(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)

(defun g-ensure-package (package)
  "Install the named package (a symbol) if needed."
  (unless (package-installed-p package)
    (package-install package)))

(g-ensure-package 'clojure-mode)
(g-ensure-package 'coffee-mode)
(g-ensure-package 'git-link)
(g-ensure-package 'haml-mode)
(g-ensure-package 'helm)
(g-ensure-package 'helm-projectile)
(g-ensure-package 'julia-mode)
(g-ensure-package 'lua-mode)
(g-ensure-package 'markdown-mode)
(g-ensure-package 'projectile)
(g-ensure-package 'rust-mode)
(g-ensure-package 'sass-mode)
(g-ensure-package 'scala-mode)
(g-ensure-package 'typescript-mode)
(g-ensure-package 'yaml-mode)

(require 'projectile)

(provide 'g-packages)
