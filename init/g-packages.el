(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)

(defun g-ensure-package (package)
  "Install the named package (a symbol) if needed."
  (unless (package-installed-p package)
    (package-install package)))

(g-ensure-package 'clojure-mode)
(g-ensure-package 'coffee-mode)
(g-ensure-package 'dockerfile-mode)
(g-ensure-package 'eat)
(g-ensure-package 'elixir-mode)
(g-ensure-package 'git-link)
(g-ensure-package 'go-mode)
(g-ensure-package 'haml-mode)
(g-ensure-package 'helm)
(g-ensure-package 'helm-projectile)
(g-ensure-package 'julia-mode)
(g-ensure-package 'lua-mode)
(g-ensure-package 'markdown-mode)
(g-ensure-package 'nim-mode)
(g-ensure-package 'projectile)
(g-ensure-package 'projectile-ripgrep)
(g-ensure-package 'rust-mode)
(g-ensure-package 'sass-mode)
(g-ensure-package 'scala-mode)
(g-ensure-package 'scss-mode)
(g-ensure-package 'sqlformat)
(g-ensure-package 'terraform-mode)
(g-ensure-package 'typescript-mode)
(g-ensure-package 'yaml-mode)
(g-ensure-package 'zig-mode)

(require 'projectile)
(require 'helm-projectile)
(defun g-project-root (dir)
  g-start-dir)
(add-to-list 'projectile-project-root-functions 'g-project-root nil)

;; We always want to use g-start-dir as the project root.
;;
;; If there's no VCS marker (e.g., .git) in the project root, projectile will go
;; up the tree looking for an ancestor that does. It uses this function to find
;; such a path. So here we force that to nil to effectively ignore all dirs
;; outside g-start-dir.
(when (boundp 'g-start-dir)
  (defun projectile-locate-dominating-file (&rest _) nil))

(provide 'g-packages)
