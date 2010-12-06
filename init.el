;;;; Load Path

(defvar rc-dir (expand-file-name "~/.emacs.d")
  "The user configuration directory.")

(defun rc-path (path)
  "Prepend the user configuration directory to the given path"
  (concat rc-dir "/" path))

(defun add-to-load-path (path)
  (interactive)
  "Add PATH to the front of `load-path'."
  (setq load-path (cons path load-path)))

(add-to-load-path (rc-path "init"))
(add-to-load-path (rc-path "lib"))
(add-to-load-path (rc-path "vendor"))

;;;; Initializers

(require 'g-customizations)
(require 'g-helpers)
(require 'g-keymaps)
(require 'g-auto-modes)
(require 'g-autoloads)
(require 'g-major-modes)
(require 'g-dired)
(require 'g-region)
(require 'g-minibuffer)
(require 'g-windows)
(require 'g-isearch)
(require 'g-indent)
(require 'g-commands)
(require 'g-blocks)
(require 'g-overrides)
(require 'g-features)

;; Load site-specific configuration from site.el.
(let ((path (concat rc-dir "/site.el")))
  (when (file-exists-p path) (load-file path)))
