(defvar g-start-dir (getenv "G_START_DIR")
  "Root of project this Emacs was started in, if any.")

;;;; Load Path

(defvar rc-dir (expand-file-name "~/.emacs.d")
  "The user configuration directory.")

(defun rc-path (path)
  "Prepend the user configuration directory to the given path"
  (concat rc-dir "/" path))

(defun add-to-load-path (path)
  (interactive)
  "Add PATH to the front of `load-path'."
  (setq load-path (cons path load-path))
  (mapc (lambda (p)
          (if (file-directory-p p)
              (add-to-load-path p)))
        (directory-files path t "^[^.]")))

(add-to-load-path (rc-path "init"))
(add-to-load-path (rc-path "lib"))

;;;; Initializers

(require 'g-fixes)
(require 'g-packages)
(require 'g-customizations)
(require 'g-helpers)
(require 'g-keymaps)
(require 'g-auto-modes)
(require 'g-autoloads)
(require 'g-major-modes)
(require 'g-dired)
(require 'g-files)
(require 'g-region)
(require 'g-minibuffer)
(require 'g-windows)
(require 'g-isearch)
(require 'g-indent)
(require 'g-commands)
(require 'g-blocks)
(require 'g-overrides)
(require 'g-features)
(require 'g-server)

;; Load site-specific configuration from site.el.
(let ((path (concat rc-dir "/site.el")))
  (when (file-exists-p path) (load-file path)))
