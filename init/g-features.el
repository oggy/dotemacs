;; html-helper-mode kills indenting
(setq magic-mode-alist
      (delq nil
            (mapcar (lambda (pair) (if (eq (cdr pair) 'html-helper-mode) nil pair))
                    magic-mode-alist)))

(require 'switcher)

(when g-start-dir
  (cd g-start-dir)
  (setq desktop-dirname (concat rc-dir "/var/desktops")
        desktop-path (list desktop-dirname)
        desktop-base-file-name (concat (string-replace "/" "!" g-start-dir) ".desktop")
        desktop-base-lock-name (concat (string-replace "/" "!" g-start-dir) ".desktop.lock"))
  (make-directory desktop-dirname t))

;; Enable active-mark-mode
(require 'active-mark-mode)

;; Use find-file-at-point.
(require 'ffap)
(ffap-bindings)
(setq ffap-url-regexp nil)
(setq ffap-machine-p-known 'accept)

;; Prefer spaces to TABs, TABs default to 8 wide.
(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 8)

;; Hide screen clutter.
(tool-bar-mode 0)
(menu-bar-mode (if (and (display-graphic-p) (eq system-type 'darwin)) 1 0))
(scroll-bar-mode 0)

;; enable minibuffer resizing
(when (boundp 'resize-minibuffer-mode)
  (resize-minibuffer-mode 1))

;; Enable narrowing.
(put 'narrow-to-region 'disabled nil)

;; Keep autosaves in one place.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Refresh files automatically when they change (if not modified).
(global-auto-revert-mode 1)

;; Enable transient-mark-mode (region highlighting).
(transient-mark-mode)

;; Enable show-paren-mode.
(show-paren-mode 1)

;; Make cursor blink.
(setq blink-cursor-interval 0.1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type '(bar . 2))
(blink-cursor-mode 1)

;; Keep everything in one window.
(setq special-display-regexps (list ".*SPEEDBAR.*"))

;; Highlight current line.
(global-hl-line-mode 1)

;; The default for these limits is too low.
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 10000)

;; Don't add newlines by default.
(setq-default require-final-newline nil)

;; Only let files set "safe" variables.
(setq enable-local-variables 'safe)

;; Put *scratch* in emacs-lisp-mode. (Using custom-set-variables does
;; not seem to work for this in Aquamacs 2.1.)
(setq initial-major-mode 'emacs-lisp-mode)

;; Detect Rails projects.
(rails-mode 1)

;; Meaningful buffer names when basenames aren't unique.
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward))

;; No thanks.
(when (functionp 'global-smart-spacing-mode)
  (global-smart-spacing-mode 0))
(remove-hook 'text-mode-hook 'smart-spacing-mode)

;; Turn off electric indent.
(when (functionp 'electric-indent-mode)
  (electric-indent-mode 0))

;; Insertions should replace an active selection.
(delete-selection-mode 1)

;; Save & restore open files.
(desktop-save-mode 1)

;; Mode line
(line-number-mode 1)
(column-number-mode 1)
(setq mode-line-position-column-line-format (list " L%l C%c"))

(when aquamacsp
  (tabbar-mode 0)
  ;; Prevent Aquamacs from opening every buffer in a new frame.
  (one-buffer-one-frame-mode 0)

  ;; Used fixed-width font for text modes.
  (aquamacs-autoface-mode 0)

  ;; smart-spacing-mode messes up my indenting.
  (global-smart-spacing-mode 0))

(provide 'g-features)
