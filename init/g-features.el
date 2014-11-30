(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; html-helper-mode kills indenting
(setq magic-mode-alist
      (delq nil
            (mapcar (lambda (pair) (if (eq (cdr pair) 'html-helper-mode) nil pair))
                    magic-mode-alist)))

;; Ensure buffer switching available all the time.
(g-pc-bufsw-mode)

;; Enable redo.
(require 'redo)

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
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; enable minibuffer resizing
(when (boundp 'resize-minibuffer-mode)
  (resize-minibuffer-mode 1))

;; Enable narrowing.
(put 'narrow-to-region 'disabled nil)

;; Keep autosaves in one place.
(setq auto-save-directory (concat rc-dir "/autosaves"))

;; Enable transient-mark-mode (region highlighting).
(transient-mark-mode)

;; Enable show-paren-mode.
(show-paren-mode 1)

;; Make cursor blink.
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

;; Start server.
(unless (server-running-p)
  (server-start))

(when aquamacsp
  ;; Prevent Aquamacs from opening every buffer in a new frame.
  (one-buffer-one-frame-mode 0)

  ;; Used fixed-width font for text modes.
  (aquamacs-autoface-mode 0)

  ;; smart-spacing-mode messes up my indenting.
  (global-smart-spacing-mode 0))

(provide 'g-features)
