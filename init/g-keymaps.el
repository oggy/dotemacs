(when aquamacsp
  ;; Remove osx-key bindings that I keep hitting by accident.
  (define-key osx-key-mode-map (kbd "A-q") nil)
  (define-key osx-key-mode-map (kbd "A-w") nil)
  (define-key osx-key-mode-map (kbd "A-n") nil)
  (define-key osx-key-mode-map (kbd "A-t") nil)
  (define-key osx-key-mode-map (kbd "A-p") nil)
  (define-key osx-key-mode-map (kbd "A-<backspace>") nil)
  (define-key osx-key-mode-map (kbd "C-x w") nil))

;;; Overrides (or non-\C-c)
(global-set-key (kbd "C-x w")   'g-save-buffer-rename)
(global-set-key (kbd "C-x C-x") 'g-exchange-point-and-mark)
(global-set-key (kbd "C-h C-a") 'apropos-documentation)
(global-set-key (kbd "M-`")     'lisp-complete-symbol)
(global-set-key (kbd "C-?")     'repeat-complex-command)
(global-set-key (kbd "M-\\")    'g-delete-horizontal-space)
(global-set-key (kbd "M-n")     'vertical-next-line)
(global-set-key (kbd "M-p")     'vertical-previous-line)
(global-set-key (kbd "M-RET")   'g-end-block)

;; Prefer Aquamacs undo/redo, fallback to vendor/redo.el.
(if (fboundp 'aquamacs-redo)
    (progn
      (global-set-key (kbd "C-/") 'aquamacs-undo)
      (global-set-key (kbd "C-'") 'aquamacs-redo))
  (global-set-key (kbd "C-'") 'redo))

(global-set-key (kbd "C-x /")   'point-to-register)
(global-set-key (kbd "C-x j")   'jump-to-register)
(global-set-key (kbd "C-TAB")   'g-select-next-window)
(global-set-key (kbd "C-S-TAB") 'g-select-previous-window)
(global-set-key (kbd "C-S-t")   'g-transpose-line-down)
(global-set-key (kbd "M-g")     'goto-line)

;; Make the quit sequence less accident-prone.
(global-unset-key (kbd "C-x C-c"))
;(global-set-key (kbd "C-x C-c C-x C-c") 'save-buffers-kill-emacs)  ;; this gives a warning on Aquamacs 2 - why?
(define-key (current-global-map) (kbd "C-x C-c C-x C-c") 'save-buffers-kill-emacs)

;;; Modes
(global-set-key (kbd "C-c m a") 'js-mode)
(global-set-key (kbd "C-c m c") 'c-mode)
(global-set-key (kbd "C-c m d") 'diff-mode)
(global-set-key (kbd "C-c m e") 'emacs-lisp-mode)
(global-set-key (kbd "C-c m h") 'html-mode)
(global-set-key (kbd "C-c m j") 'java-mode)
(global-set-key (kbd "C-c m l") 'erlang-mode)
(global-set-key (kbd "C-c m m") 'makefile-mode)
(global-set-key (kbd "C-c m o") 'objc-mode)
(global-set-key (kbd "C-c m p") 'perl-mode)
(global-set-key (kbd "C-c m q") 'fundamental-mode)
(global-set-key (kbd "C-c m r") 'ruby-mode)
(global-set-key (kbd "C-c m s") 'sh-mode)
(global-set-key (kbd "C-c m x") 'c++-mode)
(global-set-key (kbd "C-c m y") 'python-mode)

;;; Visit files
(global-set-key (kbd "C-c f i") (g-simple-command (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c f c") (g-simple-command (find-file "~/.emacs.d/custom.el")))
(global-set-key (kbd "C-c f s") (g-simple-command (find-file "~/.emacs.d/site.el")))

;;; Other
(global-set-key (kbd "C-c =")   'smart-align)
(global-set-key (kbd "C-c c")   'g-comment)
(global-set-key (kbd "C-c d")   'g-insert-breakpoint)
(global-set-key (kbd "C-c i")   'manual-entry)
(global-set-key (kbd "C-c e")   'eval-current-buffer)
(global-set-key (kbd "C-c q")   'g-insert-char-by-number)
(global-set-key (kbd "C-c x")   'g-toggle-debug-on-error)
(global-set-key (kbd "C-c R")   'g-reload-buffer)
(global-set-key (kbd "C-c b")   'browse-url-at-point)
(global-set-key (kbd "C-c s")   'shell)
(global-set-key (kbd "C-c TAB") 'g-toggle-tab-width)
(global-set-key (kbd "C-c X X") 'g-delete-this-file)
(global-set-key (kbd "C-x SPC") 'g-reselect-region)
(global-set-key (kbd "C-c ?")   'g-char-int-at-pt)
(global-set-key (kbd "C-c \\")  'slosh)

(when (featurep 'x)
  (global-set-key [button4] (g-simple-command (scroll-down 4)))
  (global-set-key [button5] (g-simple-command (scroll-up 4)))
  (global-set-key [(control button4)] (g-simple-command (scroll-down 1)))
  (global-set-key [(control button5)] (g-simple-command (scroll-up 1)))
  (global-set-key [(shift button4)] (g-simple-command (scroll-down 10)))
  (global-set-key [(shift button5)] (g-simple-command (scroll-up 10))))

(provide 'g-keymaps)
