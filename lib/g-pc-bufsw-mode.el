;; Minor mode to keep pc-bufsw bound to M-TAB irrespective of the
;; major mode.  Some modes restore the default settings of C-TAB if
;; they're just global-set-key'ed in.
(defvar g-pc-bufsw-mode-map (make-sparse-keymap 'g-pc-bufsw-mode-map)
  "Keymap for `pc-bufsw-mode'.")

(define-key g-pc-bufsw-mode-map (kbd "M-]") 'pc-bufsw::previous)
(define-key g-pc-bufsw-mode-map (kbd "M-[") 'pc-bufsw::lru)

(defvar g-pc-bufsw-mode nil
  "Toggle var for g-pc-bufsw-mode.")

;;;###autoload
(defun g-pc-bufsw-mode (&optional arg)
  "Mode to enable g-pc-bufsw-mode bindings irrespective of major mode."
  (interactive)
  (setq g-pc-bufsw-mode
	(if (null arg) (not g-pc-bufsw-mode)
	  (> (prefix-numeric-value arg) 0))))

(add-minor-mode 'g-pc-bufsw-mode nil g-pc-bufsw-mode-map)
