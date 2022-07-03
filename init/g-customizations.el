;; NOTE: There must only be one `custom-set-variables' form.
(custom-set-variables
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 150 t)
 '(aquamacs-save-options-on-quit nil)
 '(backup-directory-alist (list (cons "" (concat rc-dir "/backups/"))))
 '(blink-cursor-interval 0.1)
 '(default-fill-column 80)
 '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
 '(paren-mode (quote blink-paren) nil (paren))
 '(scrollbars-visible-p nil)
 '(mark-even-if-inactive nil)
 '(size-indication-mode t)
 '(toolbar-visible-p nil)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(visible-bell t)
 '(user-email-address "george.ogata@gmail.com"))

(custom-set-faces
 '(default ((t (:background "black" :foreground "white" :family "Monaco" :height 140))))
 '(cperl-array-face ((t (:foreground "paleturquoise")) (((type tty)) (:foreground "blue"))))
 '(cperl-hash-face ((t (:foreground "red")) (((type tty)) (:foreground "red"))))
 '(cvs-filename-face ((t (:foreground "lightskyblue")) (((type tty)) (:foreground "cyan"))))
 '(cvs-handled-face ((t (:foreground "lightgreen")) (((type tty)) (:foreground "green"))))
 '(cvs-header-face ((t (:foreground "yellow" :bold t)) (((type tty)) (:foreground "yellow" :bold t))))
 '(cvs-marked-face ((t (:foreground "orange" :bold t)) (((type tty)) (:foreground "yellow" :bold t))))
 '(cvs-need-action-face ((t (:foreground "red" :bold t)) (((type tty)) (:foreground "red" :bold t))))
 '(cvs-unknown-face ((t (:foreground "grey")) (((type tty)) (:foreground "darkgrey"))))
 '(diff-added ((t (:foreground "lawngreen")) (((type tty)) (:foreground "green" :bold t))))
 '(diff-changed ((t (:foreground "changed")) (((type tty)) (:foreground "yellow"))))
 '(diff-file-header-face ((t (:underline t)) (((type tty)) (:underline t))))
 '(diff-function-face ((t (:foreground "white")) (((type tty)) (:foreground "white"))))
 '(diff-header-face ((t (:background "darkslategray" :foreground "white")) (((type tty)) (:background "darkgrey" :foreground "white" :bold t))))
 '(diff-hunk-header-face ((t (:background "darkslategrey" :foreground "white")) (((type tty)) (:background "darkgrey" :foreground "white" :bold t))))
 '(diff-index-face ((t (:background "lightslategray" :foreground "white")) (((type tty)) (:background "darkgrey" :foreground "white" :bold t))))
 '(diff-nonexistent-face ((t (:foreground "orange")) (((type tty)) (:foreground "yellow"))))
 '(diff-removed-face ((t (:foreground "firebrick")) (((type tty)) (:foreground "red"))))
 '(dired-face-directory ((t (:foreground "yellow" :bold t)) (((type tty)) (:foreground "yellow" :bold t))))
 '(dired-face-executable ((t (:foreground "green")) (((type tty)) (:foreground "green"))))
 '(font-latex-bold-face ((t (:bold t)) (((type tty)) (:bold t))))
 '(font-latex-italic-face ((t (:italic t)) (((type tty)) (:underline t))))
 '(font-latex-math-face ((t (:foreground "lightgreen")) (((type tty)) (:foreground "green"))))
 '(font-latex-sedate-face ((t (:foreground "gray")) (((type tty)) (:foreground "blue"))))
 '(font-latex-title-1-face ((t (:foreground "orange" :bold t)) (((type tty)) (:foreground "yellow" :bold t))))
 '(font-latex-title-2-face ((t (:foreground "orange" :bold t)) (((type tty)) (:foreground "yellow" :bold t))))
 '(font-latex-title-3-face ((t (:foreground "orange" :bold t)) (((type tty)) (:foreground "yellow" :bold t))))
 '(font-latex-title-4-face ((t (:foreground "orange" :bold t)) (((type tty)) (:foreground "yellow" :bold t))))
 '(font-lock-builtin-face ((t (:foreground "#DDDD99")) (((type tty)) (:foreground "brightblue"))))
 '(font-lock-comment-face ((t (:foreground "lightslateblue" :italic t)) (((type tty)) (:foreground "brightblue"))))
 '(font-lock-constant-face ((t (:foreground "lightseagreen")) (((type tty)) (:foreground "brightblue"))))
 '(font-lock-doc-string-face ((t (:foreground "green")) (((type tty)) (:foreground "green"))))
 '(font-lock-function-name-face ((t (:foreground "orange")) (((type tty)) (:foreground "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "lightsalmon")) (((type tty)) (:foreground "red"))))
 '(font-lock-preprocessor-face ((t (:foreground "tan")) (((type tty)) (:foreground "black" :background "blue"))))
 '(font-lock-reference-face ((t (:foreground "orangered")) (((type tty)) (:foreground "brightred"))))
 '(font-lock-string-face ((t (:foreground "lightgreen")) (((type tty)) (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "paleturquoise")) (((type tty)) (:foreground "cyan"))))
 '(font-lock-variable-name-face ((t (:foreground "#FFBBFF")) (((type tty)) (:foreground "magenta"))))
 '(highlight ((t (:background "seagreen")) (((type tty)) (:background "green"))) t)
 '(hl-line ((t (:background "#003")) (((type tty)) (:background "blue"))) t)
 '(html-helper-server-script-face ((t (:foreground "orange")) (((type tty)) (:foreground "yellow"))) t)
 '(html-helper-tag-face ((t (:foreground "deep sky blue")) (((type tty)) (:foreground "blue"))) t)
 '(hyper-apropos-documentation ((t (:foreground "lightcoral")) (((type tty)) (:foreground "red"))))
 '(hyper-apropos-hyperlink ((t (:foreground "deepskyblue")) (((type tty)) (:foreground "cyan"))))
 '(isearch ((t (:background "mediumblue")) (((type tty)) (:background "blue"))) t)
 '(isearch-secondary ((t (:foreground "red")) (((type tty)) (:foreground "red"))) t)
 '(man-heading ((t (:foreground "lightgoldenrod")) (((type tty)) (:foreground "yellow"))))
 '(match ((t (:foreground "red")) (((type tty)) (:foreground "red"))))
 '(message-header-cc-face ((t (:foreground "deepskyblue")) (((type tty)) (:foreground "deepskyblue"))))
 '(message-header-newsgroups-face ((t (:foreground "deepskyblue" :italic t)) (((type tty)) (:foreground "deepskyblue" :italic t))))
 '(message-header-subject-face ((t (:foreground "dodgerblue" :bold t)) (((type tty)) (:foreground "dodgerblue" :bold t))))
 '(message-header-to-face ((t (:foreground "deepskyblue" :bold t)) (((type tty)) (:foreground "deepskyblue" :bold t))))
 '(message-header-xheader-face ((t (:foreground "lightskyblue")) (((type tty)) (:foreground "lightskyblue"))))
 '(message-separator-face ((t (:foreground "peru")) (((type tty)) (:foreground "peru"))))
 '(minibuffer-prompt ((t (:foreground "cyan")) (((type tty)) (:foreground "peru"))))
 '(mode-line ((t (:background "darkslategray" :foreground "lightgreen")) (((type tty)) (:background "black" :foreground "green"))) t)
 '(mode-line-inactive ((t (:background "#061818" :foreground "#457745" :box nil)) (((type tty)) (:background "black" :foreground "yellow"))) t)
 '(mode-line-buffer-id ((t (:foreground "yellow")) (((type tty)) (:background "black" :foreground "yellow"))) t)
 '(modeline-mousable ((t (:foreground "orangered")) (((type tty)) (:background "black" :foreground "red"))) t)
 '(modeline-mousable-minor-mode ((t (:foreground "white")) (((type tty)) (:background "black" :foreground "white"))) t)
 '(paren-blink-off ((t nil) (((type tty)) nil)) t)
 '(secondary-selection ((t (:background "royalblue")) (((type tty)) (:background "blue"))) t)
 '(sh-heredoc ((t (:foreground "green")) (((type tty)) (:foreground "green"))))
 '(cursor ((t (:background "yellow" :foreground "black")) (((type tty)) (:background "brightyellow" :foreground "black"))) t))

(provide 'g-customizations)
