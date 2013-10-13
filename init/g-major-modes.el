(defmacro g-when-starting-mode (mode &rest forms)
  "Wrap FORMS in a named function, and add it to MODE's hook.

Since the function is named, and the function is only added to
the hook if it is not there yet, a call to `g-when-starting-mode'
can be re-evaluated any time after editing it."
  (let ((function-symbol (intern (concat "g-customize-" (symbol-name mode) "-mode")))
        (hook-symbol (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,function-symbol ()
         ,(concat "Customize " (symbol-name mode) ".\n\n"
                  "(This function was created by `g-when-starting-mode'.)")
         ,@forms)
       (add-hook ',hook-symbol ',function-symbol)
       )))

(defmacro g-define-mode-keys (mode &rest bindings)
  "Add BINDINGS to MODE's keymap.

BINDINGS is a plist of the form (KEY DEF KEY DEF ...).  KEY is of
the form accepted by `kbd'.  DEF is of the form accepted by
`define-key'."
  (let ((function-symbol (intern (concat "g-customize-" (symbol-name mode) "-mode-map")))
        (map-symbol (intern (concat (symbol-name mode) "-mode-map")))
        (hook-symbol (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,function-symbol ()
         ,(concat "Customize " (symbol-name mode) "-map.\n\n"
                  "(This function was created by `g-define-mode-keys'.)")
         (g-plist-each (list ,@bindings)
                     (lambda (key def)
                       (define-key ,map-symbol (read-kbd-macro key) def))))
       (add-hook ',hook-symbol ',function-symbol))))

;;;; Shell

(g-when-starting-mode shell
 (ansi-color-for-comint-mode-on))

;;;; C

(g-define-mode-keys c
  "C-c j"   'c-comment-edit
  "C-c C-f" (lambda () (interactive) (compile "make")))

;;;; C++

(g-define-mode-keys c++
  "TAB"      'g-indent-for-tab-command)

(put 'c++-mode 'g-tab-command 'c-indent-command)

;; must come after activating font-lock-mode in the hook
(add-hook 'c++-mode-hook 'g-cc-init-buffer t)

;;;; CoffeeScript

(defvar coffee-tab-width 2)
(g-when-starting-mode coffee
  (setq coffee-tab-width 2))
(g-define-mode-keys coffee
  "RET"  'newline
  "\C-m" 'newline
  "C-c l" 'g-coffee-insert-console-log
  "\C-c \C-c" 'g-coffee-visit-js)

(defun g-coffee-insert-console-log ()
  "Insert 'console.log '."
  (interactive)
  (insert "console.log "))

(defun g-coffee-visit-js ()
  (interactive)
  (let ((coffee-path (buffer-file-name))
        js-path)
    (if (string-match "\\.coffee\\'" coffee-path)
        (progn
          (if (buffer-modified-p)
              (error "Save buffer first."))
          (call-process "coffee" nil nil nil "-c" (buffer-file-name))
          (setq js-path (replace-match ".js" t t coffee-path))
          (switch-to-buffer (find-file-noselect js-path t))
          (find-file js-path))
      (error "Cannot deduce javascript name because this file does not end in '.coffee'"))))

;;;; Comint (inferior shells)

(add-hook 'comint-exec-hook 'g-comint-clear-query-on-exit)

(defun g-comint-clear-query-on-exit ()
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))

;;;; Diff

(defun g-diff-apply-and-kill-hunk ()
  "Apply the current hunk and remove it from the buffer."
  (interactive)
  (save-excursion (diff-apply-hunk))
  (diff-hunk-kill))

(g-define-mode-keys diff
  "C-c C-k" 'g-diff-apply-and-kill-hunk)

;;;; Java

(g-when-starting-mode java
  (c-set-style "gnu"))

;; Feature (Cucumber)

(defun g-feature-default-feature-name ()
  (let* ((base-name (file-name-nondirectory (buffer-file-name)))
         (base-name-without-extension (replace-in-string base-name "\.[^.]*$" ""))
         (name (replace-in-string base-name-without-extension "_+" " ")))
    (capitalize name)))

(defun g-feature-insert-skeleton ()
  "Insert a default skeleton for the feature file."
  (let ((feature-name (g-feature-default-feature-name)))
    (insert "Feature: " feature-name "

  As a USER")
    (save-excursion (insert "
  I want to BLAH
  So I can WIBBLE

  Scenario: NAME"))))

(g-when-starting-mode feature
  (if (zerop (buffer-size))
    (g-feature-insert-skeleton)))

;; JS

(g-define-mode-keys js
  "C-c l" 'g-js-insert-console-log
  "C-c C-c" 'g-js-visit-coffee)

(g-when-starting-mode js
  (setq js-indent-level 2))

(defun g-js-insert-console-log ()
  "Insert 'console.log()', and place point between the parentheses."
  (interactive)
  (insert "console.log(")
  (save-excursion (insert ");")))

(defun g-js-visit-coffee ()
  (interactive)
  (let ((js-path (buffer-file-name))
        coffee-path)
    (if (string-match "\\.js\\'" js-path)
        (progn
          (setq coffee-path (replace-match ".coffee" t t js-path))
          (find-file coffee-path))
      (error "Cannot deduce javascript name because this file does not end in '.coffee'"))))

;; HTML

(g-define-mode-keys html
  "TAB" 'g-indent-for-tab-command)

(put 'html-mode 'g-tab-command 'sgml-indent-or-tab)

;;;; CSS

(add-hook 'css-mode-hook 'cssm-leave-mirror-mode)

;;;; Markdown

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

;;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;; Metafont, Metapost
(autoload 'metafont-mode "meta-mode" "Metafont editing mode." t)
(autoload 'metapost-mode "meta-mode" "MetaPost editing mode." t)

;;;; Manual-mode

(g-define-mode-keys Manual
  "M-RET" 'scroll-down-one
  "C-q"   (simple-command (kill-buffer (current-buffer)))
  "C-w"   (simple-command (kill-buffer (current-buffer)) (delete-window)))

;;;; Octave

(g-when-starting-mode octave
  (setq octave-comment-char ?%)
  (setq octave-comment-start "% ")
  (modify-syntax-entry ?. "." octave-mode-syntax-table))

;;;; Perl

(g-when-starting-mode cperl
  (put 'cperl-mode 'g-tab-command 'cperl-indent-command))

(g-define-mode-keys cperl
  "TAB" 'g-indent-for-tab-command)

;;;; Python

(g-when-starting-mode python
  (modify-syntax-entry ?_ "_" py-mode-syntax-table)
  (put 'python-mode 'g-breakpoint-code "import nose; nose.tools.set_trace()"))

;;;; Ruby

(require 'inf-ruby)
;; For some ghastly reason, inf-ruby.el adds to ruby-mode-hook a
;; lambda that calls inf-ruby-keys, which resets the default
;; ruby-mode-map.
(setq ruby-mode-hook nil)

(g-when-starting-mode ruby
  ;(put 'ruby-mode 'g-tab-command 'ruby-indent-command)
  (put 'ruby-mode 'g-tab-command 'ruby-indent-line)
  (setq comment-start "#")
  (setq ruby-deep-arglist nil)           ; t?
  (setq ruby-deep-indent-paren '(?\( t))
  (setq ruby-deep-indent-paren-style 'space)
  (put 'ruby-mode 'g-breakpoint-code "require 'debugger'; debugger"))

(g-define-mode-keys ruby
  "C-c C-l" 'g-ruby-load-buffer
  "C-c s"   'g-ruby-insert-heading
  "TAB"     'g-indent-for-tab-command)

;;;; Scala

(g-define-mode-keys scala
  "C-c C-l" 'g-scala-load-file)

;;;; Provide

(provide 'g-major-modes)
