(defun auto-mode (mode &rest things)
  "Add entries to `auto-mode-alist' for the given MODE and THINGS.

THINGS can be symbols or regexps.  A string means any file name
matching that regexp should be loaded with MODE.  A symbol means
any file with that extension should be loaded with MODE."
 (mapc (lambda (thing)
         (let ((regexp (if (symbolp thing)
                            (concat "\\." (symbol-name thing) "$")
                         thing)))
           (add-to-list 'auto-mode-alist (cons regexp mode))))
       things))

(defun interpreter-mode (mode &rest regexps)
  "Add (MODE . REGEXP) entries to `interpreter-mode-alist'."
  (mapc (lambda (regexp)
          (add-to-list 'interpreter-mode-alist (cons regexp mode)))
        regexps))

(auto-mode 'c++-mode 'ut)
(auto-mode 'css-mode 'scss)
(auto-mode 'coffee-mode 'coffee '_coffee "Cakefile")
(auto-mode 'octave-mode 'm)
;(auto-mode 'objc-mode 'm)
(auto-mode 'd-mode 'd 'di)
(auto-mode 'ruby-mode 'rb 'rjs 'rake 'rsel 'god 'to_json "[Rr]akefile$" "Capfile" "Gemfile" ".autotest")
(auto-mode 'feature-mode 'feature)
(auto-mode 'html-mode 'html.erb)
(auto-mode 'js-mode 'js 'json '_js 'htc "Jakefile")
(auto-mode 'yaml-mode 'yml 'yaml 'yml.sample)
(auto-mode 'html-mode 'html 'htm 'rhtml)
(auto-mode 'haml-mode 'haml)
(auto-mode 'sass-mode 'sass)
(auto-mode 'markdown-mode 'markdown 'md)
(auto-mode 'erlang-mode 'erl)
(auto-mode 'pman-mode "^/?pman_trace")
(auto-mode 'metafont-mode "\\.mf\\'")
(auto-mode 'metapost-mode "\\.mp\\'")
(auto-mode 'conf-mode 'cnf ".gitconfig")  ; e.g., my.cnf
(auto-mode 'scala-mode 'scala)

(interpreter-mode 'js-mode "node")

(provide 'g-auto-modes)
