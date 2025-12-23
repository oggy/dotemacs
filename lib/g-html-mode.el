(require 'sgml-mode)

;;;###autoload
(defun g-html-close-tag ()
  "Insert an indented closing tag on its own line."
  (interactive)
  (unless (string-match "^[ \\t\\r\\f]*$"
                        (buffer-substring (pos-bol) (pos-eol)))
    (end-of-line)
    (insert ?\n))
  (sgml-close-tag))

(provide 'g-html-mode)
