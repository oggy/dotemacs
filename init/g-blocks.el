(defun g-end-block ()
  "Insert an indented block end delimiter on its own line.

If the current line contains only whitespace, insert it on the current
line.  Otherwise, insert it on a new line after the current line.

Insert a newline after the block end delimiter and leave point after
it."
  (interactive)
  (let ((delim (get major-mode 'g-block-end-delimiter)))
    (if (null delim)
        (error (format "no block-end delimiter defined for %s" major-mode))
      (unless (string-match "^[ \\t\\r\\f]*$"
                            (buffer-substring (point-at-bol) (point-at-eol)))
        (end-of-line)
        (insert ?\n))
      (insert delim)
      (indent-for-tab-command))))

(put 'c-mode 'g-block-end-delimiter "}")
(put 'css-mode 'g-block-end-delimiter "}")
(put 'd-mode 'g-block-end-delimiter "}")
(put 'c++-mode 'g-block-end-delimiter "}")
(put 'java-mode 'g-block-end-delimiter "}")
(put 'javascript-mode 'g-block-end-delimiter "}")
(put 'julia-mode 'g-block-end-delimiter "end")
(put 'lua-mode 'g-block-end-delimiter "end")
(put 'octave-mode 'g-block-end-delimiter "end;")
(put 'ruby-mode 'g-block-end-delimiter "end")
(put 'scala-mode 'g-block-end-delimiter "}")

(provide 'g-blocks)
