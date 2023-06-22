(defvar aquamacsp (string-match "Aquamacs" (emacs-version))
  "Return true iff this is Aquamacs Emacs.")

(defun replace-in-string (target old new &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string, 
 and returns the new string."
  (replace-regexp-in-string old new target nil literal))

(defmacro g-simple-command (&rest forms)
  "Return a command that takes no arguments and runs `forms'."
  `(lambda () (interactive) ,@forms))

(defun g-plist-each (plist function)
  "Call Function with each name-value pair of PLIST."
  (let ((i 0)
        (length (length plist)))
    (while (< i length)
      (funcall function (nth i plist) (nth (1+ i) plist))
      (setq i (+ i 2)))))

(defun g-directory-files-recursive (dir &optional match)
  "Return all file names under DIR.

If MATCH is non-nil, only mention files matching that regexp."
  (let ((output (g-shell-command-output
                 "find" (expand-file-name dir) "-type" "f")))
    (cl-delete-if (lambda (path)
                    (and match (not (string-match match path))))
                  (split-string output "\n" t))))

(defun g-shell-command-output (program &rest args)
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)
    (buffer-string)))

(defun g-titleize (string)
  (if (string-match "\\`[a-z0-9_-]+\\'" string)
    (string-join (mapcar 'capitalize (split-string string "[ \f\t\n\r\v_-]+")) " ")
    string))

(defun g-project-roots ()
  (delete-dups (remove nil (mapcar 'g-buffer-project-root (buffer-list)))))

(provide 'g-helpers)
