;;;; Private

(defun etags-select (list f)
  (let (result element)
    (while list
      (setq element (car list))
      (when (funcall f element)
	(setq result (cons element result)))
      (setq list (cdr list)))
    (reverse result)))

(defun etags-directory-files-recursive (dir &optional match)
  (let ((output (etags-shell-command-output
                 "find" (expand-file-name dir) "-type" "f")))
    (etags-select (split-string output "\n" t)
		  (lambda (path) (or (null match) (string-match match path))))))

(defun etags-shell-command-output (program &rest args)
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)
    (buffer-string)))

;;;; Public

(defvar etags-bin "etags"
  "The etags executable.")

;;;###autoload
(defun etags-make-tags-file (dir re out)
  "Make a tags file for a bunch of source files.

Prompt for:
  - the directory at the root of the source tree.
  - a regex to match target filenames.
  - the output filename

The given root directory is searched recursively to find files which
match the given regex.  Any existing tags file with the same name as
the output given is overwritten.

Uses variable `etags-bin'."
  (interactive "DRoot directory: \nsFile regex: \nFOutput files: ")
  (if (file-exists-p out) (delete-file out))
  (setq out (expand-file-name out))
  (let* ((paths (etags-directory-files-recursive dir re))
         (progress-reporter (make-progress-reporter "Etagging files..."
                                                    0 (length paths))))
    (while paths
      (call-process etags-bin nil nil nil "-a" (car paths) "-o" out)
      (setq paths (cdr paths)))
    (progress-reporter-done progress-reporter)))

;;;; Provide

(provide 'etags)
