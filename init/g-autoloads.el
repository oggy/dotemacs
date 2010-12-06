(defvar g-autoloads-dir (concat rc-dir "/var/autoloads")
  "Directory where autoload files are found.")

(mapc (lambda (path) (load-file path))
      (g-directory-files-recursive g-autoloads-dir ".el"))

(provide 'g-autoloads)
