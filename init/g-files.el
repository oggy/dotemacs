;; Prompt to make directory automatically.
(defun g-find-file-make-directory-prompt ()
  "Prompt to make the directory containing current buffer file."
  (let ((file-path (buffer-file-name)))
    (when file-path
      (let ((file-dir (file-name-directory file-path)))
        (unless (file-exists-p file-dir)
          (when (yes-or-no-p "Directory does not exist - create? ")
            (make-directory file-dir)))))))

(add-hook 'find-file-hook 'g-find-file-make-directory-prompt t)

(provide 'g-files)
