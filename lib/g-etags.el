(eval-and-compile (require 'em-glob))

(defvar g-etags-bin "ctags"
  "Path to etags executable.")

;;;###autoload
(defun g-etags-make (glob out)
  "Output a tags file OUT for files matching GLOB.

Uses variable `g-etags-bin'."
  (interactive (let* ((file-name (buffer-file-name)))
                 (list
                  (let* ((dir-name (if file-name (file-name-directory file-name) nil))
                         (ext (if file-name (file-name-extension file-name) nil))
                         (initial (if file-name (concat dir-name "**/*" (if ext (concat "." ext) nil)))))
                    (read-string "Files (glob): " initial))
                  (let ((initial (if file-name
                                     (concat (file-name-directory file-name) "TAGS")
                                   (expand-file-name "~/TAGS"))))
                    (read-string "Output file: " initial)))))
  (require 'etags)
  (setq out (expand-file-name out))
  (if (file-exists-p out) (delete-file out))
  (let* ((paths (eshell-extended-glob glob)))
    (apply 'call-process g-etags-bin nil nil nil
           (nconc '("-e") paths (list "-o" out)))))

(defvar g-etags-bin "ctags"
  "Path to etags executable.")

(provide 'g-etags)
