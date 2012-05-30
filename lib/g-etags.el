(eval-and-compile (require 'em-glob))

;; For some reason, the built-in etags lib is not being loaded. We
;; only use the etags-bin variable from it, so just define it here.
(defvar etags-bin "etags"
  "The etags executable.")

;;;###autoload
(defun g-etags-make-tags-file (glob out)
  "Output a tags file OUT for files matching GLOB.

Uses variable `etags-bin'."
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
  (setq out (expand-file-name out))
  (if (file-exists-p out) (delete-file out))
  (let* ((paths (eshell-extended-glob glob))
         (progress-reporter (make-progress-reporter "Etagging files..."
                                                    0 (length paths))))
    (while paths
      (call-process etags-bin nil nil nil "-a" (car paths) "-o" out)
      (setq paths (cdr paths)))
    (progress-reporter-done progress-reporter)))

(provide 'g-etags)
