(require 'server)

;; Maintain a separate server per project.
;;
;; We write g-start-dir to a file so that bin/open can open files using the
;; right server.

(setq server-name (format "server-%d" (emacs-pid)))

(defvar g-server-path (rc-path (format "servers/%s" server-name))
  "Path to our server file.")

(defun g-server-write-file ()
  "Write the server file."
  (let* ((dir (file-name-directory g-server-path))
         (roots g-start-dir))
    (make-directory dir t)
    (with-temp-buffer
      (insert g-start-dir "\n")
      (write-file g-server-path)
      (kill-buffer))))

(defun g-server-delete-roots ()
  "Delete the roots file."
  (when (file-exists-p g-server-path)
    (delete-file g-server-path)))

(add-hook 'kill-emacs-hook 'g-server-delete-roots)

(when (and g-start-dir (null server-process))
  (server-start)
  (g-server-write-file))

(provide 'g-server)
