(require 'server)

;; Maintain a separate server per project.
;;
;; We write the project root(s) to a file so that bin/emacs can open
;; files using the right server.

(setq server-name (format "server-%d" (emacs-pid)))

(defvar g-server-roots-path (rc-path (format "servers/%s" server-name))
  "Path to our server file.")

(defvar g-server-roots-cache nil
  "The last value we wrote to the server file.")

(defun g-server-write-roots (&optional force)
  "Write the server file if the project roots have changed (or if FORCE)."
  (let* ((dir (file-name-directory g-server-roots-path))
         (roots (g-project-roots)))
    (when (or force (not (equal roots g-server-roots-cache)))
      (make-directory dir t)
      (with-temp-buffer
        (mapc (lambda (path) (insert path) (insert "\n")) roots)
        (write-file g-server-roots-path)
        (setq g-server-roots-cache roots)
        (kill-buffer)))))

(defun g-server-delete-roots ()
  "Delete the roots file."
  (when (file-exists-p g-server-roots-path)
    (delete-file g-server-roots-path)))

(add-hook 'buffer-list-update-hook 'g-server-write-roots)
(add-hook 'kill-emacs-hook 'g-server-delete-roots)

(unless server-process
  (server-start))

(provide 'g-server)
