;;;; Minor mode for buffers in a Rails application.

(defvar rails-mode nil
  "Non-nil iff Rails mode is enabled.")

(defvar rails-root nil
  "Root of the Rails app this file belongs to (buffer local).")
(make-variable-buffer-local 'rails-root)

(defvar rails-templates-directory "~/.emacs.d/etc/rails/templates"
  "Directory containing templates for rails-mode.")

;;;###autoload
(defun rails-mode (&optional arg)
  "Rails minor mode."
  (interactive)
  (let ((old-rails-mode rails-mode))
    (setq rails-mode
          (if (null arg) (not rails-mode)
            (> (prefix-numeric-value arg) 0)))
    (cond ((and (not old-rails-mode) rails-mode)
           (rails-mode-activate))
          ((and old-rails-mode (not rails-mode))
           (rails-mode-deactivate)))))

(defun rails-mode-activate ()
  (add-hook 'find-file-hook 'rails-initialize-buffer))

(defun rails-mode-deactivate ()
  (remove-hook 'find-file-hook 'rails-initialize-buffer))

(defun rails-root (path)
  "Return the root of the Rails app containing PATH."
  (let ((root (rails-project-root path)))
    (when (and root (or (file-exists-p (concat root "/script/rails"))
                        (rails-is-rails-gemfile (concat root "/Gemfile"))))
      root)))

(defun rails-is-rails-gemfile (path)
  (and (file-exists-p path)
       (with-temp-buffer
         (insert-file-contents path)
         (goto-char (point-min))
         (search-forward-regexp "^ *gem *[\"']rails[\"']" nil t))))

(defun rails-project-root (path)
  "Return the root of the Ruby project containing PATH, or nil if none."
  (setq path (expand-file-name path))
  (while (and path (not (file-exists-p (concat path "/Gemfile"))))
    (if (string= path "/")
        (setq path nil)
      (setq path (file-name-directory path))
      (unless (string= path "/")
        (setq path (replace-regexp-in-string "/$" "" path)))))
  path)

(defun rails-initialize-buffer ()
  "Initialize the buffer for Rails mode."
  (interactive)
  (let* ((path (buffer-file-name))
         (computed-rails-root (rails-root path))
         constant-name)
    (when (and path computed-rails-root)
      (setq rails-root computed-rails-root)
      (when (rails-current-buffer-empty-p)
        (cond ((rails-under-path-p path "app/models")
               (rails-initialize-model-buffer path "app/models"))
              ((rails-under-path-p path "app/controllers")
               (rails-initialize-controller-buffer path "app/controllers"))
              ((rails-under-path-p path "app/helpers")
               (rails-initialize-helper-buffer path "app/helpers"))
              ((rails-under-path-p path "db/migrate")
               (rails-initialize-migration-buffer path "db/migrate")))))))

(defun rails-current-buffer-empty-p ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (not (re-search-forward "[^ \t\r\n\f\v]" nil 'no-error))))

(defun rails-under-path-p (path prefix)
  "Return true if PATH is under PREFIX of a Rails app."
  (string-prefix-p (concat rails-root "/" prefix "/") path))

(defun rails-initialize-model-buffer (path prefix)
  (let* ((relative-path (rails-strip-prefix path prefix))
         (stem (replace-regexp-in-string "\\.rb\\'" "" relative-path))
         (module (rails-camelize stem))
         (superclass (if (file-exists-p (concat rails-root "/app/models/application_model.rb"))
                         "ApplicationModel"
                       "ActiveRecord::Base"))
         (vars (make-hash-table :test 'equal)))
    (puthash "module" module vars)
    (puthash "superclass" superclass vars)
    (rails-load-template (concat rails-templates-directory "/model.rb") vars)))

(defun rails-initialize-controller-buffer (path prefix)
  (let* ((relative-path (rails-strip-prefix path prefix))
         (stem (replace-regexp-in-string "\\.rb\\'" "" (file-name-nondirectory relative-path)))
         (resources (replace-regexp-in-string "_controller\\'" "" stem))
         (resource (rails-singularize resources))
         (module (rails-camelize stem))
         (model (rails-camelize (rails-singularize resources)))
         (vars (make-hash-table :test 'equal)))
    (puthash "resources" resources vars)
    (puthash "resource" resource vars)
    (puthash "module" module vars)
    (puthash "model" model vars)
    (rails-load-template (concat rails-templates-directory "/controller.rb") vars)))

(defun rails-initialize-helper-buffer (path prefix)
  (let* ((relative-path (rails-strip-prefix path prefix))
         (stem (replace-regexp-in-string "\\.rb\\'" "" relative-path))
         (module (rails-camelize stem))
         (vars (make-hash-table :test 'equal)))
    (puthash "module" module vars)
    (rails-load-template (concat rails-templates-directory "/helper.rb") vars)))

(defun rails-initialize-migration-buffer (path prefix)
  (let* ((relative-path (rails-strip-prefix path prefix))
         (stem (replace-regexp-in-string "\\`\\(?:[0-9]+_\\)?\\(.*?\\)\\.rb\\'" "\\1" relative-path))
         (module (rails-camelize stem))
         (vars (make-hash-table :test 'equal)))
    (puthash "module" module vars)
    (rails-load-template (concat rails-templates-directory "/migration.rb") vars)))

(defun rails-load-template (path vars)
  (rails-insert-template path vars)
  (goto-char (point-min))
  (when (search-forward "-!-" nil t)
    (delete-char -3)))

(defun rails-insert-template (path vars)
  (insert-file-contents path)
  (goto-char (point-min))
  (let* ((quoted-names (rails-mapcar-hash (lambda (k v) (regexp-quote k)) vars))
         (vars-regexp (concat "#{\\(" (rails-list-join quoted-names "\\|") "\\)}")))
    (while (search-forward-regexp vars-regexp nil t)
      (replace-match (gethash (match-string 1) vars)))))

(defun rails-new-migration (name)
  (interactive "MMigration name: ")
  (let* ((underscored-name (replace-regexp-in-string "[^A-Za-z0-9]+" "_" name))
         (module (rails-camelize underscored-name))
         (vars (make-hash_table :test 'equal)))
    (find-file (concat rails-root "/db/migrate/" timestamp "_" underscored-name ".rb"))
    (puthash "module" module vars)
    (rails-load-template (concat rails-templates-directory "/migration.rb") vars)))

;;;; ActiveSupport

(defun rails-camelize (string)
  (let ((segments (split-string string "/" 'omit-nulls)))
    (mapconcat 'rails-camelize-segment segments "::")))

(defun rails-camelize-segment (string)
  (replace-regexp-in-string "\\(?:\\`\\|_\\)\\([a-z]\\)"
                            (lambda (match) (upcase (match-string 1 match)))
                            string
                            'fixed-case))

(defun rails-singularize (string)
  ;; Based on activesupport's inflections.rb.
  (or
   (rails-replace-or-nil "\\`people\\'" "person" string)
   (rails-replace-or-nil "\\`men\\'" "man" string)
   (rails-replace-or-nil "\\`children\\'" "child" string)
   (rails-replace-or-nil "\\`sexes\\'" "sex" string)
   (rails-replace-or-nil "\\`moves\\'" "move" string)
   (rails-replace-or-nil "\\`kine\\'" "cow" string)

   (rails-replace-or-nil "\\(database\\)s\\'" "\\1" string)
   (rails-replace-or-nil "\\(quiz\\)zes\\'" "\\1" string)
   (rails-replace-or-nil "\\(matr\\)ices\\'" "\\1ix" string)
   (rails-replace-or-nil "\\(vert\\|ind\\)ices\\'" "\\1ex" string)
   (rails-replace-or-nil "\\`\\(ox\\)en\\'" "\\1" string)
   (rails-replace-or-nil "\\(alias\\|status\\)es\\'" "\\1" string)
   (rails-replace-or-nil "\\(octop\\|vir\\)i\\'" "\\1us" string)
   (rails-replace-or-nil "\\(cris\\|ax\\|test\\)es\\'" "\\1is" string)
   (rails-replace-or-nil "\\(shoe\\)s\\'" "\\1" string)
   (rails-replace-or-nil "\\(o\\)es\\'" "\\1" string)
   (rails-replace-or-nil "\\(bus\\)es\\'" "\\1" string)
   (rails-replace-or-nil "\\([ml]\\)ice\\'" "\\1ouse" string)
   (rails-replace-or-nil "\\(x\\|ch\\|ss\\|sh\\)es\\'" "\\1" string)
   (rails-replace-or-nil "\\(m\\)ovies\\'" "\\1ovie" string)
   (rails-replace-or-nil "\\(s\\)eries" "\\1eries" string)
   (rails-replace-or-nil "\\([^aeiouy]\\|qu\\)ies" "\\1y" string)
   (rails-replace-or-nil "\\([lr]\\)ves\\'" "\\1f" string)
   (rails-replace-or-nil "\\([ht]ive\\)s\\'" "\\1" string)
   (rails-replace-or-nil "\\([^f]\\)ves" "\\1fe" string)
   (rails-replace-or-nil "\\(analy\\|ba\\|diagno\\|parenthe\\|progno\\|synop\\|the\\)ses\\'" "\\1sis" string)
   (rails-replace-or-nil "\\([ti]\\)a\\'" "\\1um" string)
   (rails-replace-or-nil "\\(n\\)ews\\'" "\\1ews" string)
   (rails-replace-or-nil "s\\'" "" string)
   string))

;;;; Utility

(defun rails-strip-prefix (string prefix)
  (replace-regexp-in-string (regexp-quote (concat rails-root "/" prefix "/")) "" string))

(defun rails-replace-or-nil (regexp replacement string)
  (if (string-match regexp string)
      (replace-match replacement nil nil string)
    nil))

(defun rails-mapcar-hash (function table)
  (let (results)
    (maphash (lambda (key value)
               (setq results (cons (funcall function key value) results)))
             table)
    (reverse results)))

(defun rails-list-join (list delimiter)
  (let ((length (length list)))
    (if (null list)
        ""
      (rails-list-inject (mapcar (lambda (s) (format "%s" s)) (cdr list))
                         (format "%s" (car list))
                         (lambda (string result) (concat result delimiter string))))))

(defun rails-list-inject (list initial function)
  (while list
    (setq initial (funcall function (car list) initial)
          list (cdr list)))
  initial)
