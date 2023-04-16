(defun g-transpose-line-down (arg)
  "Move current line one line down, leaving point at beginning of that line.

This can be run repeatedly to move the current line down a number of lines."
  (interactive "*p")
  (end-of-line)
  (if (eobp)
      (newline)
    (forward-char 1))
  (transpose-lines arg)
  (forward-line -1))

(defun g-exchange-point-and-mark (&optional activatep)
  "`exchange-point-and-mark', but region isn't activated by default.

A prefix arg causes the region to be activated, like the regular
`exchange-point-and-mark'."
  (interactive "P")
  (exchange-point-and-mark (not activatep)))

(defun g-delete-horizontal-space (&optional dir)
  "`delete-horizontal-space', but a prefix may control direction.

If DIR is nil, delete all horizontal space around point.  If DIR < 0,
delete horizontal space before point.  If DIR >= 0, delete horizontal
space after point."
  (interactive "P")
  (cond ((null dir)
         (delete-horizontal-space))
        ((>= (prefix-numeric-value dir) 0)
         (delete-region (point) (progn (skip-chars-forward " \t") (point))))
        ((< (prefix-numeric-value dir) 0)
         (delete-region (save-excursion (skip-chars-backward " \t") (point)) (point)))))

(defun g-indent-line-or-region ()
  (interactive)
  (let ((indent "  "))
    (save-excursion
      (save-restriction
        (if (region-active-p)
            (narrow-to-region (region-beginning) (region-end))
          (narrow-to-region (point-at-bol) (point-at-eol)))
        (goto-char (point-min))
        (while (not (eobp))
          (unless (= (point-at-bol) (point-at-eol))
            (save-excursion (insert indent)))
          (forward-line 1))))
    (setq deactivate-mark nil)))

(defun g-outdent-line-or-region ()
  (interactive)
  (let ((indent "  "))
    (save-excursion
      (save-restriction
        (if (region-active-p)
            (narrow-to-region (region-beginning) (region-end))
          (narrow-to-region (point-at-bol) (point-at-eol)))
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at (regexp-quote indent))
            (delete-char (length indent)))
          (forward-line 1))))
    (setq deactivate-mark nil)))

(defun g-comment-or-uncomment (&optional s e n)
  "Comment or uncomment a region.

Like comment-or-uncomment-region, but if no region is active,
comment or uncomment the current line.  Also, commented lines are
indented, and comment stripping (that is, a negative prefix arg)
works for indented comment lines.

TODO: implement indented comment lines -- might be easier to rewrite
comment-region from scratch."
  (interactive)
  (if (region-active-p)
      (setq s (region-beginning)
            e (region-end))
    (setq s (point-at-bol)
          e (point-at-eol)))
  (comment-or-uncomment-region s e (or n current-prefix-arg)))

(defun g-chmod (argstr)
  "Chmod the current buffer's file."
  (interactive "sMode: ")
  (shell-command (concat "chmod " argstr " " (buffer-file-name))))

(defun g-insert-char-by-number (arg)
  "Insert the char with ascii value ARG."
  (interactive "p")
  (insert arg))

(defun g-char-int-at-pt ()
  "Echo the int value of the char at point."
  (interactive)
  (message "%s" (char-int (char-after (point)))))

(defun g-toggle-debug-on-error ()
  "Toggle `debug-on-error'."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to %s" debug-on-error))

(defun g-save-buffer-rename ()
  "Save this buffer as a new file and remove the old file."
  (interactive)
  (let ((file (buffer-file-name)))
    (call-interactively 'write-file)
    (when (and (file-exists-p file)
               (not (string= (file-truename file)
                             (file-truename (buffer-file-name)))))
      (set-file-modes (buffer-file-name)
                      (file-modes file))
      (delete-file file))
    (message "File renamed (mode = %o)." (file-modes (buffer-file-name)))))

(defun g-reload-buffer ()
  "Reload the current buffer's file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and filename
             (file-regular-p filename))
        (progn
          (kill-buffer nil)
          (find-file filename))
      (error "No such file: %s" filename))))

(defun g-delete-this-file (&optional arg)
  "Delete the file this buffer is visiting."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (if (file-exists-p file)
        (progn
          (delete-file file)
          (message "Bye-bye %s" file))
      (message "File doesn't exist: %s" file))))

(defun g-nuke-carriage-returns ()
  "Nuke all EOL CRs.

Does not affect the modifiedness for readonly buffers."
(interactive)
(save-excursion
  (goto-char (point-min))
  (let ((was-read-only-p buffer-read-only)
        (was-modified-p (buffer-modified-p)))
    (setq buffer-read-only nil)
    (while (re-search-forward "\\(\r\\)\\(\n\\|$\\)" nil t)
      (delete-region (match-beginning 1)
                     (match-end 1)))
    (when was-read-only-p
      (set-buffer-modified-p was-modified-p))
    (setq buffer-read-only was-read-only-p))))

(defun g-insert-breakpoint ()
  "Insert a breakpoint.

Requires the major-mode symbol to have a `g-breakpoint-code'
property."
  (interactive)
  (let ((code (get major-mode 'g-breakpoint-code)))
    (if (null code)
        (error (format "no breakpoint code defined for %s" major-mode))
      (unless (string-match "^[ \\t\\r\\f]*$"
                            (buffer-substring (point-at-bol) (point-at-eol)))
        (end-of-line)
        (insert ?\n))
      (insert code)
      (indent-for-tab-command))))

(defun g-copy-relative-path ()
  "Copy path to kill ring & clipboard."
  (interactive)
  (let* ((absolute-path (buffer-file-name))
         (prefix (concat g-start-dir "/"))
         (relative-path (if (and prefix
                                 (string= (substring absolute-path 0 (length prefix)) prefix))
                           (substring absolute-path (length prefix))
                         absolute-path)))
    (kill-new relative-path)
    (message "%s" relative-path)))

(defun g-copy-absolute-path ()
  "Copy path to kill ring & clipboard."
  (interactive)
  (kill-new (buffer-file-name))
  (message "%s" (buffer-file-name)))

(provide 'g-commands)
