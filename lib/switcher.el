(defvar switcher-menu-frame nil
  "The Switcher menu frame, while active.")

(defvar switcher-menu-buffer nil
  "The Switcher menu buffer, while active.")

(defvar switcher-home-window nil
  "The window to display the selected buffer in.")

(defface switcher-menu-item-face
  '((((class color)) :extend t))
  "Face used for items in Switcher menu.")

(defvar switcher-buffer-list nil
  "Ordered list of most recently switched-to buffers.

Using buffer-list seems to be affected by calls to
set-window-buffer, even when given a window that isn't the
current one.

Therefore, we maintain our own buffer list, which is not affected
by simply showing the window as we flip through items in the
menu.

This list won't necessarily contain all buffers, just the most
recently switched-to ones since switcher was loaded.")
;(setq switcher-buffer-list nil)

(defvar switcher-ignore-buffer-switch nil
  "Prevents buffer switches modifying the order of switcher buffers.")

(defvar switcher-root-function
  (if (functionp 'projectile-project-root) 'projectile-project-root 'switcher-home-root)
  "Function that returns the project root.

Directories will be shown relative to this directory.")

(defun switcher-home-root ()
  (expand-file-name "~"))

(defun switcher-buffer-list-updated ()
  (unless switcher-ignore-buffer-switch
    (switcher-move-current-buffer-to-front)))

(defun switcher-move-current-buffer-to-front ()
  (let* ((current (current-buffer))
         (tail (remove current switcher-buffer-list)))
    (unless (or (eq current switcher-menu-buffer)
                ;; This command seems to fire the buffer-list-update-hook as we
                ;; flip through buffers in the list.
                (eq this-command 'handle-switch-frame))
      (setq switcher-buffer-list (cons current tail)))))

(add-hook 'buffer-list-update-hook 'switcher-buffer-list-updated)

(defface switcher-menu-current-item-face
  '((((class color)) :background "#2850ac" :extend t))
  "Face used for the current item in Switcher menu.")

(defface switcher-menu-buffer-name-face
  '((((class color)) :foreground "white" :extend t :height 1.4 :family "Sans Serif"))
  "Face used for buffer names in Switcher menu.")

(defface switcher-menu-modified-buffer-name-face
  '((((class color)) :foreground "red" :inherit switcher-menu-buffer-name-face))
  "Face used for modified buffer names in Switcher menu.")

(defface switcher-menu-directory-face
  '((((class color)) :foreground "#d4d4d4" :extend t :height 1.0 :family "Sans Serif"))
  "Face used for buffer directories in Switcher menu.")

(defun switcher-list-to-set (list)
  (let ((set (make-hash-table :size (length list))))
    (seq-doseq (el list)
      (puthash el t set))
    set))

(defun switcher-list-buffers ()
  "The list of buffers to present as options in the Switcher menu."
  (let* ((our-list (seq-filter 'buffer-live-p switcher-buffer-list))
         (set (switcher-list-to-set our-list))
         (rest (seq-filter (lambda (buf) (not (gethash buf set))) (buffer-list)))
         (full-list (append our-list rest)))
    (seq-filter
     (lambda (buffer)
       (not (or (and (string-match "\\` " (buffer-name buffer))
                     (null (buffer-file-name buffer)))
                (string= (buffer-name buffer) "*switcher*"))))
     full-list)))

(defun switcher-buffer-item (buffer)
  (let* ((item (make-hash-table))
         (file-name (buffer-file-name buffer))
         (buffer-name (if file-name
                          (file-name-nondirectory file-name)
                        (replace-regexp-in-string "\\` " "" (buffer-name buffer))))
         (directory (when file-name
                      (switcher-buffer-directory buffer))))
    (puthash 'buffer buffer item)
    (puthash 'buffer-name buffer-name item)
    (puthash 'directory directory item)
    item))

(defun switcher-buffer-directory (buffer)
  (let* ((file-name (buffer-file-name buffer)))
    (if file-name
        (let ((truename (file-truename (file-name-directory file-name)))
              (root (with-current-buffer buffer (funcall switcher-root-function))))
          (replace-regexp-in-string
           "/\\'"
           ""
           (if (and root (string= root (substring truename 0 (length root))))
               (substring truename (length root))
             truename)))
      "")))

(defmacro switcher-ignoring-buffer-switch (&rest forms)
  `(let ((switcher-ignore-buffer-switch t))
     ,@forms))

(defun switcher-next ()
  "Show the Switcher menu and move to the next buffer."
  (interactive)
  (switcher-ignoring-buffer-switch
   (switcher-show-menu)
   (switcher-bump-timeout)
   (save-selected-window
     (select-window (car (window-list switcher-menu-frame)))
     (with-current-buffer switcher-menu-buffer
       (switcher-menu-move 1)))))

(defun switcher-previous ()
  "Show the Switcher menu and move to the previous buffer."
  (interactive)
  (switcher-ignoring-buffer-switch
   (switcher-show-menu)
   (switcher-bump-timeout)
   (save-selected-window
     (select-window (car (window-list switcher-menu-frame)))
     (with-current-buffer switcher-menu-buffer
       (switcher-menu-move -1)))))

(defvar switcher-timer nil
  "Timer to deactivate the Switcher menu, when active.")

(defvar switcher-timeout 2
  "Number of seconds after which to hide the Switcher menu (nil for never).")

(defun switcher-bump-timeout ()
  (when switcher-timer
    (cancel-timer switcher-timer))
  (when switcher-timeout
    (setq switcher-timer (run-at-time switcher-timeout nil 'switcher-hide-menu))))

(defvar switcher-items nil
  "Vector of items being displayed in the Switcher menu.")

(defvar switcher-current-index nil
  "Index of current item when Switcher menu is in use.")

(defun switcher-show-menu ()
  (interactive)
  (unless switcher-menu-frame
    (setq switcher-home-window (selected-window)
          switcher-menu-frame (make-frame '((name . "switcher")
                                            (minibuffer . nil)
                                            (border-width . 0)
                                            (internal-border-width . 15)
                                            (internal-border-width . 0)
                                            (left-fringe . 0)
                                            (right-fringe . 0)
                                            (skip-taskbar . t)
                                            (undecorated . t)
                                            (background-color . "#222")
                                            (no-focus-on-map . t)
                                            (z-group . above)
                                            (alpha . 85)
                                            (cursor-type . nil)))
          switcher-menu-buffer (generate-new-buffer "*switcher*"))
    (set-window-buffer (car (window-list switcher-menu-frame)) switcher-menu-buffer)
    (with-current-buffer switcher-menu-buffer
      (setq switcher-current-index 0
            mode-line-format nil
            line-spacing 0.2))
    (switcher-refresh-menu)
    (switcher-active-mode 1)))

(defun switcher-refresh-menu ()
  (setq switcher-items (apply 'vector (mapcar 'switcher-buffer-item (switcher-list-buffers))))
  (when (>= switcher-current-index (length switcher-items))
    (setq switcher-current-index (1- (length switcher-items))))
  (with-current-buffer switcher-menu-buffer
    (erase-buffer)
    (mapc
     (lambda (item)
       (let* ((buffer (gethash 'buffer item))
              (buffer-name-face (if (and (buffer-file-name buffer)
                                         (buffer-modified-p buffer))
                                    'switcher-menu-modified-buffer-name-face
                                  'switcher-menu-buffer-name-face)))
         (puthash
          'overlay
          (switcher-with-overlay
           `((switcher-menu-item . t) (face . switcher-menu-item-face) (priority . 2))
           (lambda ()
             (switcher-with-overlay
              `((face . ,buffer-name-face) (priority . 1))
              (lambda ()
                (insert (or (gethash 'buffer-name item) " "))))
             (insert "  ")
             (switcher-with-overlay
              '((face . switcher-menu-directory-face) (priority . 1))
              (lambda ()
                (insert (or (gethash 'directory item) " ") "\n")))))
          item)))
     switcher-items)
    (switcher-set-item-face switcher-current-index 'switcher-menu-current-item-face)
    (let* ((home-frame (window-frame switcher-home-window))
           (hpos (frame-position home-frame))
           (hx (car hpos))
           (hy (cdr hpos))
           (hw (frame-pixel-width home-frame))
           (hh (frame-pixel-height home-frame))
           (msize (window-text-pixel-size (selected-window) nil t))
           (mw (+ (car msize) 8)) ; hack to avoid a \-continuation
           (mh (min (cdr msize) hh))
           (mx (+ hx (/ (- hw mw 30) 2)))
           (my (+ hy (/ (- hh mh 30) 2))))
      (set-frame-position switcher-menu-frame mx my)
      (set-frame-size switcher-menu-frame mw mh t))))

(defun switcher-with-overlay (overlay-properties f)
  (let ((start (point)))
    (funcall f)
    (let* ((end (point))
           (overlay (make-overlay start end)))
      (mapc
       (lambda (property) (overlay-put overlay (car property) (cdr property)))
       overlay-properties)
      overlay)))

(defun switcher-hide-menu ()
  (interactive)
  (switcher-ignoring-buffer-switch
   (when switcher-menu-buffer
     (kill-buffer switcher-menu-buffer)
     (setq switcher-menu-buffer nil))
   (when switcher-menu-frame
     (delete-frame switcher-menu-frame)
     (setq switcher-menu-frame nil))
   (when switcher-timer
     (cancel-timer switcher-timer)
     (setq switcher-timer nil))
   (switcher-active-mode 0))
 (switcher-move-current-buffer-to-front))

;; switcher-active-mode

(defvar switcher-active-mode-map (make-sparse-keymap)
  "Non-nil while the Switcher menu is visible.")
(define-key switcher-active-mode-map (kbd "M-<up>") 'switcher-previous)
(define-key switcher-active-mode-map (kbd "M-<down>") 'switcher-next)
(define-key switcher-active-mode-map (kbd "M-RET") 'switcher-hide-menu)
(define-key switcher-active-mode-map (kbd "M-s") 'switcher-save-buffer)
(define-key switcher-active-mode-map (kbd "M-w") 'switcher-kill-buffer)

(define-minor-mode switcher-active-mode
  "Minor mode enabled while the Switcher menu is visible."
  :global t
  :init-value nil
  :keymap switcher-active-mode-map
  :interactive nil
  (if switcher-active-mode
      (add-hook 'pre-command-hook 'switcher-active-pre-command)
    (remove-hook 'pre-command-hook 'switcher-active-pre-command)))

(defun switcher-active-pre-command ()
  (let ((command (if (eq this-command 'handle-switch-frame) last-command this-command)))
    (unless (get command 'switcher-menu-command)
      (switcher-hide-menu))))

(put 'switcher-next 'switcher-menu-command t)
(put 'switcher-previous 'switcher-menu-command t)
(put 'switcher-hide-menu 'switcher-menu-command t)
(put 'switcher-save-buffer 'switcher-menu-command t)
(put 'switcher-kill-buffer 'switcher-menu-command t)

(defun switcher-save-buffer ()
  "Save the current buffer and update the Switcher menu."
  (interactive)
  (switcher-ignoring-buffer-switch
   (save-buffer)
   (save-selected-window
     (select-window (car (window-list switcher-menu-frame)))
     (switcher-refresh-menu)
     (switcher-recenter))))

(defun switcher-kill-buffer ()
  "Kill the current buffer and update the Switcher menu."
  (interactive)
  (switcher-ignoring-buffer-switch
   (let* ((original-buffer (current-buffer))
          (next-index (mod (1+ switcher-current-index) (length switcher-items)))
          (next-item (elt switcher-items next-index)))
     (set-window-buffer switcher-home-window (gethash 'buffer next-item))
     (when (not (kill-buffer original-buffer))
       (set-window-buffer switcher-home-window original-buffer)))
   (save-selected-window
     (select-window (car (window-list switcher-menu-frame)))
     (switcher-refresh-menu)
     (switcher-recenter))))

(defun switcher-set-item-face (index face)
  (let* ((item (elt switcher-items index))
         (overlay (gethash 'overlay item)))
    (overlay-put overlay 'face face)))

(defun switcher-menu-move (n)
  (when switcher-items
    (switcher-set-item-face switcher-current-index 'switcher-menu-item-face)
    (setq switcher-current-index (mod (+ switcher-current-index n) (length switcher-items)))
    (switcher-set-item-face switcher-current-index 'switcher-menu-current-item-face)
    (switcher-recenter)
    (let ((item (elt switcher-items switcher-current-index)))
      (set-window-buffer switcher-home-window (gethash 'buffer item)))))

(defun switcher-recenter ()
  (let* ((item (elt switcher-items switcher-current-index))
         (overlay (gethash 'overlay item)))
    (goto-char (overlay-start overlay))
    (let* ((n (length switcher-items))
           (p switcher-current-index)
           (q (- n p))
           (h (/ (window-pixel-height) (line-pixel-height)))
           (target (/ h 2))
           (min (- h q))
           (max p)
           (effective (min (max target min) max)))
      (recenter effective t))))

(provide 'switcher)
