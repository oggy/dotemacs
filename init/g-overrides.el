;; Stop cus-edit butchering my init.el.
(setq custom-file (concat rc-dir "/custom.el"))

;; And don't load what it writes. It includes settings that are machine
;; dependent, so I don't want these in my sync'd config. Settings are manually
;; maintained in g-customizations.
;; (load custom-file)

;; Don't make font-lock-mode give up so easily.
(setq font-lock-maximum-size 400000)

;; Make find-file always start at ~.
(setq default-directory "~/")

;; On Aquamacs, this has a background and foreground color set which
;; overrides the default face in g-customizations.
(when aquamacsp
  (setq default-frame-alist nil))

;; Infer project name in frame titles.
(defun g-buffer-project-root (buffer)
  "Get the root path of the project of BUFFER's file, if any."
  (let* ((root (with-current-buffer buffer (projectile-project-root))))
    (when root
      (replace-in-string root "/*\\'" ""))))

(defun g-main-worktree-p (dir)
  "Return non-nil if DIR is the main worktree of its git repository.
For the main worktree the git dir and the common git dir are the same;
for a linked worktree they differ."
  (let ((dirs (split-string
               (g-shell-command-output
                "git" "-C" dir "rev-parse" "--git-dir" "--git-common-dir")
               "\n" t)))
    (and (= (length dirs) 2)
         (string= (nth 0 dirs) (nth 1 dirs)))))

(defun g-worktree-branch-if-multi (dir)
  "Return the branch checked out at DIR, or nil.

Returns nil unless DIR is a linked (non-main) worktree of a git
repository that has more than one worktree, and nil for a detached HEAD."
  (let ((worktrees (g-shell-command-output "git" "-C" dir "worktree" "list")))
    (when (and (> (length (split-string worktrees "\n" t)) 1)
               (not (g-main-worktree-p dir)))
      (let ((branch (string-trim
                     (g-shell-command-output
                      "git" "-C" dir "rev-parse" "--abbrev-ref" "HEAD"))))
        (unless (or (string= branch "") (string= branch "HEAD"))
          branch)))))

(defun g-frame-title-base ()
  (let* ((roots (if g-start-dir (list g-start-dir) (g-project-roots)))
         (titles (mapcar
                  (lambda (root)
                    (let ((title (g-titleize (file-name-base root)))
                          (branch (g-worktree-branch-if-multi root)))
                      (if branch
                          (concat title " [" branch "]")
                        title)))
                  roots)))
    (string-join titles ", ")))

(defun g-set-frame-titles (&optional _frame)
  "Recompute `frame-title-format' from the current projects and worktrees.

This shells out to git, so it is only called when a file buffer is added
or removed, or a frame is created or deleted."
  (let ((title (g-frame-title-base)))
    (setq frame-title-format
          (if (> (length (frame-list)) 1)
              (concat title " -- %b")
            title))))

(defun g-set-frame-titles-on-kill ()
  "Recompute frame titles when a file buffer is killed.

Deferred so the recompute sees the buffer list without the dying buffer."
  (when buffer-file-name
    (run-at-time 0 nil 'g-set-frame-titles)))

;; The title's content only changes when the set of visited file buffers
;; changes (which projects/worktrees are open) or when the frame count
;; crosses one (the "-- %b" suffix).
(add-hook 'find-file-hook 'g-set-frame-titles)
(add-hook 'kill-buffer-hook 'g-set-frame-titles-on-kill)
(add-hook 'after-make-frame-functions 'g-set-frame-titles)
(add-hook 'after-delete-frame-functions 'g-set-frame-titles)

;; Set an initial title, since no file may be visited yet at startup.
(g-set-frame-titles)

(provide 'g-overrides)
