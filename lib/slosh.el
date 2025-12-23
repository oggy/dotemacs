(declare-function region-active-p "simple" nil)

(require 'simple)

;;;###autoload
(defun slosh ()
  "Slosh the line or region.

If the region is active, run `slosh-region' on it. Otherwise, if
point is in a sloshed region, reslosh it. Otherwise slosh the
current line.

slosh -- tr. v.
To append a trailing backslash to a line or all lines in a region."
  (interactive)
  (cond
   ((region-active-p)
    (slosh-region (region-beginning) (region-end)))
   ((eq (char-before (pos-eol)) ?\\)
    (let ((region (cons (pos-bol)
                        (1+ (pos-eol)))))
      (setq region (slosh-extend-region-backwards region))
      (setq region (slosh-extend-region-forwards  region))
      (slosh-region (car region) (cdr region))))
   (t
    (slosh-line))))

;;;###autoload
(defun slosh-region (s e)
  "Add backslashes to each end-of-line in the region.

If the region is already sloshed, `slosh' is smart enough to figure
out how to reslosh the region."
  (interactive "r")
  (let ((s (set-marker (make-marker) s))
        (e (set-marker (make-marker) e))
        column)
    (slosh-unslosh-region s e)
    (setq column (max (- fill-column 2) (slosh-max-line-length s e)))
    (slosh-slosh-region s e column)
    (set-marker s nil)
    (set-marker e nil)))

(defun slosh-unslosh-region (s e)
  (slosh-at-each-eol s e
    (delete-horizontal-space)
    (when (= (% (slosh-num-eol-sloshes) 2) 0)
      (delete-char -1)
      (delete-horizontal-space))))

(defun slosh-max-line-length (s e)
  (let ((max 0))
    (slosh-at-each-eol s e
     (if (> (current-column) max)
         (setq max (current-column))))
    max))

(defun slosh-slosh-region (s e column)
    (slosh-at-each-eol s e
      (if (> column (current-column))
          (insert (make-string (- column (current-column)) ?\ )))
      (insert " \\")))

(defmacro slosh-at-each-eol (s e &rest forms)
  `(save-excursion
    (goto-char ,s)
    (end-of-line)
    ,@forms
    (while (and (= (forward-line) 0)
                (progn (end-of-line) (< (point) ,e)))
      ,@forms)))

(defun slosh-num-eol-sloshes ()
  (save-excursion
    (goto-char (pos-bol))
    (save-match-data
      (search-forward-regexp "\\\\*$" (pos-eol))
      (length (match-string 0)))))

(defun slosh-extend-region-backwards (region)
  (let ((s (car region))
        (e (cdr region)))
    (save-excursion
      (goto-char s)
      (while (and (eq (forward-line -1) 0)
                  (eq (char-before (pos-eol)) ?\\))
        (setq s (point)))
      (cons s e))))

(defun slosh-extend-region-forwards (region)
  (let ((s (car region))
        (e (cdr region)))
    (save-excursion
      (goto-char e)
      (while (and (eq (char-before (pos-eol)) ?\\)
                  (eq (forward-line 1) 0))
        (setq e (point)))
      (cons s e))))

(defun slosh-line ()
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)
    (insert " \\")))
