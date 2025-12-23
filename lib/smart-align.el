;;; smart-align.el -- a simpler-but-smart `align'.
;;
;; Bind a key to `smart-align'.
;;
;; smart-align will prompt you for a string to align on.  Any leading
;; and trailing whitespace will be used to pad the alignent string on
;; the region.  The rest of the alignment string is matched exactly if
;; no prefix argument is given, or is treated as a regular expression
;; otherwise.
;;

(eval-when-compile
  (require 'cl))

;;;###autoload
(defun smart-align (start end string &optional regexp-p)
  "Align the region intelligently.

When called interactively, STRING is prompted for, and REGEXP-P is
true iff a prefix arg is supplied.

Align the given string, sans leading and trailing whitespace on each
line in the region.  The leading and trailing whitespace is used to
pad the aligned string on each line.

For example, if the selected region contains:

  1 + 2
  10 + 4
  + 8

Then passing a STRING of \"  +  \" makes it:

   1  +  2
  10  +  4
      +  8

If REGEXP-P is true (or a prefix argument is given when called
interactively), then STRING is treated as a regular expression."
  (interactive "r\nsAlign on: \nP")
  (let* ((leading-space (smart-align-whole-match "^[ \t]*" string))
         (trailing-space (smart-align-whole-match "[ \t]*$" string))
         (alignment-token (substring string
                                     (length leading-space)
                                     (- (length string) (length trailing-space))))
         matches column)
    (save-restriction
      (narrow-to-region start end)
      (setq matches (smart-align-matches alignment-token regexp-p))
      (setq column (smart-align-alignment-column matches))
      (smart-align-align-rows column matches leading-space trailing-space))))

(defun smart-align-first-match (regexp string)
  (if (string-match regexp string)
      (match-string 1 string)
    ""))

(defun smart-align-whole-match (regexp string)
  (if (string-match regexp string)
      (match-string 0 string)
    ""))

;; returns matches in reverse-order
(defun smart-align-matches (alignment-token regexp-p)
  (let ((search-function (if regexp-p 'search-forward-regexp 'search-forward))
        matches done)
    (goto-char (point-min))
    (while (not done)
      (save-excursion
        (when (funcall search-function alignment-token (point-at-eol) t)
          (let ((match (make-smart-align-match
                        :column (save-excursion
                                  (goto-char (match-beginning 0))
                                  (skip-chars-backward " \t")
                                  (current-column))
                        :start (set-marker (make-marker) (match-beginning 0))
                        :end (set-marker (make-marker) (match-end 0)))))
            (setq matches (cons match matches)))))
      (when (eq (forward-line) 1)
        (setq done t)))
    matches))

(defun smart-align-alignment-column (matches)
  (apply 'max (mapcar (lambda (match) (smart-align-match-column match)) matches)))

(defun smart-align-align-rows (alignment-column matches left-padding right-padding)
  (mapc (lambda (match)
          (let ((start (smart-align-match-start match))
                (end (smart-align-match-end match)))
            ;; Start at the end so the insertions don't mess up the
            ;; positions in `matches'.
            (goto-char end)
            (delete-horizontal-space)
            (insert right-padding)
            (goto-char start)
            (delete-horizontal-space)
            (insert (make-string (- alignment-column (current-column)) ?\ ))
            (insert left-padding)))
        matches)
)

(defstruct smart-align-match
  (column)  ;; the min column the alignment-token may be at
  (start)   ;; the start of the alignment-token match
  (end)     ;; the end of the alignment-token match
  )

(provide 'smart-align)
(global-set-key [(control ?c) ?-] 'smart-align)

;; TODO:
;; smart-align: namespace
;; free markers
