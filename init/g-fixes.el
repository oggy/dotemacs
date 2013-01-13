;; Fix uncomment-region leaving comment padding.
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-03/msg00098.html
;; Still leaves trailing comment padding...
(defun uncomment-region-default (beg end &optional arg)
  "Uncomment each line in the BEG .. END region.
The numeric prefix ARG can specify a number of chars to remove from the
comment markers."
  (goto-char beg)
  (setq end (copy-marker end))
  (let* ((numarg (prefix-numeric-value arg))
	 (ccs comment-continue)
	 (srei (comment-padright ccs 're))
	 (csre (comment-padright comment-start 're))
	 (sre (and srei (concat "^\\s-*?\\(" srei "\\)")))
	 spt)
    (while (and (< (point) end)
		(setq spt (comment-search-forward end t)))
      (let ((ipt (point))
	    ;; Find the end of the comment.
	    (ept (progn
		   (goto-char spt)
		   (unless (or (comment-forward)
			       ;; Allow non-terminated comments.
			       (eobp))
		     (error "Can't find the comment end"))
		   (point)))
	    (box nil)
	    (box-equal nil))	   ;Whether we might be using `=' for boxes.
	(save-restriction
	  (narrow-to-region spt ept)

	  ;; Remove the comment-start.
	  (goto-char ipt)
	  (skip-syntax-backward " ")
	  ;; A box-comment starts with a looong comment-start marker.
	  (when (and (or (and (= (- (point) (point-min)) 1)
			      (setq box-equal t)
			      (looking-at "=\\{7\\}")
			      (not (eq (char-before (point-max)) ?\n))
			      (skip-chars-forward "="))
			 (> (- (point) (point-min) (length comment-start)) 7))
		     (> (count-lines (point-min) (point-max)) 2))
	    (setq box t))
	  ;; Skip the padding.  Padding can come from comment-padding and/or
	  ;; from comment-start, so we first check comment-start.
	  (if (or (save-excursion (goto-char (point-min)) (looking-at csre))
		  (looking-at (regexp-quote comment-padding)))
	      (goto-char (match-end 0)))
	  (when (and sre (looking-at (concat "\\s-*\n\\s-*" srei)))
	    (goto-char (match-end 0)))

	  (if (null arg) (delete-region (point-min) (point))
            (let* ((opoint (point-marker))
                   (nchar (skip-syntax-backward " ")))
              (delete-char (- numarg))
              (unless (or (bobp)
                          (save-excursion (goto-char (point-min))
                                          (looking-at comment-start-skip)))
                ;; If there's something left but it doesn't look like
                ;; a comment-start any more, just remove it.
                (delete-region (point-min) (point)))
              (save-excursion
                (goto-char (point-min))
                (unless (looking-at comment-start-skip)
                  (goto-char opoint)
                  (delete-char nchar)))))

	  ;; Remove the end-comment (and leading padding and such).
	  (goto-char (point-max)) (comment-enter-backward)
	  ;; Check for special `=' used sometimes in comment-box.
	  (when (and box-equal (not (eq (char-before (point-max)) ?\n)))
	    (let ((pos (point)))
	      ;; skip `=' but only if there are at least 7.
	      (when (> (skip-chars-backward "=") -7) (goto-char pos))))
	  (unless (looking-at "\\(\n\\|\\s-\\)*\\'")
	    (when (and (bolp) (not (bobp))) (backward-char))
	    (if (null arg) (delete-region (point) (point-max))

              ;; We should delete these spaces too, right?  --g
              ;; TODO: submit upstream if it turns out well.
              ;(skip-syntax-forward " ")
              (delete-region (point) (save-excursion (skip-syntax-forward " ") (point)))

	      (delete-char numarg)
	      (unless (or (eobp) (looking-at comment-end-skip))
		;; If there's something left but it doesn't look like
		;; a comment-end any more, just remove it.
		(delete-region (point) (point-max)))))

	  ;; Unquote any nested end-comment.
	  (comment-quote-nested comment-start comment-end t)

	  ;; Eliminate continuation markers as well.
	  (when sre
	    (let* ((cce (comment-string-reverse (or comment-continue
						    comment-start)))
		   (erei (and box (comment-padleft cce 're)))
		   (ere (and erei (concat "\\(" erei "\\)\\s-*$"))))
	      (goto-char (point-min))
	      (while (progn
		       (if (and ere (re-search-forward
				     ere (line-end-position) t))
			   (replace-match "" t t nil (if (match-end 2) 2 1))
			 (setq ere nil))
		       (forward-line 1)
		       (re-search-forward sre (line-end-position) t))
		(replace-match "" t t nil (if (match-end 2) 2 1)))))
	  ;; Go to the end for the next comment.
	  (goto-char (point-max))))))
  (set-marker end nil))

(provide 'g-fixes)
