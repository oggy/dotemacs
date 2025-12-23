;;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'ruby-mode))

;;;; Public

(declare-function replace-in-string nil)

;;;###autoload
(defun g-gem-dir (&optional arg)
  (let ((run-chomp (lambda (command)
               (replace-in-string (shell-command-to-string command) "\n" ""))))
    (if arg
        (concat (funcall run-chomp (format "gem info --exactly-one --format %%path %s" arg)) "/lib/")
      (concat (funcall run-chomp "gem env gemdir") "/gems/"))))

;;;###autoload
(defun g-ruby-lib-dir ()
  (shell-command-to-string "ruby -rrbconfig -e \"print Config::CONFIG['libdir']\""))

;;;###autoload
(defun ruby-fill-paragraph (&optional justify)
  "Like \[fill-paragraph], but handle ruby comments.

If point is in a comment, fill the paragraph of the comment that point
is in, preserving the comment's indentation and initial hashes.

The filling behaviour is responsive to the `adaptive-fill-*'
variables.

Note that the number of hashes IS significant to distinguish between comments.

In particular, for the two comments below:

  puts 1  # comment 1
  # comment 2

... to be considered as separate comments, either put a blank line
between them or start the comments with a different number of #s."
  (interactive "P")
  ;; taken from c-indent-new-comment-line (then mangled beyond
  ;; recognition)
  (let ((bounds (ruby-current-comment-bounds))
        para-s para-e)

    (if (not bounds)
        ;; we aren't in a comment
        (fill-paragraph justify)

      ;; we're in a comment -- find the start and end of our paragraph
      (save-excursion
        (save-restriction
          ;; narrow to the comment
          (narrow-to-region (car bounds) (cdr bounds))

          ;; find paragraph bounds
          (let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
                (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
                (paragraph-ignore-fill-prefix nil))
            (forward-paragraph)
            (setq para-e (point))
            (backward-paragraph)
            (setq para-s (point)))

          ;; fill as one comment
          (let ((info (progn
                        (goto-char (point-min))
                        (ruby-line-comment-info))))
            (ruby-fill-as-one-comment
             para-s para-e
             (cadr info)
             (car info)))
          ))))
  t)

;;;###autoload
(defun g-ruby-indent-new-comment-line (&optional soft allow-auto-fill)
  "Break line at point and indent, continuing comment or macro if within one.

Try it; it's fun."
  ;; the command normally bound to M-j, `indent-new-comment-line',
  ;; mentions use of `comment-column'.  the c-mode function doesn't
  ;; use this, so we'll be lazy/ignorant too.
  (interactive)
  (let ((fill-prefix fill-prefix)
	(do-line-break
	 (lambda ()
	   (delete-horizontal-space)
	   (if soft
	       (insert-and-inherit ?\n)
	     (newline (if allow-auto-fill nil 1)))))
        comment-start-pos)
    (setq comment-start-pos
          (save-excursion
            (beginning-of-line)
            (if (ruby-goto-comment-start) (point))))
    (if comment-start-pos
        ;; in a comment
        (let (comment-prefix col)
          (save-excursion
            (goto-char comment-start-pos)
            (or (looking-at "#+[\t ]*")
                (error "`ruby-goto-comment-start' is a liar!"))
            (setq comment-prefix (match-string 0)
                  col (current-column)))
          (funcall do-line-break)
          (indent-to col)
          (insert-and-inherit comment-prefix))
      ;; TODO?: change this -- make it start an eol-commment.
      (funcall do-line-break)
      (indent-according-to-mode)
      ;; this string should probably be customizable
      (insert-and-inherit "# "))))

(defvar g-ruby-outline-regex "^ *\\(def\\|public\\|private\\|protected\\|class\\)"
  "Regex used for `g-ruby-outline'.")

;;;###autoload
(defun g-ruby-outline ()
  "Show an outline in a new buffer (uses `occur').

The regex used is g-ruby-outline-regex."
  (interactive)
  (occur g-ruby-outline-regex))

;;;; Private

(defun ruby-goto-comment-start ()
  "Goto the start of a comment ahead on this line.

If no comment lies ahead on this line, return nil and goto the end of
the line.  Otherwise, goto the leading \"#\" of the comment and return
non-nil.

Note that we only search forward from point."
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min)
                          (save-excursion (end-of-line) (point)))
        (while (not (looking-at "#\\|$"))
          (skip-chars-forward "^#\n\"\'\\\\?")
          (cond
           ((eq (char-after (point)) ?\\) (forward-char 2))
           ;; forward-sexp won't work correctly for something like
           ;; "#{'"'}", nor will it work for quotey-regexps -- not my
           ;; job.
           ((memq (char-after (point)) '(?\" ?\' ??)) (forward-sexp 1))))
        (not (eolp)))
    (error (end-of-line) nil)))

(defun ruby-line-comment-info ()
  "Return comment info for this line.

If there is no comment on this line, return nil.  Otherwise, return
(HASHES COLUMN NOCODE), where:

  - HASHES is the number of hashes that begin the comment
  - COLUMN is the column of the first hash that starts the comment
  - NOCODE is t if nothing apart from whitespace preceeds the comment,
    nil otherwise.

Assume point is at the beginning of the line."
  (save-excursion
    (ruby-goto-comment-start)
    (if (looking-at "#+")
        ;; comment exists
        (list
         (length (match-string 0))
         (current-column)
         (and (string-match "^[ \t]*$"
                            (buffer-substring (save-excursion
                                                (beginning-of-line)
                                                (point))
                                              (point)))
              t)))))

(defun ruby-current-comment-bounds ()
  "Return the start and end of the comment point is in.

The value returned is (START . END).  If point is not in a comment,
return nil."
  (let ((startinfo (save-excursion
                     (beginning-of-line)
                     (ruby-line-comment-info)))
        s e)
    (if (or (null startinfo)
            (and (> (cadr startinfo) (current-column))
                 (not (string-match "^[ \t]*$" (buffer-substring
                                                (pos-bol)
                                                (point))))))
        ;; not in a comment
        nil

      ;; we're in a comment -- find the start
      (save-excursion
        (if (not (car (cddr startinfo)))
            ;; there's code on this line -- this is the start
            (beginning-of-line)
          ;; no code -- search backwards to find start of comment
          (catch 'found-boc
            (while (zerop (forward-line -1))
              (let ((info (ruby-line-comment-info)))
                (cond ((or (null info)
                           (/= (car info) (car startinfo)))
                       ;; no comment or different number of #s -- don't
                       ;; include this line
                       (forward-line)
                       (throw 'found-boc nil))
                      ((not (car (cddr info)))
                       ;; code on this line -- comment starts here
                       (throw 'found-boc nil)))))))

        ;; found the start
        (setq s (point)))

      ;; find the end
      (save-excursion
        (catch 'found-eoc
          (while (zerop (forward-line))
            (let ((info (ruby-line-comment-info)))
              (cond ((or (null info)
                         (/= (car info) (car startinfo))
                         (not (car (cddr info))))
                     ;; no comment, different number of #s, or code on
                     ;; this line -- this is the end
                     (throw 'found-eoc nil))))))

        ;; found the end
        (setq e (point)))

      (cons s e)
      )))

;;; wrong for empty comments, or when point is in the hashes
(defun ruby-fill-as-one-comment (s e indent num-hashes &optional justify)
  "Fill the specified region as one comment.

Every line in the given region is assumed to contain one line of the
comment.  The first line may contain code; if so, the code will be
unaffected.  All other lines must start with [ \t]*# with the
exception of the last line if it is blank."
  ;; we make many little changes to the buffer -- we must make
  ;; sure we undo these changes if we quit prematurely
  (let ((orig (cons (point) (buffer-substring s e)))
        ;; marker to indicate where we should be when we're done --
        ;; save-excursion can get confused by insertions/deletions, so
        ;; we'll manage the position of point ourselves
        (pt (make-marker)))
    (set-marker pt (point))
    (save-restriction
      (narrow-to-region s e)
      (unwind-protect
          (progn
            (goto-char s)
            (let ((code-p (not (looking-at "[ \t]*#")))
                  (hashes (make-string num-hashes ?#))
                  code)
              (when code-p
                ;; temporarily remove the code so it doesn't get affected by the filling
                (ruby-goto-comment-start)
                (setq code (buffer-substring (point-min) (point)))
                (delete-region (point-min) (point)))
              ;; remove (the leading string of) #s from each line
              (while (looking-at "[ \t]*#+")
                (delete-region (match-beginning 0)
                               (match-end 0))
                (forward-line))

              ;; if the region was valid, we should now be at eob
              (or (eobp)
                  (error "Invalid region passed to ruby-fill-as-one-comment."))

              ;; fill the region with a reduced fill column
              (let ((fill-column (- fill-column indent num-hashes)))
                (fill-region-as-paragraph (point-min) (point-max) justify))

              ;; if it's an empty comment, put a blank line in so it
              ;; doesn't disappear
              (when (eq (point-min) (point-max))
                (goto-char (point-min))
                (set-marker pt (point))
                (insert-before-markers "\n"))

              ;; prefix and indent the region
              (goto-char (point-min))
              (when code-p
                ;; restore the code and insert hashes for the first line
                (insert code)
                (insert hashes)
                (forward-line))
              (while (not (eobp))
                (insert hashes)
                (indent-line-to indent)
                (forward-line))
              )
            ;; finished all changes -- no need to undo them anymore
            (setq orig nil))
        (if orig
            (progn
              (delete-region (point-min) (point-max))
              (insert (cdr orig))
              (goto-char (car orig)))
          (goto-char pt)))
      )))

;;;; Provide

(provide 'g-ruby)
