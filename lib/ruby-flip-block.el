;;; -*- lexical-binding: t -*-
;; Provides a command ruby-flip-block, which Flips between single-line and
;; multi-line ruby blocks.
;;
;; For example, a single-line argument list in a function call:
;;
;;     foo { a; b }
;;
;; And a multi-line one:
;;
;;     foo do
;;       a
;;       b
;;     end

(defmacro ruby-flip-block:with-match (var &rest forms)
  `(condition-case err
       (progn
         (let ((,var (ruby-flip-block:find-match)))
           ,@forms
           (when ,var
             (ruby-flip-block:free-match ,var))))
     (err
      (when ,var
        (ruby-flip-block:free-match ,var))
      (signal (car err) (cdr err)))))

;;;###autoload
(defun ruby-flip-block ()
  "Flip a delimited list between one-line and multiline.

For example, switch between:

    foo { a; b }

and:

    foo do
      a
      b
    end

Point can be anywhere inside the block. The innermost block is
flipped."
  (interactive)
  (ruby-flip-block:with-match
   match
   (when (null match)
     (error "Not in block."))
   (if (ruby-flip-block:expanded-p match)
       (ruby-flip-block:contract match)
     (ruby-flip-block:expand match))
   ))

(defun ruby-flip-block:find-match ()
  (save-excursion
    (let ((delimiters (ruby-flip-block:go-to-opener)))
      (when delimiters
        (condition-case nil
            (let ((begin (point)))
              (forward-sexp)
              (apply 'ruby-flip-block:make-match begin (point) delimiters))
          (scan-error))))))

(defun ruby-flip-block:expanded-p (match)
  (< (line-number-at-pos (ruby-flip-block:match-begin match))
     (line-number-at-pos (ruby-flip-block:match-end match))))

(defun ruby-flip-block:contract (match)
  (save-excursion
    (let ((begin (ruby-flip-block:match-begin match))
          (end (ruby-flip-block:match-end match))
          (continue t)
          (at-opener t))
      (goto-char begin)
      (delete-char 2)
      (insert "{")
      (end-of-line)
      (while continue
        (if (< (point) end)
            (progn
              (delete-horizontal-space)
              (if at-opener
                  (setq at-opener nil)
                (insert "; "))
              (delete-char 1)
              (just-one-space)
              (end-of-line))
          (setq continue nil)))
      (goto-char end)
      (delete-char -3)
      (delete-horizontal-space)
      (when (= (char-before) ?\;)
        (delete-char -1))
      (insert (if (eq (point) (1+ begin)) "}" " }")))))

(defun ruby-flip-block:expand (match)
  (save-excursion
    (let ((begin (ruby-flip-block:match-begin match))
          (end (ruby-flip-block:match-end match)))
      (goto-char begin)
      (delete-char 1)
      (insert "do")
      (just-one-space)
      (when (= (char-after) ?\|)
        (forward-char)
        (skip-chars-forward "^|")
        (forward-char))
      (while (< (point) end)
        (delete-horizontal-space)
        (insert "\n")
        (indent-for-tab-command)
        (skip-chars-forward "^;"))
      (goto-char end)
      (delete-char -1)
      (delete-horizontal-space)
      (unless (bolp)
        (insert "\n"))
      (insert "end")
      (indent-for-tab-command))))

(defun ruby-flip-block:looking-at-string (string)
  (string= (buffer-substring (point) (+ (point) (length string))) string))

(defun ruby-flip-block:go-to-opener ()
  (interactive)
  (condition-case nil
      (progn
        (backward-up-list)
        (while (not (looking-at "\\bdo\\b\\|{"))
          (backward-up-list))
        (if (looking-at "do") (list "do" "end") (list "{" "}")))
    (scan-error)))

(defun ruby-flip-block:at-opener (delimiter)
  (ruby-flip-block:looking-at-string (nth 0 delimiter)))

(defun ruby-flip-block:make-marker-at (pos)
  (let ((marker (make-marker)))
    (set-marker marker pos)
    marker))

(defun ruby-flip-block:make-match (begin end opener closer)
  (list
   (ruby-flip-block:make-marker-at begin)
   (ruby-flip-block:make-marker-at end)
   opener
   closer))

(defun ruby-flip-block:free-match (match)
  (set-marker (ruby-flip-block:match-begin match) nil)
  (set-marker (ruby-flip-block:match-end match) nil))

(defun ruby-flip-block:match-begin (match) (nth 0 match))
(defun ruby-flip-block:match-end (match) (nth 1 match))
(defun ruby-flip-block:match-opener (match) (nth 2 match))
(defun ruby-flip-block:match-closer (match) (nth 3 match))

(provide 'ruby-flip-block)
