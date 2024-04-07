;; Provides a command flip-list, which Flips between single-line and multi-line
;; lists of things.
;;
;; For example, a single-line argument list in a function call:
;;
;;     foo(1, 2)
;;
;; And a multi-line one:
;;
;;     foo(
;;       1,
;;       2,
;;     )

(defmacro flip-list:with-match (var &rest forms)
  `(condition-case err
       (progn
         (let ((,var (flip-list:find-match)))
           ,@forms
           (when ,var
             (flip-list:free-match ,var))))
     (err
      (when ,var
        (flip-list:free-match ,var))
      (signal (car err) (cdr err)))))

;;;###autoload
(defun flip-list ()
  "Flip a delimited list between one-line and multiline.

For example, switch between:

    foo(a, b)

and:

    foo(
      a,
      b,
    )

There are configurable variables, which can be overridden by
properties on the mode symbol. See the respective variables for
more info:

 * flip-list:delimiters
 * flip-list:include-trailing-separator-p

Point can be anywhere inside the delimiters. The innermost such
list is flipped."
  (interactive)
  (flip-list:with-match
   match
   (when (null match)
     (error "Not in delimited list."))
   (if (flip-list:expanded-p match)
       (flip-list:contract match)
     (flip-list:expand match))
   ))

;;;###autoload
(defvar flip-list:delimiters '(("(" ")" ",") ("[" "]" ",") ("{" "}" ","))
  "List of delimiter pairs used by flip-list.

Each element is a list of the form:

    (OPENER CLOSER SEPARATOR INNER-SPACE-P)

where:

 * OPENER is the opening delimiter, as a string
 * CLOSER is the closing delimiter, as a string
 * SEPARATOR is the item separator, as a string
 * INNER-SPACE-P is t if there are spaces inside the delimiters
   when contracted

For example, (\"(\" \")\" \",\") flips between:

(1, 2, 3)
(
  1,
  2,
  3,
)

And (\"[\" \"]\" \"|\" t) flips between:

[ 1 | 2 | 3 ]
[
  1 |
  2 |
  3 |
]

(Assuming flip-list:include-trailing-separator-p is t.)")

;;;###autoload
(defvar flip-list:include-trailing-separator-p t
  "Non-nil if a trailing separator should be added by flip-list.

Only when expanding. For example, when non-nil:

    foo(
      1,
      2,
    )

When nil:

    foo(
      1,
      2
    )")

(defun flip-list:current-delimiters ()
  (or (get major-mode 'flip-list:delimiters)
      flip-list:delimiters))

(defun flip-list:current-include-trailing-separator-p ()
  (or (get major-mode 'flip-list:include-trailing-separator-p)
      flip-list:include-trailing-separator-p))

(defun flip-list:find-match ()
  (save-excursion
    (let ((delimiter (flip-list:go-to-opener)))
      (when delimiter
        (condition-case nil
            (let ((begin (point)))
              (forward-list)
              (flip-list:make-match begin (point) delimiter))
          (scan-error))))))

(defun flip-list:expanded-p (match)
  (< (line-number-at-pos (flip-list:match-begin match))
     (line-number-at-pos (flip-list:match-end match))))

(defun flip-list:contract (match)
  (save-excursion
    (let ((opener (nth 0 (flip-list:match-delimiter match)))
          (closer (nth 1 (flip-list:match-delimiter match)))
          (separator (nth 2 (flip-list:match-delimiter match)))
          (inner-space-p (nth 3 (flip-list:match-delimiter match)))
          (end (flip-list:match-end match))
          (continue t)
          (at-opener t)
          at-closer)
      (goto-char (flip-list:match-begin match))
      (forward-char (length opener))
      (while continue
        (end-of-line)
        (if (< (point) end)
            (progn
              (delete-char 1)
              (if at-opener
                  (progn
                    (if inner-space-p (just-one-space) (delete-horizontal-space))
                    (setq at-opener nil))
                (just-one-space)))
          (setq continue nil)))
      (goto-char (- end (length closer)))
      (skip-syntax-backward " ")
      (when (string= (buffer-substring (- (point) (length separator)) (point)) separator)
        (delete-char (- (length separator)))
        (if inner-space-p (just-one-space) (delete-horizontal-space))))))

(defun flip-list:expand (match)
  (save-excursion
    (let ((begin (flip-list:match-begin match))
          (end (flip-list:match-end match))
          (opener (nth 0 (flip-list:match-delimiter match)))
          (closer (nth 1 (flip-list:match-delimiter match)))
          (separator (nth 2 (flip-list:match-delimiter match)))
          (continue t)
          at-closer)
      (goto-char begin)
      (forward-char (length opener))
      (while continue
        (insert "\n")
        (indent-for-tab-command)
        (skip-syntax-forward " ")
        (while (and (< (point) end)
                    (not (flip-list:looking-at-string closer))
                    (not (flip-list:looking-at-string separator)))
          (forward-sexp)
          (skip-syntax-forward " "))
        (when (< (point) end)
          (if (flip-list:looking-at-string closer)
              (progn
                (when (flip-list:current-include-trailing-separator-p)
                  (delete-horizontal-space)
                  (insert separator))
                (insert "\n")
                (indent-for-tab-command)
                (setq continue nil))
            (delete-horizontal-space)
            (forward-char (length separator))))))))

(defun flip-list:looking-at-string (string)
  (string= (buffer-substring (point) (+ (point) (length string))) string))

(defun flip-list:go-to-opener ()
  (interactive)
  (condition-case var
      (let (delimiter)
        (backward-up-list)
        (while (null (setq delimiter (seq-find 'flip-list:at-opener (flip-list:current-delimiters))))
          (backward-up-list))
        delimiter)
    (scan-error)))

(defun flip-list:at-opener (delimiter)
  (flip-list:looking-at-string (nth 0 delimiter)))

(defun flip-list:make-marker-at (pos)
  (let ((marker (make-marker)))
    (set-marker marker pos)
    marker))

(defun flip-list:make-match (begin end delimiter)
  (list
   (flip-list:make-marker-at begin)
   (flip-list:make-marker-at end)
   delimiter))

(defun flip-list:free-match (match)
  (set-marker (flip-list:match-begin match) nil)
  (set-marker (flip-list:match-end match) nil))

(defun flip-list:match-begin (match) (nth 0 match))
(defun flip-list:match-end (match) (nth 1 match))
(defun flip-list:match-delimiter (match) (nth 2 match))

(provide 'flip-list)
