;(eval-when-compile (require 'subr))
(declare-function region-active-p "simple" nil)
(declare-function c-indent-line "cc-cmds" (&optional syntax quiet ignore-point-pos))
(declare-function c-fill-paragraph "cc-cmds" (&optional arg))

(defun g-cc-replace-in-string (target old new &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string, 
 and returns the new string."
  (replace-regexp-in-string old new target nil literal))

;;;###autoload
(defun g-cc-header-guard-token ()
  "Return the token used for header guards."
  (interactive)
  (upcase
   (g-cc-replace-in-string
    (g-cc-replace-in-string
     (file-name-nondirectory (buffer-file-name))
     "[^A-Za-z0-9]" "_")
    "^[0-9]+" "")))

;;;###autoload
(defun g-cc-init-buffer ()
  "Insert boilerplate C++ header code if the buffer is empty."
  (when (and (string-match "\\.h$" (buffer-file-name))
             (= (buffer-size) 0))
    (let ((tok (g-cc-header-guard-token)))
      (insert "// -*- mode: c++ -*-\n"
              "\n"
              "#ifndef " tok "\n"
              "#define " tok "\n"
              "\n")
      (save-excursion
        (insert "\n"
                "\n"
                "#endif  // ndef " tok
                "\n")))
    (when font-lock-mode
      (font-lock-fontify-buffer))))

;;;###autoload
(defun g-cc-insert-ifndef-wrap (&optional s e)
  "Wrap s and e in #ifndef FILENAME directives.

The actual text inserted is:

  #ifndef FILENAME
  #define FILENAME
  ...
  #endif  // ndef FILENAME

where FILENAME is the buffer filename converted to a valid
C-identifier.

If S and E are not given, they are taken as the region extents.  If
there is no region, they are taken to be the buffer extents, sans an
optional first line containing \"-*-...-*-\"."
  (interactive)
  ;; find s and e
  (save-excursion
    (unless s
      (if (region-active-p)
          (setq s (region-beginning)
                e (region-end))
        (setq s (progn (goto-char (point-min))
                       (when (looking-at ".*-\\*-.*-\\*-")
                         (when (= (forward-line) -1)
                           (goto-char (point-max))
                           (insert "\n")))
                       (point))
              e (progn (goto-char (point-max))
                       (skip-chars-backward " \t\n\r\f\v")
                       (delete-region (point) (point-max))
                       (insert "\n\n")
                       (point)))))
    (setq s (set-marker (make-marker) s)
          e (set-marker (make-marker) e))
    ;; insert text at (or near) s and e
    (let ((token (g-cc-header-guard-token)))
      (goto-char s)
      (unless (= (current-column) 0)
        (beginning-of-line))
      (insert "#ifndef " token "\n#define " token "\n\n")
      (goto-char e)
      (unless (= (current-column) 0)
        (beginning-of-line)
        (unless (looking-at "[ \t]*$")
          (end-of-line)
          (insert "\n")))
      (insert "#endif  // ndef " token "\n"))))

;;;###autoload
(defun g-cc-heading-at-point (delim)
"Return the region containing the heading at point.

If point is not in a heading, return nil, otherwise return (s . e),
where `s' and `e' are the start and end of the region containing the
heading.

A heading is a (part of a) comment that looks like:

// DELIM
// ...
// DELIM

DELIM is a regexp.  The region returned starts at the beginning of the
first comment line and ends at the end of the last comment line."
(let* ((pt (point))
       (comment-re "\\s-*//")
       (delim-re (concat comment-re " " delim "\\s-*$"))
       s e)
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (when (looking-at comment-re)
        ;; goto beginning of comment
        (while (save-excursion
                 (and (= (forward-line -1) 0)
                      (looking-at comment-re)))
          (forward-line -1))
        ;; find if point was in a heading
        (while (funcall (if s '< '<=)
                        (point) pt)
          (when (looking-at delim-re)
            (setq s (if s nil (point))))
          (forward-line 1))
        ;; if we were in a heading (s non-nil), find the end of the
        ;; heading
        (when s
          (while (and (looking-at comment-re)
                      (not (looking-at delim-re))
                      (= (forward-line 1) 0)))
          (when (looking-at delim-re)
            (end-of-line)
            (cons s (point)))))))))

;;;###autoload
(defun g-cc-insert-heading (&optional level heading)
  "Insert a section heading.

If point is in a section heading, just realign it, otherwise prompt
for a heading and insert it."
  (interactive "p")
  (let* ((delim (cond ((< level 1)
                       (error 'args-out-of-range "invalid level" level))
                      ((= level 1)
                       ?-)
                      ((= level 2)
                       ?=)
                      ((= level 3)
                       ?#)
                      (t
                       (error 'invalid-level level))))
         (region (g-cc-heading-at-point
                  (concat (regexp-quote (char-to-string delim))
                          "\\{10,\\}")))
         finalpt)
    (unless region
      ;; insert a new heading
      (unless heading
        (setq heading
              (if (= level 3)
                  ""
                (read-string "Section: "))))
      (beginning-of-line)
      (open-line 2)
      (delete-blank-lines)
      (newline-and-indent)
      (insert "// " (make-string 10 delim))
      (setq region (point-at-bol))
      (unless (= level 1)
        (newline-and-indent)
        (insert "//"))
      (newline-and-indent)
      (if (string= heading "")
          (progn
            (insert "//")
            (setq finalpt (point-marker))
            (newline-and-indent))
        (mapc (lambda (line)
                (insert "// " line)
                (newline-and-indent))
              (split-string heading "\r\n\\|\r\\|\n")))
      (unless (= level 1)
        (insert "//")
        (newline-and-indent))
      (insert "// " (make-string 10 delim))
      (setq region (cons region (point)))
      (insert "\n")
      (forward-line))
    ;; realign the heading
    (save-excursion
      ;; convert region to a pair of markers
      (setq region (cons
                    (set-marker (make-marker) (car region))
                    (set-marker (make-marker) (cdr region))))
      ;; top delim line
      (goto-char (car region))
      (c-indent-line)
      (end-of-line)
      (delete-horizontal-space)
      (if (< (current-column) fill-column)
          (insert (make-string (- fill-column (current-column))
                               delim))
        (delete-char (- fill-column (current-column))))
      (forward-line 1)
      ;; center first paragraph
      (unless (= level 3)
        (let ((start (point-marker))
              (end (save-excursion
                     (goto-char (cdr region))
                     (beginning-of-line)
                     (point-marker)))
              (blank-re "\\s-*//\\s-*$"))
          ;; skip blank lines before first paragraph
          (while (and (< (point) end)
                      (looking-at blank-re))
            (progn
              (end-of-line)
              (delete-horizontal-space)
              (forward-line 1)
              (set-marker start (point))))
          (while (< (point) end)
            (if (looking-at blank-re)
                ;; end of paragraph, but not heading -- delete
                ;; trailing whitespace and stop here
                (progn
                  (end-of-line)
                  (delete-horizontal-space)
                  (beginning-of-line)
                  (set-marker end (point)))
              ;; a line in the first paragraph -- squash leading
              ;; whitespace
              (skip-syntax-forward "-")
              (skip-chars-forward "/")
              (delete-horizontal-space)
              (insert "   ")
              ;;(end-of-line)
              ;;(delete-horizontal-space)
              )
            (forward-line 1))
          ;; fill the first paragraph
          (save-restriction
            (narrow-to-region start end)
            (c-fill-paragraph))
          ;; center the first paragraph
          (goto-char start)
          (while (< (point) end)
            (skip-syntax-forward "-")
            (skip-chars-forward "/")
            (skip-syntax-forward "-")
            (just-one-space)
            (insert (make-string (/ (- fill-column (- (point-at-eol)
                                                      (point-at-bol)))
                                    2) ? ))
            (forward-line 1))
          (set-marker start nil)
          (set-marker end nil)))
      ;; align remaining lines
      (let ((end (save-excursion
                   (goto-char (cdr region))
                   (beginning-of-line)
                   (point-marker))))
        (while (< (point) end)
          (c-indent-line)
          (forward-line 1)))
      ;; bottom delim line
      (goto-char (cdr region))
      (c-indent-line)
      (end-of-line)
      (delete-horizontal-space)
      (if (< (current-column) fill-column)
          (insert (make-string (- fill-column (current-column))
                               delim))
        (delete-char (- fill-column (current-column))))
      ;; unset markers
      (set-marker (car region) nil)
      (set-marker (cdr region) nil)
      )
    (when finalpt
      (goto-char finalpt)
      (insert " ")
      (set-marker finalpt nil))))
