;;; -*- lexical-binding: t -*-
;;;;
;;;; Commands to transform the words around point or in the region.
;;;;
;;;; XEmacs comes with some such commands out of the box.  For
;;;; example, `upcase-region-or-word'.  GNU Emacs comes with
;;;; `upcase-word' and `upcase-region', but no such combined command.
;;;; This library adds cross-Emacs commands to perform these
;;;; transforms, other transforms such as camelize and snakize, as
;;;; well as a facility to easily define new transforms.
;;;;
;;;; The complete list of transforms are as follows:
;;;;   * `word-transforms:camelize-region-or-word'
;;;;   * `word-transforms:capicamelize-region-or-word'
;;;;   * `word-transforms:snakize-region-or-word'
;;;;   * `word-transforms:capitalize-region-or-word'
;;;;   * `word-transforms:upcase-region-or-word'
;;;;   * `word-transforms:downcase-region-or-word'
;;;;
;;;; The easy way to bind these to keys is to run
;;;; `word-transforms-mode', which is a minor mode which sets a
;;;; keymap.  See `word-transforms-mode' to view the bindings.  All
;;;; the key bindings begin with `M-c', which is the default key for
;;;; `capitalize-region-or-word'.  `capitalize-region-or-word' has
;;;; been moved to `M-c M-c'.
;;;;
;;;; For emacs lisp writers, also provided is:
;;;;
;;;;   * `word-transforms:transform-region-or-word'
;;;;
;;;; This function takes a single argument: a function that transforms
;;;; a string which is called for each word to be transformed.  This
;;;; can be used to define new transformations easily.  For example:
;;;;
;;;; (defun embolden (word)
;;;;   (concat "*" word "*"))
;;;;
;;;; (defun embolden-word-or-region
;;;;   (word-transforms:transform-region-or-word 'embolden))
;;;;

;;;; Compiler Support

;;;; Commands

;;;###autoload
(defun word-transforms:camelize-region-or-word (&optional arg)
  ""
  (interactive)
  (word-transforms:transform-region-or-word 'word-transforms:camelize arg))

;;;###autoload
(defun word-transforms:capicamelize-region-or-word (&optional arg)
  ""
  (interactive)
  (word-transforms:transform-region-or-word 'word-transforms:capicamelize arg))

;;;###autoload
(defun word-transforms:snakize-region-or-word (&optional arg)
  ""
  (interactive)
  (word-transforms:transform-region-or-word 'word-transforms:snakize arg))

;;;###autoload
(defun word-transforms:capitalize-region-or-word (&optional arg)
  ""
  (interactive)
  (word-transforms:transform-region-or-word 'capitalize arg))

;;;###autoload
(defun word-transforms:upcase-region-or-word (&optional arg)
  ""
  (interactive)
  (word-transforms:transform-region-or-word 'upcase arg))

;;;###autoload
(defun word-transforms:downcase-region-or-word (&optional arg)
  ""
  (interactive)
  (word-transforms:transform-region-or-word 'downcase arg))

;;;###autoload
(defun word-transforms:transform-region-or-word (transform-fuction arg)
  ""
  (let (s e)
    (setq arg (or arg 1))
    (cond ((region-active-p)
           ;; transform all words in the region
           (setq s (region-beginning))
           (setq e (region-end)))
          ((> arg 0)
           ;; transform from point to the end of the ARG-th next word
           (save-excursion
             (setq s (point))
             (cl-loop for i from 1 to arg do
                      (skip-syntax-forward "^w_")
                      (skip-syntax-forward "w_"))
             (setq e (point))))
          ((< arg 0)
           ;; transform from point to the beginning of the ARG-th previous word
           (save-excursion
             (setq e (point))
             (cl-loop for i from 1 to arg do
                      (skip-syntax-backward "^w_")
                      (skip-syntax-backward "w_"))
             (setq s (point))))
          (t ;; (= arg 0)
           ;; transform the word point is on (if any)
           (save-excursion
             (skip-syntax-backward "w_")
             (setq s (point)))
           (save-excursion
             (skip-syntax-forward "w_")
             (setq e (point)))))
    (word-transforms:transform-region transform-fuction s e)))

;;;; Library

(defun word-transforms:transform-region (transform-fuction start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (skip-syntax-forward "^w_")
      (let (p word)
        (while (not (eobp))
          (setq p (point))
          (skip-syntax-forward "w_")
          (setq word (buffer-substring p (point)))
          (delete-region p (point))
          (insert-before-markers (funcall transform-fuction word))
          (skip-syntax-forward "^w_"))))))

(defun word-transforms:camelize (word)
  "Transform the given string into camel case (first letter downcased)."
  (word-transforms:gsub
   word "_+."
   (lambda (s) (upcase (substring s -1)))))

(defun word-transforms:capicamelize (word)
  "Transform the given string into capitalized camel case."
  (word-transforms:camelize (capitalize word)))

(defun word-transforms:snakize (word)
  "Transform the given string into snake case."
  (let ((case-fold-search nil))
    (downcase
     (word-transforms:gsub
      word "\\([a-z]\\)\\([A-Z]\\)"
      (lambda (s) (concat (substring s 0 1) "_" (substring s 1 2)))))))

;;;; Helpers

(defun word-transforms:gsub (string regexp function)
  (let ((s 0)
        e
        (result ""))
    (while (and s
                (setq e (string-match regexp string s)))
      (setq result (concat result
                           (substring string s e)
                           (save-match-data
                             (funcall function (match-string 0 string)))))
      ;; ensure s advances so we don't loop perpetually
      (cond ((> (match-end 0) s)
             (setq s (match-end 0)))
            ;; s = (match-end 0)
            ((< s (length string))
             (setq result (concat result (substring string s (1+ s))))
             (setq s (1+ s)))
            (t
             (setq s nil))))
    (when s
      (setq result (concat result (substring string s))))
    result))

;;;; Minor mode

(defvar word-transforms-mode-map (make-sparse-keymap 'word-transforms-mode-map)
  "Keymap for `word-transforms-mode'.")
(define-key word-transforms-mode-map (kbd "M-c M-u") 'word-transforms:upcase-region-or-word)
(define-key word-transforms-mode-map (kbd "M-c M-d") 'word-transforms:downcase-region-or-word)
(define-key word-transforms-mode-map (kbd "M-c M-c") 'word-transforms:capitalize-region-or-word)
(define-key word-transforms-mode-map (kbd "M-c M-m") 'word-transforms:camelize-region-or-word)
(define-key word-transforms-mode-map (kbd "M-c M-S-m") 'word-transforms:capicamelize-region-or-word)
(define-key word-transforms-mode-map (kbd "M-c M-s") 'word-transforms:snakize-region-or-word)

(defvar word-transforms-mode nil
  "Non-nil if `word-transforms-mode' is enabled.")

;;;###autoload
(defun word-transforms-mode (&optional arg)
  "Mode to enable keys to transform text around point or in the region."
  (interactive)
  (setq word-transforms-mode
        (if (null arg) (not word-transforms-mode)
          (> (prefix-numeric-value arg) 0))))

(add-minor-mode 'word-transforms-mode nil word-transforms-mode-map)

;;;; Provide

(provide 'word-transforms)
