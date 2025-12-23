;;; -*- lexical-binding: t -*-
;;;;
;;;; Echo the syntax of point wherever you go.
;;;;
;;;; echo-syntax-mode is a minor mode.  Run (echo-syntax-mode) to toggle.
;;;;

;;;; Commands

;;;###autoload
(defun echo-syntax-mode:show-syntax ()
  "Echo and return syntax."
  (interactive)
  (unless (echo-syntax-mode:in-minibuffer-p)
    (let* ((table-syntax-descriptor (aref (syntax-table) (char-after)))
           (properties (text-properties-at (point)))
           (property-syntax-descriptor (plist-get properties 'syntax-table)))
      (message
       "from properties: %s, from table: %s"
       (echo-syntax-mode:describe-syntax-class property-syntax-descriptor)
       (echo-syntax-mode:describe-syntax-class table-syntax-descriptor)
       ))))

(defun echo-syntax-mode:in-minibuffer-p ()
  (if (functionp 'minibufferp)
      ;; GNU Emacs
      (minibufferp (current-buffer))
    ;; XEmacs
    (active-minibuffer-window)))

;;;; Mode

(defvar echo-syntax-mode nil
  "Non-nil iff echo-syntax-mode is on.

See the `echo-syntax-mode' function for more information.")

;;;###autoload
(define-minor-mode echo-syntax-mode
  "Toggle, enable, or disable echo-syntax-mode.

When echo-syntax-mode is enabled, the syntax at point is echoed
after every command. Intended as a development aid."
  :lighter " syntax"
  :after-hook (echo-syntax-mode:after-hook))

(defun echo-syntax-mode:after-hook ()
  (if echo-syntax-mode
      (add-hook 'post-command-hook 'echo-syntax-mode:show-syntax)
    (remove-hook 'post-command-hook 'echo-syntax-mode:show-syntax)))

(defun echo-syntax-mode:describe-syntax-class (descriptor)
  (if descriptor
      (let* ((s-class (syntax-class descriptor))
             (matching-char (cdr descriptor))
             (text (cond
                    ((eq s-class 0) "whitespace")
                    ((eq s-class 1) "punctuation")
                    ((eq s-class 2) "word")
                    ((eq s-class 3) "symbol")
                    ((eq s-class 4) "open parenthesis")
                    ((eq s-class 5) "close parenthesis")
                    ((eq s-class 6) "expression prefix")
                    ((eq s-class 7) "string quote")
                    ((eq s-class 8) "paired delimiter")
                    ((eq s-class 9) "escape")
                    ((eq s-class 10) "character quote")
                    ((eq s-class 11) "comment-start")
                    ((eq s-class 12) "comment-end")
                    ((eq s-class 13) "inherit")
                    ((eq s-class 14) "generic comment")
                    ((eq s-class 15) "generic string")
                    (t "unknown")))
             (flags (echo-syntax-mode:syntax-descriptor-flags descriptor)))
        (format
         "[%c (%s, %d) matching:%s flags:%s]"
         (syntax-class-to-char s-class)
         text
         s-class
         (if matching-char (format "%c" 47) "nil")
         flags))
    "nil"))

(defun echo-syntax-mode:syntax-descriptor-flags (descriptor)
  (let* ((code (car descriptor))
         (f1 (echo-syntax-mode:bit-set-p code 16))
         (f2 (echo-syntax-mode:bit-set-p code 17))
         (f3 (echo-syntax-mode:bit-set-p code 18))
         (f4 (echo-syntax-mode:bit-set-p code 19))
         (fp (echo-syntax-mode:bit-set-p code 20))
         (fb (echo-syntax-mode:bit-set-p code 21))
         (fn (echo-syntax-mode:bit-set-p code 22))
         (fc (echo-syntax-mode:bit-set-p code 23)))
    (concat
     (if f1 "1" "")
     (if f2 "2" "")
     (if f3 "3" "")
     (if f4 "4" "")
     (if fp "p" "")
     (if fb "b" "")
     (if fn "n" "")
     (if fc "c" ""))))

(defun echo-syntax-mode:bit-set-p (i b)
  (> (logand i (ash 1 b)) 0))

;;;; Provide

(provide 'echo-syntax-mode)
