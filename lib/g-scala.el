(eval-when-compile
  (require 'scala-mode-inf))

;;;###autoload
(defun g-scala-load-file ()
  "Load the current file in the Scala interpreter.

Start the interpreter if necessary."
  (interactive)
  (unless (scala-interpreter-running-p-1)
    (save-excursion
      (scala-run-scala scala-interpreter)))
  (scala-load-file (buffer-file-name)))

(provide 'g-scala)
