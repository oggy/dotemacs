(require 'lorem-ipsum)

(scene "lorem-ipsum"
       (wrap (let ((lorem-ipsum '("abcde abcde abcde abcde" "abcde abcde abcde abcde"))
                   (fill-column 11))
               (with-temp-buffer (run))))

       (test "without an argument, it inserts all the paragraphs in `lorem-ipsum'"
             (lorem-ipsum)
             (check (string= "abcde abcde\nabcde abcde\n\nabcde abcde\nabcde abcde" (buffer-string))))

       (test "all lines are wrapped between the current indentation level and `fill-prefix'"
             (let ((fill-column 13))
               (insert "  ")
               (lorem-ipsum)
               (check (string= "  abcde abcde\n  abcde abcde\n\n  abcde abcde\n  abcde abcde" (buffer-string))))))
