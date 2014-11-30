(require 'active-mark-mode)

(scene "active-mark-mode"
       (wrap (let ((tab-width 2))
               (with-temp-buffer (run))))

       (test "it indents by tab-width spaces by default"
             (in-buffer simple
                        (active-mark-indent-region l2 l4 1)
                        (check (string= "aa\n  bb\n  cc\ndd\n" (buffer-string)))))

       (test "it indents the given number of times"
             (in-buffer simple
                        (active-mark-indent-region l2 l4 2)
                        (check (string= "aa\n    bb\n    cc\ndd\n" (buffer-string)))))

       (test "it extends the region to full lines"
             (in-buffer midlines
                        (active-mark-indent-region l2 l3 1)
                        (check (string= "aa\n  bb\n  cc\ndd\n" (buffer-string)))))

       (test "it indents the first line correctly if at the beginning of the buffer"
             (in-buffer midlines
                        (active-mark-indent-region l1 l2 1)
                        (check (string= "  aa\n  bb\ncc\ndd\n" (buffer-string)))))

       (test "it indents the last line correctly if at the end of the buffer"
             (in-buffer midlines
                        (active-mark-indent-region l3 l4 1)
                        (check (string= "aa\nbb\n  cc\n  dd\n" (buffer-string)))))

       (test "it adds to any existing indentation"
             (in-buffer indented
                        (active-mark-indent-region l2 l3 1)
                        (check (string= "  aa\n    bb\n    cc\n  dd\n" (buffer-string)))))

       (buffers "
== simple
-<l1>-aa
-<l2>-bb
-<l3>-cc
-<l4>-dd
== midlines
a-<l1>-a
b-<l2>-b
c-<l3>-c
d-<l4>-d
== indented
  a-<l1>-a
  b-<l2>-b
  c-<l3>-c
  d-<l4>-d
"))
