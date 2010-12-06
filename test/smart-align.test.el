(require 'smart-align)

(scene "smart align"
  (test "aligns the lines in the region on the given token"
        (in-buffer simple
          (select-region)
          (smart-align (point-min) (point-max) "bb")
          (check (equal (buffer-string) simple:aligned))))

  (test "does not touch lines outside the region"
        (in-buffer simple
          (smart-align eol1 (point-max) "bb")
          (check (equal (buffer-string) simple:last-two-lines-aligned))))

  (test "ignores lines which do not contain the token"
        (in-buffer one-line-with-no-token
          (select-region)
          (smart-align (point-min) (point-max) "bb")
          (check (equal (buffer-string) one-line-with-no-token:aligned))))

  (test "treats leading whitespace on the token as the left-padding to use on the token when aligning"
        (in-buffer simple
                   (smart-align (point-min) (point-max) " \tbb")
                   (check (equal (buffer-string) simple:aligned-with-leading-whitespace))))

  (test "treats trailing whitespace on the token as the right-padding to use on the token when aligning"
        (in-buffer simple
                   (smart-align (point-min) (point-max) "bb \t")
                   (check (equal (buffer-string) simple:aligned-with-trailing-whitespace))))

  (buffers "
== simple
aa bb cc-<eol1>-
aaaabbcc
a bb cc

== one-line-with-no-token
aa bb cc
xxxxx
aaaa bb ccc
aaa bb

")

  (strings "
== simple:aligned
aa  bbcc
aaaabbcc
a   bbcc

== simple:last-two-lines-aligned
aa bb cc
aaaabbcc
a   bbcc

== simple:aligned-with-leading-whitespace
aa   \tbbcc
aaaa \tbbcc
a    \tbbcc

== simple:aligned-with-trailing-whitespace
aa  bb \tcc
aaaabb \tcc
a   bb \tcc

== one-line-with-no-token:aligned
aa  bbcc
xxxxx
aaaabbccc
aaa bb

"))
