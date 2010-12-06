(require 'word-transforms)

(scene "library functions"
       (test "camelize turns a word from snake-case to camel-case"
             (check (string= (word-transforms:camelize "foo_bar") "fooBar")))
       (test "capicamelize turns a word from snake-case to capitalized camel-case"
             (check (string= (word-transforms:capicamelize "foo_bar") "FooBar")))
       (test "snakize turns a word from camel-case to snake-case"
             (check (string= (word-transforms:snakize "fooBar") "foo_bar")))
       (test "snakize turns a word from capitalized camel-case to snake-case"
             (check (string= (word-transforms:snakize "FooBar") "foo_bar")))
)

(scene "acting on a single word"
       (test "camelizing turns words from snake-case to camel-case"
             (in-buffer snake-case-word
                        (goto-char before)
                        (word-transforms:camelize-region-or-word)
                        (check (string= (buffer-string) "before fooBar after\n"))
                        (check (= (point) after))))

       (test "capicamelizing turns words from snake-case to capitalized camel-case"
             (in-buffer snake-case-word
                        (goto-char before)
                        (word-transforms:capicamelize-region-or-word)
                        (check (string= (buffer-string) "before FooBar after\n"))
                        (check (= (point) after))))

       (test "snakizing turns words from camel-case into snake-case"
             (in-buffer camel-case-word
                        (goto-char before)
                        (word-transforms:snakize-region-or-word)
                        (check (string= (buffer-string) "before foo_bar after\n"))
                        (check (= (point) after))))

       (test "snakizing turns words from capitalized camel-case into snake-case"
             (in-buffer capitalized-camel-case-word
                        (goto-char before)
                        (word-transforms:snakize-region-or-word)
                        (check (string= (buffer-string) "before foo_bar after\n"))
                        (check (= (point) after))))

       (buffers "
== snake-case-word
before -<before>-foo_bar-<after>- after
== camel-case-word
before -<before>-fooBar-<after>- after
== capitalized-camel-case-word
before -<before>-FooBar-<after>- after
"))

(scene "acting on a region"
       (before (setq transient-mark-mode t))

       (test "camelizing turns words from snake-case to camel-case"
             (in-buffer snake-case-words
                        (select-region before after)
                        (word-transforms:camelize-region-or-word)
                        (check (string= (buffer-string) "before fooBar bazQuux after\n"))))

       (test "capicamelizing turns words from snake-case to capitalized camel-case"
             (in-buffer snake-case-words
                        (select-region before after)
                        (word-transforms:capicamelize-region-or-word)
                        (check (string= (buffer-string) "before FooBar BazQuux after\n"))))

       (test "snakizing turns words from camel-case into snake-case"
             (in-buffer camel-case-words
                        (select-region before after)
                        (word-transforms:snakize-region-or-word)
                        (check (string= (buffer-string) "before foo_bar baz_quux after\n"))))

       (test "snakizing turns words from capitalized camel-case into snake-case"
             (in-buffer capitalized-camel-case-words
                        (select-region before after)
                        (word-transforms:snakize-region-or-word)
                        (check (string= (buffer-string) "before foo_bar baz_quux after\n"))))

       (buffers "
== snake-case-words
before -<before>-foo_bar baz_quux-<after>- after
== camel-case-words
before -<before>-fooBar bazQuux-<after>- after
== capitalized-camel-case-words
before -<before>-FooBar BazQuux-<after>- after
"))
