(load "../huskeme/fixtures/lib.scm")

(write ((curry map length) '((1 2 3) () (1 2))))
