(load "fixtures/lib.scm")

(define contains_empty_list ((compose (curry any? zero?) (curry map length)) '((1 2 3) () (1 2))))
contains_empty_list
