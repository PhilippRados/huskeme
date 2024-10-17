(load "fixtures/lib.scm")

(define mapping (map (curry + 3) '(1 2 3 5 6 7)))
(define filtering (filter odd? mapping))

(list-ref filtering 1)
