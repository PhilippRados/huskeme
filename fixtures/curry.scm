(define (foo a) (+ a 2))
(define (cur n f) (f n))
(cur 7 foo)
