(define (baz . args) (cdr args))
(define n ((lambda (a b . args) (car args)) 1 12 34))
(define m ((lambda args (car args)) 1 12 34))

(cons n (cons m (baz 23 4 5)))
