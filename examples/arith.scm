;; (if #t (define a 2) (define a 3))

(define a 2)
(define a 3)

(display a)

(begin (display a))

(define (summing . elems)
    (apply + elems))
