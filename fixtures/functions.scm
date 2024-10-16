(define n 2)

(define (foo a) 
  (define m 10)
  (+ a n (- m 3)))

(define n 10)

(foo 4)
