(define g (+ 1 2))
(define (foo)
  (define g 6)
  (set! g 4)
  g)

(+ g (foo))
