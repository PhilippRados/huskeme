(define first (lambda (x) (lambda (y) (+ x y))))
(define snd (lambda (x) (lambda (y) (set! x 8) (+ x y))))

(define (bar) 
  (define s 19)
  s)

(write ((first 3) 2))
(write ((snd 3) 2))
(write (bar))
(write (bar))
(write (bar))
(write (bar))
