(define (f x y)
  ((lambda (a b)
    (+ (* x (* a a))
          (* y b)
          (* a b)))
  (+ 1 (* x y))
  (- 1 y)))

(f 2 4)

; (define reverse-subtract
;   (lambda (x y) (- y x)))
; (write (reverse-subtract 7 10))

;; (define (foo . b)
;;   (+ b))
;; (write (foo 1 2 3))
