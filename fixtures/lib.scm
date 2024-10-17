(define (not x)            (if x #f #t))
(define (null? obj)        (if (eqv? obj '()) #t #f))
(define (id obj)           obj)
(define (list . objs)       objs)

(define (curry func arg1)  (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g)      (lambda (arg) (f (apply g (list arg)))))

(define zero?              (curry = 0))
(define positive?          (curry < 0))
(define negative?          (curry > 0))
(define (odd? num)         (= (mod num 2) 1))
(define (even? num)        (= (mod num 2) 0))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))
(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (map func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define (list-ref lst i)
  (if (= i 0)
    (car lst)
    (list-ref (cdr lst) (- i 1))))

(define (length lst)
  (if (null? lst)
    0
    (+ 1 (length (cdr lst)))))

(define (any? pred lst)
  (positive? (length (filter pred lst))))
