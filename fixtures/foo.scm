;; (define n (lambda args
;;       (apply + (apply * args) args)))

;; (define n (lambda (args)
;;       (apply + (apply * args) args)))

;; (write (apply + 1 4 '(1 2)))
;; (write (n '(1 2)))
(define arguments '(10 50 100))
(write (apply + arguments))
