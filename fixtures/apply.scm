(define apaply
  (lambda (f g)
    (lambda args
      (apply f (apply g args) args))))

((apaply + *) 12 75)
