;; rules: https://en.wikipedia.org/wiki/Rule_110
(define (next_cell current_cell)
    (cond current_cell 
      ('(1 1 1) 0) 
      ('(1 1 0) 1)
      ('(1 0 1) 1)
      ('(1 0 0) 0)
      ('(0 1 1) 1)
      ('(0 1 0) 1)
      ('(0 0 1) 1)
      ('(0 0 0) 0)
    ))

(display (next_cell '(1 1 0)))
(newline)
