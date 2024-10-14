(define filename "fixtures/data.scm")
(define in (open-input-file filename))
(define raw-data (read in))
(close-input-port in)

(cdr raw-data)
