(define fd (open-output-file "testfile"))
(write "wazzp" fd)
(close-output-port fd)
