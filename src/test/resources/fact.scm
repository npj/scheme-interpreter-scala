(define (fact n)
    (if (<= n 0)
        1
        (* n (fact (- n 1)))))

(map fact '(1 2 3 4 5 6))