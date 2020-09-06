(define (fact n)
    (if (<= n 0)
        1
        (* n (fact (- n 1)))))

(define (outer n)
	(define (inner x)
        (if (= n x)
            x
            20))
    (inner 5))

(map fact '(1 2 3 4 5 6))