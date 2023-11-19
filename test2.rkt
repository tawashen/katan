#lang racket

(define (find-even lst)
  (for ([num lst]
        #:break (= num 2))
    (display num)))

(define numbers '(1 3 5 2 4 6 8))

 (find-even numbers)