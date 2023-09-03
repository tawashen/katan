#lang racket


(define data #(outer outer outer outer outer outer outer outer outer outer outer empty empty
                     empty empty empty empty empty empty outer outer empty empty empty empty
                     empty empty empty empty outer outer empty empty empty empty empty empty
                     empty empty outer outer empty empty empty white black empty empty empty
                     outer outer empty empty empty black white empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer empty empty
                     empty empty empty empty empty empty outer outer empty empty empty empty
                     empty empty empty empty outer outer outer outer outer outer outer outer
                     outer outer outer))

(define (format-piece piece)
  (cond
    [(eq? piece 'outer) "O"]
    [(eq? piece 'empty) "."]
    [(eq? piece 'white) "W"]
    [(eq? piece 'black) "B"]))

(define (print-chessboard data)
  (for ([row (in-range 8)])
    (for ([col (in-range 8)])
      (display (format "~a " (format-piece (vector-ref data (+ col (* row 8)))))))
    (newline)))

(print-chessboard data)

