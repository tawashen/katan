#lang racket

(define all-squares
  (let loop ((i 10) (squares '()))
    (if (= i 89)
        (reverse squares)
        (if (or (= 0 (remainder i 10)) (= 9 (remainder i 10)))
            (loop (+ i 1) squares)
            (loop (+ i 1) (cons i squares))))))


(define (initial-board)
  (let ((board (make-vector 100 'outer)))
    (define (set-piece! board square piece)
      (vector-set! board square piece))

    (define (initialize-board! board)
      (for-each
       (lambda (square)
         (set-piece! board square 'empty))
       all-squares)
      (set-piece! board 44 'white)
      (set-piece! board 45 'black)
      (set-piece! board 54 'black)
      (set-piece! board 55 'white))

    (initialize-board! board)
    board))



(define initial-board-result (initial-board))
(display initial-board-result)



(define data #(outer outer outer outer outer outer outer outer outer outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty white black empty empty empty outer outer
                     empty empty empty black white empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     outer outer outer outer outer outer outer outer outer))

(define (format-piece piece)
  (cond
    [(eq? piece 'outer) "O"]
    [(eq? piece 'empty) "."]
    [(eq? piece 'white) "W"]
    [(eq? piece 'black) "B"]))

(define (print-chessboard data)
  (for ([row (in-range 10)])
    (for ([col (in-range 10)])
      (display (format "~a " (format-piece (vector-ref data (+ col (* row 10)))))))
    (newline)))

(print-chessboard data)


