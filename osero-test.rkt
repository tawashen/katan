#lang racket


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

(define all-squares
  (let loop ((i 11) (squares '()))
    (if (> i 88)
        (reverse squares)
        (if (<= 1 (remainder i 10) 8)
            (loop (+ i 1) (cons i squares))
            (loop (+ i 1) squares)))))

(define initial-board-result (initial-board))
(display initial-board-result)


(define (print-board lst)
  (let loopA ((row 1) (lst lst))
    (if (<= row 8)
        (loopB ((col 1) (lst lst))
              (if (<= col 8)
        (case (car lst)
          (('outer) " ")
          (('empty) ".")
          (('black) "@")
          (else "0"))
        (loop 
