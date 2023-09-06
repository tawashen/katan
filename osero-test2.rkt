#lang racket

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



(define all-directions '(-11 -10 -9 -1 1 9 10 11))
(define (opponent player) (if (equal? player 'black) 'white 'black))

(define (valid-p move) ;ok
  (and (integer? move) ;整数で
       (<= 11 move 88) ;11-88で
       (not (or (= 0 (remainder move 10)) (= 9 (remainder move 10))))));両端で無いか

      
(define (legal-p move player board)
  (and (eqv? (vector-ref board move) empty) ;空のマス目か？
       (member #t (map (lambda (dir) (would-flip? move player board dir))
  all-directions))))

(map (lambda (x) (legal-p  x 'black data)) data)

(define (make-move move player board)
  (vector-set! (vector-ref board move) player)
  (for/list ((dir all-directions))
          (make-flips move player board dir))
  board)

(define (make-flips move player board dir)
  (let ((bracketer (would-flip? move player board dir)))
    (when bracketer
      (let loop ((c (+ move dir)) (board board))
        (if (equal? c bracketer) board
            (loop (+ c dir) (vector-set! board c player)))))))


(define (would-flip? move player board dir)
  (let ((c (+ move dir)))
    (and (equal? (vector-ref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(define (find-bracketing-piece square player board dir)
  (cond ((equal? (vector-ref board square) player) square)
        ((equal? (vector-ref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (else #f)))



        
   








