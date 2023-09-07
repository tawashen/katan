#lang racket


(require srfi/1)
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
  (let ((c (+ move dir))) ;c 打つ場所の一つDir側
    (and (equal? (vector-ref board c) (opponent player));一つDir側が敵側（White）であり
         (find-bracketing-piece (+ c dir) player board dir))))

(define (find-bracketing-piece square player board dir)
  (cond ((equal? (vector-ref board square) player) square);打つ場所の一つDir側の更に一つ先がが自分のコマだったら
        ((equal? (vector-ref board square) (opponent player));↑が敵のコマだったら
         (find-bracketing-piece (+ square dir) player board dir));更に一つ先のコマで再帰する
        (else #f)))

;(would-flip? 57 'black data -1)


(define (legal-p move player board)
  (and (equal? (vector-ref board move) 'empty) ;空のマス目か?
       (filter (lambda (dir) (would-flip? move player board dir))
  all-directions)))

(display (map (lambda (x) (legal-p x 'black data)) (iota 100)))


;(display (map (lambda (move) (would-flip? move 'black data -1)) (iota 99 1 1)))
;(display (map (lambda (x) (equal? (vector-ref data x) 'empty)) (iota 100)))

(define (some prc lst)
  (

        
   








