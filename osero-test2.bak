#lang racket




(define (valid-p move)
  (and (integer? move) (<= 11 move 88)
       (not (or (= 0 (remainder move 10)) (= 9 (remainder move 10))))))

      
(define (legal-p move player board)
  (and (eqv? (vector-ref board move) empty)
       (member #t (map (lambda (dir) (would-flip? move player board dir))
  all-directions))))

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
        ((equal? (vectro-ref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (else 
        
   








