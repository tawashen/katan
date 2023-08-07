#lang racket

(require 2htdp/universe 2htdp/image lang/posn)
(require srfi/1)
(require srfi/13)
(require racket/struct)
(require racket/match)

(struct WORLD (PLAYERS MAP PHASE TURN))
(struct PLAYER (NAME COLOR CARDS ROAD VILLAGE TOWN))
(struct CARD (WOOD IRON SHEEP BLOCK))

(define *map-zero* '(
                     (0 1 2 3 4)
                     (1 2 3 4 0)
                     (2 3 4 0 1)
                     (3 4 0 1 2)
                     (4 0 1 2 3)))

(define (x32 x y)
  (make-posn (case x
                ((1) 32)
                (else (+ (* x 32) (* (- x 1) 32))))
              (case y
                ((1) 32)
                 (else (+ (* y 32) (* (- y 1) 32))))))

(place-images
 (map (lambda (x) (square 64 "solid"
                         (case x
                           ((0) "red")
                           ((1) "white")
                           ((2) "green")
                           ((3) "yellow")
                           (else "blue")))) (flatten *map-zero*))
  (flatten (for/list ((i (iota 5 1 1)))
             (for/list ((j (iota 5 1 1)))
    (x32 i j))))
 (rectangle 320 320 "solid" "blue"))




  