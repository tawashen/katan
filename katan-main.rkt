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
                     (1 2 3 4)
                     (2 3 4 1)
                     (3 4 1 2)
                     (4 1 2 3)
                     ))

(define (x40 x y)
  (make-posn (+ (* x 40) (* (- x 1) 40))
             (+ (* y 40) (* (- y 1) 40))))


(define (place-map)
(place-images/align
 (map (lambda (x) (square 80 "solid"
                         (case x
                           ((1) "red")
                           ((2) "white")
                           ((3) "green")
                           ((4) "yellow")
                           (else "blue")))) (flatten *map-zero*))
  (flatten (for/list ((i (iota 4 1 1)))
             (for/list ((j (iota 4 1 1)))
    (x40 i j)))) "left" "top"
 (rectangle 400 400 "solid" "blue")))

(define (place-town)
(place-images/align
 (list (text "●" 30 "olive"))
 (list (make-posn 32 28))
 "left" "top" (place-map)))


(define (place-road)
  (place-images/align
   (list (rectangle 60 10 "solid" "black")
         (circle 10 "solid" "yellow"))
   (list (make-posn 50 36)
         (make-posn 30 30))
   "left" "top" (place-town)))

(place-road)








  