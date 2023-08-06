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

(place-images
 (list
               (square 64 "solid" "red") 
               (square 64 "solid" "white")
               (square 64 "solid" "green")
               (square 64 "solid" "yellow"))
 (list
  (make-posn 32 32)
  (make-posn 96 32)
  (make-posn 32 96)
  (make-posn 96 96))
 (rectangle 128 128 "solid" "blue"))
              


  