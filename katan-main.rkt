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


(define *cross-p* '(#f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f 2 #f #f #f #f #f #f #f #f)) ;25
(define *roads-p* '(1 1 #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 2
                     #f #f #f #f #f 2 #f #f #f #f #f #f #f #f 1)) ;40

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
 (list
  ;(text "●" 30 "olive")
  )
 (list
  ;(make-posn 32 28)
  )
 "left" "top" (place-map)))


(define (tate&yoko x)
  (cond ((<= x 4) 'yoko)
        ((and (>= x 5) (<= x 9)) 'tate)
        ((and (>= x 10) (<= x 13)) 'yoko)
        ((and (>= x 14) (<= x 18)) 'tate)
        ((and (>= x 19) (<= x 22)) 'yoko)
        ((and (>= x 23) (<= x 27)) 'tate)
        ((and (>= x 28) (<= x 31)) 'yoko)
        ((and (>= x 32) (<= x 36)) 'tate)
        (else 'yoko)))

(define (tate&yokoX x)
  (cond ((<= x 4) (+ 50 (* 80 (- x 1))))
        ((and (>= x 5) (<= x 9)) (+ 34 (* 80 (- x 5))))
        ((and (>= x 10) (<= x 13)) (+ 50 (* 80 (- x 10))))
        ((and (>= x 14) (<= x 18)) (+ 34 (* 80 (- x 14))))
        ((and (>= x 19) (<= x 22)) (+ 50 (* 80 (- x 19))))
        ((and (>= x 23) (<= x 27)) (+ 34 (* 80 (- x 23))))
        ((and (>= x 28) (<= x 31)) (+ 50 (* 80 (- x 28))))
        ((and (>= x 32) (<= x 36)) (+ 34 (* 80 (- x 32))))
        (else (+ 50 (* 80 (- x 37))))))

(define (tate&yokoY x)
    (cond ((<= x 4) 36)
        ((and (>= x 5) (<= x 9)) 50)
        ((and (>= x 10) (<= x 13)) 116)
        ((and (>= x 14) (<= x 18)) 130)
        ((and (>= x 19) (<= x 22)) 196)
        ((and (>= x 23) (<= x 27)) 210)
        ((and (>= x 28) (<= x 31)) 276)
        ((and (>= x 32) (<= x 36)) 290)
        (else 356)))
  

(define (place-road)
  (place-images/align
 ;  (list
   ; (circle 10 "solid" "yellow")
    (let loop ((lst *roads-p*) (count 1) (acc '()))
      (if (null? lst)
          (reverse acc)
          (loop (cdr lst) (+ count 1) (if (car lst) (cons (case (tate&yoko count)
                                                      ((yoko) (rectangle 60 10 "solid" (case (car lst)
                                                                                         ((1) "black")
                                                                                         ((2) "blue"))))
                                                      ((tate) (rectangle 10 60 "solid" (case (car lst)
                                                                                         ((1) "black")
                                                                                         ((2) "blue"))))) acc)
                                          acc))))
    (let loopB ((lst *roads-p*) (count 1) (acc '()))
      (if (null? lst)
          (reverse acc)
          (loopB (cdr lst) (+ count 1) (if (car lst) (cons (make-posn (tate&yokoX count) (tate&yokoY count)) acc) acc))))
                                                             
  ;                     (
   ; (rectangle 60 10 "solid" "black")
   ; (rectangle 60 10 "solid" "black")
   ; (rectangle 60 10 "solid" "black")

   ; (rectangle 10 60 "solid" "white")
   ; (rectangle 10 60 "solid" "blue")
   ; (rectangle 10 60 "solid" "blue")
     ;    )
 ;  (list
 ;   (make-posn 30 30)
  ;  (make-posn 50 36)
  ;  (make-posn 130 36)
  ;  (make-posn 50 116)
    
   ; (make-posn 34 50)　　
   ; (make-posn 114 50)
   ; (make-posn 114 130)
    ;     )
   "left" "top" (place-town)))

(place-road)












  