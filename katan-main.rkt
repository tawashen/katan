#lang racket

(require 2htdp/universe 2htdp/image lang/posn)
(require srfi/1)
(require srfi/13)
(require racket/struct)
(require racket/match)

(struct WORLD (PLAYERS MAP PHASE TURN))
(struct PLAYER (NAME COLOR CARDS ROAD VILLAGE TOWN))
(struct CARD (WOOD IRON SHEEP BLOCK))


;data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *map-zero* '(
                     (1 2 3 4)
                     (2 3 4 1)
                     (3 4 1 2)
                     (4 1 2 3)
                     ))


(define *cross-p* '(t1 v1 #f #f #f #f #f #f #f v1 #f t1 v1 t1 v1 #f #f #f #f #f #f #f #f #f #f)) ;25

(define *roads-p* '(1 1 #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 2
                     #f #f #f #f #f 2 #f #f #f #f #f #f #f #f 1)) ;40

(define (x40 x y)
  (make-posn (+ (* x 40) (* (- x 1) 40))
             (+ (* y 40) (* (- y 1) 40))))




;補助関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  


;map配置関係;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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




(define (place-road)
  (place-images/align
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
          (loopB (cdr lst) (+ count 1)
                 (if (car lst)
                      (cons (make-posn (tate&yokoX count) (tate&yokoY count)) acc)
                       acc))))                                                         
   "left" "top" (place-map)))


(define (place-town)
  (place-images/align
   (let loopA ((lst *cross-p*) (acc '()))
     (if (null? lst) (reverse acc)
         (loopA (cdr lst) (if (car lst)
                              (cons (case (car lst)
                                      ((t1) (circle 10 "solid" "black"))
                                      ((v1) (triangle 20 "solid" "black"))
                                      (else (circle 10 "solid" "blue"))) acc)
                              acc))))
   
   (let loopB ((lst *cross-p*) (count 1) (acc '()))
     (if (null? lst) (reverse acc)
         (loopB (cdr lst) (+ count 1) (if (car lst)
                                          (cons (make-posn (+ 30 (* 80 (cond
                                                                 ((= (remainder count 5) 0) 4) 
                                                                 (else (- (remainder count 5) 1)))))
                                                           (+ 28 (* 80 (cond
                                                                 ((= (remainder count 5) 0) (- (quotient count 5) 1))
                                                                 (else (quotient count 5))))))
                                                              acc) acc))))
 "left" "top" (place-road)))


(define number-list '(1 2 3 4 5 5 6 6 7 8 8 9 9 10 11 12))
(define number-list-S (shuffle number-list))

(define (place-number)
  (place-images/align
   
   (map (lambda (x) (text (number->string x) 25 "black")) number-list-S)
  ; (list (text "9" 25 "black")
   ;      (text "12" 25 "black"))

  (let loopB ((lst number-list-S) (count 1) (acc '()))
     (if (null? lst) (reverse acc)
         (loopB (cdr lst) (+ count 1) 
                                (cons (make-posn (+ 70 (* 80 (cond
                                                       ((= (remainder count 4) 0) 3) 
                                                       (else (- (remainder count 4) 1)))))
                                                 (+ 70 (* 80 (cond
                                                       ((= (remainder count 4) 0) (- (quotient count 4) 1))
                                                       (else (quotient count 4))))))
                                                       acc)))) 
   
;   (list (make-posn 70 70)
 ;        (make-posn 150 150))

   "left" "top" (place-town)))


(place-number)












  