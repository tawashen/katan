#lang racket

(require 2htdp/universe 2htdp/image lang/posn)
(require srfi/1)
(require srfi/13)
(require racket/struct)
(require racket/match)

(struct WORLD (PLAYERS MAP PHASE TURN))
(struct CARD (WOOD BLOCK IRON SHEEP))


;data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *map-zero* '(
                     (1 2 3 4)
                     (2 3 4 1)
                     (3 4 1 2)
                     (4 1 2 3)
                     ))


(define *cross-p* '(#f #f #f #f #f #f v1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)) ;25

;(define *roads-p* '(1 1 #f #f 1 #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
;                     #f #f #f #f #f #f #f #f #f #f 1 2 #f #f #f)) ;40

;(define *roads-p* '(1 #f #f 1 #f 1 #f #f 1 1 #f #f #f 1 #f 1 1 #f #f #f 1 #f #f #f #f
;                     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define *roads-p* '(1 #f #f 1 #f 1 #f #f 1 1 1 #f #f 1 #f 1 1 1 1 #f 1 #f #f 1 1
                     #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f)) ;

(define (x40 x y)
  (make-posn (+ (* x 40) (* (- x 1) 40))
             (+ (* y 40) (* (- y 1) 40))))




;補助関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tate&yoko x)
  (cond ((<= x 4) 'yoko) 
        ((<= 5 x 9) 'tate) ;(and (>= x 5) (<= x 9)) 'tate) 
        ((<= 10 x 13) 'yoko) ; and (>= x 10) (<= x 13)) 'yoko)
        ((<= 14 x 18) 'tate) ;(and (>= x 14) (<= x 18)) 'tate)
        ((<= 19 x 22) 'yoko) ;(and (>= x 19) (<= x 22)) 'yoko)
        ((<= 23 x 27) 'tate) ;(and (>= x 23) (<= x 27)) 'tate)
        ((<= 28 x 31) 'yoko) ;(>= x 28) (<= x 31)) 'yoko)
        ((<= 32 x 36) 'tate) ; (>= x 32) (<= x 36)) 'tate)
        (else 'yoko)))

(define (tate&yokoX x)
  (cond ((<= x 4) (+ 50 (* 80 (- x 1))))
        ((<= 5 x 9) (+ 34 (* 80 (- x 5))))
        ((<= 10 x 13) (+ 50 (* 80 (- x 10))))
        ((<= 14 x 18) (+ 34 (* 80 (- x 14))))
        ((<= 19 x 22) (+ 50 (* 80 (- x 19))))
        ((<= 23 x 27) (+ 34 (* 80 (- x 23))))
        ((<= 28 x 31) (+ 50 (* 80 (- x 28))))
        ((<= 32 x 36)(+ 34 (* 80 (- x 32))))
        (else (+ 50 (* 80 (- x 37))))))

(define (tate&yokoY x)
    (cond ((<= x 4) 36)
        ((<= 5 x 9)  50)
        ((<= 10 x 13) 116)
        ((<= 14 x 18) 130)
        ((<= 19 x 22) 196)
        ((<= 23 x 27)  210)
        ((<= 28 x 31) 276)
        ((<= 32 x 36) 290)
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
 (rectangle 800 400 "solid" "blue")))




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
   "left" "top" (place-town)))


(struct PLAYER (NO COLOR SCORE CARDS))
(define test-cards (CARD 1 2 3 4))
(define PLAYER-1 (PLAYER 1 "black" 0 test-cards))

  
#|
(define (place-status)
  (match-let (((PLAYER NO COLOR SCORE CARDS) PLAYER-1))
  (match-let (((CARD WOOD BLOCK IRON SHEEP) CARDS))
    (place-images/align
     (list
  (text (format "PLAYER ~a~%" NO) 15 COLOR)
  (text (format "CARDS 木：~a 土：~a 鉄：~a 羊：~a~%" WOOD BLOCK IRON SHEEP) 15 COLOR)
  (text (format "SCORE ~a~%" SCORE) 15 COLOR)
  (text (format "MAX-ROAD-LENGTH ~a~%"  15 COLOR
  )
  
    (list
     (make-posn 420 40)
     (make-posn 450 60)
     (make-posn 450 80)
     (make-posn 450 100)
     )
     "left" "top"
    (place-image/align (rectangle 380 380 "solid" "white") 400 10 "left" "top" (place-number))))))
    ))
|#
(place-number)

;町村が置けるかチェック関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;隣り合う交点には町村を設置できない


(define *cross-p2* '(#f #f #f #f v1 #f t1 #f #f #f #f #f #f #f v1 #f t1 #f #f #f #f #f #f #f #f)) ;25
(define (tonari-VT? cross-map c-point) ;隣に町村がある？
  (let ((c-num (- c-point 1)))
  (if (or
           (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (- c-num 1)) (not (= 0 (remainder c-num 5)))))
           (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (- c-num 5)) (not (<= c-point 4)))) 
           (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (+ c-num 1)) (not (= 0 (remainder c-point 5)))))
           (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (+ c-num 5)) (not (>= c-point 21)))))
      #t #f)))

;(tonari-VT? *cross-p2* 16)

;道が伸びてる交点にしか町村を設置できない

(define *roads-p2* '(1 1 #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 2
                     #f #f #f #f #f 2 #f #f #f #f #f #f #f #f 1)) ;40

;p2                  r #f 1 2 6       p4 #f 3 4 8         1-5    ?(-5) -1 0 4 gyou1 
;point7の場合　road6,10,11,15  P8 R7,11,12,16              6-10   -1 3 4 8 gyou2
;point12        r15,19,20,24      P13 r16,20,21,25        11-15  3 7 8 12 gyou3
;point17       r24 28 29 33      P18 r25 29 30 34         16-20  7 11 12 16 gyou4
;p22             r33 37 38 42                             21-25  11 15 16 20 gyou5


(define (road-kiteru? road-map player c-point)
  (let ((gyou-num (* 4 (quotient (- c-point 1) 5))) ;指定ポイントの存在する行数->行倍率で足す数
        (c-num (- c-point 1))) ;list-ref用に変換したC-point
  (if (or
           (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+ (- c-num 1) gyou-num));
                                                        (not (= 0 (remainder c-num 5)))
                                                        (= player (list-ref road-map (+ (- c-num 1) gyou-num))))) ;left
           (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+ (- c-num 5) gyou-num))
                                                        (not (<= c-point 4))
                                                        (= player (list-ref road-map (+ (- c-num 5) gyou-num))))) ;up
           (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+ c-num gyou-num))
                                                        (not (= 0 (remainder c-point 5)))
                                                        (= player (list-ref road-map (+ c-num gyou-num))))) ;right
           (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+ (+ c-num 4) gyou-num))
                                                        (not (>= c-point 21))
                                                        (= player (list-ref road-map (+ (+ c-num 4) gyou-num)))))) ;down
      #t #f)))

;(road-kiteru? *roads-p* 2 21)

(define (can-build? cross-map road-map player c-point) ;playerが町村を作れる場所 list->list
  (if (and (not (tonari-VT? cross-map c-point)) (road-kiteru? road-map player c-point)) #t #f))

;(map (lambda (x) (can-build? *cross-p* *roads-p* 1 x)) (iota 25 1 1))


 
;(define (max-road-length roads player)
  


;main
