#lang racket

(require srfi/1)
(require 2htdp/universe 2htdp/image lang/posn)


(define *roads-p* '(1 1 #f 1 #f 1 #f #f 1 1 1 #f #f 1 #f 1 1 #f #f #f 1 #f #f #f #f
                     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)) ;


(define left (lambda (road-map c-point)
               (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (- (+ (- c-point 1) (* 4 (quotient (- c-point 1) 5))) 1));
                                                        (not (member c-point '(1 6 11 16 20))))))) ;
(define up (lambda (road-map c-point)
              (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (- (+ (- c-point 5) (* 4 (quotient (- c-point 1) 5))) 1))
                                                        (not (member  c-point '(1 2 3 4 5)))))))
(define right (lambda (road-map c-point)
                 (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map  (- (+ c-point (* 4 (quotient (- c-point 1) 5))) 1))
                                                        (not (member c-point '(5 10 15 20 25)))))))
(define down (lambda (road-map c-point)
               (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (- (+ (+ c-point 4) (* 4 (quotient (- c-point 1) 5))) 1))
                                                        (not (member c-point '(21 22 23 24 25)))))))
(define check-funcs `(,left ,up ,right ,down))
               

(define (hazi? roads c-point)
  (let ((gyou-num (* 4 (quotient (- c-point 1) 5))))
    (let loop ((funcs check-funcs) (counter 0))
      (if (null? funcs) (cond ((= 1 counter) #t)
                                          (else #f))
          (loop (cdr funcs) (if ((car funcs) roads c-point)
                                (+ counter 1) counter))))))

; (filter (lambda (x) (hazi? *roads-p* x)) (iota 25 1 1))


(define (dokohe? roads c-point)
  (cond ((left roads c-point) (- c-point 1))
        ((up roads c-point) (- c-point 5))
        ((right roads c-point) (+ c-point 1))
        ((down roads c-point) (+ c-point 5))))


(dokohe? *roads-p* 1)

(define (tadoru roads c-point point-list bunki max)
  (if (and (null? bunki) (car (dokohe? roads c-point)

(let loop ((roads *roads-p*) (c-point 1) (point-list '(1)) (bunki '()) (max 0))
  (if (and (null? bunki) (dokohe? roads 


(define (max-length roads cross-p)
  (let ((hazi-points (filter (lambda (x) (hazi? roads x)) (iota 25 1 1))))
  (let loop ((roads roads) (hazi-points hazi-points) (points '()) (max #f))
    (if (null? hazi-points) length
        (
