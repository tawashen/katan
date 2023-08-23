#lang racket

;(require srfi/1)
(require 2htdp/universe 2htdp/image lang/posn)


(define *roads-p1* '(1 #f #f 1 #f 1 #f #f 1 1 #f #f #f 1 #f 1 1 1 1 #f 1 #f #f 1 #f
                    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)) ;

(define *roads-p2* '(1 #f #f 1 #f 1 #f #f 1 1 1 #f #f 1 #f 1 1 1 1 #f 1 #f #f 1 1
                     #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f)) 


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
               

(define (road-num roads c-point)
  (let ((gyou-num (* 4 (quotient (- c-point 1) 5))))
    (let loop ((funcs check-funcs) (counter 0))
      (if (null? funcs) counter
          (loop (cdr funcs) (if ((car funcs) roads c-point)
                                (+ counter 1) counter))))))

(define (hazi? roads c-point)
  (if (= 1 (road-num roads c-point)) #t #f))

; (filter (lambda (x) (hazi? *roads-p* x)) (iota 25 1 1))


(define (dokohe? roads c-point)
  (flatten (for/list ((func check-funcs)
             (change `(,(- c-point 1) ,(- c-point 5) ,(+ c-point 1) ,(+ c-point 5))))
    (if (func roads c-point) change '()))))



(define (ippon-length roads c-point)
  (let loop ((c-point-d c-point) (point-list `(,c-point)) (pre-p #f))
    (cond ((and (not (= c-point (car point-list))) (hazi? roads c-point-d)) (display (reverse point-list)));終了
        ;  ((member c-point-d point-list) (display (reverse point-list)));円環パターン終了
          (else
           (let ((num (remove pre-p (dokohe? roads c-point-d))))
             (cond ((>= (length num) 2)
                    (for/list ((num num))
                      (loop num (cons num point-list) c-point-d)))
                          ;  (if (< (length max) (length point-list))
                          ;  point-list)))
                            ;max))))
                   (else
                   (loop (car num) (cons (car num)  point-list) c-point-d))))))))
                          ;(if (< (length max) (length point-list))
                        ; point-list))))))))
                         ;max)))))))))


(ippon-length *roads-p1* 1)

;(define (




#|
(define (tadoru roads c-point point-list bunki max) ;bunkiにポイントと向かった先、その時点のpoint-list '(bunki-point yukisaki point-list) 
  (cond  ((and (null? bunki) (not (null? point-list)) (hazi? roads c-point)) max) ;終了条件
         ((and bunki (not (null? point-list)) (hazi? roads c-point)) (tadoru roads (car bunki) 
  
         
                              


(define (max-length roads cross-p)
  (let ((hazi-points (filter (lambda (x) (hazi? roads x)) (iota 25 1 1))))
  (let loop ((roads roads) (hazi-points hazi-points) (points '()) (max #f))
    (if (null? hazi-points) length
        (
|#