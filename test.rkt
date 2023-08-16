#lang racket

(require srfi/1)
(require 2htdp/universe 2htdp/image lang/posn)


(define *roads-p* '(1 1 #f 1 #f 1 #f #f 1 1 1 #f #f 1 #f 1 1 #f #f #f 1 #f #f #f #f
                     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)) ;


(define left (lambda (road-map c-point gyou-num)
               (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+ (- (- c-point 1) 1) gyou-num));
                                                        (not (= 0 (remainder (- c-point 1) 5))))))) ;
(define up (lambda (road-map c-point gyou-num)
              (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+ (- c-point 1) gyou-num))
                                                        (not (<= c-point 4))))))
(define right (lambda (road-map c-point gyou-num)
                 (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+ (- c-point 1) gyou-num))
                                                        (not (= 0 (remainder c-point 5)))))))
(define down (lambda (road-map c-point gyou-num)
               (with-handlers ((exn:fail? (const #f))) (and (list-ref road-map (+  (+ c-point 3) gyou-num))
                                                        (not (>= c-point 21))))))
(define check-funcs `(,left ,up ,right ,down))
               

(define (hazi? roads c-point)
  (let ((gyou-num (* 4 (quotient (- c-point 1) 5))))
    (let loop ((funcs check-funcs) (counter 0))
      (if (null? funcs) (cond ((= 1 counter) #t)
                                          (else #f))
          (loop (cdr funcs) (if ((car funcs) roads c-point gyou-num)
                                (+ counter 1) counter))))))

(hazi? *roads-p* 5)


(define (max-length roads cross-p)
