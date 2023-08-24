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
    (cond ((and (not (= c-point (car point-list))) (hazi? roads c-point-d)) (reverse point-list));(display (reverse point-list)));終了
        ;  ((member c-point-d point-list) (display (reverse point-list)));円環パターン終了
          (else
           (let ((num (remove pre-p (dokohe? roads c-point-d))))
           ;  (display num)
          ;   (cond ((>= (length num) 2)
                    (for/list ((num num))
                      (loop num (cons num point-list) c-point-d)))))))
                           ; (if (< (length max) (length point-list))
                           ; point-list)))))))
                            ;max))))
               ;    (else
                  ;  (loop (car num) (cons (car num)  point-list) c-point-d
                         ; (if (< (length max) (length point-list)) point-list max)))))))))


(ippon-length *roads-p2* 1)

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


(define test '((    (       ((((1 2 7 6 11 12 17))))       ((((1 2 7 8 13 14 9)) ((1 2 7 8 13 18 23))))  ) ) ) )


(define (tansaku lst acc)
  (cond ((number? (car lst)) acc) ;数字まで到達したら終了
        ((and (list? (car lst)) (number? (car (car lst)))) (tansaku (car lst) (cons (car lst) acc)));()に数字の入ってるリストが来たらAccに追加
        ((not (null? (cdr lst))) (tansaku (car lst) acc)  (tansaku (cdr lst) acc));Cdr部がNullでなければCar、Cdrをそれぞれ再帰
        ((and (list? (car lst)) (not (number? (car (car lst))))) (tansaku (car lst) acc))));Carで一段階中に入ってそのリストのCarが数字でなければCarで再帰

;(tansaku test '())






(define tes1 '((1 2 7 6 11 12 17)))
(define tes2 '(((1 2 7 6 11 12 17))))
#|
(and (list? (car tes1)) (number? (car (car tes1))))
(and (list? (car tes2)) (not (number? (car (car tes2)))))
(cdr tes1)
(cdr tes2)
|#

(define test2 '(       ((((1 2 7 6 11 12 17))))       ((((1 2 7 8 13 14 9)) ((1 2 7 8 13 18 23))))))
;(cdr test2)

(define (search-for-numbers lst)
  (cond
    [(null? lst) '()] ; リストが空の場合は空リストを返す
    [(number? (car lst)) ; 先頭が数値の場合
     (cons (car lst) (search-for-numbers (cdr lst)))] ; 数値を結果のリストに追加し、残りの部分に再帰
    [(list? (car lst)) ; 先頭がリストの場合
     (append (search-for-numbers (car lst)) (search-for-numbers (cdr lst)))] ; リスト内を再帰的に検索
    [else
     (search-for-numbers (cdr lst))])) ; それ以外の場合はスキップして次の要素を探索

(define testv '((    (       ((((1 2 7 6 11 12 17))))       ((((1 2 7 8 13 14 9)) ((1 2 7 8 13 18 23))))  ) ) ) )

;(search-for-numbers testv)

(define test3 '(1 2 7 6 11 12 17 1 2 7 8 13 14 9 1 2 7 8 13 18 23))

(define (devide-numbers lst c-point)
  (let loop ((lst lst) (acc '()) (temp '()))
  (cond ((null? lst) (reverse (cons (reverse temp) acc)))
        ((= (car lst) c-point) (loop (cdr lst) (if (null? temp) acc (cons (reverse temp) acc)) (list c-point)))
        (else (loop (cdr lst) acc (cons (car lst) temp))))))

(devide-numbers test3 1)