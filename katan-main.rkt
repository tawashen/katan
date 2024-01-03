#lang racket

(require 2htdp/universe 2htdp/image lang/posn)
(require srfi/1)
(require srfi/13)
(require racket/struct)
(require racket/match)

(struct WORLD (PLAYERS MAP PHASE TURN))
(struct CARD (WOOD BLOCK IRON SHEEP))

(struct PLAYER (NO COLOR SCORE CARDS))
(define test-cards (CARD 1 2 3 4))
(define PLAYER-1 (PLAYER 1 "black" 0 test-cards))


;data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;マップの元になるデータ、各パネルに対応
(define *map-zero* '(
                     (1 2 3 4)
                     (2 3 4 1)
                     (3 4 1 2)
                     (4 1 2 3)
                     ))

;交差点の座標
(define *cross-p* '(#f #f #f #f #f #f v1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)) ;25

;(define *roads-p* '(1 1 #f #f 1 #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
;                     #f #f #f #f #f #f #f #f #f #f 1 2 #f #f #f)) ;40

;(define *roads-p* '(1 #f #f 1 #f 1 #f #f 1 1 #f #f #f 1 #f 1 1 #f #f #f 1 #f #f #f #f
;                     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

;道路のリスト４，５，４，５，４，５で分けられる
(define *roads-p* '(1 2 #f 1 #f 1 #f #f 1 1 1 #f #f 1 #f 1 1 1 1 #f 1 #f #f 1 1
                     #f #f #f #f #f #f #f #f 1 #f #f #f #f 1 #f)) ;

;座標を実際のグラに合わせる変数
(define (x40 x y)
  (make-posn (+ (* x 40) (* (- x 1) 40))
             (+ (* y 40) (* (- y 1) 40))))




;補助関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Cross-p　x　を縦横判別するため
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

;縦の道をグラ表示するための変数
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


;横の道を表示するための変数
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


;上下左右のポイントへ道が伸びているかをチェックする述語をそれぞれ別のクロージャで
(define left (lambda (road-map c-point color)
               (with-handlers ((exn:fail? (const #f))) (and
                                                        (= color (list-ref road-map (- (+ (- c-point 1)
                                                                   (* 4 (quotient (- c-point 1) 5))) 1)))
                                                        (not (member c-point '(1 6 11 16 20))))))) ;
(define up (lambda (road-map c-point color)
              (with-handlers ((exn:fail? (const #f))) (and
                                                       (= color (list-ref road-map (- (+ (- c-point 5)
                                                                  (* 4 (quotient (- c-point 1) 5))) 1)))
                                                        (not (member  c-point '(1 2 3 4 5)))))))
(define right (lambda (road-map c-point color)
                 (with-handlers ((exn:fail? (const #f))) (and
                                                          (= color (list-ref road-map  (- (+ c-point
                                                                     (* 4 (quotient (- c-point 1) 5))) 1)))
                                                        (not (member c-point '(5 10 15 20 25)))))))
(define down (lambda (road-map c-point color)
               (with-handlers ((exn:fail? (const #f))) (and
                                                        (= color (list-ref road-map (- (+ (+ c-point 4)
                                                                   (* 4 (quotient (- c-point 1) 5))) 1)))
                                                        (not (member c-point '(21 22 23 24 25)))))))
  



;町村が置けるかチェック関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;隣り合う交点には町村を設置できない
(define *cross-p2* '(#f #f #f #f v1 #f t1 #f #f #f #f #f #f #f v1 #f t1 #f #f #f #f #f #f #f #f)) ;25

;隣に町村がある？　→Bool
(define (tonari-VT? cross-map c-point)
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

;現在のポイントに自道路が繋がっているか？　→Bool
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


;任意のポイントにに町村を作ることが出来るか？
 ;playerが町村を作れる場所 list->list
(define (can-build? cross-map road-map player c-point)
  (if (and (not (tonari-VT? cross-map c-point)) (road-kiteru? road-map player c-point)) #t #f))

;(map (lambda (x) (can-build? *cross-p* *roads-p* 1 x)) (iota 25 1 1))


;ロンゲストロード関連;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;上下左右チェッククロージャをリストにしておく
(define check-funcs `(,left ,up ,right ,down))
               

;ポイントから伸びている道の数を返す
(define (road-num roads c-point color)
 ; (let ((gyou-num (* 4 (quotient (- c-point 1) 5))));何行目かを調べて束縛
    (let loop ((funcs check-funcs) (counter 0))
      (if (null? funcs) counter
          (loop (cdr funcs) (if ((car funcs) roads c-point color)
                                (+ counter 1) counter)))))


;伸びている道が1本であれば端と判定する
(define (hazi? roads c-point color)
  (if (= 1 (road-num roads c-point color)) #t #f))




;ポイントから道が伸びているポイントを返す ->num-list
(define (dokohe? roads c-point color)
  (flatten (for/list ((func check-funcs);上下左右が繋がっているかをチェックするクロージャのリスト
             (change `(,(- c-point 1) ,(- c-point 5) ,(+ c-point 1) ,(+ c-point 5))));↑に対応する座標変化のリスト
    (if (func roads c-point color);4方向それぞれで道が来ているかチェックして
        change;Boolのリストを作る
        '()))));無い場合はNullを

(define (my-remove val lst)
  (let loop ((val val) (lst lst) (acc '()))
  (if (null? lst) (reverse acc)
      (loop val (cdr lst) (if (equal? val (car lst)) acc (cons (car lst) acc))))))

;(my-remove 1 '(1 2 3 4 5))

#|
(define final 0)
(define (longest-r roads c-point p-point color longest count)
      (if (and ;終着点か？
           (= 1 (length (dokohe? roads c-point color)));向かってるポイントが１つだけ
           (= (car (dokohe? roads c-point color)) p-point));向かってるポイントがもと来た方向だけ
          (if (> longest final) (set! final longest) '());副作用で大域変数を更新
          (for ((next-point (my-remove p-point (dokohe? roads c-point color))))
            (let ((val (longest-r roads next-point c-point color (if (> count longest) count longest) (+ 1 count))))
              (display "val ") (display val) (display " point ") (display c-point) (newline)))) longest)

(longest-r *roads-p* 1 1 1 0 1)
|#
(define (longest-r2 roads c-point color)
  (define final 0)
  (let loop ((roads roads) (c-point c-point) (p-point c-point) (color color) (longest 0) (count 1))
      (if (and ;終着点か？
           (= 1 (length (dokohe? roads c-point color)));向かってるポイントが１つだけ
           (= (car (dokohe? roads c-point color)) p-point));向かってるポイントがもと来た方向だけ
          (if (> longest final) (set! final longest) '());副作用で大域変数を更新
          (for ((next-point (my-remove p-point (dokohe? roads c-point color))))
            (let ((val (loop roads next-point c-point color (if (> count longest) count longest) (+ 1 count))))
              '()))) longest) final)

;あるポイントから伸びる道のロンゲストを探す
(define (longest-r3 roads c-point color)
  (define final 0)
  (let loop ((roads roads) (c-point c-point) (p-point c-point) (color color) (longest 0) (count 1))
      (if (and ;終着点か？
           (= 1 (length (dokohe? roads c-point color)));向かってるポイントが１つだけ
           (= (car (dokohe? roads c-point color)) p-point));向かってるポイントがもと来た方向だけ
          (if (> longest final) (set! final longest) '());副作用で大域変数を更新
          (for/list ((next-point (my-remove p-point (dokohe? roads c-point color))))
            (let ((val (loop roads next-point c-point color (if (> count longest) count longest) (+ 1 count))))
              longest)))) final)
  
;全マスから自道の端を探して最長を探す
(define (max-longest-r3 roads color)
  (apply max (map (lambda (x) (longest-r3 *roads-p* x color))
     (filter (lambda (y) (hazi? *roads-p* y color)) (range 1 25 1)))))

(max-longest-r3 *roads-p* 1)


              
     
      


;ポイントから道路で繋がっているポイントをリストにする
(define (roads-point-list roads c-point color)
  (let loop ((c-point-d c-point) (point-list `(,c-point)) (pre-p c-point) (color color))
    (cond ((and (not (= c-point (car point-list))) (hazi? roads c-point-d color)) (reverse point-list))
        ;  ((member c-point-d point-list) (display (reverse point-list)));円環パターン終了
          (else
           (let ((num (remove (lambda (x) (= pre-p x)) (dokohe? roads c-point-d color))))
                    (for/list ((num num))
                      (loop num (cons num point-list) c-point-d color)))))))


;(roads-point-list *roads-p* 1 1)


#|
;自作探索関数　（（あ））を探すため、動かず 後で動くようにしてみる？
(define (tansaku lst acc)
  (cond ((number? (car lst)) acc) ;数字まで到達したら終了
        ((and (list? (car lst)) (number? (car (car lst)))) (tansaku (car lst) (cons (car lst) acc)));()に数字の入ってるリストが来たらAccに追加
        ((not (null? (cdr lst))) (tansaku (car lst) acc)  (tansaku (cdr lst) acc));Cdr部がNullでなければCar、Cdrをそれぞれ再帰
        ((and (list? (car lst)) (not (number? (car (car lst))))) (tansaku (car lst) acc))));Carで一段階中に入ってそのリストのCarが数字でなければCarで再帰
|#

;（（あ））を取り出して単一のリストにする関数
(define (search-for-numbers lst)
  (cond
    [(null? lst) '()] ; リストが空の場合は空リストを返す
    [(number? (car lst)) ; 先頭が数値の場合
     (cons (car lst) (search-for-numbers (cdr lst)))] ; 数値を結果のリストに追加し、残りの部分に再帰
    [(list? (car lst)) ; 先頭がリストの場合
     (append (search-for-numbers (car lst)) (search-for-numbers (cdr lst)))] ; リスト内を再帰的に検索
    [else
     (search-for-numbers (cdr lst))])) ; それ以外の場合はスキップして次の要素を探索


;単一リストを任意の数値を頭にして複数のリストを作成
(define (devide-numbers lst c-point)
  (let loop ((lst lst) (acc '()) (temp '()))
  (cond ((null? lst) (reverse (cons (reverse temp) acc)))
        ((= (car lst) c-point) (loop (cdr lst) (if (null? temp) acc (cons (reverse temp) acc)) (list c-point)))
        (else (loop (cdr lst) acc (cons (car lst) temp))))))

;自分のロンゲストロードの最終的出力
(define (my-longest-road roads c-point color)
  (apply max (map (lambda (x) (- (length x) 1))
                     (devide-numbers (search-for-numbers (roads-point-list roads c-point color)) c-point))))


  
 
  
;画面表示関係;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (place-map);資源マスを表示
(place-images/align
 (map (lambda (x) (square 80 "solid"
                         (case x
                           ((1) "red");
                           ((2) "white");羊
                           ((3) "green");木
                           ((4) "yellow");
                           (else "blue")))) (flatten *map-zero*))
  (flatten (for/list ((i (iota 4 1 1)))
             (for/list ((j (iota 4 1 1)))
    (x40 i j)))) "left" "top"
 (rectangle 800 400 "solid" "blue")));青バックを表示


(define (place-road);道を表示
  (place-images/align
    (let loop ((lst *roads-p*) (count 1) (acc '()));グラフィックループ
      (if (null? lst)
          (reverse acc)
          (loop (cdr lst) (+ count 1) (if (car lst) (cons (case (tate&yoko count)
                                                      ((yoko) (rectangle 60 10 "solid" (case (car lst);道が横向きの場合
                                                                                         ((1) "black")
                                                                                         ((2) "blue"))))
                                                      ((tate) (rectangle 10 60 "solid" (case (car lst);道が縦向きの場合
                                                                                         ((1) "black")
                                                                                         ((2) "blue"))))) acc)
                                          acc))))
    (let loopB ((lst *roads-p*) (count 1) (acc '()));座標ループ
      (if (null? lst)
          (reverse acc)
          (loopB (cdr lst) (+ count 1)
                 (if (car lst)
                      (cons (make-posn (tate&yokoX count) (tate&yokoY count)) acc)
                       acc))))                                                         
   "left" "top" (place-map)))


(define (place-town);町を表示
  (place-images/align
   (let loopA ((lst *cross-p*) (acc '()));グラフィックループ
     (if (null? lst) (reverse acc)
         (loopA (cdr lst) (if (car lst)
                              (cons (case (car lst)
                                      ((t1) (circle 10 "solid" "black"))
                                      ((v1) (triangle 20 "solid" "black"))
                                      (else (circle 10 "solid" "blue"))) acc)
                              acc))))
   
   (let loopB ((lst *cross-p*) (count 1) (acc '()));座標ループ
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

(define (place-number);マスのナンバー表示
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


(define (color-to-num color)
  (case color
    (("black") 1)))

(define (place-status);右枠のステータス表示
  (match-let (((PLAYER NO COLOR SCORE CARDS) PLAYER-1))
  (match-let (((CARD WOOD BLOCK IRON SHEEP) CARDS))
    (place-images/align
     (list
  (text (format "PLAYER~a" NO) 15 COLOR)
  (text (format "CARDS 木:~a 土:~a 鉄:~a 羊:~a" WOOD BLOCK IRON SHEEP) 15 COLOR)
  (text (format "SCORE: ~a" SCORE) 15 COLOR)
  (text (format "MAX-ROAD-LENGTH: ~a" (max-longest-r3 *roads-p* (color-to-num COLOR))) 15 COLOR)
  )
    (list
     (make-posn 420 40)
     (make-posn 450 60)
     (make-posn 450 80)
     (make-posn 450 100)
     )
     "left" "top"
    (place-image/align (rectangle 380 380 "solid" "white") 400 10 "left" "top" (place-number))))))
    

(place-status)
