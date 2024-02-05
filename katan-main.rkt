
#lang racket/gui

(require 2htdp/universe 2htdp/image lang/posn)
(require (except-in srfi/1 remove))
(require srfi/13)
(require racket/struct)
(require racket/match)


;wood brick sheep wheat iron
(struct CARD (WOOD BRICK SHEEP WHEAT IRON) #:transparent #:mutable)
(define PLAYER-0-CARDS (CARD 0 0 0 0 0))
(define PLAYER-1-CARDS (CARD 0 0 0 0 0))
(define PLYAER-2-CARDS (CARD 0 0 0 0 0))
(define PLAYER-3-CARDS (CARD 0 0 0 0 0))

(struct PLAYER (NO COLOR SCORE CARDS D-CARDS KNIGHT)#:transparent);knight=盗賊を退治した回数
(define PLAYER-0 (PLAYER 0 "green" 0 PLAYER-0-CARDS '(() . (Roads Victory Victory Victory Victory)) 0))
(define PLAYER-1 (PLAYER 1 "red" 0 PLAYER-1-CARDS '(() . ()) 0))
(define PLAYER-2 (PLAYER 2 "orange" 0 PLYAER-2-CARDS '(() . ()) 0))
(define PLAYER-3 (PLAYER 3 "blue" 0 PLAYER-3-CARDS '(() . ()) 0))

(define *PLAYERS-LIST4* `(,PLAYER-0 ,PLAYER-1 ,PLAYER-2 ,PLAYER-3))
(define *PLAYERS-LIST3* `(,PLAYER-0 ,PLAYER-1 ,PLAYER-2))
(define *PLAYERS-LIST2* `(,PLAYER-0 ,PLAYER-1))

(define (my-circular lst);PHASE用Circular関数
  (append (cdr lst) (list (car lst))))

(define *PHASE-LIST-4* '(0 1 2 3))
(define *PHASE-LIST-3* '(0 1 2))
(define *PHASE-LIST-2* '(0 1))

(define *KNIGHT-KING* #f)




;data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;マップの元になるデータ、各パネルに対応
(define *map-zero* '(
                     (0 2 3 4)
                     (2 3 4 1)
                     (3 0 1 2)
                     (4 1 0 3)
                     ))



;wood brick sheep wheat iron
(define *material-list* '((0 . "WOOD") (1 . "BRICK") (2 . "SHEEP") (3 . "WHEAT") (4 . "IRON")))

(define d-card-rules '((14 . Knight) (5 . Victory) (2 . Roads) (2 . Discover) (2 . Monopoly)))

(define (make-d-cards rule-list)
  (for/list ((cards rule-list))
    (for/list ((times (range 0 (car cards))))
      (list (cdr cards)))))




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

;座標を実際のグラに合わせる変数
(define (x40 x y)
  (make-posn (+ (* x 40) (* (- x 1) 40))
             (+ (* y 40) (* (- y 1) 40))))


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
  





;(tonari-VT? *cross-p* 21)

;道が伸びてる交点にしか町村を設置できない

;現在のポイントに自道路が繋がっているか？　→Bool
(define (road-kiteru? road-map player c-point)
  (let ((gyou-num (* 4 (quotient c-point 5))) ;指定ポイントの存在する行数->行倍率で足す数
        (c-num c-point)) ;(display gyou-num)
    (if (or
         (with-handlers ((exn:fail? (const #f))) (and 
                                                      (not (member c-num '(0 5 10 15 20)));左端
                                                      (= player (list-ref road-map (- (+ c-num gyou-num) 1))))) ;left
         (with-handlers ((exn:fail? (const #f))) (and 
                                                      (not (<= c-point 4));上端
                                                      (= player (list-ref road-map (- (+ c-num gyou-num) 5))))) ;up
         (with-handlers ((exn:fail? (const #f))) (and 
                                                      (not (member c-num '(4 9 14 19 24)));右端
                                                      (= player (list-ref road-map (+ c-num gyou-num))))) ;right
         (with-handlers ((exn:fail? (const #f))) (and 
                                                      (not (>= c-point 20));下端
                                                      (= player (list-ref road-map (+ (+ c-num 4) gyou-num)))))) ;down
        #t #f)))





;ロンゲストロード関連;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;上下左右チェッククロージャをリストにしておく
(define check-funcs `(,left ,up ,right ,down))
               

;ポイントから伸びている道の数を返す
(define (road-num roads c-point color)
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




;円環道路対応版
(define (longest-r4 roads c-point color)
  (define final 0)
  (let loop ((roads roads) (c-point c-point) (p-point c-point) (color color) (longest 0) (count 1) (k-point '()))
    (if (or
         (and ;終着点か？
          (= 1 (length (dokohe? roads c-point color)));向かってるポイントが１つだけ
          (= (car (dokohe? roads c-point color)) p-point));向かってるポイントがもと来た方向だけ
         (member c-point k-point))
        (if (> longest final) (set! final longest) '());副作用で大域変数を更新
        (for/list ((next-point (my-remove p-point (dokohe? roads c-point color))))
          (let ((val (loop roads next-point c-point color
                           (if (> count longest) count longest) (+ 1 count) (cons c-point k-point))))
            longest)))) final)

;全マスから自道の端を探して最長を探す
(define (max-longest roads color)
  (with-handlers ((exn:fail? (const 0)))
    (apply max (map (lambda (x) (longest-r4 roads x color))
                    (filter (lambda (y) (hazi? roads y color)) (range 1 25 1))))))

                         


;ポイントから道路で繋がっているポイントをリストにする
(define (roads-point-list roads c-point color)
  (let loop ((c-point-d c-point) (point-list `(,c-point)) (pre-p c-point) (color color))
    (cond ((and (not (= c-point (car point-list))) (hazi? roads c-point-d color)) (reverse point-list))
          ;  ((member c-point-d point-list) (display (reverse point-list)));円環パターン終了
          (else
           (let ((num (remove (lambda (x) (= pre-p x)) (dokohe? roads c-point-d color))))
             (for/list ((num num))
               (loop num (cons num point-list) c-point-d color)))))))



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


;wood brick sheep wheat iron desert


(define wood-image (bitmap/file "pictures/wood.png"))
(define brick-image (bitmap/file "pictures/brick.png"))
(define sheep-image (bitmap/file "pictures/sheep.png"))
(define wheat-image (bitmap/file "pictures/wheat.png"))
(define iron-image (bitmap/file "pictures/iron.png"))
(define tile-list `(,wood-image ,brick-image ,sheep-image ,wheat-image ,iron-image))
(define robber-image (bitmap/file "pictures/robber.png"))
(define knight-image (bitmap/file "pictures/knight.png"))
(define road-image (bitmap/file "pictures/road.png"))

(define (place-map world);資源マスを表示
  (place-images/align
   (map (lambda (x) (list-ref tile-list x)) (flatten *map-zero*))
   (flatten (for/list ((i (iota 4 1 1)))
              (for/list ((j (iota 4 1 1)))
                (x40 i j)))) "left" "top"
                             (rectangle 800 400 "solid" "pink")));背景


(define (place-road world);道を表示
   (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
  (place-images/align
   (let loop ((lst R-MAP) (count 1) (acc '()));グラフィックループ
     (if (null? lst)
         (reverse acc)
         (loop (cdr lst) (+ count 1) (if (car lst)
                                         (let ((color (case (car lst)
                                                        ((0) "green") ((1) "red") ((2) "orange") ((3) "blue"))))
                                                (cons (case (tate&yoko count)
                                                           ((yoko) (rectangle 60 10 "solid" color))
                                                           ((tate) (rectangle 10 60 "solid" color))) acc))
                                         acc))))
   (let loopB ((lst R-MAP) (count 1) (acc '()));座標ループ
     (if (null? lst)
         (reverse acc)
         (loopB (cdr lst) (+ count 1)
                (if (car lst)
                    (cons (make-posn (tate&yokoX count) (tate&yokoY count)) acc)
                    acc))))                                                         
   "left" "top" (place-map world))))))

(define (place-road-waku world);道を表示
   (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
  (place-images/align
   (let loop ((lst R-MAP) (count 1) (acc '()));グラフィックループ
     (if (null? lst)
         (reverse acc)
         (loop (cdr lst) (+ count 1) (if (car lst) (cons (case (tate&yoko count)
                                                           ((yoko) (rectangle 60 10 "outline" "black"))
                                                           ((tate) (rectangle 10 60 "outline" "black"))) acc)
                                         acc))))
   (let loopB ((lst R-MAP) (count 1) (acc '()));座標ループ
     (if (null? lst)
         (reverse acc)
         (loopB (cdr lst) (+ count 1)
                (if (car lst)
                    (cons (make-posn (tate&yokoX count) (tate&yokoY count)) acc)
                    acc))))                                                         
   "left" "top" (place-road world))))))



(define (place-town world);町を表示
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (place-images/align
         (let loopA ((lst C-MAP) (acc '()));グラフィックループ
           (if (null? lst) (reverse acc)
               (loopA (cdr lst) (if (car lst);#fでなければ
                                      (let ((color (case (string-delete char-lower-case? (symbol->string (car lst)))
                                                        (("0") "green") (("1") "red") (("2") "orange") (("3") "blue"))))
                                    (cons (case (string-delete number-case? (symbol->string (car lst)))
                                            (("t") (circle 10 "solid" color))
                                            (("v") (triangle 20 "solid" color)))
                                          acc))
                                    acc))))
   
         (let loopB ((lst C-MAP) (count 1) (acc '()));座標ループ
           (if (null? lst) (reverse acc)
               (loopB (cdr lst) (+ count 1) (if (car lst)
                                                (cons (make-posn (+ 30 (* 80 (cond
                                                                               ((= (remainder count 5) 0) 4) 
                                                                               (else (- (remainder count 5) 1)))))
                                                                 (+ 30 (* 80 (cond
                                                                               ((= (remainder count 5) 0) (- (quotient count 5) 1))
                                                                               (else (quotient count 5))))))
                                                      acc) acc))))
         "left" "top" (place-road-waku world))))))

(define (place-town-waku world);町の黒枠を表示
   (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
  (place-images/align
   (let loopA ((lst C-MAP) (acc '()));グラフィックループ
     (if (null? lst) (reverse acc)
         (loopA (cdr lst) (if (car lst)
                              (cons (case (string-delete number-case? (symbol->string (car lst)))
                                      (("t") (circle 10 "outline" "black"))
                                      (("v") (triangle 20 "outline" "black")))
                                       acc)
                              acc))))
   
   (let loopB ((lst C-MAP) (count 1) (acc '()));座標ループ
     (if (null? lst) (reverse acc)
         (loopB (cdr lst) (+ count 1) (if (car lst)
                                          (cons (make-posn (+ 30 (* 80 (cond
                                                                         ((= (remainder count 5) 0) 4) 
                                                                         (else (- (remainder count 5) 1)))))
                                                           (+ 30 (* 80 (cond
                                                                         ((= (remainder count 5) 0) (- (quotient count 5) 1))
                                                                         (else (quotient count 5))))))
                                                acc) acc))))
   "left" "top" (place-town world))))))


(define number-list '(" 2" " 4" " 3" " 4" " 5" " 5" " 6" " 6" " 7" " 8" " 8" " 9" " 9" "10" "11" "12"));マスに表示する数字リスト
(define number-list-S (shuffle number-list));↑を混ぜて実際に使うリスト
(define (circle-list)
  (let loop ((lst number-list) (acc '()))
    (if (null? lst) (reverse acc)
        (loop (cdr lst) (cons (circle 16 "solid" "white") acc)))))

(define (place-circle world)
 (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
  (place-images/align
   (circle-list)
   (let loopB ((lst number-list-S) (count 1) (acc '()))
     (if (null? lst) (reverse acc)
         (loopB (cdr lst) (+ count 1) 
                (cons (make-posn (+ 70 (* 80 (cond
                                               ((= (remainder count 4) 0) 3) 
                                               (else (- (remainder count 4) 1)))))
                                 (+ 68 (* 80 (cond
                                               ((= (remainder count 4) 0) (- (quotient count 4) 1))
                                               (else (quotient count 4))))))
                      acc)))) 
   "left" "top" (place-town-waku world))))))


(define (knight-king? world);騎士王の条件を満たしているか？
 (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
        (if (member #t (map (lambda (x) (if (<= 3 (PLAYER-KNIGHT x)) #t #f)) PLAYERS-LIST)) #t #f)))

(define (who-is-knight-king world);KNIGHTが一番多いPLAYERのIndexを開けす ->Num
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (PLAYER-NO (car (sort PLAYERS-LIST #:key PLAYER-KNIGHT >)))))



(define (road-lord? world);PLAYERSの内一人でもMax-roadが5以上がいるか？の述語
 (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
        (if (member #t (map (lambda (x) (if (<= 5 (max-longest R-MAP (PLAYER-NO x)))#t #f)) PLAYERS-LIST)) #t #f)))

(define (who-is-road-lord world);PLAYERS-LISTのIndex ->num
    (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
      (let ((road-length-list
      (for/list ((player PLAYERS-LIST) (count (range 0 (length PHASE))))
        (cons count (max-longest R-MAP (PLAYER-NO player))))));'((0 . num) (1 . num) ...
         (car (car (sort road-length-list #:key cdr >)))))) ;numでSortして最長のIndexを返す



(define (place-robber-knight-road world);盗賊、騎士王、ロンゲストロードの表示
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((knight-king-player (who-is-knight-king world))
              (road-lord (who-is-road-lord world)))
          (place-images/align
           (filter (lambda (x) (not (null? x)))
                   (cons (if (knight-king? world) knight-image '())
                         (cons (if (road-lord? world) road-image '()) (cons robber-image '()))))

           (filter (lambda (y) (not (null? y)))
                   (cons (if (knight-king? world) (make-posn 370 (+ 65 (* 80 knight-king-player))) '())
                         (cons (if (road-lord? world) (make-posn 370 (+ 75 (* 80 road-lord))) '())
               

                               (cons (make-posn (+ 45 (* 80 (cond
                                                              ((member ROBBER '(0 4 8 12)) 0)
                                                              ((member ROBBER '(1 5 9 13)) 1)
                                                              ((member ROBBER '(2 6 10 14)) 2)
                                                              ((member ROBBER '(3 7 11 15)) 3))))
                                                ;(else (- (remainder ROBBER 4) 1)))))
                                                (+ 76 (* 80 (cond
                                                              ((member ROBBER '(0 1 2 3)) 0)
                                                              ((member ROBBER '(4 5 6 7)) 1)
                                                              ((member ROBBER '(8 9 10 11)) 2)
                                                              ((member ROBBER '(12 13 14 15)) 3)))))
                             '()))))                   
           "left" "top" (place-circle world)))))))


(define (place-number world);マスのナンバー表示
 (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
  (place-images/align  
   (map (lambda (x) (text x 25 "black")) number-list-S)
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
   "left" "top" (place-robber-knight-road world))))))


(define (place-index world);Knightを使用するときに選ぶIndexをパネルに表示
 (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((index (range 0 16)))
  (place-images/align  
   (map (lambda (x) (text x 25 "purple")) index)
   (let loopB ((lst number-list-S) (count 1) (acc '()))
     (if (null? lst) (reverse acc)
         (loopB (cdr lst) (+ count 1)
                (if (not (equal? (- count 1) ROBBER))
                (cons (make-posn (+ 70 (* 80 (cond
                                               ((= (remainder count 4) 0) 3) 
                                               (else (- (remainder count 4) 1)))))
                                 (+ 70 (* 80 (cond
                                               ((= (remainder count 4) 0) (- (quotient count 4) 1))
                                               (else (quotient count 4))))))
                      acc) acc)))) 
   "left" "top" (place-number world)))))))




(define (place-town-koho-waku world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((town-ok-points (make-town-ok-points C-MAP R-MAP NO)))
          (place-images/align
           (let((acc1
                 (let loopA ((lst town-ok-points) (acc '()));グラフィックループ
                   (if (null? lst) (reverse acc)
                       (loopA (cdr lst) (cons (circle 10 "outline" "black") acc)))))
                (acc2
                 (let loopB ((lst town-ok-points) (acc '()))
                   (if (null? lst) (reverse acc)
                       (loopB (cdr lst) (cons (text (number->string (car lst)) 25 "black") acc))))))
             (apply append `(,acc1 ,acc2)))
           (let loopB ((lst (apply append `(,(map add1 town-ok-points) ,(map add1 town-ok-points)))) (acc '()));座標ループ
             (if (null? lst) (reverse acc)
                 (loopB (cdr lst) 
                        (cons (make-posn (+ 30 (* 80 (cond
                                                       ((= (remainder (car lst) 5) 0) 4) 
                                                       (else (- (remainder (car lst) 5) 1)))))
                                         (+ 30 (* 80 (cond
                                                       ((= (remainder (+ 1 (car lst)) 5) 0) (- (quotient (+ 1 (car lst)) 5) 1))
                                                       (else (quotient (+ 1 (car lst)) 5)))))) acc))))
           "left" "top" (place-number world)))))))
  

(define (place-village-koho-waku world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((village-ok-points (make-village-ok-points C-MAP R-MAP NO)))
          (place-images/align
           (let((acc1
                 (let loopA ((lst village-ok-points) (acc '()));グラフィックループ
                   (if (null? lst) (reverse acc)
                       (loopA (cdr lst) (cons (triangle 20 "outline" "black") acc)))))
                (acc2
                 (let loopB ((lst village-ok-points) (acc '()))
                   (if (null? lst) (reverse acc)
                       (loopB (cdr lst) (cons (text (number->string (car lst)) 25 "violet") acc))))))
             (apply append `(,acc1 ,acc2)))
           (let loopB ((lst (apply append `(,(map add1 village-ok-points) ,(map add1 village-ok-points))))
                       (acc '()));座標ループ
             (if (null? lst) (reverse acc)
                 (loopB (cdr lst)                       
                        (cons (make-posn (+ 30 (* 80 (cond
                                                       ((= (remainder (car lst) 5) 0) 4) 
                                                       (else (- (remainder (car lst) 5) 1)))))
                                         (+ 30 (* 80 (cond
                                                       ((member (- (car lst) 1) '(0 1 2 3 4)) 0)
                                                       ((member (- (car lst) 1) '(5 6 7 8 9)) 1)
                                                       ((member (- (car lst) 1) '(10 11 12 13 14)) 2)
                                                       ((member (- (car lst) 1) '(15 16 17 18 19)) 3)
                                                       (else 4)))))
                                                      ; ((member (car lst) '(20 21 22 23 24)) 4)))))
                              acc))))
           "left" "top" (place-number world)))))))


(define (place-road-koho-waku world);建設可能な道を表示
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((road-ok-points (make-road-ok-points C-MAP R-MAP NO)))
            (place-images/align
             (let ((acc1
                    (let loop1 ((lst (map add1 road-ok-points)) (acc '()));グラフィックループ
                      (if (null? lst)
                          (reverse acc)
                          (loop1 (cdr lst) (cons (case (tate&yoko (car lst))
                                                   ((yoko) (rectangle 60 10 "outline" "black"))
                                                   ((tate) (rectangle 10 60 "outline" "black"))) acc)))))                                            
                   (acc2 (let loop2 ((lst (map add1 road-ok-points)) (acc '()))
                           (if (null? lst)
                               (reverse acc)
                               (loop2 (cdr lst) (cons (text (number->string (- (car lst) 1)) 20 "black") acc))))))
               (apply append `(,acc1 ,acc2)))
                                                   
             (let loopB ((lst (apply append `(,road-ok-points ,road-ok-points)))
                         (acc '()));座標ループ
               (if (null? lst)
                   (reverse acc)
                   (loopB (cdr lst)
                          (if (car lst)
                              (cons (make-posn (tate&yokoX (add1 (car lst))) (tate&yokoY (add1 (car lst)))) acc)
                              acc))))                     
             "left" "top" (place-number world)))))))


;wood brick sheep wheat iron
(define (place-status world);右枠のステータス表示
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (define gra-list '())
    (define posn-list '())
    (for ((player PLAYERS-LIST))
      (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) player))
        (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
          (set! gra-list (cons
                          (list
                           (text (format "PLAYER~a" NO) 15 COLOR)
                           (text (format "CARDS 木:~a 土:~a 羊:~a 麦:~a 鉄:~a " WOOD BRICK SHEEP WHEAT IRON) 15 COLOR)
                           (text (format "SCORE: ~a" SCORE) 15 COLOR)
                           (text (format "MAX-ROAD-LENGTH: ~a" (max-longest R-MAP NO)) 15 COLOR)
                           ) gra-list))
          (set! posn-list (cons 
                           (list
                            (make-posn 420 (+ (* NO 80) 40))
                            (make-posn 450 (+ (* NO 80) 60))
                            (make-posn 450 (+ (* NO 80) 80))
                            (make-posn 450 (+ (* NO 80) 100))
                            ) posn-list)))))
    (place-images/align
     (apply append (reverse gra-list))
     (apply append (reverse posn-list))    
     "left" "top"
     (place-image/align (rectangle 280 330 "solid" "white") 400 34 "left" "top"
                        (case DISP
                          ((ROAD-KOHO) (place-road-koho-waku world))
                          ((VILLAGE-KOHO) (place-village-koho-waku world))
                          ((TOWN-KOHO) (place-town-koho-waku world))
                          ((KNIGHT-KOHO) (place-index world))
                          (else (place-number world)))
                          ))))



;can-build-road関連;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;マップから自町村のあるポイントを返す
(define (make-town-points map player)
  (let ((village (case player
                   ((0) 'v0) ((1) 'v1) ((2) 'v2) ((3) 'v3)))
        (town (case player
                ((0) 't0) ((1) 't1) ((2) 't2) ((3) 't3))))
    (let loop ((points (range 0 25)) (point-list '()))
      (if (null? points) (reverse point-list)
          (loop (cdr points) (if (or (equal? (list-ref map (car points)) village)
                                     (equal? (list-ref map (car points)) town))
                                 (cons (car points) point-list) point-list))))))
;(make-town-points *cross-p* 1)

(define left2 (lambda (road-map c-point color)
                (with-handlers ((exn:fail? (const #f)))
                  (let ((val (- (+ c-point
                                   (* 4 (quotient c-point 5))) 1)))
                    (if (and
                         (eq? #f (list-ref road-map val))
                         (not (member c-point '(0 5 10 15 20)))) val #f)))))
                
(define up2 (lambda (road-map c-point color)
              (with-handlers ((exn:fail? (const #f)))
                (let ((val (- (+ c-point
                                 (* 4 (+ (quotient c-point 5) 1))) 9)))
                  (if (and
                       (eq? #f (list-ref road-map val))
                       (not (member c-point '(0 1 2 3 4)))) val #f)))))

(define right2 (lambda (road-map c-point color)
                 (with-handlers ((exn:fail? (const #f)))
                   (let ((val (+ c-point
                                 (* 4 (quotient c-point 5)))))
                     (if (and
                          (eq? #f (list-ref road-map val))
                          (not (member c-point '(4 9 14 19 24)))) val #f)))))
                 
(define down2 (lambda (road-map c-point color)
                (with-handlers ((exn:fail? (const #f)))
                  (let ((val (+ c-point
                                (* 4 (+ (quotient c-point 5) 1)))))
                    (if (and
                         (eq? #f (list-ref road-map val))
                         (not (member c-point '(20 21 22 23 24)))) val #f)))))

(define check-funcs2 `(,up2 ,left2 ,down2 ,right2))


;施設があるポイントから道が無い道座標を返す funcで#fが帰ってきた場合に
(define (null-road-neighbor-town C-MAP R-MAP PLAYER)
  (let ((town-list (make-town-points C-MAP PLAYER)))
    (filter (lambda (z) z)
            (apply append
                   (map (lambda (x) (map (lambda (y) (x R-MAP y PLAYER)) town-list)) check-funcs2))))) 

;集合的な合体関数
(define (set-set lst1 lst2)
  (if (null? lst1) lst2
      (set-set (cdr lst1) (if (member (car lst1) lst2) lst2 (cons (car lst1) lst2)))))




;隣が自分の道　か　隣接するポイントに自分の村・町がある
(define (can-build-road? C-MAP R-MAP PLAYER c-road);述語
  (case (tate&yoko (+ 1 c-road))
    ((tate)
     (cond
       ((member c-road '(4))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or
              (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
              (equal? PLAYER (list-ref R-MAP (- c-road 4)))
              (equal? PLAYER (list-ref R-MAP (+ c-road 9)))))
            #t #f))
       ((member c-road '(8))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or
              (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
              (equal? PLAYER (list-ref R-MAP (- c-road 5)))
              (equal? PLAYER (list-ref R-MAP (+ c-road 9)))))
            #t #f))
       ((member c-road '(31))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or
              (equal? PLAYER (list-ref R-MAP (- c-road 4)))
              (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
              (equal? PLAYER (list-ref R-MAP (- c-road 9)))))
            #t #f))
       ((member c-road '(35))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or
              (equal? PLAYER (list-ref R-MAP (- c-road 5)))
              (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
              (equal? PLAYER (list-ref R-MAP (- c-road 9)))))
            #t #f))

       ((member c-road '(5 6 7))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or
              (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
              (equal? PLAYER (list-ref R-MAP (+ c-road 5)))                      
              (equal? PLAYER (list-ref R-MAP (+ c-road 9)))))
            #t #f))
       ((member c-road '(32 33 34))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or
              (equal? PLAYER (list-ref R-MAP (- c-road 4)))
              (equal? PLAYER (list-ref R-MAP (- c-road 5)))                      
              (equal? PLAYER (list-ref R-MAP (- c-road 9)))))
            #t #f))         
       ((member c-road '(13 22))
        (if (and
             (equal? (list-ref R-MAP c-road) #f);現在の道用地が何もなくて
             (or 
              (equal? PLAYER (list-ref R-MAP (- c-road 4)))
              (equal? PLAYER (list-ref R-MAP (- c-road 9)))
              (equal? PLAYER (list-ref R-MAP (+ c-road 5)))   
              (equal? PLAYER (list-ref R-MAP (+ c-road 9)))))
            #t #f))
       ((member c-road '(17 26))
        (if (and
             (equal? (list-ref R-MAP c-road) #f);現在の道用地が何もなくて
             (or 
              (equal? PLAYER (list-ref R-MAP (- c-road 5)))
              (equal? PLAYER (list-ref R-MAP (- c-road 9)))
              (equal? PLAYER (list-ref R-MAP (+ c-road 4)))   
              (equal? PLAYER (list-ref R-MAP (+ c-road 9)))))
            #t #f))
       (else
        (if (and
             (equal? (list-ref R-MAP c-road) #f);現在の道用地が何もなくて
             (or (equal? PLAYER (list-ref R-MAP (- c-road 5)));いずれかに自道がある
                 (equal? PLAYER (list-ref R-MAP (- c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 9)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 9)))))
            #t #f))))
    ((yoko)
     (cond
       ((member c-road '(0))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (+ c-road 1)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 5)))))
            #t #f))
       ((member c-road '(3))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 1)))))
            #t #f))
       ((member c-road '(36))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (- c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 1)))))
            #t #f))
       ((member c-road '(39))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (- c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 1)))))
            #t #f))
       ((member c-road '(1 2))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 1)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 1)))))
            #t #f))
       ((member c-road '(37 38))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (- c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 1)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 1)))))
            #t #f))
       ((member c-road '(9 18 27))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 1)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 5)))
                 )) #t #f))         
       ((member c-road '(12 21 30))
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 1)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 5)))
                 )) #t #f))
       (else
        (if (and
             (equal? (list-ref R-MAP c-road) #f)
             (or (equal? PLAYER (list-ref R-MAP (+ c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 5)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 1)))
                 (equal? PLAYER (list-ref R-MAP (+ c-road 1)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 4)))
                 (equal? PLAYER (list-ref R-MAP (- c-road 5)))
                 )) #t #f))))))



(define (null-road-neighbor-road C-MAP R-MAP PLAYER)
  (let ((roads-bool (map (lambda (x) (can-build-road? C-MAP R-MAP PLAYER x)) (range 0 40))))
    (let loop ((counts (range 0 40)) (road-points '()))
      (if (null? counts) (reverse road-points)
          (loop (cdr counts)
                (if (list-ref roads-bool (car counts))
                    (cons (car counts) road-points) road-points))))))


(define (make-road-ok-points C-MAP R-MAP PLAYER);隣に
  (sort (set-set (null-road-neighbor-road C-MAP R-MAP PLAYER)
                 (null-road-neighbor-town C-MAP R-MAP PLAYER)) <))


;町村が置けるかチェック関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;隣に町村がある? →Bool
(define (tonari-VT? cross-map c-point)
  (let ((c-num c-point) (gyou-num (quotient c-point 5))); (display gyou-num)
    (if (or
         (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (- c-num 1)) (not (member c-point '(0 5 10 15 20)))));left
         (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (- c-num 5)) (not (<= c-point 4)))) ;up
         (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (+ c-num 1)) (not (member c-point '(4 9 14 19 24)))));right
         (with-handlers ((exn:fail? (const #f))) (and (list-ref cross-map (+ c-num 5)) (not (>= c-point 20)))));down
        #t #f)))


;任意のポイントにに町村を作ることが出来るか?
;playerが町村を作れる場所か？　述語
(define (can-build-town? cross-map road-map player c-point)
  (cond ((and (not (tonari-VT? cross-map c-point)) (road-kiteru? road-map player c-point)) #t)
        ((and (not (member player road-map)) (not (tonari-VT? cross-map c-point))) #t)
        ;↑初期化時、「道がない」のを利用して建設を可能にする
        (else #f)))

;(map (lambda (x) (can-build-town? *cross-p* *roads-p* 1 x)) (iota 25 1 1))


;全座標の中から村を作れる座標をリストで返す
(define (make-village-ok-points C-MAP R-MAP PLAYER);->point list (number)
  (let loop ((count 0) (acc '()))
    (if (= count 25) (reverse acc)
        (loop (+ 1 count) (if (and (can-build-town? C-MAP R-MAP PLAYER count)
                                   (equal? #f (list-ref C-MAP count)));その座標に既に町村がある場合は除く
                              (cons count acc) acc)))))

;全座標の中から村のある場所を返す（町に出来る場所）
(define (make-town-ok-points C-MAP R-MAP PLAYER);->point list
  (let ((points (range 0 25)))
    (let loop ((points points) (player (case PLAYER ((0) 'v0) ((1) 'v1) ((2) 'v2) ((3) 'v3)))
                               (village-points '()))
      (if (null? points) (reverse village-points)
          (loop (cdr points) player (if (equal? player (list-ref C-MAP (car points)))
                                        (cons (car points) village-points) village-points))))))

;(make-town-ok-points *cross-p* *roads-p* 1)

;wood brick sheep wheat iron
(define road-pattern '(1 1 0 0 0))
(define village-pattern '(1 1 1 1 0))
(define town-pattern '(0 0 0 2 2))
(define develop-pattern '(0 0 1 1 1))

(define material-check ;各施設の必要資材が手持ちのカードで使えるかどうか->bool
  (lambda (player-cards check-pattern)
        (let loop ((pattern-list check-pattern) (card-list player-cards) (result '()))
          (if (null? pattern-list) (not (member #f result));リストに#fがあれば#tなので反転が結果
              (loop (cdr pattern-list) (cdr card-list);1つずつチェック
                    (cons (if (<= (car pattern-list) (car card-list)) #t #f) result))))))


;特定パネルの4隅の座標をリストで返す
(define (four-corners panel-num)
  (cond ((<= 0 panel-num 3) `(,panel-num ,(+ panel-num 1) ,(+ panel-num 5) ,(+ panel-num 6)))
        ((<= 4 panel-num 7) `(,(+ panel-num 1) ,(+ panel-num 2) ,(+ panel-num 6) ,(+ panel-num 7)))
        ((<= 8 panel-num 11) `(,(+ panel-num 2) ,(+ panel-num 3) ,(+ panel-num 7) ,(+ panel-num 8)))
        (else `(,(+ panel-num 3) ,(+ panel-num 4) ,(+ panel-num 8) ,(+ panel-num 9)))))

;パネルから四隅をチェックして施設名を返す
(define (four-corner? C-MAP panel-index panel-kind)
  (map (lambda (x) (cons panel-kind x))
       (map (lambda (z) (symbol->string z))
            (filter (lambda (y) y)
                    (for/list ((four-corner (four-corners panel-index)))
                      (list-ref C-MAP four-corner))))))

;(four-corner? *cross-p* 4 1)


;v1とかにPanel種別を合体させたリストを作る　-> ((1 . v1) (3 . t1))
(define (change-vt-to-pvt panel vt-list);これをmapする
  (let loop ((vt-Tlist (filter (lambda (x) x) vt-list)) (result '()));#fを取り除く
    (if (null? vt-Tlist) (reverse result) ;一応Reverse
        (loop (cdr vt-Tlist) (cons (cons panel (car vt-Tlist)) result)))))

;与えられたCharが数字化どうかをチェックする
(define number-case? 
  (lambda (c) 
    (char-numeric? c)))

;Loop用に新たなカードのセットを作る
(define (make-new-cards PHASE panel-num v-or-t player-num old-sets)
  (let ((player-set (list-ref old-sets player-num)))
    (let loop ((p-set player-set) (count 0) (result '()))
      (if (null? p-set)
          (for/list ((old-set old-sets) (count (range 0 (length PHASE))));'(0 1 2 3)))
            (if (= count player-num) (reverse result) old-set))
          (loop (cdr p-set) (+ count 1)
                (cons (if (= count panel-num)
                          (+ (car p-set) (case v-or-t (("v") 1) (("t") 2)))
                          (car p-set)) result))))))

                                                   
;上で作ったリストをAppendして一次元リストを作って各プレイヤーの獲得カードリストを作る
(define (change-pvt-to-card-list PHASE pvt-list)
  (let loop ((pvt-list pvt-list)
             (result (for/list
                         ((zeros '((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))
                          (count (length PHASE))) zeros)));'((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))))
    (if (null? pvt-list) result
        (let ((panel-num (car (car pvt-list)))
              (v-or-t (string-delete number-case? (cdr (car pvt-list))))
              (player-num (string->number (string-delete char-lower-case? (cdr (car pvt-list))))))               
          (loop (cdr pvt-list) (make-new-cards PHASE panel-num v-or-t player-num result))))))
                         
      

(define (distribute-material world);
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS));不要
  (let ((panel-list (apply append *map-zero*));各資材の番号のリスト
        (num-list (map (lambda (y) (string->number y));パネルにある数＝ダイスと連動
                       (map (lambda (x) (string-trim x)) number-list-S)))
        (index-list (range 0 16)));パネルのIndex
    (let ((composite-list
           (map (lambda (x y z) (list x y z)) panel-list num-list index-list)));資材番号　ダイス番号　Index
      (let ((choiced-list;ここからROBBERの座標と同じ要素をRemoveする
             (filter (lambda (x) (and (not (equal? ROBBER (caddr x))) (equal? (cadr x) DICE))) composite-list))) 
        (let loop ((choiced-list choiced-list) (result '()))
          (if (null? choiced-list)
              (change-pvt-to-card-list ;'((1 0 2 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)
               PHASE (apply append (reverse result)));'((0 . "v1") (2 . "t1")))
              (loop (cdr choiced-list)
                    (cons (four-corner? C-MAP (caddr (car choiced-list)) (car (car choiced-list)))
                          result)))))))))))



;trade可能か？どれかのカードが４枚以上ある
(define (trade-ok? player-cards)
  (let loop ((count (range 0 5)) (result '()))
    (if (null? count) (member #t result)
        (loop (cdr count)
              (cons (if (>= (list-ref player-cards (car count)) 4) #t #f) result)))))

;wood brick sheep wheat iron
;パス　交易　村　町　道　発展カードが作れるならメニューに表示
(define (display-menu v-p t-p r-p player-cards D-CARDS);->(1 . "1:Trade  ") (3 . "3:Make-town  ") ..
  (let loop ((count (range 0 7)) (result '()))
    (if (null? count) (reverse result)
        (loop (cdr count) (case (car count)
                            ((0) (cons (cons 0 "0:Pass ") result))
                            ((1) (if (trade-ok? player-cards) (cons (cons 1 "1:Trade ") result) result))
                            ((2) (if (and
                                      (material-check player-cards village-pattern)
                                      (not (null? v-p))) (cons (cons 2 "2:Make-village ") result) result))
                            ((3) (if (and
                                      (material-check player-cards town-pattern)
                                      (not (null? t-p))) (cons (cons 3 "3:Make-town ") result) result))
                            ((4) (if (and
                                      (material-check player-cards road-pattern)
                                      (not (null? r-p))) (cons (cons 4 "4:Make-road ") result) result))
                            ((5) (if  (material-check player-cards develop-pattern)
                                      (cons (cons 5 "5:Get-develop-card ") result) result))
                            ((6) (if (not (null? (filter (lambda (x) (not (equal? 'Victory x))) (cdr D-CARDS))))
                                     (cons (cons 6 "6:Use-develop-card") result) result))
                            (else result))))))



;listをnum要素で分割する
(define (list-divide lst num)
  (let loop ((lst lst) (result '()))
    (if (null? lst) (reverse result)
        (loop (drop lst num) (cons (take lst num) result)))))


;'(0 0 0 1 2 0 0 0 0 0 0 ....)->Player0は麦を1枚、鉄を2枚手に入れた、Player2は・・・
(define (add-card-list-to-string PHASE add-card-list)
  (let ((card-string-source
         (filter (lambda (x) (not (zero? (list-ref x 2))));枚数0は省く
                 (for/list ((add-card add-card-list);カード種類の枚数
                            (count (apply append;カードの種類Index
                                          (for/list ((a (range 0 (length PHASE))))
                                            (for/list ((b (range 0 5))) b))))
                            (player (apply append;PlayerのIndex
                                           (for/list ((a (range 0 (length PHASE))))
                                             (for/list ((b (range 0 5))) a)))))
                   `(,player ,count ,add-card))))) ;card-string-source))
    (apply string-append (map (lambda (y) (format "PLAYER-~aは~aを~a枚手に入れた  "
                                                  (car y)
                                                  (cdr (list-ref *material-list* (cadr y)))
                                                  (caddr y))) card-string-source))))           
    

;kokokara
;Phaseの最初にこれを呼び出すこと、次にMain-loop-readへ
(define (main-loop-dice world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (cond
       ((= 4 (length PHASE))
        (match-let (((PLAYER NO-0 COLOR-0 SCORE-0 CARDS-0 D-CARDS-0 KNIGHT-0) (list-ref PLAYERS-LIST 0)))
          (match-let (((PLAYER NO-1 COLOR-1 SCORE-1 CARDS-1 D-CARDS-1 KNIGHT-1) (list-ref PLAYERS-LIST 1)))
            (match-let (((PLAYER NO-2 COLOR-2 SCORE-2 CARDS-2 D-CARDS-2 KNIGHT-2) (list-ref PLAYERS-LIST 2)))
              (match-let (((PLAYER NO-3 COLOR-3 SCORE-3 CARDS-3 D-CARDS-3 KNIGHT-3) (list-ref PLAYERS-LIST 3)))  
                (let ((dice (+ (random 1 7) (random 1 7))) (cards-list `(,CARDS-0 ,CARDS-1 ,CARDS-2 ,CARDS-3)))
                  (let ((add-card-list (apply append (distribute-material
                                                      (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP dice D-CARDS-SET ROBBER))));足すリスト
                        (addend-card-list (apply append (map (lambda (x) (struct->list x)) cards-list))));足されるリスト
                    (displayln (format "サイコロころころ・・出目の合計は~a！" dice))
                    (displayln (add-card-list-to-string  PHASE add-card-list))
                    (let loop ((add-card-list add-card-list) (addend-card-list addend-card-list) (result '()))
                      (if (null? add-card-list)
                          (let-values (((card0 card1 card2 card3)
                                        (apply values (map (lambda (x) (apply CARD x)) (list-divide (reverse result) 5)))))
                            (let* ((new-P0 (PLAYER NO-0 COLOR-0 SCORE-0 card0 D-CARDS-0 KNIGHT-0))
                                   (new-P1 (PLAYER NO-1 COLOR-1 SCORE-1 card1 D-CARDS-1 KNIGHT-1))
                                   (new-P2 (PLAYER NO-2 COLOR-2 SCORE-2 card2 D-CARDS-2 KNIGHT-2))
                                   (new-P3 (PLAYER NO-3 COLOR-3 SCORE-3 card3 D-CARDS-3 KNIGHT-3))
                                   (new-PLAYERS-LIST `(,new-P0 ,new-P1 ,new-P2 ,new-P3))                                                 
                                   (new-WORLD (WORLD new-PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP #f D-CARDS-SET ROBBER)))
                              (main-loop-read new-WORLD)))                                                    
                          (loop (cdr add-card-list) (cdr addend-card-list)
                                (cons (+ (car add-card-list) (car addend-card-list)) result)))))))))))
       ((= 3 (length PHASE))
        (match-let (((PLAYER NO-0 COLOR-0 SCORE-0 CARDS-0 D-CARDS-0 KNIGHT-0) (list-ref PLAYERS-LIST 0)))
          (match-let (((PLAYER NO-1 COLOR-1 SCORE-1 CARDS-1 D-CARDS-1 KNIGHT-1) (list-ref PLAYERS-LIST 1)))
            (match-let (((PLAYER NO-2 COLOR-2 SCORE-2 CARDS-2 D-CARDS-2 KNIGHT-2) (list-ref PLAYERS-LIST 2)))
              (let ((dice (+ (random 1 7) (random 1 7))) (cards-list `(,CARDS-0 ,CARDS-1 ,CARDS-2)))
                (let ((add-card-list (apply append (distribute-material
                                                    (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP dice D-CARDS-SET ROBBER))));足すリスト
                      (addend-card-list (apply append (map (lambda (x) (struct->list x)) cards-list))));足されるリスト
                  (displayln (format "サイコロころころ・・出目の合計は~a！" dice))
                  (displayln (add-card-list-to-string  PHASE add-card-list))
                  (let loop ((add-card-list add-card-list) (addend-card-list addend-card-list) (result '()))
                    (if (null? add-card-list)
                        (let-values (((card0 card1 card2)
                                      (apply values (map (lambda (x) (apply CARD x)) (list-divide (reverse result) 5)))))
                          (let* ((new-P0 (PLAYER NO-0 COLOR-0 SCORE-0 card0 D-CARDS-0 KNIGHT-0))
                                 (new-P1 (PLAYER NO-1 COLOR-1 SCORE-1 card1 D-CARDS-1 KNIGHT-1))
                                 (new-P2 (PLAYER NO-2 COLOR-2 SCORE-2 card2 D-CARDS-2 KNIGHT-2))
                            
                                 (new-PLAYERS-LIST `(,new-P0 ,new-P1 ,new-P2))                                                 
                                 (new-WORLD (WORLD new-PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP #f D-CARDS-SET ROBBER)))
                            (main-loop-read new-WORLD)))                                                    
                        (loop (cdr add-card-list) (cdr addend-card-list)
                              (cons (+ (car add-card-list) (car addend-card-list)) result))))))))))
       ((= 2 (length PHASE))
           (match-let (((PLAYER NO-0 COLOR-0 SCORE-0 CARDS-0 D-CARDS-0 KNIGHT-0) (list-ref PLAYERS-LIST 0)))
             (match-let (((PLAYER NO-1 COLOR-1 SCORE-1 CARDS-1 D-CARDS-1 KNIGHT-1) (list-ref PLAYERS-LIST 1)))
   
               (let ((dice (+ (random 1 7) (random 1 7))) (cards-list `(,CARDS-0 ,CARDS-1)))
                 (let ((add-card-list (apply append (distribute-material
                                                     (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP dice D-CARDS-SET ROBBER))));足すリスト
                       (addend-card-list (apply append (map (lambda (x) (struct->list x)) cards-list))));足されるリスト
                   (displayln (format "サイコロころころ・・出目の合計は~a！" dice))
                   (displayln (add-card-list-to-string  PHASE add-card-list))
                   (let loop ((add-card-list add-card-list) (addend-card-list addend-card-list) (result '()))
                     (if (null? add-card-list)
                         (let-values (((card0 card1)
                                       (apply values (map (lambda (x) (apply CARD x)) (list-divide (reverse result) 5)))))
                           (let* ((new-P0 (PLAYER NO-0 COLOR-0 SCORE-0 card0 D-CARDS-0 KNIGHT-0))
                                  (new-P1 (PLAYER NO-1 COLOR-1 SCORE-1 card1 D-CARDS-1 KNIGHT-1))                            
                                  (new-PLAYERS-LIST `(,new-P0 ,new-P1))                                                 
                                  (new-WORLD (WORLD new-PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP #f D-CARDS-SET ROBBER)))
                             (main-loop-read new-WORLD)))                                                    
                         (loop (cdr add-card-list) (cdr addend-card-list)
                               (cons (+ (car add-card-list) (car addend-card-list)) result))))))))))))
           

;メイン
(define (main-loop-read world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
                (displayln (place-status world))                
        (let* ((village-ok-points (make-village-ok-points C-MAP R-MAP NO))
               (town-ok-points (make-town-ok-points C-MAP R-MAP NO))
               (road-ok-points (make-road-ok-points C-MAP R-MAP NO))
               (menu-text (map (lambda (x) (cdr x))
                               (display-menu village-ok-points town-ok-points road-ok-points (struct->list CARDS) D-CARDS)))
               (menu-number (map (lambda (x) (car x))
                                 (display-menu village-ok-points town-ok-points road-ok-points (struct->list CARDS) D-CARDS))))
          (displayln (format "PLAYER-~aは何をしますか？ ~a"  NO menu-text))         
          (let ((answer (string->number (read-line))))            
            (cond ((not (member answer menu-number)) (main-loop-read world))
                  ((= 0 answer) (main-loop-pass (update-score world))) ;次のプレイヤーに移る関数＋勝利判定 scoreの更新をする!
                  ((= 1 answer) (trade-4to1 world));tradeで手持ち4枚から任意の一枚をゲットする関数
                  ((= 2 answer) (make-village world));村を作る関数
                  ((= 3 answer) (make-town world));町に発展させる関数
                  ((= 4 answer) (make-road world));道
                  ((= 5 answer) (get-d-card world));Develop-cardを引く関数
                  ((= 6 answer) (use-develop-card world));Develop-cardを使う関数
                  (else (main-loop-read world)))))))));念の為

;use-develop-card;;;;;;;;;;;;;;;;;;;;;

(define (make-d-card-list lst)
  (let loop ((lst lst) (result '()))
    (if (null? lst) (sort result symbol<?)
        (loop (cdr lst) (if (member (car lst) result) result (cons (car lst) result))))))

(define (make-d-cards-string lst)
  (for/list ((lst lst) (range (range 0 6)))
     (apply string-append (list (number->string range) ":" (symbol->string lst)))))

(define (use-develop-card world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
       (let* ((d-card-list  (make-d-card-list (filter (lambda (x) (not (equal? x 'Victory))) (cdr D-CARDS))));手持ち各種カードを目録にする ->(symbol symbol ...)
               (d-card-string-list (make-d-cards-string d-card-list));↑のリストを"0:Discover 1:Knight ...
               (d-card-range-list (range 0 (length d-card-list))))
         (for-each (lambda (x) (display x) (display "  ")) (cons "どのカードを使いますか？　" d-card-string-list)) (newline)
         (let ((answer (string->number (read-line))))
           (if (not (member answer d-card-range-list)) (use-develop-card world)
               (case (list-ref d-card-list answer)
                 ((Knight) (use-knight-card world))
                 ((Roads) (use-road-card world))
                 ((Discover) (use-discover-card world))
                 ((Monopoly) (use-monopoly-card world))
                 (else (use-develop-card world))))))))))

;monopoly;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-use-monopoly-menu world);->資源Index
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (displayln (cons "どの資源カードを独占したいですか？  "
                         (map (lambda (x) (string-append (number->string (car x)) ":" (cdr x))) *material-list*)))
        (let ((answer (string->number (read-line))))
          (if  (not (member answer (range 0 5))) (display-use-monopoly-menu world)
               answer))))))

(define (use-monopoly-card world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (cond
      ((= 4 (length PHASE))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((PLAYER NO-0 COLOR-0 SCORE-0 CARDS-0 D-CARDS-0 KNIGHT-0) (list-ref PLAYERS-LIST 0)))
        (match-let (((PLAYER NO-1 COLOR-1 SCORE-1 CARDS-1 D-CARDS-1 KNIGHT-1) (list-ref PLAYERS-LIST 1)))
          (match-let (((PLAYER NO-2 COLOR-2 SCORE-2 CARDS-2 D-CARDS-2 KNIGHT-2) (list-ref PLAYERS-LIST 2)))
            (match-let (((PLAYER NO-3 COLOR-3 SCORE-3 CARDS-3 D-CARDS-3 KNIGHT-3) (list-ref PLAYERS-LIST 3)))      
              (let* ((answer (display-use-monopoly-menu world))
                     (listed-list (map (lambda (x) (struct->list x)) `(,CARDS-0 ,CARDS-1 ,CARDS-2 ,CARDS-3)));'((1 2 3 4 5) (2 3 4 5 6) ...)
                     (material-total (for/sum ((materials (map (lambda (x) (list-ref x answer)) listed-list))) materials));全員分の任意資材合計
                     (new-listed-list (map (lambda (x) (list-set x answer 0)) listed-list)));'((0 2 3 4 5) (0 3 4 5 6) (0...
                (let-values (((card0 card1 card2 card3) (apply values (map (lambda (x) (apply CARD x)) new-listed-list))));新たな全員のCARD
                  (let* ((new-P0 (PLAYER NO-0 COLOR-0 SCORE-0 card0 D-CARDS-0 KNIGHT-0))
                         (new-P1 (PLAYER NO-1 COLOR-1 SCORE-1 card1 D-CARDS-1 KNIGHT-1))
                         (new-P2 (PLAYER NO-2 COLOR-2 SCORE-2 card2 D-CARDS-2 KNIGHT-2))
                         (new-P3 (PLAYER NO-3 COLOR-3 SCORE-3 card3 D-CARDS-3 KNIGHT-3))
                         (new-PLAYER-LIST `(,new-P0 ,new-P1 ,new-P2 ,new-P3))
                         (new-d-cards (cons (car D-CARDS) (remove 'Monopoly (cdr D-CARDS))))
                         (Monopoly-card-set (apply CARD (list-set (struct->list CARDS) answer material-total)));総取りした人のCARD
                         (new-PLAYER (PLAYER NO COLOR SCORE Monopoly-card-set new-d-cards KNIGHT));総取りした現プレイヤーのPLAYER
                         (last-PLAYERS-list (list-set new-PLAYER-LIST (car PHASE) new-PLAYER));出来上がった新たなPLAYERS-LIST
                         (new-WORLD (WORLD last-PLAYERS-list C-MAP R-MAP PHASE TURN #f DICE D-CARDS-SET ROBBER)))
                    (displayln (format "PLAYER-~aは　~aを独占した！" NO (cdr (list-ref *material-list* answer))))
                    (main-loop-read new-WORLD))))))))))
      ((= 3 (length PHASE))
           (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((PLAYER NO-0 COLOR-0 SCORE-0 CARDS-0 D-CARDS-0 KNIGHT-0) (list-ref PLAYERS-LIST 0)))
        (match-let (((PLAYER NO-1 COLOR-1 SCORE-1 CARDS-1 D-CARDS-1 KNIGHT-1) (list-ref PLAYERS-LIST 1)))
          (match-let (((PLAYER NO-2 COLOR-2 SCORE-2 CARDS-2 D-CARDS-2 KNIGHT-2) (list-ref PLAYERS-LIST 2)))
              (let* ((answer (display-use-monopoly-menu world))
                     (listed-list (map (lambda (x) (struct->list x)) `(,CARDS-0 ,CARDS-1 ,CARDS-2)));'((1 2 3 4 5) (2 3 4 5 6) ...)
                     (material-total (for/sum ((materials (map (lambda (x) (list-ref x answer)) listed-list))) materials));全員分の任意資材合計
                     (new-listed-list (map (lambda (x) (list-set x answer 0)) listed-list)));'((0 2 3 4 5) (0 3 4 5 6) (0...
                (let-values (((card0 card1 card2 card3) (apply values (map (lambda (x) (apply CARD x)) new-listed-list))));新たな全員のCARD
                  (let* ((new-P0 (PLAYER NO-0 COLOR-0 SCORE-0 card0 D-CARDS-0 KNIGHT-0))
                         (new-P1 (PLAYER NO-1 COLOR-1 SCORE-1 card1 D-CARDS-1 KNIGHT-1))
                         (new-P2 (PLAYER NO-2 COLOR-2 SCORE-2 card2 D-CARDS-2 KNIGHT-2))                   
                         (new-PLAYER-LIST `(,new-P0 ,new-P1 ,new-P2))
                         (new-d-cards (cons (car D-CARDS) (remove 'Monopoly (cdr D-CARDS))))
                         (Monopoly-card-set (apply CARD (list-set (struct->list CARDS) answer material-total)));総取りした人のCARD
                         (new-PLAYER (PLAYER NO COLOR SCORE Monopoly-card-set new-d-cards KNIGHT));総取りした現プレイヤーのPLAYER
                         (last-PLAYERS-list (list-set new-PLAYER-LIST (car PHASE) new-PLAYER));出来上がった新たなPLAYERS-LIST
                         (new-WORLD (WORLD last-PLAYERS-list C-MAP R-MAP PHASE TURN #f DICE D-CARDS-SET ROBBER)))
                    (displayln (format "PLAYER-~aは　~aを独占した！" NO (cdr (list-ref *material-list* answer))))
                    (main-loop-read new-WORLD)))))))))
      ((= 2 (length PHASE))
                  (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((PLAYER NO-0 COLOR-0 SCORE-0 CARDS-0 D-CARDS-0 KNIGHT-0) (list-ref PLAYERS-LIST 0)))
        (match-let (((PLAYER NO-1 COLOR-1 SCORE-1 CARDS-1 D-CARDS-1 KNIGHT-1) (list-ref PLAYERS-LIST 1)))
              (let* ((answer (display-use-monopoly-menu world))
                     (listed-list (map (lambda (x) (struct->list x)) `(,CARDS-0 ,CARDS-1)));'((1 2 3 4 5) (2 3 4 5 6) ...)
                     (material-total (for/sum ((materials (map (lambda (x) (list-ref x answer)) listed-list))) materials));全員分の任意資材合計
                     (new-listed-list (map (lambda (x) (list-set x answer 0)) listed-list)));'((0 2 3 4 5) (0 3 4 5 6) (0...
                (let-values (((card0 card1 card2 card3) (apply values (map (lambda (x) (apply CARD x)) new-listed-list))));新たな全員のCARD
                  (let* ((new-P0 (PLAYER NO-0 COLOR-0 SCORE-0 card0 D-CARDS-0 KNIGHT-0))
                         (new-P1 (PLAYER NO-1 COLOR-1 SCORE-1 card1 D-CARDS-1 KNIGHT-1))            
                         (new-PLAYER-LIST `(,new-P0 ,new-P1))
                         (new-d-cards (cons (car D-CARDS) (remove 'Monopoly (cdr D-CARDS))))
                         (Monopoly-card-set (apply CARD (list-set (struct->list CARDS) answer material-total)));総取りした人のCARD
                         (new-PLAYER (PLAYER NO COLOR SCORE Monopoly-card-set new-d-cards KNIGHT));総取りした現プレイヤーのPLAYER
                         (last-PLAYERS-list (list-set new-PLAYER-LIST (car PHASE) new-PLAYER));出来上がった新たなPLAYERS-LIST
                         (new-WORLD (WORLD last-PLAYERS-list C-MAP R-MAP PHASE TURN #f DICE D-CARDS-SET ROBBER)))
                    (displayln (format "PLAYER-~aは　~aを独占した！" NO (cdr (list-ref *material-list* answer))))
                    (main-loop-read new-WORLD)))))))))))
       
                           
          


;discover;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-use-discover-menu world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (displayln (cons "どの資源カードが欲しいですか?1枚目  "
                         (map (lambda (x) (string-append (number->string (car x)) ":" (cdr x))) *material-list*)))
        (let ((answer1 (string->number (read-line))))
          (cond  ((not (member answer1 (range 0 5))) (display-use-discover-menu world))
                 (else
                  (displayln (cons "どの資源カードが欲しいですか?2枚目  "
                                   (map (lambda (x) (string-append (number->string (car x)) ":" (cdr x))) *material-list*)))
                          (let ((answer2 (string->number (read-line))))
                            (if (not (member answer2 (range 0 5))) (display-use-discover-menu world)
                                (values answer1 answer2))))))))))

(define (use-discover-card world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))      
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let-values (((answer1 answer2) (display-use-discover-menu world)))
          (let* ((new-add-list (for/list ((count (range 0 5))) (cond ((and (= count answer1) (= count answer2)) 2)
                                                       ((or (= count answer1) (= count answer2)) 1)                                                       
                                                       (else 0))))
                 (new-card-list (map + (struct->list CARDS) new-add-list))
                 (new-d-cards (cons (car D-CARDS) (remove 'Discover (cdr D-CARDS))))
                 (new-PLAYER (PLAYER NO COLOR SCORE (apply CARD new-card-list) new-d-cards KNIGHT))
                 (new-PLAYERS-LIST (list-set PLAYERS-LIST (car PHASE) new-PLAYER))
                 (new-WORLD (WORLD new-PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER)))
            (displayln (format "PLAYER~aは ~aカードと ~aカードを得た！"
                               NO (cdr (list-ref *material-list* answer1)) (cdr (list-ref *material-list* answer2))))
            (main-loop-read new-WORLD)))))))
                 
;develop-road;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-use-road-menu world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((road-ok-points (make-road-ok-points C-MAP R-MAP NO)))
          (displayln (place-status (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER)))
          (displayln (cons (format "PLAYER~aはどこに道路を建設する?　1つ目　" NO) (map (lambda (x) (number->string x)) road-ok-points)))
          (let ((answer1 (string->number (read-line))))
            (cond ((not (member answer1 road-ok-points)) (display-initial-road-menu world NO R-MAP))
                  (else
                   (displayln (place-status (WORLD PLAYERS-LIST C-MAP (list-set R-MAP answer1 NO) PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER)))                
                   (displayln (cons (format "PLAYER~aはどこに道路を建設する?　2つ目　" NO)　;1つめの道の先を追加
                                    (map (lambda (x) (number->string x))
                                         (remove answer1 (make-road-ok-points C-MAP (list-set R-MAP answer1 NO) NO)))))
                   (let ((answer2 (string->number (read-line))))
                     (if (not (member answer2
                                      (remove answer1 (make-road-ok-points C-MAP (list-set R-MAP answer1 NO) NO))))
                         (display-use-road-menu world)
                         (values answer1 answer2)))))))))))




(define (use-road-card world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))      
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let-values (((answer1 answer2)
                      (display-use-road-menu
                       (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER))))         
          (displayln (format "PLAYER~aは　~aと　~aに　道路を建設した！" NO answer1 answer2))
          (let* ((new-PLAYER (remove 'Roads D-CARDS))
                 (new-d-cards (cons (car D-CARDS) (remove 'Roads (cdr D-CARDS))))
                 (new-PLAYER (PLAYER NO COLOR SCORE CARDS new-d-cards KNIGHT))
                 (new-PLAYERS-LIST (list-set PLAYERS-LIST (car PHASE) new-PLAYER))
                 (new-R-MAP  (list-set (list-set R-MAP answer1 NO) answer2 NO))
                 (new-WORLD (WORLD new-PLAYERS-LIST C-MAP new-R-MAP PHASE TURN #f DICE D-CARDS-SET ROBBER)))
            (main-loop-read new-WORLD)))))))
          
        


;knight;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-use-knight-menu1 world);->騎士が盗賊を追い払う先のPnael-index-num
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS)) ;(display ROBBER)
        (let ((index (filter (lambda (x) (not (equal? x ROBBER))) (range 0 16))))
          (displayln (place-status (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'ROBBER DICE D-CARDS-SET ROBBER)))
          (display (cons "どこに盗賊を追い払いますか？" (map (lambda (x) (number->string x)) index))) (newline)
          (let ((answer1 (string->number (read-line))))
            (if (not (member answer1 index))
                (display-use-knight-menu1 world)
                answer1)))))))

(define (display-use-knight-menu2 world answer1);->追い払った先のパネルの四隅にある町村を持つプレイヤーを選ぶ　->Player-num
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))   
      (let ((corner-player-list (filter (lambda (y) (not (equal? (number->string NO) y)))
                                        (map (lambda (x) (string-delete char-lower-case? (cdr x)))
                                             (four-corner? C-MAP answer1 999)))));Car部は不要なのでテキトー999
        (cond  ((null? corner-player-list) ;Cornerに誰もプレイヤーがいない
                (let* ((new-player (PLAYER NO COLOR SCORE CARDS (cons (car D-CARDS) (remove 'Knight (cdr D-CARDS))) (+ KNIGHT 1)))
                       (new-player-list (list-set PLAYERS-LIST (car PHASE) new-player)))
                  (main-loop-read (WORLD new-player-list C-MAP R-MAP PHASE TURN #f DICE D-CARDS-SET ROBBER))))
                
               (else (displayln (cons "誰から奪う？　" (map (lambda (y) (string->number y)) corner-player-list)))
                     (let ((answer2  (read-line)))
                       (if (not (member answer2 corner-player-list)) (display-use-knight-menu2 world answer1)
                           answer2))))))))

;indexのリストを想定してランダムのIndexを返す
(define (random-list-up index-list)
  (car (shuffle index-list)))
    

(define (use-knight-card world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))      
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let* ((answer1 (display-use-knight-menu1 world));ROBBERを移動させるIndex
               (answer2 (display-use-knight-menu2 world answer1)));カードを奪われるPlayer-num
          (match-let (((PLAYER NO2 COLOR2 SCORE2 CARDS2 D-CARDS2 KNIGHT2) (list-ref PLAYERS-LIST (string->number answer2))))
            (let* ((choiced-player-cards (struct->list CARDS2));num-list '(1 2 3 4 5)
                   (over-0-list (let loop1 ((choiced-c choiced-player-cards) (count 0) (acc '()));引けるカードのIndex
                                  (if (null? choiced-c) (reverse acc)
                                      (loop1 (cdr choiced-c) (+ count 1) (if (< 0 (car choiced-c)) (cons count acc) acc)))))
                   (choiced-card (random-list-up over-0-list));index-num
                   (new-sub-cards (apply CARD (list-set choiced-player-cards choiced-card
                                                        (- (list-ref choiced-player-cards choiced-card) 1))))         
                   (new-choiced-player (PLAYER NO2 COLOR2 SCORE2 new-sub-cards D-CARDS2 KNIGHT2))
                   (new-add-cards (apply CARD (list-set (struct->list CARDS) choiced-card
                                                        (+ (list-ref (struct->list CARDS) choiced-card) 1))))
                   (new-d-cards (cons (car D-CARDS) (remove 'Knight (cdr D-CARDS))))
                   (new-get-player (PLAYER NO COLOR SCORE new-add-cards new-d-cards (+ 1 KNIGHT)));騎士王ポイント＋１
                   (new-players-list (list-set (list-set PLAYERS-LIST (car PHASE) new-get-player) choiced-card new-choiced-player))
                   (new-world (WORLD new-players-list C-MAP R-MAP PHASE TURN #f #f D-CARDS-SET answer1)))
              (displayln (format "PLAYER~aの資源カード~aが1枚 PLAYER~aに奪われた！"
                                 (string->number answer2) (cdr (list-ref *material-list* choiced-card)) (car PHASE)))
              (main-loop-read new-world))))))))
          

;get-develop-card;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-d-card world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (let* ((choiced-card (car D-CARDS-SET))
             (new-D-CARDS-SET (cdr D-CARDS-SET));WORLDのD-CARDS-SETから減らす
             (new-cards (apply CARD (map - (struct->list CARDS) develop-pattern)));使った資源カードを減らす
             (new-player
              (PLAYER NO COLOR SCORE new-cards
                      (cons (cons choiced-card (car D-CARDS)) (cdr D-CARDS)) KNIGHT)));(('Knight) . ('Knight 'Discover ....)
        (displayln (format "PLAYER-~aは　~aカードを手にれいた！" NO choiced-card))
        (main-loop-read;develop-cardは一度しか引けないためLoopへ
         (WORLD (list-set PLAYERS-LIST (car PHASE) new-player) C-MAP R-MAP PHASE TURN DISP DICE　new-D-CARDS-SET ROBBER))))))
  

;road;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-road-menu world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world));dispに画像レイヤー部分を合わせる
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
 (let ((road-ok-points (make-road-ok-points C-MAP R-MAP NO)))
          (cons "どこに道を作りますか？　" (map (lambda (x) (number->string x)) road-ok-points)))))))

(define (make-road world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (displayln (place-status (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER)))
        (displayln (display-road-menu (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER)))
        (let ((road-ok-points (make-road-ok-points C-MAP R-MAP NO)))
          (let ((answer (string->number (read-line))))
            (if (not (member answer road-ok-points)) (make-road world)
                (let* ((new-R-MAP (list-set R-MAP answer NO))
                       (new-cards (apply CARD (map - (struct->list CARDS) road-pattern)))
                       (new-player (PLAYER NO COLOR SCORE new-cards D-CARDS KNIGHT)))
                  (main-loop-read
                   (WORLD (list-set PLAYERS-LIST (car PHASE) new-player) C-MAP new-R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER))))))))))


;town;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-town-menu world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world));dispに画像レイヤー部分を合わせる
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
 (let ((town-ok-points (make-town-ok-points C-MAP R-MAP NO)))
   (displayln (place-status world))
          (cons "どの村を町にしますか？　" (map (lambda (x) (number->string x)) town-ok-points)))))))
  

(define (make-town world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (displayln (display-town-menu (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'TOWN-KOHO DICE D-CARDS-SET ROBBER)))
        (let ((town-ok-points (make-town-ok-points C-MAP R-MAP NO)));(number? (car town-ok-points))
          (let ((answer (string->number (read-line)))) 
            (if (not (member answer town-ok-points)) (make-town world)
                (let* ((new-C-MAP (list-set C-MAP answer (string->symbol (string-append "t" (number->string NO)))))
                       (new-cards (apply CARD (map - (struct->list CARDS) town-pattern)))
                       (new-player (PLAYER NO COLOR SCORE new-cards D-CARDS KNIGHT)))
                  (main-loop-read
                   (WORLD (list-set PLAYERS-LIST (car PHASE) new-player) new-C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER))))))))))
        

;village;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-village-menu world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world));dispに画像レイヤー部分を合わせる
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((village-ok-points (make-village-ok-points C-MAP R-MAP NO)))
          (cons "どこに村を作りますか？　" (map (lambda (x) (number->string x)) village-ok-points)))))))
          
        

;make-village
(define (make-village world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (displayln (place-status (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'VILLAGE-KOHO DICE D-CARDS-SET ROBBER)))
        (displayln (display-village-menu (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER)))
        (let ((village-ok-points (make-village-ok-points C-MAP R-MAP NO)))
          (let ((answer (string->number (read-line))))
            (if (not (member answer village-ok-points)) (make-village world)
                (let* ((new-C-MAP (list-set C-MAP answer (string->symbol (string-append "v" (number->string NO)))))
                       (new-cards (apply CARD (map - (struct->list CARDS) village-pattern)))
                       (new-player (PLAYER NO COLOR SCORE new-cards D-CARDS KNIGHT)))
                  (main-loop-read
                   (WORLD (list-set PLAYERS-LIST (car PHASE) new-player) new-C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER))))))))))
       

;trade;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (display-trade-menu1 cards)
  (let loop ((cards cards) (count 0) (result '()))
    (if (null? cards) (cons "どのカードを手放しますか?  "
                            (map (lambda (x) (string-append (format "~a:~a" (car x) (cdr x)))) (reverse result)))
        (loop (cdr cards) (+ count 1)
              (if (>= (car cards) 4)
                  (cons (assoc count *material-list*) result)
                  result)))))

(define (display-trade-menu2)
  (cons "どのカードが欲しいですか?  " (map (lambda (x) (string-append (format "~a:~a" (car x) (cdr x)))) *material-list*)))



(define (trade-4to1 world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (define (menu1 CARDS)
          (displayln (display-trade-menu1 (struct->list CARDS)))
          (let ((answer1 (string->number (read-line))))
            (if (with-handlers ((exn:fail? (const #f)))
                  (not (>= (list-ref (struct->list CARDS) answer1) 4))) (trade-4to1 world)
                                                                        (menu2 CARDS answer1))))
        (define (menu2 CARDS answer1)
          (displayln (display-trade-menu2))
          (let ((answer2 (string->number (read-line))))
            (if (not (member answer2 (range 0 5))) (menu2 CARDS answer1)
                (let* ((new-cards1 (list-set (struct->list CARDS) answer1 (- (list-ref (struct->list CARDS) answer1) 4)))
                       (new-cards2 (list-set new-cards1 answer2 (+ (list-ref new-cards1 answer2) 1)))
                       (new-player (PLAYER NO COLOR SCORE (apply CARD new-cards2) D-CARDS KNIGHT))
                       (new-players-list (list-set PLAYERS-LIST (car PHASE) new-player))
                       (new-world (WORLD new-players-list C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER)))
                  (main-loop-read new-world)))))
        (menu1 CARDS)))))
                
                     
                      
;pass;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;全プレイヤーのScoreを更新する
(define (update-score world) ;->world
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))    
    (let ((new-player-list
           (let ((max-longest-road '(0 . -100)));初期値
             (for ((phase (range 0 (length PHASE)))) ;どのロンゲストトロードが一番長いか
               (if (< (cdr max-longest-road) (max-longest R-MAP phase))
                   (set! max-longest-road (cons phase (max-longest R-MAP phase))) '()))

             (for/list ((phase (range 0 (length PHASE))))
               (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST phase)))
                 (let* ((road-score (if (and (road-lord? world) (= NO (who-is-road-lord world))) 2 0))
                        (village-score (length
                                        (filter (lambda (x)
                                                  (equal? (case phase ((0) 'v0) ((1) 'v1) ((2) 'v2) ((3) 'v4))
                                                          x)) C-MAP)))
                        (town-score  (* 2 (length
                                           (filter (lambda (x)
                                                     (equal? (case phase ((0) 't0) ((1) 't1) ((2) 't2) ((3) 't4))
                                                             x)) C-MAP))))
                        (develop-score (* 2 (length (filter (lambda (x) (equal? x 'Victory)) (cdr D-CARDS)))))
                        (knight-score (if (and (knight-king? world) (= NO (who-is-knight-king world))) 2 0))
                        (total-score (+ road-score village-score town-score develop-score knight-score)))
                   (PLAYER NO COLOR total-score CARDS D-CARDS KNIGHT)))))))
      (WORLD new-player-list C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER))))
                
                
                              
;main-loop-pass
(define (main-loop-pass world);ここで勝利判定する設計にする、全員分をループで調べる
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE　D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let* ((new-D-CARDS
                (if (pair? D-CARDS) (cons '() (append (car D-CARDS) (cdr D-CARDS))) D-CARDS));Pass時にD-CARDSのCarをCdrに統合
              (new-PLAYER  (PLAYER NO COLOR SCORE CARDS new-D-CARDS KNIGHT))
              (new-PLAYERS-LIST (list-set PLAYERS-LIST (car PHASE) new-PLAYER)))
        (if (member #t (winner-check? PLAYERS-LIST PHASE)) (game-end PLAYERS-LIST PHASE);勝利条件が＃tなら終了
            (main-loop-dice
             (WORLD new-PLAYERS-LIST C-MAP R-MAP (my-circular PHASE);PHASEを進める
                     TURN  DISP #f　D-CARDS-SET ROBBER));現在のPHASEが3だったら次はTURN更新
            ))))))

;game-end
(define (game-end player-list PHASE)
  (display (format "~a win." 
                   (PLAYER-COLOR (list-ref player-list (index-of (winner-check? player-list PHASE) #t)))
                   )))


;勝利条件をチェック
(define (winner-check? PLAYERS-LIST PHASE) ;->'(#f #f #t #f)
  (for/list ((index (range 0 (length PHASE))))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST index)))
      (if (>= SCORE 10) #t #f))))
    
    
    
  
        
#|
(require 2htdp/image
         pict/convert)
(require (only-in pict
                  pict->bitmap))



(define my-bitmap (pict->bitmap (pict-convert (place-status K-WORLD))))

(define frame (new frame%
                   [label "Example"]
                   [width 760]   
                   [height 700]))

(define canvas (new canvas% [parent frame]
                     [paint-callback
                      (lambda (canvas dc)
                        (send dc draw-bitmap my-bitmap 0 200))]))

(send frame show #t)

|#

;(place-status K-WORLD)
;(main-loop-dice K-WORLD)

;(CARD-WOOD PLAYER-1-CARDS)
;(struct->list PLAYER-1-CARDS)





;初期設定のためのループ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Choice-initial;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-choice-village-menu world index) ;->C-MAP
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (let ((village-ok-points (make-village-ok-points C-MAP R-MAP index)))       
      (displayln (place-status world))
      (displayln (cons (format "PLAYER-~aは　どこに村を作りますか？" index)
                       (map (lambda (x) (number->string x)) village-ok-points)))
      (let ((answer1 (string->number (read-line))))
        (if (or (not (equal? (list-ref C-MAP answer1) #f)) (<= 25 answer1)) (display-choice-village-menu world index)
            (list-set C-MAP answer1 (case index ((0) 'v0) ((1) 'v1) ((2) 'v2) ((3) 'v3))))))))

(define (display-choice-road-menu world index)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (let ((road-ok-points (make-road-ok-points C-MAP R-MAP index)))       
      (displayln (place-status world))
      (displayln (cons (format "PLAYER-~aは どこに道を作りますか?" index)
                       (map (lambda (x) (number->string x)) road-ok-points)))
      (let ((answer1 (string->number (read-line))))
        (if (or (not (equal? (list-ref R-MAP answer1) #f)) (<= 40 answer1)) (display-choice-road-menu world index)
            (list-set R-MAP answer1 index))))))
  
              
  

(define (initial-choice-read world)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (let ((new-C-MAP
           (for/fold ((c-map C-MAP)) ((index1 (case TURN
                                                ((1) (reverse (range 0 (length PHASE))))
                                                ((2) (append (range 0 (length PHASE)) (reverse (range 0 (length PHASE))))))))
             (display-choice-village-menu
              (WORLD PLAYERS-LIST c-map R-MAP PHASE TURN 'VILLAGE-KOHO DICE D-CARDS-SET ROBBER) index1))))
      (let ((new-R-MAP
             (for/fold ((r-map R-MAP)) ((index2 (append (range 0 (length PHASE)) (reverse(range 0 (length PHASE))))));'(0 1 2 3 3 2 1 0)))
               (display-choice-road-menu
                (WORLD PLAYERS-LIST new-C-MAP r-map `(,index2) TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER) index2))))
             
             (main-loop-dice (WORLD PLAYERS-LIST new-C-MAP new-R-MAP PHASE TURN #f DICE D-CARDS-SET ROBBER))))))


;(define (initial-choice-road world)
  

;(initial-choice-read A-WORLD)




#|
;Random-initial;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initial-random-read world) ;->WORLD 
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (let-values (((C0 C1 C2 C3)
                  (apply values (for/list ((player (range 0 4))
                                           (zahyou (shuffle (lset-difference equal? (range 0 25) (four-corners ROBBER))))) zahyou))))
      (let ((new-C-MAP (let loop ((player-zahyou `(,C0 ,C1 ,C2 ,C3)) (village '(v0 v1 v2 v3)) (c-map C-MAP))
                         (if (null? player-zahyou) c-map
                             (loop (cdr player-zahyou) (cdr village) (list-set c-map (car player-zahyou) (car village)))))))       
        (let loop ((player-index (range 0 4)) (initial-phase (circular-list 0 1 2 3)) (r-map R-MAP));reverseするか？
          (if (null? player-index) (main-loop-dice (WORLD PLAYERS-LIST new-C-MAP r-map PHASE TURN DISP #f D-CARDS-SET ROBBER)) 
              (loop (cdr player-index) (cdr initial-phase)
                    (initial-road (WORLD PLAYERS-LIST new-C-MAP r-map initial-phase TURN DISP DICE D-CARDS-SET ROBBER) (car player-index)))))))))

|#
  

(define (display-initial-road-menu world index)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
    (match-let (((PLAYER NO COLOR SCORE CARDS D-CARDS KNIGHT) (list-ref PLAYERS-LIST (car PHASE))))
      (match-let (((CARD WOOD BRICK SHEEP WHEAT IRON) CARDS))
        (let ((road-ok-points (make-road-ok-points C-MAP R-MAP index)))
          (displayln (place-status (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER)))
          (displayln (cons (format "PLAYER~aはどこに道路を建設する?　1つ目　" NO) (map (lambda (x) (number->string x)) road-ok-points)))
          (let ((answer1 (string->number (read-line))))
            (cond ((not (member answer1 road-ok-points)) (display-initial-road-menu world index))
                  (else
                   (displayln (place-status (WORLD PLAYERS-LIST C-MAP (list-set R-MAP answer1 NO) PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER)))                
                   (displayln (cons (format "PLAYER~aはどこに道路を建設する?　2つ目　" NO)　;1つめの道の先を追加
                                    (map (lambda (x) (number->string x))
                                         (remove answer1 (make-road-ok-points C-MAP (list-set R-MAP answer1 NO) index)))))
                   (let ((answer2 (string->number (read-line))))
                     (if (not (member answer2
                                      (remove answer1 (make-road-ok-points C-MAP (list-set R-MAP answer1 NO) index))))
                         (display-initial-road-menu world index)
                         (values answer1 answer2)))))))))))

#|
(define (initial-road world index)
  (match-let (((WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER) world))
        (let-values (((answer1 answer2)
                      (display-initial-road-menu
                       (WORLD PLAYERS-LIST C-MAP R-MAP PHASE TURN 'ROAD-KOHO DICE D-CARDS-SET ROBBER) index)))         
          (displayln (format "PLAYER~aは ~aと ~aに 道路を建設した!" index answer1 answer2))
          (let ((new-R-MAP  (list-set (list-set R-MAP answer1 index) answer2 index)))
            new-R-MAP))))
 |#  

                
                 

(define *d-cards* (shuffle (apply append (apply append (make-d-cards d-card-rules)))))

(define *original-R-MAP* (for/list ((count (range 0 40))) #f))
(define *original-C-MAP* (for/list ((count (range 0 25))) #f))
;(define *map* (shuffle (list-divide '(0 0 0 1 1 1 2 2 2 3 3 3 4 4 4 9) 4)));sabaku
(define *map*  (list-divide (shuffle (apply append `((0 0 0 1 1 1 2 2 2 3 3 3 4 4 4) (,(random 0 5))))) 4))
(define *robber-in* (random 0 16))

(struct WORLD (PLAYERS C-MAP R-MAP PHASE TURN DISP DICE D-CARDS-SET ROBBER)#:transparent);D-CARDS=develop-cardsのデッキ ROBBER=座標

;(define A-WORLD (WORLD *PLAYERS-LIST* *original-C-MAP* *original-R-MAP* *PHASE-LIST* 1 #f #f *d-cards* *robber-in*))

;(define K-WORLD (WORLD *PLAYERS-LIST* *cross-p* *roads-p* *PHASE-LIST* 1 #f #f *d-cards* (random 0 16)))



;players-number choice;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-initial-condition);->world
  (displayln "何人でプレイしますか? ")
  (let ((answer1 (string->number (read-line))))
    (cond ((not (member answer1 '(4 3 2))) make-initial-condition)
          (else  (displayln "村の初期配置数は?(1~2)")
                 (let ((answer2 (string->number (read-line))))
                   (cond ((not (member answer2 '(1 2))) make-initial-condition)
                         (else
                          (let ((new-PLAYERS-LIST (case answer1 ((4) *PLAYERS-LIST4*) ((3) *PLAYERS-LIST3*) ((2) *PLAYERS-LIST2*)))
                                (new-PHASE (case answer1 ((4) *PHASE-LIST-4*) ((3) *PHASE-LIST-3*) ((2) *PHASE-LIST-2*))))
                            (initial-choice-read
                             (WORLD new-PLAYERS-LIST *original-C-MAP* *original-R-MAP* new-PHASE answer2 #f #f *d-cards* *robber-in*))))))))))
      
(make-initial-condition)
  
          