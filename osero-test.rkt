#lang racket

(define all-squares
  (let loop ((i 10) (squares '()))
    (if (= i 89)
        (reverse squares)
        (if (or (= 0 (remainder i 10)) (= 9 (remainder i 10)))
            (loop (+ i 1) squares)
            (loop (+ i 1) (cons i squares))))))


(define (initial-board)
  (let ((board (make-vector 100 'outer)))
    (define (set-piece! board square piece)
      (vector-set! board square piece))

    (define (initialize-board! board)
      (for-each
       (lambda (square)
         (set-piece! board square 'empty))
       all-squares)
      (set-piece! board 44 'white)
      (set-piece! board 45 'black)
      (set-piece! board 54 'black)
      (set-piece! board 55 'white))

    (initialize-board! board)
    board))



(define initial-board-result (initial-board))
;(display initial-board-result)



(define data #(outer outer outer outer outer outer outer outer outer outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty black black empty empty empty outer outer
                     empty empty empty black black empty empty empty outer outer
                     empty empty empty empty black empty empty empty outer outer
                     empty empty black black white empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     outer outer outer outer outer outer outer outer outer))

(define (format-piece piece)
  (cond
    [(eq? piece 'outer) "O"]
    [(eq? piece 'empty) "."]
    [(eq? piece 'white) "W"]
    [(eq? piece 'black) "B"]))

(define (print-chessboard data)
  (display (format "B:~a W:~a~%" (count-difference 'black data) (count-difference 'white data)))
  (for ([row (in-range 10)])
    (for ([col (in-range 10)])
      (display (format "~a " (format-piece (vector-ref data (+ col (* row 10)))))))
    (newline)))

;(print-chessboard data)


(define all-directions '(-11 -10 -9 -1 1 9 10 11))
(define (opponent player) (if (equal? player 'black) 'white 'black))

(define (valid-p move) ;ok
  (and (integer? move) ;整数で
       (<= 11 move 88) ;11-88で
       (not (or (= 0 (remainder move 10)) (= 9 (remainder move 10))))));両端で無いか

      


(define (would-flip? move player board dir);OK move=打つ場所
  (let ((c (+ move dir))) ;c 打つ場所の一つDir側
    (and (equal? (vector-ref board c) (opponent player));一つDir側が敵側(White)であり
         (find-bracketing-piece (+ c dir) player board dir))));その先に自分のコマがある


(define (find-bracketing-piece square player board dir  (lst '()));ここを座標のリストにして返さないといけない
  (cond ((equal? (vector-ref board square) player) (cons (- square dir) lst));打つ場所の一つDir側の更に一つ先がが自分のコマだったらソコ
        ((equal? (vector-ref board square) (opponent player));↑が敵のコマだったら
         (find-bracketing-piece (+ square dir) player board dir (cons (- square dir) lst)));更に一つ先のコマで再帰する
        (else #f)));自分で挟めなかったら#f

;(would-flip? 57 'black data -1)
;(find-bracketing-piece 56 'black data -1)


(define (some proc lst);OK
  (cond ((null? lst) #f)
        ((proc (car lst)) (car lst))
        (else (some proc (cdr lst)))))


(define (legal-p move player board);OK 
  (and (equal? (vector-ref board move) 'empty) ;空のマス目か?
        (some
         (lambda (dir) (would-flip? move player board dir))
  all-directions)))

;(display (map (lambda (x) (legal-p x 'black data)) (iota 100)))

(define (search-for-numbers lst)
  (cond
    [(null? lst) '()] ; リストが空の場合は空リストを返す
    [(number? (car lst)) ; 先頭が数値の場合
     (cons (car lst) (search-for-numbers (cdr lst)))] ; 数値を結果のリストに追加し、残りの部分に再帰
    [(list? (car lst)) ; 先頭がリストの場合
     (append (search-for-numbers (car lst)) (search-for-numbers (cdr lst)))] ; リスト内を再帰的に検索
    [else
     (search-for-numbers (cdr lst))])) ; それ以外の場合はスキップして次の要素を探索

(define (make-flips move player board);->board
  (let ((bracketer (search-for-numbers (append
                        (filter (lambda (z) z)
                                (foldl (lambda (dir acc)
                                         (cons (would-flip? move player board dir) acc))
                                       '() all-directions))))));ひっくり返す座標のリストが入る
    (if bracketer ;(display bracketer)
        (list->vector (foldl (lambda (x acc) (list-set acc x player)) (vector->list board) bracketer))
        #f)));偽でなければ
   ;   (let loop ((c (+ move dir)) (board board));マス目に移動距離を足したものをC
    ;    (cond ((equal? c bracketer) board);move + dir が挟めるマスまで来たらボードを返す
      ;      (loop (+ c dir) (list->vector (list-set (vector->list board) c player)))))
    ;  #f)))
;(make-flips 75 'black data)


(define (make-move move player board);->board
 ; (vector-set! board move player);moveの場所にPlayerを配置する
  (let ((board2 (list->vector (list-set (vector->list board) move player))))
  (let ((board3 ;(for/list ((dir all-directions));(-11 -10 -1 1 ...)
          (make-flips move player board2))) board3)))
   ; (car (filter (lambda (x) x) board3)))))

    
;(make-move 75 'black data)


           

;(make-flips 57 'black data -1)
;(make-move 56 'black data)



;ok?
(define (any-legal-move? player board);#tかどうかだけの返り値
  (some (lambda (move) (legal-p move player board)) all-squares));一つでも指せる場所があるか?

;ok?
(define (next-to-play board previous-player print);->player
  (let ((opp (opponent previous-player)));
    (cond ((any-legal-move? opp board) opp);敵が一つでも指せる場所がある場合、敵を返す
          ((any-legal-move? previous-player board);自分が一つでも指せれば
       ;    (when print;そしてPrintが#tだったら
         ;    (display (format "~a has no moves and must pass.~%"
         ;    opp)));メッセージを表示
           previous-player);自分を返す
          (else #f))))

(define (win-lose-count player board)
  (define lst (vector->list board))
  (let loop ((lst lst) (count 0))
    (if (null? lst) count
        (loop (cdr lst) (if (equal? player  (car lst)) (+ 1 count) count)))))
  
(define (end board)
  (let ((black (win-lose-count 'black board)) (white (win-lose-count 'white board)))
 (display (format "black:~a white:~a~%" black white)) board))

;ok?
(define (get-move strategy player board print);->board
  (when print (print-chessboard board))
  (let ((move (strategy player board)));strategyで出されたマス目をmoveに束縛
    (cond ((or (not move) (= 10000 move) (= -10000 move)) (end board))
      ((and (valid-p move) (legal-p move player board));条件どっちもオッケイでなら
           (when print (display (format "~a moves to ~a.~%" player move)));Printが真なら表示
           (make-move move player board));
          (else (display (format "illegal move: ~a~%" move)) (newline) ;駄目な手なら再帰
                (get-move strategy player board print)))))

;ok?      
(define (human player board);humanの場合
  (display (format "~a to move:" player))
  (string->number (read-line)));人力で手を入力

;ok
(define (random-elt lst);リストの中からランダムで一つ選ぶ
  (define lst-length (length lst))
  (define random-index (if (null? lst) #f (random lst-length)))
  (if random-index (list-ref lst random-index) #f))

;ok?
(define (random-strategy player board);打てる手の中からランダムで一つ選ぶ
  (random-elt (legal-moves player board)))

#|
;CL
(define (legal-moves player board)
  (loop for move in all-squares
        when (legal-p move player board) collect move))
|#

;ok?
(define (legal-moves player board);適法な手のリストを返す
  (filter (lambda (x) x)
          (for/list ((move all-squares))
            (if (legal-p move player board) move #f))))

;(display all-squares)
;(legal-moves 'black data)
;(legal-p 56 'black data)


(define (othello bl-strategy wh-strategy
                ; #:optional
                 (print #t))
    (let loop ((player 'black)  (strategy bl-strategy) (board (initial-board)))
      (if (not player) (display "end")
         ; (end board)
          (loop
           (next-to-play board player print);ループするごとにプレイヤー入れ替え           
           (if (equal? player 'black) bl-strategy wh-strategy)
           (get-move (if (equal? player 'black) bl-strategy wh-strategy) player board print)))));ストラテジーをプレイヤーに従って




#|
;CL
(defun othello (bl-strategy wh-strategy &optional (print t))
  (let ((board (initial-board)))
    (loop for player = black
          then (next-to-play board player print)
          for strategy = (if (eql player black)
                             bl-strategy
                             wh-strategy)
          until (null player)
          do (get-move strategy player board print))
    (when print
      (format t "~%The game is over.Final result:")
      (print-board board))
    (count-difference black board)))
|#

            


;(define (max lst)
 ; (apply max lst))

(define (find-position item lst)
  (let loop ((lst lst)
             (index 0))
    (cond ((null? lst) #f)
          ((equal? item (car lst)) index)
          (else (loop (cdr lst) (+ index 1))))))

#|
;cl
(defun maximize-difference (player board)
  (funcall (maximizer #'count-difference)
           player board))
|#

;scheme
(define (maximize-difference player board)
  ((maximizer count-difference) player board));maximizer関数を呼び出して出来たクロージャにPlayerとBoardを入れる

#|
;cl
(defun maximizer (eval-fn)
  #'(lambda (player board)
      (let* ((moves (legal-moves player board))
             (scores (mapcar #'(lambda (move)
                                 (funcall eval-fn player
                                          (make-move move player
                                                     (copy-board board))))
                             moves))
             (best (apply #'max scores)))
        (elt moves (position best scores)))))
|#

;ok
(define (count-difference player board);board内のPlayer数を数える
  (let loop ((lst (vector->list board)) (count 0))
    (if (null? lst) count
        (loop (cdr lst) (if (equal? player (car lst)) (+ count 1) count)))))
          
  

;scheme
(define (maximizer eval-fn);今回はEval-fnはCount-difference
  (lambda (player board)
    (let* ((moves (legal-moves player board));movesに打てる手をリストで束縛
           (scores (map (lambda (move);movesを使ってmap
                          (eval-fn player;Count-differenceにPlayerをクロージャしたものに
                           (make-move move player;盤面のデータを生成
                                      board)))
                        moves)) 
           (best (if (null? scores) #f (apply max scores))));Scoresのリストの中で最大のものをBestに束縛
      (if best (list-ref moves (find-position best scores)) #f))));movesリストの中からBestと数値のmoveを返す

;(maximize-difference 'black data)
;moves uteru-te scores move gotono tensuu best tensuunobesut 


;(othello human random-strategy)
;(othello random-strategy maximize-difference)


(define *weights* #(0 0 0 0 0 0 0 0 0 0
                      0 120 -20 20 5 5 20 -20 120 0
                      0 -20 -40 -5 -5 -5 -40 -20 0
                      0 20 -5 15 3 3 15 -5 20 0
                      0 5 -5 3 3 3 3 -5 5 0
                      0 5 -5 3 3 3 3 -5 5 0
                      0 20 -5 15 3 3 15 -5 20 0
                      0 -20 -40 -5 -5 -5 -40 -20 0
                      0 120 -20 20 5 5 20 -20 120 0
                      0 0 0 0 0 0 0 0 0 0))


(define (weighted-squares player board)
  (let ((opp (opponent player)) (l-board (vector->list board)) (l-weights (vector->list *weights*)))
    (let loop ((lst all-squares) (score 0))
      (if (null? lst) score
          (loop (cdr lst) (cond ((equal? (list-ref l-board (car lst)) player)
                                 (+ score (list-ref l-weights (car lst))))
                                ((equal? (list-ref  l-board (car lst)) opp)
                                 (- score (list-ref l-weights (car lst))))
                                (else score)))))))

#|
;cl
(defun  weighted-squares (player board)
  (let ((opp (opponent plyaer)))
    (loop for i in all-squares
          when (eql (bref board i) player)
          sum (aref *weights* i)
          when (eql (bref board i) opp)
          sum (- (aref *weights* i)))))
|#

;(weighted-squares 'black data)


#|
;cl
(defun final-value (player board)
  (case (signum (count-difference player board))
    (-1 losing-value)
    (0 0)
    (+1 winning-value)))
|#

(define losing-value -10000)
(define winning-value 10000)

(define (signum num)
  (cond ((positive? num) 1)
        ((negative? num) -1)
        (else 0)))

(define (final-value player board)
  (let ((sig (signum (count-difference player board))))
  (case sig
    ((-1) losing-value)
    ((0) 0)
    ((1) winning-value))))

    

;(display all-squares)



;test
(define (minimax player board ply eval-fn)
  (if (= ply 0);深度ゼロなら
      (eval-fn player board);今回はプレイヤーの枚数を返す
      (let ((moves (legal-moves player board)));打てる手をリストで返す
        (if (null? moves);打てる手がもうなくて
            (if (any-legal-move? (opponent player) board);敵の打つ手があるならば
                 (- (minimax (opponent player) board (- ply 1) eval-fn));敵側でMinimax
                (final-value player board));敵も打つ手が無いなら最終評価というか勝ち負け
              (let loop ((moves moves) (best-move 0) (best-val 0));打てる手がまだあるなら
                (if (null? moves) best-move
                  ;  (values best-val best-move);最適の値を返す                         
                      (let* ((board2 (make-move (car moves) player board))
                             (val (- (minimax (opponent player) board2 (- ply 1) eval-fn))))
                        (loop (cdr moves) (if (> val best-val) val best-val) (if (> val best-val) (car moves) best-move)))))))))

#|
(define (minimax-sellf board ply eval-fn)
  (if (= ply 0)
      (eval-fn player board)
      (let ((moves (legal-moves plyaer board)))
        (cond
          ((and
           (and (any-legal-move? (opponent plyaer) board) (any-legal-move? player board))
           (positive? (eval-fn player board)))
            (car 
 |#        
 
        
;(minimax 'black data 0 count-difference) 

#|
               
;cl        
(defun minimax (player board ply eval-fn)
  (if (= ply 0)
      (funcall eval-fn player board) ; プレイアウトの深さが0に達したら評価関数を呼び出して評価値を返す
      (let ((moves (legal-moves player board))) ; プレイヤーが可能な手を取得
        (if (null moves) ; プレイヤーが手を打つことができない場合
            (if (any-legal-move? (opponent player) board) ; 相手プレイヤーが打てる手があるかをチェック
                (- (minimax (opponent player) board (- ply 1) eval-fn)) ; 相手プレイヤーの手番に切り替えて再帰的に探索
                (final-value player board)) ; 相手プレイヤーも手を打てない場合、ゲームの最終評価値を計算
            (let ((best-move nil) (best-val nil));手がある場合、最終的にはValues best-val best-move
              (dolist (move moves) ; プレイヤーが可能な手をループ
                (let* ((board2 (make-move move player (copy-board board))) ; 手を打った後の新しいボードを生成
                       (val (- (minimax (opponent player) board2 (- ply 1) eval-fn)))) ; 相手プレイヤーの手番に切り替えて再帰的に探索
                  (when (or (null best-val) ; 最良の手を見つけた場合、best-moveとbest-valを更新
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move)))))) ; 最適な手の評価値と手を返す
|#



(define (minimax-gpt player board ply eval-fn)
  (if (= ply 0);ここが終着点で枚数を返す
      (eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null? moves);もうその深度で他に手がないか？
            (if (any-legal-move? (opponent player) board);敵も手がないか？
                (minimax-gpt (opponent player) board (- ply 1) eval-fn)
               ; (display "end"))
               (final-value player board));外部関数を呼び出して独立させるべき？
         ;   (let loop ((moves moves) (best-move '()) (best-val -100));同じ深度で手があれば以下実行
           ;   (if (null? moves)
             ;     best-move
            (let ((best-val 0) (best-move #f))
                
                      (for ((move moves))
                          (let* ((board2 (make-move (car moves) player board))
                                 (val
                                  (let-values (((best-v best-m)
                                               (minimax-gpt (opponent player) board2 (- ply 1) eval-fn))) best-v)))
                   (newline) (display (format "moves:~a" moves)) (display " ") (display (format "move:~a"(car moves)))
                    (display " ") (display (format "B-move:~a" best-move)) (display " ") (display (format "val:~a" val))
                    (display " ") (display (format "B-val:~a" best-val))
                    (newline) (print-chessboard board) (newline) (print-chessboard board2)                    
                    (when (or (null? best-val) (> val best-val))
                      (set! best-move move) (set! best-val val))))
              (values best-val best-move)))))) ;ここはSearcherに送るだけかな？
                     ;   (loop (cdr moves) (car moves) val)
                     ;   (loop (cdr moves) best-move best-val)))))))))



;(define (e-minimax player board ply eval-fn)
  
  

;(minimax-gpt 'black data 0 count-difference)
;(count-difference 'black data)

#|
;CL
(defun mimimax-searcher (ply eval-fn)
  (lambda (player board)
    (multiple-value-bind (value move)
                         (minimax player board ply eval-fn)
                         (declare (ignore value))
                         move)))
|#

(define (minimax-searcher ply eval-fn)
  (lambda (player board) ;(minimax-gpt player board ply eval-fn)
   (let-values (((value move) (minimax-gpt player board ply eval-fn))) move))) 

;(othello (maximizer count-difference) (minimax-searcher 3 count-difference))
(othello random-strategy (minimax-searcher 2 count-difference))
;(othello random-strategy (maximizer count-difference)) ;ok
;(othello human (maximizer count-difference))

#|
;cl
(defun alpha-beta (player board achievable cutoff ply eval-fn)
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null? moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta (opponent plyaer) board
                               (- cutoff) (- achievable)
                               (- ply 1) eval-fn))
                (final-value player board))
            (let ((best-move (first moves)))
              (loop for move in moves do
                    (let* ((board2 (make-move move player (copy-board board)))
                           (val (- (alpha-beta (opponent plyaer) borad2 (- cutoff) (- achievable) (-ply 1) eval-fn))))
                      (when (> val achivable)
                        (setf achievable val)
                        (setf best-move move)))
                    until (>= achievable cutoff))
              (values achievable best-move))))))

(define (alpha-beta player board achievable cutoff ply eval-fn)
  

;cl
(defun alhpa-beta-searcher (depth eval-fn)
  #'(lambda (player board)
      (multiple-value-bind (value move)
                           (alpha-beta player board losing-value winning-value depth eval-fn)
                           (declare (ignore value))
                           move)))

|#

