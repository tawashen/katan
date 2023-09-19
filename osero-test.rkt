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
                     empty empty empty white black empty empty empty outer outer
                     empty empty empty black white empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     outer outer outer outer outer outer outer outer outer))

(define (format-piece piece)
  (cond
    [(eq? piece 'outer) "O"]
    [(eq? piece 'empty) "."]
    [(eq? piece 'white) "W"]
    [(eq? piece 'black) "B"]))

(define (print-chessboard data)
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
         (find-bracketing-piece (+ c dir) player board dir))))

(define (find-bracketing-piece square player board dir #:optional (lst '()));ここを座標のリストにして返さないといけない
  (cond ((equal? (vector-ref board square) player) lst);打つ場所の一つDir側の更に一つ先がが自分のコマだったらソコ
        ((equal? (vector-ref board square) (opponent player));↑が敵のコマだったら
         (find-bracketing-piece (+ square dir) player board dir (cons square lst)));更に一つ先のコマで再帰する
        (else #f)));自分で挟めなかったら#f

;(would-flip? 57 'black data -1)

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



(define (make-move move player board);->board
 ; (vector-set! board move player);moveの場所にPlayerを配置する
  (let ((board2 (list->vector (list-set (vector->list board) move player))))
  (let ((board3 (for/list ((dir all-directions));(-11 -10 -1 1 ...)
          (make-flips move player board2 dir))))
  (car (filter (lambda (x) x) board3)))))

(define (make-flips move player board dir);->board
  (let ((bracketer (would-flip? move player board dir)));ひっくり返す座標のリストが入る
    (if (not (null? bracketer));偽でなければ
      (let loop ((c (+ move dir)) (board board));マス目に移動距離を足したものをC
        (cond ((equal? c bracketer) board);move + dir が挟めるマスまで来たらボードを返す
            (loop (+ c dir) (list->vector (list-set (vector->list board) c player)))))
      #f)))
           
      
      
;(make-move 56 'black data)



;ok?
(define (any-legal-move? player board);#tかどうかだけの返り値
  (some (lambda (move) (legal-p move player board)) all-squares));一つでも指せる場所があるか?

;ok?
(define (next-to-play board previous-player print);->player
  (let ((opp (opponent previous-player)));
    (cond ((any-legal-move? opp board) opp);敵が一つでも指せる場所がある場合、敵を返す
          ((any-legal-move? previous-player board);自分が一つでも指せれば
           (when print;そしてPrintが#tだったら
             (format "~a has no moves and must pass."
             opp));メッセージを表示
           previous-player);Printが#fなら自分を返す
          (else #f))))

;ok?
(define (get-move strategy player board print);->board
  (when print (print-chessboard board))
  (let ((move (strategy player board)));strategyで出されたマス目をmoveに束縛
    (cond ((and (valid-p move) (legal-p move player board));条件どっちもオッケイでなら
           (when print (format "~a moves to ~a." player move));Printが真なら表示
           (make-move move player board));
          (else (display (format "illegal move: ~a" move)) (newline) ;駄目な手なら再帰
                (get-move strategy player board print)))))

;ok?      
(define (human player board);humanの場合
  (format "~a to move:" player)
  (string->number (read-line)));人力で手を入力

;ok
(define (random-elt lst);リストの中からランダムで一つ選ぶ
  (define lst-length (length lst))
  (define random-index (random lst-length))
  (list-ref lst random-index))

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
            (legal-p move player board))))


(define (othello bl-strategy wh-strategy
                 #:optional (print #t))
    (let loop ((player 'black) (board (initial-board)) (strategy bl-strategy))
      (if (not player) board
          (loop
           (next-to-play board player print)
           (get-move strategy player board print)
           (if (equal? player 'black) bl-strategy wh-strategy)))))

(othello human human)

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

            

        
   











