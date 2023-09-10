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
(display initial-board-result)



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

      


(define (would-flip? move player board dir);OK
  (let ((c (+ move dir))) ;c 打つ場所の一つDir側
    (and (equal? (vector-ref board c) (opponent player));一つDir側が敵側(White)であり
         (find-bracketing-piece (+ c dir) player board dir))))

(define (find-bracketing-piece square player board dir);OK
  (cond ((equal? (vector-ref board square) player) square);打つ場所の一つDir側の更に一つ先がが自分のコマだったら
        ((equal? (vector-ref board square) (opponent player));↑が敵のコマだったら
         (find-bracketing-piece (+ square dir) player board dir));更に一つ先のコマで再帰する
        (else #f)))

;(would-flip? 57 'black data -1)

(define (some proc lst);OK
  (cond ((null? lst) #f)
        ((proc (car lst)) (car lst))
        (else (some proc (cdr lst)))))


(define (legal-p move player board);OK
  (and (equal? (vector-ref board move) 'empty) ;空のマス目か?
       (some (lambda (dir) (would-flip? move player board dir))
  all-directions)))

;(display (map (lambda (x) (legal-p x 'black data)) (iota 100)))



(define (make-move move player board)
 ; (vector-set! board move player);moveの場所にPlayerを配置する
  (let ((board2 (list->vector (list-set (vector->list board) move player))))
  (let ((board3 (for/list ((dir all-directions));(-11 -10 -1 1 ...)
          (make-flips move player board2 dir))))
  (filter (lambda (x) x) board3))))

(define (make-flips move player board dir)
  (let ((bracketer (would-flip? move player board dir)));挟める場合は(-11 -10 -1 ...)のいずれかの数値が入る
    (if bracketer;偽でなければ
      (let loop ((c (+ move dir)) (board board));マス目に移動距離を足したものをC
        (cond ((equal? c bracketer) board);move + dir が挟めるマスまで来たらボードを返す
            (loop (+ c dir) (list->vector (list-set (vector->list board) c player)))))
      #f)))
           
      
      
(make-move 56 'black data)



        
   











