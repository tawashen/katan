#lang racket


(require srfi/1)
(define data #(outer outer outer outer outer outer outer outer outer outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty white black empty empty empty outer outer
                     empty empty empty black white empty empty empty outer outer
                     empty empty empty black empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     empty empty empty empty empty empty empty empty outer outer
                     outer outer outer outer outer outer outer outer outer))


(define (othello (bl-strategy wh-strategy)
                 #:optional (print #t))
 ; (let ((board (intial-board)))
    (let loop ((player 'black) (board (intial-board)))
      

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


;ok?
(define (next-to-play board previous-player print)
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp);敵が一つでも指せる場所がある場合、敵を返す
          ((any-legal-move? previous-player board);自分が一つでも指せれば
           (when print;そしてPrintが#tだったら
             (format "~a has no moves and must pass."
             (name-of opp)));メッセージを表示
           previous-player);Printが#fなら自分を返す
          (else #f))))


;ok?
(define (any-legal-move? player board);#tかどうかだけの返り値
  (some (lambda (move) (legal-p move player board)) all-squares));一つでも指せる場所があるか？


;ok?
(define (get-move strategy player board print)
  (when print (print-board board))
  (let ((move (strategy player (copy-board board))))
    (cond ((and (valid-p move) (legal-p move player board))
           (when print (format "~a moves to ~a." (name-of player) move))
           (make-move move playr board))
          (else (warn "illegal move: ~a" move)
                (get-move strategy player board print)))))

;ok?      
(define (human player board)
  (format "~a to move:" (name-of player))
  (string->number (read-line)))

;ok
(define (random-elt lst)
  (define lst-length (length lst))
  (define random-index (random lst-length))
  (list-ref lst random-index))

;ok?
(define (random-strategy player board)
  (random-elt (legal-moves player board)))

;CL
(define (legal-moves player board)
  (loop for move in all-squares
        when (legal-p move player board) collect move))

;ok?
(define (legal-moves player board)
  (let ((acc '()))
  (for ((move all-squares))
    (filter (lambda (x) x) (cons (legal-p move player board) acc)))
    acc))


(define (legal-moves lst)
  (filter (lambda (x) x)
          (for/list ((move lst))
            (if (even? move) move #f))))

(legal-moves '(1 2 3 4 5))


                