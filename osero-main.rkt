#lang racket

; Your code here!

(define all-directions '(-1 1 -10 -9 -1 1 9 10 11))
(define empty 0)
(define black 1)
(define white 2)
(define outer 3)

;(deftype piece () `(integer ,empty ,outer)) 不要

(define (name-of piece) ;Piece型を作ってないので数字だけで管理
  (string-ref ".@0?" piece))

(define (opponent player) (if (eql player black) white black))

;(deftype board () '(simple=array piece (100))) 不要

(define (bref (board squre) (aref board square)));意味ある？

;(defsetf bref (board square) (val) `(setf (aref ,board ,square) ,val));vector-setでOK


;(defun (copy-board board) (copy-seq board));Schemeだともともと破壊しないので不要では？

;(defconstant all-squares
;              (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))↓に書き換え

#|
(define (all-squares)
  (define (loop i result)
    (if (> i 88)
        result
        (if (<= 1 (remainder i 10) 8)
            (loop (+ i 1) (cons i result))
            (loop (+ i 1) result))))
  (reverse (loop 11 '())))

(define all-squares-list (all-squares))
|#

#|
(defun initial-board ()
        (let ((board (make-array 100 :element-type 'piece :initial-element outer)))
             (dolist (square all-squares)
                     (setf (bref board square) empty))
                 (setf (bref board 44) white (bref board 45) black
                       (bref board 54) black (bref board 55) white)
                       board))
|#

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

(define all-squares
  (let loop ((i 11) (squares '()))
    (if (> i 88)
        (reverse squares)
        (if (<= 1 (remainder i 10) 8)
            (loop (+ i 1) (cons i squares))
            (loop (+ i 1) squares)))))

(define initial-board-result (initial-board))

#|     
(defun print-board (board)
       (format t "~2&  1 2 3 4 5 6 7 8 [~c=~2 a ~c=~2a (~@d)]"
               (name-of black) (count black board)
               (name-of white) (count white board)
               (count-difference black board))
           (loop for row from 1 to 8 do
                 (format t "~& ~d " (* 10 row))
                 (loop for col from 1 to 8
                       for piece = (bref board (+ col (* 10 row)))
                       do (format t "~c" (name-of piece))))
                   (format t "~2&"))
|#


               
(defun count-difference (player board)
       (- (count player board)
          (count (opponent player) board)))
      
      
                   









