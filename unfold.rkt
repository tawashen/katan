#lang racket/gui

(require (only-in srfi/1 unfold)
         (only-in math/statistics mean))
 
(define one
  (case-lambda
    ((n)
     (for-each display (unfold zero?  (lambda (x) "SPAM\n") sub1 n)))
    (()
     (one 10))))

;(one)


(define two
  (case-lambda
    ((n)
     (for-each display (unfold (lambda (x) (> x 9))
                               (lambda (x) (format "~a~%" (* n x)))
                               add1
                               1)))
    (()
     (two 3))))


(define three
  (case-lambda
    ((base power)
     (for-each display (unfold (lambda (y) (> y power))
                               (lambda (x) (format "~a~%" (expt base x)))
                               add1
                               1)))
    (()
     (three 2 8))))



(define four
  (case-lambda
    ((n)
     (printf "~a~%" (apply * (unfold zero? identity sub1 n))))
    (()
     (four 7))))


(define five
  (case-lambda
    ((n)
     (floor (mean (unfold zero? (lambda (x) (read)) sub1 n))))
    (()
     (five 10))))


(define six
  (case-lambda
    ((n)
     (let ((match-result (unfold zero? (lambda (x) (read)) sub1 n)))
       (printf "~a ~a~%" (count (lambda (x) (= x 1)) match-result)
               (count zero? match-result))))
    (()
     (six 10))))


(define seven
  (case-lambda
    ((n)
     (let* ((games (unfold (lambda (x) (> x n))
                           (lambda (x)
                             `(,(begin (printf "~a回表、巨人の得点は?" x)
                                       (read))
                               ,(begin (printf "~a回裏、阪神の得点は?" x)
                                       (read))))
                           add1
                           1))
            (giants (apply + (map car games)))
            (tigers (apply + (map cadr games))))
       (printf "巨人 ~a点、阪神:~a点~%\t~aの勝ち~%" giants tigers (if (> tigers giants) "阪神" "巨人"))))
    (()
     (seven 9))))


(define eight
  (case-lambda
    ((n)
     (apply max (unfold zero? (lambda (x) (read)) sub1 n)))
    (()
     (eight 10))))
 
#| 練習問題 9
整数を 10 回入力し、最大値と最小値を求めるプログラムを作成しなさい。 |#
(define nine
  (case-lambda
    ((n)
     (let ((lst (unfold zero? (lambda (x) (read)) sub1 n)))
       (values (apply max lst) (apply min lst))))
    (()
     (nine 10))))
 
#| 練習問題 10
個数を示す数値を入力し、その個数分だけ‘*’を表示するプログラムを作成しなさい。 |#
(define (ten n)
  (for-each display (unfold zero? (lambda (x) "*") sub1 n)))
 
#| 練習問題 11
   個数を示す数値を入力し、その個数分だけ 0 ~ 9 の数字を表示するプログラムを作成しなさい。数字は 0 , 1 , 2 , 3 , , の順に表示し、9  の次は 0 に戻るものとします。
     例:     14
  01234567890123  |#
(define (eleven n)
  (for-each display (unfold (lambda (x) (>= x n))
                            (lambda (x) (modulo x 10))
                            add1
                            0)))
 
#| 練習問題 12
10000 より小さい 3 の累乗( 3, 9, 27, , , )をすべて表示するプログラムを作成しなさい。 |#
(define twelve
  (case-lambda
    ((n d)
     (let ((y (floor (/ (log n) (log d)))));
       (for-each display (unfold (lambda (x) (> x y))
                                 (lambda (x) (format "~a~%" (expt d x)))
                                 add1
                                 1))))
    (()
     (twelve 10000 3))))
 
#| 練習問題 13
数値を繰り返し入力し、合計が 100 を超えたら入力を止めて合計を表示するプログラムを作成しなさい。 |#
(define thirteen
  (case-lambda
    ((n)
     (let ((i 0))
       (printf "~a~%" ;(last
               (unfold (lambda (x) (> i n));end
                                    (lambda (x) (set! i (+ i (read)));proc
                                      i)
                                    identity;step
                                    0))));initi
    (()
     (thirteen 100))))
;(thirteen)
 
#| 練習問題 14
   ストライク・カウントを数えるプログラムを作成しなさい。
   1球ごとにストライクかボールかを入力する。
   3ストライクまたは4ボールになったら入力を止め、ストライクとボールのカウントを表示する。
   ※ 	ストライクの場合は 1、ボールの場合は 2 を入力する。
   ストライク=1 or ボール=2 ?
   1
   ストライク=1 or ボール=2 ?
   2
   ストライク=1 or ボール=2 ?
   1
   ストライク=1 or ボール=2 ?
   1
1ボール,3ストライク |#
(define (fourteen)
  (let ((strike-max 3) (ball-max 4))
    (define (foo v)
      (println "ストライク=1 or ボール=2 ?")
      (let ((i (read)))
        (match v
          ((vector strike ball)
           (vector-set! v (sub1 i) (add1 (vector-ref v (sub1 i)))) ;(vector st ba) (vector st ba) ...
           v))))
    (apply printf "~aボール, ~aストライク~%"
           (reverse
            (vector->list
             (last
              (unfold (lambda (x);end
                        (match x
                          ((vector strike ball)
                           (or (= strike strike-max)
                               (= ball ball-max)))))
                      identity ;proc　そのまま返すことでListになる？
                      foo;step クロージャ？ 引数は1つ
                      (foo (vector 0 0)))))))));initial
 
#| 練習問題 15
   前の問題に次の修正を加えなさい。
   1球ごとにストライク、ボール、ファウルの何れかを入力する。(残念ながらヒットにはなりません)
   ファウルの場合、2ストライクまではストライクにカウントするが、3ストライクにはならない。
   3ストライクまたは4ボールになったら入力を止め、ストライクとボールのカウントを表示する。
 |#
(define (fifteen)
  (let ((strike-max 3) (ball-max 4))
    (define (foo v)
      (println "ストライク=1 or ボール=2 ?")
      (let ((i (read)))
        (match v
          ((vector strike ball)
           (with-handlers ((exn:fail?;3のエラーで発動
                            (lambda (exn)
                              (when (< strike 2)
                                (vector-set! v 0 (add1 (vector-ref v 0))))
                              v)))
             (vector-set! v (sub1 i) (add1 (vector-ref v (sub1 i))))
             v)))))
    (apply printf "~aボール, ~aストライク~%"
           (reverse
            (vector->list
             (last
              (unfold (lambda (x)
                        (match x
                          ((vector strike ball)
                           (or (= strike strike-max)
                               (= ball ball-max)))))
                      identity
                      foo
                      (foo (vector 0 0)))))))))
 
#| 練習問題 16
   入力された数が素数かどうかを判定するプログラムを作成しなさい。
※ 	判定する数は 4 以上としてよい。 |#
(define (sixteen n)
  (= (length (unfold (lambda (x);end
                       (> x n))
                     (lambda (x);proc
                       (if (> (expt x 2) n);initiの2乗が任意より大きかったら
                           n;任意の数
                           x));でないならIniti
                     (lambda (x);step
                       (let loop ((x x))
                         (if (zero? (modulo n x));任意の数をinitiで割り切れる
                             (add1 x);add1
                             (loop (add1 x)))));割り切れるまでadd1
                     2));initi x?
     1));length = 1 つまり生成されたリストの要素が１つだけの場合　`(,n)の場合のみ


(define (sixteenL n)
 ; (= (length
      (unfold (lambda (x);end
                       (> x n))
                     (lambda (x);proc
                       x)
                     ;  (if (> (expt x 2) n);initiの2乗が任意より大きかったら
                     ;      n;任意の数
                      ;     x));でないならIniti
                     (lambda (x);step
                       (let loop ((x x))
                         (if (zero? (modulo n x));任意の数をinitiで割り切れる
                             (add1 x);add1
                             (loop (add1 x)))));割り切れるまでadd1
                     2));initi x?
    ; 1));length = 1 つまり生成されたリストの要素が1つだけの場合 `(,n)の場合のみ
;(sixteenL 71)



#| 練習問題 17
   2 以上の数値を入力し、素因数分解した結果を表示しなさい。
   例:
    20100
 2 2 3 5 5 67 |#
(define (seventeen n)
  (define (foo x)
    (let loop ((x x))
      (cond ((and (sixteen x) (zero? (modulo n x)));素数かつ割り切れる場合
             (set! n (quotient n x));数値を商に書き換える
             (displayln (cons x n))
             x);xを返す
            (else (loop (add1 x))))))
  (for-each (lambda (x)
              (printf "~a " x)) (unfold (lambda (x)
                                          (> x (ceiling (sqrt n))));End　Initi>数値の平方根切り上げ
                                        foo;Proc　数値を変更しつつ割り切れる素数をリスト
                                        identity
                                        2))
  (newline))

(seventeen 2024)

(define (seventeen2 n)
  (define (foo x)
    (let loop ((x x))
      (cond ((and (sixteen x) (zero? (modulo n x)));素数か割り切れる場合
             (set! n (quotient n x));数値を商に書き換える
             x);xを返す
            (else (loop (add1 x))))))
  (for-each (lambda (x)
              (printf "~a " x)) (unfold (lambda (x)
                                        ;  (> x (ceiling (sqrt n))));End　Initi>数値の平方根切り上げ
                                         ; (sixteen n))
                                          (= n 1))
                                        foo;Proc　数値を変更しつつ割り切れる素数をリスト
                                        identity
                                        2))
  (newline))

;(seventeen2 2024)

 
#| 練習問題 18
   九九表(一の段~九の段)を表示するプログラムを作成しなさい。
※ 	printf(" %2d", x ); のように、%2d と記述すると表示が 2 桁に揃う。 |#
(define (eighteen)
  (display
   (string-join
    (unfold (lambda (x);end 
              (> (cadr x) 9))
            (lambda (x);proc
              (let ((x (car x)) (y (cadr x)))
                (format "~a × ~a = ~a~a" x y (* x y) (if (= x 9) "\n" " "))))
            (lambda (x);step
              (let ((x (car x)) (y (cadr x)))
                (if (< x 9)
                    `(,(add1 x) ,y)
                    `(1 ,(add1 y)))))
            '(1 1)) ""));init 
  (newline))

;(eighteen)
 
#| 練習問題 19
数値を繰り返して入力し、0 が入力されたら入力を止め、それまでの合計を表示するプログラムを作成しなさい。 |#
(define (nineteen)
  (display (apply + (unfold zero?;end
                            identity;proc
                            (lambda (x)
                              (read));step
                            (read))));initi
  (newline))
 
#| 練習問題 20
   数値を繰り返して入力し、0 が入力されたら入力を止め、平均値を表示するプログラムを作成しなさい。
   ※ 	計算は整数で行い、小数点以下は切り捨ててよい。
   ※ 	最後に入力された 0 は平均に含めない。
   ※ 	少なくとも 1 回は入力が行われるものとする。(最初に 0 を入力してはいけない)
 |#
(define (twenty)
  (display (floor (mean (unfold zero?
                                identity
                                (lambda (x)
                                  (read))
                                (read)))))
  (newline))
 
#| 練習問題 21
   サイズを示す数値を入力し、何等かの文字で例のような三角形を表示するプログラムを作成しなさい。
   サイズ 4 の例
   $
   $$
   $$$
$$$$ |#
(define (twentyOne n)
  (display (string-join (unfold (lambda (x)
                                  (> x n))
                                (lambda (x)
                                  (string-append (make-string x #\$) "\n"))
                                add1
                                1) "")))
;(twentyOne 10)
 
#| 練習問題 22
   サイズを示す数値を入力し、何等かの文字で、そのサイズの×印を表示するプログラムを作成しなさい。
   サイズ 3 の例
   X X
    X
   X X
   サイズ 4 の例
   X  X
    XX
    XX
   X  X
   サイズ 5 の例
   X   X
    X X
     X
    X X
X   X  |#
(define (twentyTwo n)
  (display (string-join (unfold (lambda (x)
                                  (> x (sub1 n)))
                                (lambda (x)
                                  (let ((s (make-string n #\space)))
                                    (string-set! s x #\X)
                                    (string-set! s (- n x 1) #\X)
                                    (string-append s "\n")))
                                add1
                                0) "")))
;(twentyTwo 10)
 
#| 練習問題 23
   フィボナッチ数列を表示するプログラムを作成しなさい。
   最初の2つの項を 0、1 とし、1000 まで( 1000 以下の項)を表示するものとします。
   ※ 	         フィボナッチ数列:
    それぞれの項がその直前の2つの項の和になっている数列のこと。
         例:0, 1, 1, 2, 3, 5, 8, 13, 21, ... |#
(define twentyThree
  (case-lambda
    ((n) (display (string-join
                   (unfold (lambda (x)
                             (> (car x) n))
                           (lambda (x)
                             (number->string (car x)))
                           (lambda (x)
                             `(,(cadr x) ,(+ (car x) (cadr x))))
                           '(0 1)) ", "))
         (newline))
    (()
     (twentyThree 1000))))