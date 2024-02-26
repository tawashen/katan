#lang racket

(define (brain-read)
  (displayln "メモリ容量を決めてくれ")
  (let ((memory (string->number (read-line))))
    (if (not (number? memory)) brain-read
        (brain-eval (make-vector (+ memory 1) 0)))))

(define (vector-last vec)
  (let ((num (vector-length vec)))
    (vector-ref vec (- num 1))))

(define (brain-eval world)
  (let ((point (vector-last world)))
    (display point)
    (let ((command (read-line)) (vec (vector-copy world)))
      (case command
        ((">") (vector-set! vec (- (vector-length world) 1) (+ point 1)) (brain-eval vec))
        ((">") (vector-set! vec (- (vector-length world) 1) (- point 1)) (brain-eval vec))
        (("+") (vector-set! vec point (+ (vector-ref vec point) 1)) (brain-eval vec))
        (("-") (vector-set! vec point (- (vector-ref vec point) 1)) (brain-eval vec))
        ((".") (begin (displayln (integer->char (vector-ref vec point))) (brain-eval vec)))
        ((",") (displayln (integer->char (vector-ref vec point)))
             (let ((new-v (string-ref (read-line) 0)))
               (vector-set! vec point (char->integer new-v)) (brain-eval vec)))
        
        ))))
  
      ;  ))))

(brain-read)
        
