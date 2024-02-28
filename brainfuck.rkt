#lang racket

(define (brain-read)
  (displayln "メモリ容量を決めてくれ")
  (let ((memory (string->number (read-line))))
    (if (not (number? memory)) brain-read
        (brain-eval (make-vector (+ memory 1) 0)))))

(define (vector-last vec)
  (let ((num (vector-length vec)))
    (vector-ref vec (- num 1))))

(define @> (lambda (vec)
            (vector-set! vec (- (vector-length vec) 1) (+ (vector-last vec) 1)) vec))
(define @< (lambda (vec)
            (vector-set! vec (- (vector-length vec) 1) (- (vector-last vec) 1)) vec))
(define @+ (lambda (vec)
            (vector-set! vec (vector-last vec) (+ (vector-ref vec (vector-last vec)) 1)) vec))
(define @- (lambda (vec)
            (vector-set! vec (vector-last vec) (- (vector-ref vec (vector-last vec)) 1)) vec))
(define |@.| (lambda (vec)
              (begin (displayln (integer->char (vector-ref vec (vector-last vec)))) (brain-eval vec))))
(define |@,| (lambda (vec)
              (displayln (integer->char (vector-ref vec (vector-last vec))))
              (let ((new-v (string-ref (read-line) 0)))
                (vector-set! vec (vector-last vec) (char->integer new-v)) (brain-eval vec))))
(define |[| (lambda (vec)
              (let loopA ((answer (read-line)) (command-list '()))
                (case answer
                  ((">") (loopA (read-line) (cons @> command-list)))
                  (("<") (loopA (read-line) (cons @< command-list)))
                  (("+") (loopA (read-line) (cons @+ command-list)))
                  (("-") (loopA (read-line) (cons @- command-list)))
                  ((".") (loopA (read-line) (cons |@.| command-list)))
                  ((",") (loopA (read-line) (cons |@,| command-list)))
                  (("]") (let ((point (vector-last vec)))
                           ;  (brain-eval                            
                           ; (for/fold ((new-vec vec)) ((new-command command-list)) (new-command new-vec)))))))) ;fold-pattern
                           (let loopC ((vec vec))
                             (if (zero? (vector-ref vec point)) vec
                                 (loopC 
                                  (let loopB ((command (reverse command-list)) (new-vec vec))
                                    (let ((new-point (vector-last new-vec)))
                                      (if (null? command) (loopC new-vec)
                                          (loopB (cdr command) ((car command) new-vec))))))))))
                  (else (loopA (read-line) command-list))))))
                 
                  
                
              

(define (brain-eval world)
  (let ((point (vector-last world)))
    (let ((command (read-line)) (vec (vector-copy world)))
      (cond ((= 1 (string-length command))
             (case command
               (("p") (display (vector-ref vec (vector-last vec))) (brain-eval vec))
               ((">") (brain-eval (@> vec)))
               (("<") (brain-eval (@< vec)))
               (("+") (brain-eval (@+ vec)))
               (("-") (brain-eval (@- vec)))
               ((".") (|@.| vec))
               ((",") (|@,| vec))
               (("[") (|[| vec))
               (else (brain-eval world))))
            (else
             (let ((command-list (make-list (string-length command)
                                            (case (string-ref command 0)
                                              ((#\+) @+)
                                              ((#\-) @-)))))
               (let ((last-vec (for/fold ((new-vec vec)) ((command2 command-list)) (command2 new-vec))))
                 (brain-eval last-vec))))))))


(brain-read)
        
