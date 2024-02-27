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
           ; (brain-eval vec)))
(define @< (lambda (vec)
            (vector-set! vec (- (vector-length vec) 1) (- (vector-last vec) 1)) vec))
          ;  (brain-eval vec)))
(define @+ (lambda (vec)
            (vector-set! vec (vector-last vec) (+ (vector-ref vec (vector-last vec)) 1)) vec))
          ;  (brain-eval vec)))
(define @- (lambda (vec)
            (vector-set! vec (vector-last vec) (- (vector-ref vec (vector-last vec)) 1)) vec))
           ; (brain-eval vec)))
(define |@.| (lambda (vec)
              (begin (displayln (integer->char (vector-ref vec (vector-last vec)))) (brain-eval vec))))
(define |@,| (lambda (vec)
              (displayln (integer->char (vector-ref vec (vector-last vec))))
              (let ((new-v (string-ref (read-line) 0)))
                (vector-set! vec (vector-last vec) (char->integer new-v)) (brain-eval vec))))
(define |[| (lambda (vec)
              (let loop ((answer (read-line)) (acc '()))
                (case answer
                  ((">") (loop (read-line) (cons @> acc)))
                  (("<") (loop (read-line) (cons @< acc)))
                  (("+") (loop (read-line) (cons @+ acc)))
                  (("-") (loop (read-line) (cons @- acc)))
                  ((".") (loop (read-line) (cons |@.| acc)))
                  ((",") (loop (read-line) (cons |@,| acc)))
                  (("]") (let loop ((commands (reverse acc)) (new-vec vec))
                           (if (zero? (vector-ref new-vec (vector-last new-vec))) new-vec
                               (loop (cdr commands) ((car commands) new-vec)))))
                  (else (loop (read-line) acc))))))
                  
                  
                
              

(define (brain-eval world)
  (let ((point (vector-last world)))
   ; (display point)
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
            (else (cond
                    ((equal? #\+ (string-ref command 0))
                     (let ((command-list (make-list (string-length command) @+)))
                       (let ((last-vec (for/fold ((new-vec vec)) ((command2 command-list)) (command2 new-vec))))
                         (brain-eval last-vec))))
                    ((equal? #\- (string-ref command 0))
                     (let ((command-list (make-list (string-length command) @-)))
                       (let ((last-vec (for/fold ((new-vec vec)) ((command2 command-list)) (command2 new-vec))))
                         (brain-eval last-vec))))))))))
        
   
  
      ;  ))))

(brain-read)
        
