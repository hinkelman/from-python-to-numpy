
(define (random-help)
  ;; (random 2) returns 0 or 1
  (- (* 2 (random 2)) 1))

;; procedural (do loop)
(define (random-walk-vec n)
  (let ([position 0]
        [walk (make-vector n)])
    (do ((i 0 (add1 i)))
        ((= i (sub1 n)))
      (set! position (+ position (random-help)))
      (vector-set! walk i position))
    walk))

;; recursive
(define (random-walk-lst n)
  (let loop ([step 0]
             [position (random-help)]
             [walk '()])
    (if (= step n)
        (reverse walk)
        (loop (add1 step)
              (+ position (random-help))
              (cons position walk)))))

;; return void for timing purposes
(define (void-return proc)
  (let ([x (proc)])
    (void)))

;; need to be run in REPL
(time (void-return (lambda () (random-walk-vec 1000))))
(time (void-return (lambda () (random-walk-lst 1000))))

;; https://stackoverflow.com/a/28034455
(define (first-n lst n)
  (if (zero? n)            
      '()                
      (cons (car lst)         
            (first-n (cdr lst)    
                     (- n 1)))))

(define (find-crossing-lst seq sub)
  (let loop ([index 0]
             [lst seq]
             [results '()])
    (if (< (length lst) (length sub))
        (reverse results)
        (if (equal? (first-n lst (length sub)) sub)
            (loop (add1 index) (cdr lst) (cons index results))
            (loop (add1 index) (cdr lst) results)))))

(define W-lst (random-walk-lst 1000))
(time (find-crossing-lst W-lst '(1 0 -1)))
(define W-vec (random-walk-vec 1000))
(time (find-crossing-lst (vector->list W-vec) '(1 0 -1)))
