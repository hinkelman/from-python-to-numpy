
(define (random-help)
  ;; (random 2) returns 0 or 1
  (- (* 2 (random 2)) 1))

;; procedural (do loop)
(define (random-walk n)
  (let ([position 0]
        [walk (make-vector n)])
    (do ((i 0 (add1 i)))
        ((= i (sub1 n)))
      (set! position (+ position (random-help)))
      (vector-set! walk i position))
    walk))

;; recursive
(define (random-walk-rec n)
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
(time (void-return (lambda () (random-walk 1000))))
(time (void-return (lambda () (random-walk-rec 1000))))
    
    
