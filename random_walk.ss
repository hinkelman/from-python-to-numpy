;; shadows Chez's version of vector-copy
(import (only (srfi :43 vectors)
              vector-copy
              vector-append))

;; random walk procedures ---------------------------------

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

;; find crossing procedures -------------------------------

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

;; same-head? and find-crossing-lst-2 provided by oaktownsam on Scheme Discord server
(define (same-head? seq sub)
  (cond
    [(and (null? seq) (null? sub)) #t]
    [(null? sub)                   #t]
    [(null? seq)                   #f]
    [(equal? (car seq) (car sub))  (same-head? (cdr seq) (cdr sub))]
    [else #f]))

(define (find-crossing-lst-2 seq sub)
  (define (step index seq results)
    (cond
      [(null? seq) (reverse results)]
      [(same-head? seq sub) (step (add1 index) (cdr seq) (cons index results))]
      [else (step (add1 index) (cdr seq) results)]))
  (step 0 seq '()))

;; oaktownsam also pointed out that if you are going to use these indices in subsequent steps,
;; then you should be using a vector as a data structure
(define (find-crossing-vec seq sub)
  (let* ([seq-len (vector-length seq)]
         [sub-len (vector-length sub)]
         [results (vector)])
    (do ((i 0 (add1 i)))
        ((> i (- seq-len sub-len)))
      (when (equal? (vector-copy seq i (+ i sub-len)) sub)
        (set! results (vector-append results (vector i)))))
     results))

;; timing procedures ---------------------------------
;; return void for timing random walk procedures

(define (void-return proc)
  (let ([x (proc)])
    (void)))

;; random walk
(time (void-return (lambda () (random-walk-vec 1000))))
(time (void-return (lambda () (random-walk-lst 1000))))

;; find-crossing
(define W-lst (random-walk-lst 1000))
(time (find-crossing-lst W-lst '(1 0 -1)))
(time (find-crossing-lst-2 W-lst '(1 0 -1)))
;; random-walk-vec could be used to generate the vector
;; but want to compare timing on same vector
(define W-vec (list->vector W-lst))
(time (find-crossing-vec W-vec (vector 1 0 -1)))
;; find-crossing-vec not very fast so checking overhead of just converting to vector
(time (find-crossing-lst-2 (vector->list W-vec) '(1 0 -1)))

