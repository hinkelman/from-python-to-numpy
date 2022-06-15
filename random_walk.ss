
(define (random-help)
  ;; (random 2) returns 0 or 1
  (- (* 2 (random 2)) 1))

;; procedural
(define (random-walk n)
  (let ([steps (make-vector n)
