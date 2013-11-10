;問題3.5
;モンテカルロ積分
(load "./ch3/3_Modularity_Objects_and_State.scm")
(use srfi-27)
;(load "/usr/share/slib/random.scm")
;(define (random-in-range low high)
;  (let ((range (- high low)))
;    (+ low (random range))))
;(random-in-range 0.0 1.0)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))
(random-in-range 0.0 1.0)

(define (P px py r)
  (define (square value)
    (* value value))
  (lambda (x y)
    (>= (square r) 
     (+ (square (- x px)) (square (- y py))))))
(define p1 (P 0 0 1))
(p1 0 0)
(p1 1 0)
(p1 1 1)

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (p-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (- x1 x2) (- y1 y2))
     (monte-carlo trials p-test)))
(estimate-integral (P 0.0 0.0 1.0) 1.0 -1.0 1.0 -1.0 100000)
