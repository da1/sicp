;; 問題1.29
;; Simpsonの公式

(load "./ch1/1-3_Formulating_Abstractings_with_Higher-Order_Procedures.scm")

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (coeffient k)
    (cond ((or (= k 0) (= k n)) 0)
          ((even? k) 2)
          (else 4)))
  (define (simpson-term k)
    (* (coeffient k) (f (+ a (* k h)))))
  (* (/ h 3)
    (sum simpson-term 0 inc n)))

(simpson cube 0.0 1.0 100)
(simpson cube 0.0 1.0 1000)
