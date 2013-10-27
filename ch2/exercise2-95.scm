;; 問題2.95

(load "./ch2/exercise2-94.scm")

;2.95
(define p1 (make-polynomial 'x (make-sparse-term '((2 1) (1 -2) (0 1)))))
(define p2 (make-polynomial 'x (make-sparse-term '((2 11) (0 7)))))
(define p3 (make-polynomial 'x (make-sparse-term '((1 13) (0 5)))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)

;13x^4 + ???
;11x^4 + ???
;11/13を上式にかけるて下式を引くとx^4の項が消せるが、上式の他の項に11/13がかかる。
;そういう計算を他の項でもやっていくと結果が変なことになる

