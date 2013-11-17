;; 問題3.36
(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

(for-each-except setter inform-about-value constraints)
;; 上式の環境の図を描け

