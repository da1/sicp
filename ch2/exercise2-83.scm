;; 問題2.83
;; 各型について，型の塔を一レベル高める手続きを設計せよ

(load "./ch2/2-5-2_combining_data_of_different_types.scm")
(load "./ch2/exercise2-78.scm")

(define (raise x) (apply-generic 'raise x))

;;http://d.hatena.ne.jp/awacio/20101017/1287326044
(define scheme-num (make-scheme-number 2))
scheme-num
(define rational-num (raise scheme-num))
rational-num
(define complex-num (raise rational-num))
complex-num

