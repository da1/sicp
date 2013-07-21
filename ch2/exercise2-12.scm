;; 問題2.12
;; 中央値とパーセント相対許容誤差をとり，望みどおりの区間を返す構成子を定義せよ
(load "./ch2/exercise2-7.scm")

(define (make-center-percent c e)
  (cons (* c (- 1 e)) (* c (+ 1 e))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (width i) (center i)))

(define p (make-center-percent 100 0.05))

(lower-bound p)
(upper-bound p)
(center p)
(percent p)
