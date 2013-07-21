;; 問題2.9

; http://www.serendip.ws/archives/526
; 区間aの幅を返す手続き
(define (width-of-interval a)
    (/ (- (upper-bound a) (lower-bound a)) 2.0))

; (+ x y)の幅
(/
(-
  (+ (upper-bound x) (upper-bound y))
  (+ (lower-bound x) (lower-bound y))
  2)

(/
  (+
    (- (upper-bound x) (lower-bound x))
    (- (upper-bound y) (lower-bound y)))
  2)

; http://d.hatena.ne.jp/awacio/20100329/1269869969
;; 上のURLから引用
;; 区間の幅 = (x_u * y_u) - (x_l * y_l)
; 幅の関数にできない
