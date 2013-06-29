;; 問題2.2
;; 平面上の線分を表現する問題を考える

;; 線分には出発点と終着点がある
;; 点はxとy座標がある

;; 選択子と構成子を使い引数に線分を取り，中間点を返す手続きmidpoint-segmentを定義せよ
(load "./utils.scm")

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define a (make-segment (make-point 0 0) (make-point 1 1)))

(print-point (start-segment a))
(print-point (end-segment a))

(define (midpoint-segment segment)
  (let ((s-point (start-segment segment))
        (e-point (end-segment segment)))
  (make-point
    (average (x-point s-point) (x-point e-point))
    (average (y-point s-point) (y-point e-point)))))

(print-point (midpoint-segment a))
