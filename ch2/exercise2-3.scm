;; 問題2.3 rectangleの表現を実装せよ
;; 構成子と選択子を使い周囲の長さと面積を計算する手続きを作れ

(load "./ch2/exercise2-2.scm")

(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (left-upper rec)
  (car rec))

(define (right-lower rec)
  (cdr rec))

(define (side-length rec)
  (abs (- (x-point (right-lower rec)) (x-point (left-upper rec)))))

(define (side-height rec)
  (abs (- (y-point (left-upper rec)) (y-point (right-lower rec)))))

(define (perimeter rec)
  (+ (* (side-length rec) 2) (* (side-height rec) 2)))

(define (area rec)
  (* (side-length rec) (side-height rec)))

(define rectangle1 (make-rectangle (make-point 10 10) (make-point 20  0)))
(perimeter rectangle1)
(area rectangle1)

(define rectangle2 (make-rectangle (make-point -5  5) (make-point  5 -5)))
(perimeter rectangle2)
(area rectangle2)

(define rectangle3 (make-rectangle (make-point 0 0) (make-point -10 -10)))
(perimeter rectangle3)
(area rectangle3)


