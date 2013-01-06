(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
