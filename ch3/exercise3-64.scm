;; 問題3.64
(define (stream-limit s q)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (< (abs (- s1 s2)) q)
      s2
      (stream-limit (stream-cdr s) q))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2.0 0.001)

