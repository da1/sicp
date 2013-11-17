;; 問題3.60
;; 係数のストリームとして表現したべき級数で，乗算の手続きの定義を完成せよ
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                 (stream-map
                   (lambda (x) (* (stream-car s1) x))
                   (stream-cdr s2))
                 (mul-series (stream-cdr s1) s2))))
;; scale-stream
;; (sinx)^2 + (consx)^2 = 1によるテスト
(define square-sine-and-square-cosine
   (add-streams
  (mul-series sine-series sine-series)
  (mul-series cosine-series cosine-series)))

(show-stream square-sine-and-square-cosine 0 3)


