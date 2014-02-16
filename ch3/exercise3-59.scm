;; 問題3.59
(load "./ch3/3-5-2_Infinite_Streams.scm")
;; 無限ストリームでべき級数を扱う
;; a. べき級数を表現するストリームを受け取り，積分（定数項を除く）した項の係数ストリームを返す
(define (integrate-series s)
  (stream-map / s integers))
;; s0/1, s1/2, s2/3, ...

;; b. expは積分しても定数項を除けば同じ
(define exp-series (cons-stream 1 (integrate-series exp-series)))
(show-stream exp-series 0 10)

;; sinとcosの級数を生成する方法を示せ
;; sinの微分がcos，cosの微分が-sinであることを使え
(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(show-stream cosine-series 0 5)
(show-stream sine-series 0 5)

