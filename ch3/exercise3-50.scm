;; 問題3.50
;; stream-mapの手続き
(load "./ch3/3-5_Streams.scm")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

;(display-stream
;  (stream-map +
;              (stream-enumerate-interval 1 10)
;              (stream-enumerate-interval 1 10)))
