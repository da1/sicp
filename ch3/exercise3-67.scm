;; 問題3.67
;; http://www.serendip.ws/archives/1675
(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car t) x))
                    (stream-cdr s))
        (stream-map (lambda (x) (list x (stream-car s)))
                    (stream-cdr t)))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define pairs-of-integers (pairs integers integers))
(show-stream pairs-of-integers 0 10)

