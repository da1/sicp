;; 問題 3.69
(load "./ch3/3-5-3_Exploiting_the_Stream_Paradigm.scm")

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs (stream-cdr t) (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define triples-of-integers (triples integers integers integers))
(show-stream triples-of-integers 0 1)

(define pythagoras
  (stream-filter (lambda (triple)
                   (= (+ (square (car triple))
                         (square (cadr triple)))
                      (square (caddr triple))))
                 triples-of-integers))
(show-stream pythagoras 0 5)

