;問題3.2
(define (make-monitored f)
  (define count 0)
  (define (reset-count)
    (begin (set! count 0)
           count))
  (define (dispatch mf)
    (cond ((eq? mf 'reset-count) (reset-count))
          ((eq? mf 'how-many-calls?) count)
          (else (begin
                  (set! count (+ count 1))
                  (f mf)))))
  dispatch)


(define s (make-monitored sqrt))
(s 100)
;10
(s 'how-many-calls?)
;1

(define s2 (make-monitored sqrt))
(s2 9)
(s2 25)
(s2 'how-many-calls?)
(s2 'reset-count)
(s2 'how-many-calls?)
