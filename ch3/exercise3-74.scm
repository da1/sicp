;; 問題3.74
(load "./ch3/3-5-3_Exploiting_the_Stream_Paradigm.scm")

(define sense-data
  (stream-map (lambda (x) (sin x)) integers))

(define (sign-change-detector s2 s1)
  (cond ((and (<= 0 s2) (> 0 s1)) 1)    ;; 負->正 1
        ((and (> 0 s2) (<= 0 s1)) -1)   ;; 正->負 -1
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream)
                         (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector sense-data
              (cons-stream 0 sense-data)))

(show-stream zero-crossings 0 10)

