;; 問題3.81
(load "./ch3/3-5-3_Exploiting_the_Stream_Paradigm.scm")

(define (rand-update x)
  (remainder (+ x 1812433253) 4294967296))
(define (rand input-stream random-init)
  (define random-stream
    (if (stream-null? input-stream)
      the-empty-stream
      (let ((request (stream-car input-stream)))
        (cons-stream
          (cond ((eq? request 'generate) (rand-update random-init))
                ((number? request) (rand-update request))
                (else (error "Unknown request -- RAND" request)))
          (rand (stream-cdr input-stream) (stream-car random-stream))))))
  random-stream)

(define request-stream
  (cons-stream
    100
    (cons-stream
      'generate
      (cons-stream
        'generate
        (cons-stream
          100
          (cons-stream
            'generate
            (cons-stream 'generate the-empty-stream)))))))
(show-stream (rand request-stream 1) 0 6)
;;何か正しく動いてない

