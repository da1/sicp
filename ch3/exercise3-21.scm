;; 問題3.21
(define q1 (make-queue))
(insert-queue! q1 'a)
;; ((a) a)

(insert-queue! q1 'b)
;; ((a b) b)

(delete-queue! q1)
;; ((b) b)

(delete-queue! q1)
;; (() b)

(define (print-queue queue)
  (define (print-head q)
    (if (null? q)
      '()
      (cons (car q) (print-head (cdr q)))))
  (if (empty-queue? queue)
    '()
    (print-head (car queue))))

(print-queue q1)
(print-queue (insert-queue! q1 'a))


