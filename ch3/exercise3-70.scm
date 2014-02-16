;; 問題3.70
(load "./ch3/3-5-3_Exploiting_the_Stream_Paradigm.scm")

(define (merge-weighted pairs1 pairs2 weight)
  (cond ((stream-null? (stream-car pairs1)) pairs2)
        ((stream-null? (stream-car pairs2)) pairs1)
        (else
          (let ((p1car (stream-car pairs1))
                (p2car (stream-car pairs2)))
            (if (< (weight p1car) (weight p2car))
              (cons-stream p1car (merge-weighted pairs2 (stream-cdr pairs1) weight))
              (cons-stream p2car (merge-weighted pairs1 (stream-cdr pairs2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (add-pairs-weight pair)
  (+ (car pair) (cadr pair)))

(define p (weighted-pairs integers integers add-pairs-weight))
(show-stream p 0 3)

(define (add-pairs-weight2 pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define integers-no-remainder-2-3-5
  (stream-filter (lambda (x)
                   (not (and (= 0 (remainder x 2))
                             (= 0 (remainder x 3))
                             (= 0 (remainder x 5)))))
                 integers))

(define q (weighted-pairs integers-no-remainder-2-3-5 integers-no-remainder-2-3-5 add-pairs-weight2))
(show-stream q 0 3)
