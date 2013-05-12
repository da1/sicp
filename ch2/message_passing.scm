(define (square x) (* x x))

; メッセージパッシング
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude)
	   (sqrt (+ (square x) (square y))))
	  ((eq? op 'angle)
	   (atan y x))
	  (else
	   (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
dispatch)

(define (apply-generic op arg) (arg op))

;q2.75
;make-from-mag-angをメッセージパッシング流儀で実装せよ
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
dispatch)

;sample
(define sample1 (make-from-real-imag 3 4))
(sample1 'real-part)
(sample1 'imag-part)
(sample1 'magnitude)
(sample1 'angle)

;sample
(define sample2 (make-from-mag-ang 1 3.14))
(sample2 'real-part)
(sample2 'imag-part)
(sample2 'magnitude)
(sample2 'angle)

