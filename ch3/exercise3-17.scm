;;3.17
(define (make-count-pairs walks)
  (define (count-pairs x)
    (cond ((not (pair? x)) 0)
          ((memq x walks) 0)
          (else
            (set! walks (cons x walks))
            (+ (count-pairs (car x))
               (count-pairs (cdr x))
               1))))
  count-pairs)

;; memqについて
;; (memq x y) yにxが含まれていれば真、ないなら偽を返す
;; eq?で比べているので、参照先が違ってると同じでない

(define CP (make-count-pairs '()))
(define x (cons 'a (cons 'b (cons 'c '()))))
(CP x)
(display x)

(define x (cons 'd (cons 'a '())))
(set-car! x (cons 'b (cdr x)))
(CP x)
;gosh> 3
(display x)
;gosh> ((b a) a)#<undef>

(define x (cons 'a (cons 'b (cons 'c '()))))
(set-car! (cdr x) (cdr (cdr x)))
(set-car! x (cdr x))
(CP x)
;gosh> 3
(display x)
;gosh> (((c) c) (c) c)#<undef>

