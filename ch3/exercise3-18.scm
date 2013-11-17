;;問題3.18
(define (circulate? items)
  (define walks '())
  (define (has-circulate? x)
    (if (memq x walks)
      #t
      (begin (set! walks (cons x walks))
             #f)))
  (define (circulate?-iter i)
    (if (not (pair? i))
      #f
      (if (has-circulate? (car i))
        #t
        (circulate?-iter (cdr i)))))
  (circulate?-iter items))

(define z (make-cycle (list 'a 'b 'c)))
(circulate? (list 'a 'b 'c))
(circulate? z)
(circulate? '(a b c)) ;;3
(circulate? '((b a) a)) ;;4
(define x '(a b c))
(circulate? (cons x x)) ;;7

(define infinity
  (let ((x '(a b c)))
    (set-cdr! (last-pair x) x)
    x))
(circulate? infinity) ;;帰ってこない
(circulate? '(((c) c) (c) c)) ;;7

