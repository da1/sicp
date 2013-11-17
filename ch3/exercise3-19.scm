;;問題3.19
(define (cycle? items)
  (define (terminate? x)
    (or (null? x)
        (null? (cdr x))))
  (define (contains-loop? trace1 trace2)
    (cond ((eq? trace1 trace2) #t)
          ((terminate? trace2) #f)
          (else
            (contains-loop? (cdr trace1) (cddr trace2)))))
  (if (terminate? items)
    #f
    (contains-loop? (cdr items) (cddr items))))

(cycle? '(a b c)) ;;#f
(define z (make-cycle (list 'a 'b 'c)))
(cycle? z)

;; trace1は1個づつ進む。trace2は2個づつ進む。
;; もしループしていたら、いつのまにかtrace2がtrace1の後ろにいっていつか追いつく。
;; 追いつくことがあったらそれはループしている
;; MSの入社試験？か何かにでてるらしい有名な問題

