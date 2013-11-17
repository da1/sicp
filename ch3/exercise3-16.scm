;;問題3.16
;;リスト構造中の対の個数を数える手続き
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(count-pairs 'a)
(count-pairs (cons 1 2))
(count-pairs '((a b)c d))

(count-pairs '(a b c)) ;;3
(count-pairs '((b a) a)) ;;4
(define x '(a b c))
(count-pairs (cons x x)) ;;7

(define infinity
  (let ((x '(a b c)))
    (set-cdr! (last-pair x) x)
    x))
(count-pairs infinity) ;;帰ってこない
(count-pairs '(((c) c) (c) c)) ;;7

;;ポインタが指し示す場所が重複している場合、count-pairも重複して呼び出される

