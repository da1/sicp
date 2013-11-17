;図3.10
;make-withdraw手続きで、局所変数balanceをletを使って明示的に作った例
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

;(let ((<var> <exp>)) <body>)
;((lambda (var) (body)) <exp>) のシンタックスシュガー

(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
;示す環境構造の図をかけ

