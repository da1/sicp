;; デッドロック
;; 問題3.48
;; 口座に番号付して，若い番号の口座から取得しようとする．
;; デッドロック回避法が交換問題でデッドロックを回避する理由を説明せよ
;; a1,a2のロックは必ずa1からロックを取りに行くようになるため，
;; a2をとってa1待ちの状態が発生しない

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request -- make-account" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (> (account1 'id) (account2 'id))
      ((serializer1 (serializer2 exchange))
       account1
       account2)
      ((serializer2 (serializer1 exchange))
       account2
       account1))))

