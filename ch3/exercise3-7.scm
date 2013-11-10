;問題3.7
;問題3.3で考えてたパスワード付きのmake-account
;共同口座を作る機能make-joint
;引数 パスワードで保護された口座 パスワード 新しいパスワード

(define (make-joint account account-password new-password)
  (define (dispatch password m)
    (if (eq? password new-password)
      (account account-password m)
      (error "Incorrect password account-password")))
  dispatch)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (define (authenticate p m)
    (if (eq? password p)
      (dispatch m)
      (error "Incorrect Password")))
  authenticate)

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)
;60
((peter-acc 'some-other-password 'diposit) 50)
;Incorrect password

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'open-sesame 'withdraw) 60)
((paul-acc 'rosebud 'withdraw) 50)
((peter-acc 'rosebud 'withdraw) 10)
;make-joinしたときにパスワードが違ってると動かない
;パスワード変えられない
