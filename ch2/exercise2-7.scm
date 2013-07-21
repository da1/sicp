;; 問題2.7
;; 区間の抽象化の実装を規定しなかったのでAlyssaのプログラムは不完全である
;; 区間構成子は
;; (define (make-interval a b) (cons a b))
;; である．実装を完成させるため，選択子upper-boundとlower-boundを定義せよ

(load "./ch2/2_Building_Abstractions_with_Data.scm")
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define r1 (make-interval 6.12 7.48))
(define r2 (make-interval 4.465 4.935))

(define rp (div-interval (make-interval 1 1)
                         (add-interval
                           (div-interval (make-interval 1 1) r1)
                           (div-interval (make-interval 1 1) r2))))

(lower-bound rp)
(upper-bound rp)
