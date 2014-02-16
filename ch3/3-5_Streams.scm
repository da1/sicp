;; 3.5 ストリーム
;; 状態をモデル化する，ストリームというデータ構造

;; 遅延評価（delayed evaluation）の技法を取り入れる
;; ストリーム処理は，代入や可変データを使わずに状態を持つシステムをモデル化させる

;; 3.5.1 ストリームは遅延リスト
;; ストリームは並びをリストとして操作するコストを追うことなく，並びの操作を使わせる懸命な方法である

;; 表面的には，ストリームはそれを操作する手続きに，異なる名前が付いているリストである
; (stream-car (cons-stream x y)) = x
; (stream-cdr (cons-stream x y)) = y
(load "./utils.scm")

;; リスト演算のストリーム版
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

;; ストリームの実装のためには，cons-streamでストリームが構成されたときではなく，stream-cdrでアクセスされた時にストリームのcdrが評価されるようにする

;; ストリームの実装にはdelayという特殊形式を使う
;; (delay <exp>)の評価は式を評価せずに，遅延オブジェクト（delayed object）を返す

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; ストリーム実装の働き
;(stream-car
;  (stream-cdr
;    (stream-filter prime?
;                   (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

;; cons-streamで作られたstream-enumerate-intervalの返す値は
;; (cons 10000 (delay (stream-enumerate-interval 10001 1000000)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))

;; delayとforceの実装
;(define (force delayed-object)
;  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? #t)
               result)
        result))))

;; delayの定義
;; (memo-proc (lambda () <exp>))
;(define-macro (delay x) `(memo-proc (lambda () ,x)))
; (define-macro (delay x) `(lambda () ,x))
;(define (force x) (x))

