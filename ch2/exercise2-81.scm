;; 問題2.81
;; Louis Reasoner は，apply-genericは引数がすでに同じ型を持っていても，互いの型への強制型変換をするべきだと考えた．
;; 彼が考えるには，強制型変換の表に，各型の引数を自分の型へ強制変換させる手続きを置く必要がある

(load "./ch2/2-5-2_combining_data_of_different_types.scm")
(load "./ch2/exercise2-78.scm")

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(exp (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 2 2))

;; a. Louisの強制型変換手続きが設定されると，apply-generic が型scheme-numberの2つの引数や型complexの2つの引数で，これらの型で表に見つからない手続きに対して呼び出されると何が起こるか

(exp 2 2)

;;無限ループで帰ってこない
;;

;; b. 同じ型の引数の強制型変換について何かすべきというLouisの主張は正しいか
;; それともこのままapply-generic は正しく働くか

;;なんかよくないよね
;;なにかするべきだけど強制型変換はないね

;; c. 2つの引数が同じ型を持っていれば，強制型変換を試みないようにapply-generic を修正せよ
;;同じtypeのときはerror
