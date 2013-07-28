;; 問題2.20
;; +や*, listは任意個の引数をとる
;; defineをドット末尾記法で使う

; (define (f x y . z) <body>)
; fは2個以上の引数で呼び出せる
; zはのこりの引数のリストになる

;; 一つか，それをこえる個数の整数を受け取り，先頭と同じ偶奇性をもつ引数のリストを返す手続きを書け

(define (same-parity head . tail)
  (cons head
        (filter (lambda (x) (= (mod head 2) (mod x 2))) tail)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
