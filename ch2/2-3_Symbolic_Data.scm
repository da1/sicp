;; 2.3 記号データ

;; これまで扱っていたのは数値で構成されたデータであった．
;; ここからは，データとして任意の記号を扱えるようにする

;2.3.1 クォート
; 記号を操作するためには，データオブジェクトをクォートする能力が必要である．

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))

(cdr '(a b c))

;; 記号を操作するのにつかうもう一つの基本的要素は，eq?である
;; eq?があれば，memqが実装できる

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))
