;; 2.3.3 例: 集合の表現
; 集合を扱う演算を定義することで，集合を定義する

;; 順序付けられないリストとしての集合
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define set1 '(1 2 3 4 5))
(define set2 '(3 4 5 6 7))

(element-of-set? 1 set1)
(element-of-set? 0 set1)

(adjoin-set 10 set1)

(intersection-set set1 set2)

;; 集合演算の効率について考える
;; element-of-set?の効率は，全体の効率に関わる
; element-of-set? はO(n)
; adjoin-set もO(n)
; intersection-set はO(n^2)

;; * 順序付けられたリストとしての集合

; 順序付けできるとelement-of-set?が高速化できる
; ここでは，集合の要素が数値であると前提を置く

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; 平均でステップ数が半分になった

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

; set1 と set2のサイズの和

(intersection-set '(1 2 3) '(3 4 5))

;* 二進木としての集合
; 二分木にすれば，木の探索に必要なステップ数はO(logn)になる

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; これを使ったelement-of-set?

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define tree
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(element-of-set? 1 tree)
(element-of-set? 10 tree)
(element-of-set? 10 (make-tree 1 '() '()))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(adjoin-set 8 tree)

;; このadjoin-setだと，要素の追加順によっては木がバランスしないことがある．
;; そうなってしまうと単純なリストとの優位性がなくなってしまう．

;; * 集合と情報検索
; 集合にこだわるのは，これらの技法は，情報検索に関する応用に繰り返し現れるからである．

; 大量の個々レコードを持つデータベースを考えよう
; レコードを一意に識別するキー

; データベースをレコードの集合として表現しよう
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;; 木構造に基づいた実装をされたシステムを設計する際に，データ抽象は大いに役に立つ
;最初は単純直截な実装にしておき青tでデータ表現をより精巧なものに修正できる
; データベースが抽象的選択子と構成子を使ってアクセスされているならば，表現の変更は他の部分の変更を必要としない
