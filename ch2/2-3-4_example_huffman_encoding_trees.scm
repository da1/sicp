; 2.3.4 ハフマン符号木

; 本節では集合と木を操作する

; 一般に，通信文中の記号の相対頻度を符号化に利用した可変長語頭符号をつかうとかなり節約できる

; Huffman木の生成
; 頻度が最小の葉を合体させ続けることで木をつくる
; (a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)
; (a 8) (b 3) (c d 2) (e 1) (f 1) (g 1) (h 1)
; (a 8) (b 3) (c d 2) (e f 2) (g 1) (h 1)
; (a 8) (b 3) (c d 2) (e f 2) (g h 2)
; (a 8) (b 3) (c d 2) (e f g h 4)
; (a 8) (b c d 5) (e f g h 4)
; (a 8) (b c d e f g h 9)
; (a b c d e f g h 17)
;

; Huffman木の表現

; 葉の節は記号leaf, その葉の記号と重みからなるリストで表現する
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; 木は左右の枝と記号の集合と重みからなるリスト
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; 復号化手続き
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; 重み付き要素の集合
; Sets of weighted elements
; われわれの木の表現で，葉でない節は単純なリストで表現する記号の集合を持っている
; 最小の項を探す必要があるので順序付けられた表現ができると便利

; 葉と木の集合を重みの大きくなる順に並べたリストの要素として表現する
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; ((a 4) (b 2) (c 1) (d 1))のような記号と頻度のリストをとり，
; Huffmanアルゴリズムによりすぐに合体される葉の最初の順序付けられた集合を構成する
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) ;symbol
                             (cadr pair)) ;frequency
                  (make-leaf-set (cdr pairs))))))

