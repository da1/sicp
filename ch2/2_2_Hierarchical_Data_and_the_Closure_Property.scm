;; 2.2 階層データ構造と閉包性

;; 対は基本的糊を提供し，それを使って合成データオブジェクトが構成できる
;; 各オブジェクトは箱へのポインタで示す
;; 対は二連の箱で，左部分には対のcarが，右にはcdrがある

;; 2.2.1 ならびの表現
;; 対を使って作ることのできる有用な構造は並びである
;; 並びは入れ子になったcons演算で構成される

;; consの入れ子で作られた対の並びをlistとよぶ

(define one-through-four (list 1 2 3 4))

;; 式 (list 1 2 3 4)
;; リスト ( 1 2 3 4)
;; 2つを混同しないように注意

(car one-through-four)

(cdr one-through-four)

(car (cdr one-through-for))

(cons 10 one-through-four)

;; * リスト演算

;; リストのn番目の要素を返す
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(length odds)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(append squares odds)

;; * リスト写像
;; 極めて有用な演算は，ある変換をリストの各要素に作用させ結果のリストを作るものである
(load "./utils.scm")

(define (scale-list items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)
(scale-list '() 1)

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(map (lambda (x) (* x x))
     (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
