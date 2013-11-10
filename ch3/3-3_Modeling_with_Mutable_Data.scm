;;3.3 可変データでのモデル化
;;2章のデータ抽象
;;データ構造はデータオブジェクトを作り出す構成子と、合成データオブジェクトの部分にアクセスする選択子を使って規定する。
;データオブジェクトを修正する変更し(mutator)を含んだデータ抽象を設計する

;;銀行システムのモデル化では、口座の残高を変更する必要がある。
;;銀行口座を表現するデータ構造
;; (set-balance! <account> <new-value>)
;;指定された口座の残高を、支持された新しい金額に変更する
;;変更子が定義されているデータオブジェクトを可変データオブジェクト（mutable data object）という。

;;3.3.1 可変リスト構造
;; set-car! set-cdr!
;;破壊的操作なのでビックリマークつける

(define x '((a b) c d))
(define y '(e f))
x
y
(set-car! x y)
;; xのcarをyで置き換える
;; set-cdr!はset-car!とだいたい同じ。cdrを入れ替える

;; どこからも参照されないゴミができる。ゴミ集めをする必要がありますね。
;; ただし、後々実装する処理系には組み込まない。

;;問題3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
(define w (append! x y))
w
(cdr x)

;;問題3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
x)

(define z (make-cycle (list 'a 'b 'c)))
z
;(last-pair z)

;;問題3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
(loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
v
w
;;リストの順番をひっくりかえす

;;共有と同一
;; 3.1.3では、「同じ」と「変化」の理論的論点を話した。
;;この論点は、実際には異なるデータオブジェクト間で個々の対が共有されたときに現れる。

(define x (list 'a 'b))
(define z1 (cons x x))
x
z1
(define z2 (cons (list 'a 'b) (list 'a 'b)))
;;一般にリストをcons, car およびcdrだけを使って作用させると共有していても検出できない
;;リスト構造に変更を認めると、共有が重要となる

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
x)
z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))

;; 代入のおかげでポインタがぐちゃぐちゃしちゃってもうやだね
;; という話

;;問題3.15
;;z1とz2へのset-to-wow!の効果を説明する箱とポインタ図を書け
;;問題3.16
;;リスト構造中の対の個数を数える手続き
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(count-pairs 'a)
(count-pairs (cons 1 2))
(count-pairs '((a b)c d))

(count-pairs '(a b c)) ;;3
(count-pairs '((b a) a)) ;;4
(define x '(a b c))
(count-pairs (cons x x)) ;;7

(define infinity 
  (let ((x '(a b c)))
    (set-cdr! (last-pair x) x)
    x))
(count-pairs infinity) ;;帰ってこない
(count-pairs '(((c) c) (c) c)) ;;7

;;ポインタが指し示す場所が重複している場合、count-pairも重複して呼び出される

;;3.17
(define (make-count-pairs walks)
  (define (count-pairs x)
    (cond ((not (pair? x)) 0)
	  ((memq x walks) 0)
	  (else
	   (set! walks (cons x walks))
	   (+ (count-pairs (car x))
	      (count-pairs (cdr x))
	      1))))
count-pairs)

;; memqについて
;; (memq x y) yにxが含まれていれば真、ないなら偽を返す
;; eq?で比べているので、参照先が違ってると同じでない

(define CP (make-count-pairs '()))
(define x (cons 'a (cons 'b (cons 'c '()))))
(CP x)
(display x)

(define x (cons 'd (cons 'a '())))
(set-car! x (cons 'b (cdr x)))
(CP x)
;gosh> 3
(display x)
;gosh> ((b a) a)#<undef>

(define x (cons 'a (cons 'b (cons 'c '()))))
(set-car! (cdr x) (cdr (cdr x)))
(set-car! x (cdr x))
(CP x)
;gosh> 3
(display x)
;gosh> (((c) c) (c) c)#<undef>

;;問題3.18
(define (circulate? items)
  (define walks '())
  (define (has-circulate? x)
    (if (memq x walks)
	#t
	(begin (set! walks (cons x walks))
	       #f)))
  (define (circulate?-iter i)
    (if (not (pair? i))
	#f
	(if (has-circulate? (car i))
	    #t
	    (circulate?-iter (cdr i)))))
(circulate?-iter items))

(define z (make-cycle (list 'a 'b 'c)))
(circulate? (list 'a 'b 'c))
(circulate? z)
(circulate? '(a b c)) ;;3
(circulate? '((b a) a)) ;;4
(define x '(a b c))
(circulate? (cons x x)) ;;7

(define infinity 
  (let ((x '(a b c)))
    (set-cdr! (last-pair x) x)
    x))
(circulate? infinity) ;;帰ってこない
(circulate? '(((c) c) (c) c)) ;;7

;;問題3.19
(define (cycle? items)
  (define (terminate? x)
    (or (null? x)
	(null? (cdr x))))
  (define (contains-loop? trace1 trace2)
    (cond ((eq? trace1 trace2) #t)
	  ((terminate? trace2) #f)
	  (else
	   (contains-loop? (cdr trace1) (cddr trace2)))))
  (if (terminate? items)
      #f
      (contains-loop? (cdr items) (cddr items))))

(cycle? '(a b c)) ;;#f
(define z (make-cycle (list 'a 'b 'c)))
(cycle? z)

;; trace1は1個づつ進む。trace2は2個づつ進む。
;; もしループしていたら、いつのまにかtrace2がtrace1の後ろにいっていつか追いつく。
;; 追いつくことがあったらそれはループしている
;; MSの入社試験？か何かにでてるらしい有名な問題

