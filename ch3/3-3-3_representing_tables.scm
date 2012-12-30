;; 3.3.3 表の表現
;; 可変リスト構造として，表を作る方法を見る

;; それぞれの値がひとつのキーで格納される一次元の表
;; 表をレコードのリストとして実装する．
;; レコードは，carが順次のレコードを指しているついで糊付けされたリストとなる

;; 表から値を取り出す
;; 引数eとしｔキーを取り，対応する値を返す
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))
;; keyとrecordsを渡すとkeyを持つペアを返す
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;; caar カカー

(define true #t)
(define false #f)

;; キーを表に挿入する
;; 表に既にレコードがあるか見る．なければ新しいレコードを作って入れる．
;; ある場合は，値を置き換える
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)

;; 1ステップで終わるから先頭にinsertしましょう

;; 新しいテーブルの作成
(define (make-table)
  (list '*table*))

;; 二次元の表
;; 二次元の表では，2つのキーで指標付けされる．
;; 各キーが，下位の表を識別する一次元の表として構成できる
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;; 局所表の作り方
;; 表を手続きとして表現する
;; 内部の表をt局所状態として持つオブジェクトにする
;; 適切なメッセージを送ると，この表オブジェクトは，内部の表に操作する手続きをくれる．
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;2.4.3節のデータ主導プログラミングで使ったgetとputの演算を，次のように実装できる
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; 3.24
;; キーの等価性のテストにつかうsame-key?手続きを取るものを設計せよ
;; make-table時にsame-key?を受け取って，等価性の判定に使う.
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
       (define (lookup key-1 key-2)
         (let ((subtable (same-key? key-1 (cdr local-table))))
              (if subtable
                  (let ((record (same-key? key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           #f))
                  #f)))
       (define (insert! key-1 key-2 value)
         (let ((subtable (same-key? key-1 (cdr local-table))))
              (if subtable
                  (let ((record (same-key? key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable
                                     (cons (cons key-2 value)
                                           (cdr subtable)))))
                  (set-cdr! local-table
                            (cons (list key-1
                                        (cons key-2 value))
                                  (cdr local-table)))))
         'ok)
       (define (dispatch m)
         (cond ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "Unknown operation -- TABLE" m))))
       dispatch))


(define (search key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	((and (number? key) (< (abs (- key (caar records))) 0.5))
	 (car records))
	(else (search key (cdr records)))))

(define tb3 (make-table search))
(define insert! (tb3 'insert-proc!))
(define lookup (tb3 'lookup-proc))

(insert! 'tokyo 10.5 '2009/2/15)
(insert! 'tokyo 20.1 '2009/4/25)
(insert! 'osaka 12.8 '2009/1/15)
(insert! 'osaka 21.1 '2009/4/10)
(insert! 'osaka 30.2 '2009/7/24)
(lookup 'tokyo 10.5)
(lookup 'osaka 21.0)
(lookup 'osaka 40.0)
(lookup 'tokyo 20.0)
(lookup 'osaka 20.0)

;; 3.25
;; 任意個のキーを持つ表を実装する方法を示せ
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (lookup-iter key-list local-table))
    (define (lookup-iter key-list local-table)
      (if (null? key-list)
	  #f
	  (let ((subtable (assoc (car key-list) (cdr local-table))))
	    (if subtable
		(if (null? (cdr key-list))
		    (cdr subtable)
		    (lookup-iter (cdr key-list) subtable))
		#f))))
    (define (insert! key-list value)
      (insert-iter! key-list value local-table))
    (define (insert-iter! key-list value local-table)
      (if (null? key-list)
	  #f
	  (let ((subtable (assoc (car key-list) (cdr local-table))))
	    (if subtable
		(if (null? (cdr key-list))
		    (set-cdr! subtable value)
		    (insert-iter! (cdr key-list) value subtable))
		(set-cdr! local-table
			  (cons (insert-iter key-list value)
				(cdr local-table))))))
      'ok)
    (define (insert-iter key-list value)
      (if (null? (cdr key-list))
	  (cons (car key-list) value)
	  (list (car key-list) (insert-iter (cdr key-list) value))))
    (define (print-table)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'print-table) print-table)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define tb (make-table))
(define lookup (tb 'lookup-proc))
(define insert! (tb 'insert-proc!))
(define print-table (tb 'print-table))

(insert! '(japan tokyo 2009/2/13) 20.1)
(insert! '(japan osaka 2009/2/13) 22.0)
(insert! '(usa newyork 2009/2/13) 14.5)
(lookup '(japan tokyo 2009/2/13))
;; gosh> 20.1
(lookup '(japan osaka 2009/2/13))
;; gosh> 22.0
(lookup '(usa newyork 2009/2/13))
;;gosh> 14.5
(lookup '(usa tokyo 2009/2/13))
;;gosh> #f
(print-table)

;; 3.26
;; (key value left right)の形でデータを保持する
(define (make-table)
  (let ((local-table '*table*))
    (define (key-tree tree)
      (car tree))
    (define (value-tree tree)
      (cadr tree))
    (define (left-branch tree)
      (caddr tree))
    (define (right-branch tree)
      (cadddr tree))
    (define (make-tree key value left right)
      (list key value left right))
    (define (set-value-tree! tree value)
      (set-car! (cdr tree) value))
    (define (set-left-branch-tree! tree left)
      (set-car! (cddr tree) left))
    (define (set-right-branch-tree! tree right)
      (set-car! (cdddr tree) right))

    (define (lookup key)
      (define (iter key tree)
	(cond ((null? key) #f)
	      ((= key (key-tree tree)) (value-tree tree))
	      ((< key (key-tree tree))
	       (iter key (left-branch tree)))
	      ((> key (key-tree tree))
	       (iter key (right-branch tree)))))
      (iter key local-table))

    (define (insert! key value)
      (define (make-branch key value)
	(make-tree key value '() '()))
      (define (iter key value tree)
	(cond ((eq? tree '*table*)
	       (set! local-table (make-branch key value)))
	      ((= key (key-tree tree))
	       (set-value-tree! tree value))
	      ((< key (key-tree tree))
	       (if (null? (left-branch tree))
		   (set-left-branch-tree! tree (make-branch key value))
		   (iter key value (left-branch tree))))
	      ((> key (key-tree tree))
	       (if (null? (right-branch tree))
		   (set-right-branch-tree! tree (make-branch key value))
		   (iter key value (right-branch tree))))))
      (iter key value local-table)
      'ok)

    (define (print-table)
      (display local-table)
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'print-table) print-table)
	    ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation TABLE" m))))
    dispatch))

(define tb (make-table))
(define lookup (tb 'lookup-proc))
(define insert! (tb 'insert-proc!))
(define print-table (tb 'print-table))
(insert! '6 'b)
(insert! '4 'd)
(insert! '3 'e)
(insert! '1 'a)
(insert! '5 'c)
(lookup '6)
(lookup '4)
(lookup '3)
(lookup '1)
;; gosh> a
(lookup '5)
;; gosh> c
(print-table)

;; 3.27
;; memoization
;; フィボナッチ数を計算する手続き
(define memo-fib
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else (+ (memo-fib (- n 1))
			    (memo-fib (- n 2))))))))

;; memoizationの手続き
;; ここで使うlookupとinsert!は最初に定義したバージョン
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))

(memo-fib 3)
(define x 10000)
(memo-fib x)
(memo-fib x)

;; これの環境を書け
;; グローバル環境 に手続きmemo-fibとmemoize
;; (memo-fib 3)を実行するときに環境ができる．
;; (let ..)はlambdaを定義して，それを評価している.
;; f lambdaの環境
;; lambdaの環境 ここでmake-tableされる
;; そこからlambda(x)が生えてる
;; さらに生えるlambda xに3が束縛されてる
;; そこから生える環境 let
;; tableのある環境からnが2のとき，1のときの環境が生える

;; 最終的にtableのある環境にいろいろ値が入る

;; たいへんめんどくさいですね

;; 途中経過はメモ化されない

