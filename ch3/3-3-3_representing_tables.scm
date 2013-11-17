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

