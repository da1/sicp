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

