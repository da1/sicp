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

