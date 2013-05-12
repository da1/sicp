;; 各自行書の従業員レコードが、従業員の名前でキーをつけたレコードの集合からなる一つのファイルでできている。
;; セットの構造は、部門ごとに異なる。
;; 各従業員のレコード自体が事務所ごとに異なる構造の週烏合でaddress, salaryという識別子でキーをつけた情報を含んでいる。

;; 参考URL
;; http://d.hatena.ne.jp/awacio/20100907/1283868117
;; example data
(define xxx-data '(("name1" "address1" 210000)
		   ("name2" "address2" 220000)
		   ("name3" "address3" 230000)))

;; xxx-dataのpackage
(define (install-xxx-package)
  ;;Private
  ;;db
  (define (make-db records)
    (attach-tag 'xxx records))
  (define (get-record db name)
    (let loop ((records (cdr db)))
      (cond ((null? records)
	     #f)
	    ((string=? name (get-name (car records)))
	     (car records))
	    (else
	     (loop (cdr records))))))
  ;record
  (define (make-record name address salary)
    (list name address salary))
  (define (get-name record)
    (car record))
  (define (get-salary record)
    (caddr record))
  
  ;;public
  (put 'make-db 'xxx make-db)
  (put 'make-record 'xxx make-record)
  (put 'get-record 'xxx get-record)
  (put 'get-salary 'xxx get-salary)
  'done)
(install-xxx-package)

;; create data-base
(define xxx-db ((get 'make-db 'xxx) xxx-data))
;; record
(define (make-record-xxx name address salary)
  ((get 'make-record 'xxx) name address salary))

;; a
(define (get-record db name)
  ((get 'get-record (type-tag db)) db name))

;; b
(define (get-salary db name)
  (let ((target (get-record db name))
	(proc (get 'get-salary (type-tag db))))
    (if (eq? #f target)
	#f
	(proc target))))

;; c
(define (fine-employee-record db-list name)
  (let loop ((dbs db-list))
    (if (null? dbs)
	#f
	(let ((target (get-record (car dbs) name)))
	  (if (eq? #f target)
	      (loop (cdr dbs))
	      target)))))

;; yyy-data
(define yyy-data '((1 ("yyy-name1" "yyy-address1" 310000))
		   (2 ("yyy-name2" "yyy-address2" 320000))
		   (3 ("yyy-name3" "yyy-address3" 330000))))
;; yyy-dataのpackage
(define (install-yyy-package)
  ;;Private
  ;db
  (define (make-db records)
    (attach-tag 'yyy records))
  (define (get-record db name)
    (let loop ((records (cdr db)))
      (cond ((null? records)
	     #f)
	    ((string=? name (get-name (car records)))
	     (car records))
	    (else
	     (loop (cdr records))))))
  ;record
  (define (make-record name address salary)
    (list -1 (list name address salary)))
  (define (get-contents record) ;xxxと違う
    (cadr record))
  (define (get-name record)
    (car (get-contents record)))
  (define (get-address record)
    (cadr (get-contents record)))
  (define (get-salary record)
    (caddr (get-contents record)))

  ;Public
  (put 'make-db 'yyy make-db)
  (put 'make-record 'yyy make-record)
  (put 'get-record 'yyy get-record)
  (put 'get-salary 'yyy get-salary)
  'done)
(install-yyy-package)

(define yyy-db ((get 'make-db 'yyy) yyy-data))
(define (make-record-yyy name address salary)
  ((get 'make-record 'yyy) name address salary))
