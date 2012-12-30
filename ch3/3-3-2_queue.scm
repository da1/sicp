;; 変更は単なる代入
(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (else "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else (error "Undefined operation -- CONS" m))))
 dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value))

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
z)

;; 問題3.20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

;; 大域環境で変数xに手続きconsが束縛される
;; 環境E1が定義されて、引数x, yに1,2が束縛される
;; 環境E2が定義されて、引数x,yに大域変数xが指す手続きが束縛される
;; 環境E3で(set-car! . .),環境4で(cdr z)

;; 3.3.2 キューの表現
;; (make-queue) 空のキューを返す
;; (empty-queue? <queue>) キューが空であることをテストする
;; (fron-queue <queue>) キューの先頭のオブジェクトを返す。空ならエラー、キューは変化しない
;; (insert-queue! <queue> <item>) キューの末尾に項目を挿入
;; (delete-queue! <queue>)

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue )
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))

;; 問題3.21
(define q1 (make-queue))
(insert-queue! q1 'a)
;; ((a) a)

(insert-queue! q1 'b)
;; ((a b) b)

(delete-queue! q1)
;; ((b) b)

(delete-queue! q1)
;; (() b)

(define (print-queue queue)
  (define (print-head q)
    (if (null? q)
	'()
	(cons (car q) (print-head (cdr q)))))
  (if (empty-queue? queue)
      '()
      (print-head (car queue))))

(print-queue q1)
(print-queue (insert-queue! q1 'a))

;; 問題3.22
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called to an empty queue")
	  (car front-ptr)))
    (define (rear-queue)
      (if (empty-queue?)
	  (error "REAR called to on empty queue")
	  (car rear-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set-front-ptr! new-pair)
	       (set-rear-ptr! new-pair)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set-rear-ptr! new-pair)
	       front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called to an empty queue"))
	    (else
	     (set-front-ptr! (cdr front-ptr)))))
    (define (print-queue)
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    ((eq? m 'front) front-queue)
	    ((eq? m 'rear) rear-queue)
	    ((eq? m 'print-queue) print-queue)
	    (else
	     (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

(define q (make-queue))
((q 'insert-queue!) 'a)
((q 'insert-queue!) 'b)
((q 'insert-queue!) 'c)
((q 'delete-queue!))
((q 'delete-queue!))
((q 'insert-queue!) 'd)
((q 'delete-queue!))
((q 'delete-queue!))
((q 'delete-queue!))

;; 3.23
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-item value)
  (cons value (cons '() '())))
(define (set-next-item! item next)
  (set-cdr! (cdr item) next))
(define (set-prev-item! item prev)
  (set-car! (cdr item) prev))
(define (next-item item)
  (cddr item))
(define (prev-item item)
  (cadr item))
(define (value-of-item item)
  (car item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (list '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT callled with an empty queue" queue)
      ((value-of-item (front-ptr queue)))))
(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty queue" queue)
      ((value-of-item (rear-ptr queue)))))

(define (front-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-item)
	   (set-rear-ptr! queue new-item)
	   queue)
	  (else
	   (set-next-item! new-item (front-ptr queue))
	   (set-front-ptr! queue new-item)
	   queue))))

(define (rear-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-item)
	   (set-rear-ptr! queue new-item)
	   queue)
	  (else
	   (set-prev-item! new-item (rear-ptr queue))
	   (set-next-item! (rear-ptr queue) new-item)
	   (set-rear-ptr! queue new-item)
	   queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (next-item (front-ptr queue)))
	 queue)))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-rear-ptr! queue (prev-item (rear-ptr queue)))
	 queue)))

(define (print-queue queue)
  (define (print-iter q)
    (cond ((eq? q (rear-ptr queue))
	   (display " ")
	   (display (value-of-item q)))
	  (else
	   (begin (display " ")
		  (display (value-of-item q))
		  (print-iter (next-item q))))))
  (if (empty-queue? queue)
      (begin (display "empty")
	     (newline))
      (begin (display "(")
	     (print-iter (front-ptr queue))
	     (display ")"))))

(define q (make-queue))
(front-insert-queue! q 'a)
(print-queue q)
(front-insert-queue! q 'b)
(print-queue q)
(rear-insert-queue! q 'c)
(print-queue q)
(rear-insert-queue! q 'd)
(print-queue q)
(front-delete-queue! q)
(print-queue q)
(front-delete-queue! q)
(print-queue q)
(rear-delete-queue! q)
(print-queue q)
(rear-delete-queue! q)
(print-queue q) ;;?

