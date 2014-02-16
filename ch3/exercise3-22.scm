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

