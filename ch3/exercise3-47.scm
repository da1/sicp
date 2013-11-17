;; 問題3.47
;; 大きさnのセマフォはmutexの一般化である
;; mutexとtest-and-set!でセマフォを実装せよ

(define (make-semaphore n)
  (let ((counter 0)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (> counter n)
               (begin
                 (mutex 'release)
                 (the-semaphore m)) ; retry
               (begin
                 (set! counter (+ counter 1))
                 (mutex 'release))))
            ((eq? m 'release)
             (mutex 'acquire)
             (set! counter (- counter 1))
             (mutex 'release))
            (else (error "Unknown message -- SEMAPHORE" m))))
    the-semaphore))

(define (make-semaphore n)
  (let ((counter 0)
        (cell (list #f)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (or (> counter n) (test-and-set! cell))
               (the-semaphore m) ; retry
               (begin
                 (set! counter (+ counter 1))
                 (clear! cell))))
            ((eq? m 'release)
             (set! counter (- counter 1))
             (clear! cell))
            (else (error "Unknown message -- SEMAPHORE" m))))
    the-semaphore))

;; 余談
;; shared lock, shared lock同士なら競合しない．exclusive lockとは競合する
;; mvcc DBでよくやるやり方 readとwriteが競合しないようになる
;; シリアライズする必要があるのか？
;; アイソレーションをどのくらい確保するか，という概念
;; isolation level
;;  read uncommited, read commited, repeatable read, serializable

