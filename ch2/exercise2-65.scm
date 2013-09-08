;; 問題2.65
(load "./ch2/exercise2-63.scm")
(load "./ch2/exercise2-64.scm")

;; 2.3.3より
;; 順序付けられないリストとしての集合
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (list-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (list-intersection-set (cdr set1) set2)))
        (else (list-intersection-set (cdr set1) set2))))

(define (list-union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (list-union-set (cdr set1) set2))
        (else
         (cons (car set1) (list-union-set (cdr set1) set2)))))

(define (union-set set1 set2)
  (let ((list-set1 (tree->list-2 set1))
        (list-set2 (tree->list-2 set2)))
    (list->tree (list-union-set list-set1 list-set2))))

(define (intersection-set set1 set2)
  (let ((list-set1 (tree->list-2 set1))
        (list-set2 (tree->list-2 set2)))
    (list->tree (list-intersection-set list-set1 list-set2))))

(define tree1
  (make-tree 2
             (make-tree 1 '() '())
             (make-tree 3 '() '())))

(define tree2
  (make-tree 3
             (make-tree 2 '() '())
             (make-tree 5 '() '())))

(union-set tree1 tree2)
(intersection-set tree1 tree2)
