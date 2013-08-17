;; 2.2.2 階層構造

(cons (list 1 2) (list 3 4))

;; 最初の項がリストである3つの項のリストだと見える．
;; その要素自身が並びであるような並びのもう一つの味方が木（tree）である．
;; 並びの要素が木の枝，それ自身が並びのようそは部分木である．

(define x (cons (list 1 2) (list 3 4)))

(length x)

(count-leaves x)

(list x x)

(length (list x x))

(count-leaves (list x x))

;; 木xのcount-leavesはxのcarのcount-leaves足すxのcdrのcount-leavesである

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; * 木の写像
; 木に対するscale-list
(load "./utils.scm")

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

;; scale-treeを実装するもう一つの方法
;; 木を部分木の並びとみてmapを使うことだ．

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))
