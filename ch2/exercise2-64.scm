;; 問題2.64
; list->treeは順序付けられたリストを釣り合っている二進木に変換する

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

;; a. partial-tree がどう働くか説明を書け
(list->tree '(1 3 5 7 9 11))

; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;5
;  - 1
;    -
;    - 3
;  - 9
;    - 7
;    - 11
;
; memo
;(partial-tree (1 3 5 7 9 11) 6)
;L10 left-size is 2
;L11 left-result is (partial-tree (1 3 5 7 9) 2)
;
;left-result の中身
;(left-tree non-left-elts)
;L14 right-size is 2
;L13 non-left-elts (this-entry  ..)
;right-result is (partial-tree (cdr non-left-elts) 2)
;right-result の中身
;(right-tree remaining-elts)

; 再帰的に 左右の部分木を作って，最終的にconsして返している．

;#?="(stdin)":102:(make-tree this-entry left-tree right-tree)
;#?-    (3 () ())
;#?="(stdin)":102:(make-tree this-entry left-tree right-tree)
;#?-    (1 () (3 () ()))
;#?="(stdin)":102:(make-tree this-entry left-tree right-tree)
;#?-    (7 () ())
;#?="(stdin)":102:(make-tree this-entry left-tree right-tree)
;#?-    (11 () ())
;#?="(stdin)":102:(make-tree this-entry left-tree right-tree)
;#?-    (9 (7 () ()) (11 () ()))
;#?="(stdin)":102:(make-tree this-entry left-tree right-tree)
;#?-    (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;; b. ステップ数
; リストの長さに比例する．O(n)
