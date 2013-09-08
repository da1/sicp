;; 問題2.66
; 二進木で構造化されたデータを扱うlookup手続き
(load "./ch2/2-3-3_Example_Representing_Sets.scm")

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (entry set-of-records))
         (entry set-of-records))
        (else
          (if (< given-key (entry set-of-records))
            (lookup given-key (left-branch set-of-records))
            (lookup given-key (right-branch set-of-records)))
          )))

(define tree
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(lookup 7 tree)
(lookup 3 tree)
(lookup 1 tree)
(lookup 5 tree)
(lookup 9 tree)
(lookup 11 tree)
(lookup 10 tree)
