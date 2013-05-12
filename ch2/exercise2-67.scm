;Define an encoding tree and a sample message:
;Use the decode procedure to decode the message, and give the result.
(define sample-tree (make-code-tree (make-leaf 'A 4)
				    (make-code-tree
				     (make-leaf 'B 2)
				     (make-code-tree (make-leaf 'D 1)
						     (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;A B D C
;A:0, B:10, C:111, D:110
;ADABBCA