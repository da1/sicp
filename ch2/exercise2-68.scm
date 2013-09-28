; 問題2.68
; 
(load "./ch2/2-3-4_example_huffman_encoding_trees.scm")
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (enc-iter tree)
    (if (leaf? tree)
      '()
      (if (memq symbol (symbols (left-branch tree)))
        (cons 0 (enc-iter (left-branch tree)))
        (cons 1 (enc-iter (right-branch tree))))))
  (if (memq symbol (symbols tree))
    (enc-iter tree)
    (error "encode error: symbol not found" symbol)))

(define sample-tree (make-code-tree (make-leaf 'A 4)
                                    (make-code-tree
                                      (make-leaf 'B 2)
                                      (make-code-tree (make-leaf 'D 1)
                                                      (make-leaf 'C 1)))))
(define message '(A D A B B C A))
(encode message sample-tree)

; (0 1 1 0 0 1 0 1 0 1 1 1 0)
