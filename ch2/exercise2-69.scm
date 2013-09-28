; 問題2.69
; 次の手続きは，引数として記号と頻度の対のリストをとり，Huffmanアルゴリズムに従いHuffman符号木を生成する
(load "./ch2/2-3-4_example_huffman_encoding_trees.scm")
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                    (cadr pairs))
                                    (cddr pairs)))))

(define pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(generate-huffman-tree pairs)

