;; 問題2.71

; n=5
; 1,2,4,8,16,32

(load "./ch2/exercise2-69.scm")

(define pairs '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32)))
(generate-huffman-tree pairs)

(define pairs '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512) (k 1024)))
(generate-huffman-tree pairs)

; 最高頻度は1bit
; 最低頻度はnbit
