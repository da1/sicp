;; 問題2.53
; 次の各式を評価した結果，解釈系は何を印字するか

(list 'a 'b 'c)
;(a b c)

(list (list 'george))
;((george))

(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; (y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; #f

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

