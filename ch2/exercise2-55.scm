;; 問題2.55
(car ''abracadabra)

; 'はquoteのシンタックスシュガー

(car (quote (quote abracadabra)))

(quote (quote abracadabra))
; は '(quote abracadabra) なので，
; これの carはquote
