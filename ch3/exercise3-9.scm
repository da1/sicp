;問題3.9
;1.2.1節の階乗を計算する手続き
;再帰版
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 6)

;反復版
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
(factorial 6)
;これらを評価するときの環境構造を示せ
