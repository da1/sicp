;; 問題 1.26
;; Louis Reasoner の書いたfast-primeが遅い
;; Eva Lu Atorに助けを求めた．
;; Louisのコードはsquareを呼ぶ代わりに*を使っていた．

;; Louis 「違いがわからん」
;; Eva 「わかる」

;; Θ(logn)が，Θ(n)になってしまった．
;; 理由を説明せよ

;; squareを使ってないので，(expmod ...)の式が2階呼ばれる
