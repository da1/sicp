;; 問題2.15 
;; Eva Lu Atorも代数的に等価だが異なる式を使うと違う区間が計算されることに気づいた
;; 不確かな数を表現する変数が繰り返し現れないように書けるならきちんとした誤差限界を返す
;; 彼女によるとpar1よりpar2のほうが並列抵抗のよいプログラムである
;; それはなぜか

;; par2は誤差のある値どうしの乗算をしないようにしている
;; 誤差が拡大しないようにするのがよい
