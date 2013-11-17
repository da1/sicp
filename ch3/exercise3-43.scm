;; 問題3.43
;; 口座A,B,Cで残高が10,20,30
;; 口座の残高交換をしながら複数のプロセスが走る
;; プロセスが逐次的に走ったなら，口座残高は，ある順序で10,20,30である．

;; 最初の版の口座交換プログラムだと，この条件が破られる
;; 残高の合計は保持される
A: 10 ->                           -> 20
B: 20 ->         30      -> 20
C: 30 ->         20      ->
P1:   -> dif(10) ->      -> B with -> A dep
P2:              (BCの交換)
;; 各講座の取引を直列しなければ，この条件も破れる
withdrowの手続きで，subとsetの間でamountの値が変われば，合計値も変わる

