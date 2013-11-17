;問題3.11
;(define acc (make-account 50))
;環境E1の中に、引数balanceに50が束縛される
;make-accountが評価されて、E1の中に、手続きwithdraw、deposit、dispatchが定義される。
;変数accに、手続きdispatchが束縛される

;((acc 'deposit) 40)
;; 環境E2が作られる。dispathの引数mに、depositが束縛される
;; depositが評価されて、手続きdepositが呼び出される。
;; 環境E3が作られて、amountに40が束縛される
;; 評価されて、balanceの値が書き換わる

;;((acc 'withdraw) 60)
;;

;;(define acc2 (make-account 100))
;;make-accountが指す手続きが共通
;;balanceの値や、その下の環境などは別
