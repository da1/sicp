;; 問題3.20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

;; 大域環境で変数xに手続きconsが束縛される
;; 環境E1が定義されて、引数x, yに1,2が束縛される
;; 環境E2が定義されて、引数x,yに大域変数xが指す手続きが束縛される
;; 環境E3で(set-car! . .),環境4で(cdr z)

