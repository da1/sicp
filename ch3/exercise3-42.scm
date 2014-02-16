;; 問題3.42
;; make-accountを変更し，protectedの呼び出しは，dispatch手続きの外でするようにできる．
;; 口座は，払い出し手続きから呼び出されるたびに，同一の直列化された手続きを返す

;; この変更は安全か．何か違いがあるか．
;; 一緒

;; =====
;; 複数の共有資源を使う複雑さ
;; 共有資源が増えると，困難になる
;; seriarizerを外に出した．
;; 直列化を明示的に管理する役割が移った．
