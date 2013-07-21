;; 問題2.13

(a x)
(b y)

(define ap (* a (- 1 x)))
(define am (* a (+ 1 x)))

(define bp (* b (- 1 y)))
(define bm (* b (+ 1 y)))

;(* ap bp)
;a+ax
;b+by
;ab+aby+abx+abxy
;ab+aby+abx
;ab(1+x+y)
;
;(* am bm)
;ab-aby-abx+abxy
;ab(1-x-y)

;; Alyssa P. Hackerは完成したシステムを公開した
;; Lem E. Tweakitが逆上した電話をかけた

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Lem はAlyssaのプログラムが2つの計算で違う結果になると文句を言っている
;; 相対許容誤差0の値との計算は，通常版と近似版で値は変わらない
