; 2.4.3
; メッセージパッシング
; データ手動プログラミングの基本の考えは，演算対型の表を使ってプログラムの汎用演算を操作することであった． 
; 演算対型の表を行方向に分割して，拡販用演算手続きは表の行を代表している

; もう一つの戦略として，表を列方向に分割する
; 手続名で振り分ける賢明なデータオブジェクトで仕事をする

(load "./utils.scm")
(load "./ch2/2-4_Multiple_Representations_for_Abstract_Data.scm")

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
dispatch)

(define (apply-generic op arg) (arg op))

