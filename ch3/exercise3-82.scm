;; 問題3.82 ;;***要再チェック
(define (estimate-integral p x1 x2 y1 y2)
  (define monte-carlo-stream
    (stream-map (lambda (m)
                  (* (- x2 x1) (- y2 y1) m))
                (monte-carlo random-in-range-experiment-stream 0.0 0.0)))
  (define random-in-range-experiment-stream
    (stream-map p random-in-range-x-stream random-in-range-y-stream))
  (define random-in-range-x-stream
    (stream-map (lambda (x) (random-in-range x1 x2)) ones))
  (define random-in-range-y-stream
    (stream-map (lambda (x) (random-in-range y1 y2)) ones))
  monte-carlo-stream)

;;;; よりスマートな estimate-integral
(define (estimate-integral p x1 x2 y1 y2)
  (stream-map (lambda (m) (* (- x2 x1) (- y2 y1) m))
              (monte-carlo (stream-map p
                                       (stream-map
                                         (lambda (x) (random-in-range x1 x2)) ones)
                                       (stream-map
                                         (lambda (x) (random-in-range y1 y2)) ones))
                           0.0 0.0)))

;; 面積と面積から算出した円周率piを表示する手続き
(define (pi-from-monte-carlo-simulation circle-area radius)
  (display circle-area)
  (newline)
  (/ circle-area radius))

; 中心(5, 5) 半径5 の円の場合
;テスト手続き
(define (p-test x y)
  (<= (+ (square (- x 5)) (square (- y 5))) (square 5)))
(use srfi-27)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))

;;結果
(pi-from-monte-carlo-simulation
 (stream-ref (estimate-integral p-test 0 10 0 10) 100000) (* 5 5))

;gosh> 79.02120978790212
;3.160848391516085

