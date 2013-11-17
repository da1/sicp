;; 問題3.56
;; Sは１から始まる
;; (scale-stream S 2), (scale-stream S 3), (scale-stream S 5)の要素はSの要素

;; 2^n・3^m・5^rで表せる整数のことをハミング数と呼ぶらしいです。
;; ストリームをマージする手続き
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1)
                             (stream-cdr s2)))))))))
;; ハミング数のストリーム
(define S
  (cons-stream
    1
    (merge
      (scale-stream S 2)
      (merge
        (scale-stream S 3)
        (scale-stream S 5)))))

(show-stream S 0 10)
;; 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, 40, 45, 48, 50, 54, 60, ... (sequence A051037 in OEIS).
;; http://en.wikipedia.org/wiki/Hamming_numbers

