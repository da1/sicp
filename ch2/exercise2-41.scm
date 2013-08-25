;; 問題2.41
;; 与えられた整数nに対し，nより小さいか等しい相異なる正の整数i,j,kの順序付けられた3つ組で，和が与えられた整数sになるものを全て見つけよ

(load "./ch2/2-2-3_Sequences_as_Conventional_Interfaces.scm")

(define (unique-trio n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-of-trio trio)
  (+ (car trio) (cadr trio) (cadr (cdr trio))))

(define (equal-sum-of-trio n s)
  (filter (lambda (x) (= (sum-of-trio x) s)) (unique-trio n)))

(unique-trio 4)

(equal-sum-of-trio 10 9)
