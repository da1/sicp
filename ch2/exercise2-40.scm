;; 問題2.40
; 与えられた整数nに対し，1<=j<i<=nの対(i,j)の並びを生成する手続きunique-pairsを定義せよ
; unique-pairsを使って，prime-sum-pairsの定義を簡単にせよ
(load "./ch2/2-2-3_Sequences_as_Conventional_Interfaces.scm")

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(unique-pairs 2)
(unique-pairs 3)
(unique-pairs 4)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
