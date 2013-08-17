;; 問題2.35
; 2.2.2のcount-leavesをアキュムレーションとして再定義せよ

(load "./ch2/2-2-3_Sequences_as_Conventional_Interfaces.scm")

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
(count-leaves (list x x))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                       (count-leaves x)
                       1)) t)))
