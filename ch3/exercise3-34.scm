;; 問題3.34,3.35
;; bをセットしたときに未設定の変数が2つできてしまう

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0 -- SQUARE" (get-value b))
        (set-value! a (sqrt b)))
      (set-value! b (* (get-value a) (get-value a)))))
  (define (process-forget-value)
    ((forget-value! a me)
     (forget-value! b me)))
  (define (me require)
    (cond ((eq? require 'I-have-a-value)
           (process-new-value))
          ((eq? require 'I-lost-my-value)
           (process-forget-value!))
          (error "Unknown request -- SQUARE" require)))
  me)

