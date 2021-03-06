;; 問題3.73
(load "./ch3/3-5-3_Exploiting_the_Stream_Paradigm.scm")

(define (RC R C dt)
  (define (rc i-st v-zero)
    (add-streams
      (scale-stream i-st R)
      (integral (scale-stream i-st (/ 1 C))
                v-zero
                dt)))
  rc)

(define RC1 (RC 5 1 0.5))
(show-stream (RC1 ones 0) 0 10)

