;; 問題2.27
; 問題2.18の手続きreverseを修正して引数としてリストをとり，要素を逆順にし，更に部分木も奥まで逆順にする手続きdeep-reverseを作れ

(define x (list (list 1 2) (list 3 4)))

x
; ((1 2) (3 4))

(reverse x)
; ((3 4) (1 2))

(define (reverse a)
  (define (reverse-iter a b)
    (if (= (length a) 1)
      (cons (car a) b)
      (reverse-iter (cdr a) (cons (car a) b))))
  (reverse-iter a ()))

(define (deep-reverse x)
  (define (rev a b)
    (if (pair? (car a))
      (cons (deep-reverse (car a)) b)
      (cons (car a) b)))
  (define (iter a b)
    (if (= (length a) 1)
      (rev a b)
      (iter (cdr a) (rev a b))))
  (iter x ()))

(deep-reverse x)
; ((4 3) (2 1))
