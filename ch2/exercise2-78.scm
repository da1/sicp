;; 問題2.78
; 2.4.2節の type-tag，contentsおよびattach-tagを修正して，この汎用システムがschemeの型システムの利点が使えるようにせよ

(load "./ch2/2-5_system_with_generic_operations.scm")

;; http://d.hatena.ne.Jp/kiririmode/20080119/p2
;; http://d.hatena.ne.jp/awacio/20101009/1286585236
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define a1 (attach-tag 'scheme-number 1))
(type-tag a1)
(contents a1)

(add 1 2)
