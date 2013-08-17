;; 問題2.29
;; 二進モービル
;; 二進モービルは，左の枝と右の枝でできている
;; それぞれの枝は長さのある棒で，そこから錘か別の二進モービルがぶら下がっている

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

; 一つの枝はlengthとstructureで構成する
; structureは数か別のモービルである．

;; 参照
;; http://d.hatena.ne.jp/awacio/20100509/1273412540

; a. これに対応する選択子left-branchとright-branchとbranch-lengthとbranch-structureを書け

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;枝の重さを計算
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (number? structure)
      structure
      (mobile-weight structure))))

;１つのモービルの重さを計算
(define (mobile-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (branch-weight left)
       (branch-weight right))))

; b. モービルの全重量を返すtotal-weight
(define (total-weight mobile)
  (mobile-weight mobile))

(define left-1 (make-branch 2 5))
(define right-1 (make-branch 3 2))
(define mobile-1 (make-mobile left-1 right-1))
(define left-2 (make-branch 8 mobile-1))
(define right-2 (make-branch 4 7))
(define mobile-2 (make-mobile left-2 right-2))

(total-weight mobile-1)
(total-weight mobile-2)

; c. モービルは最上段左の枝による回転力と，最上段右の枝による回転力が等しく，しかも枝にぶら下がっている部分モービルのそれぞれが釣り合っている時，釣り合っているという．
; 二進モービルが釣り合っているかどうかをテストする述語を設計せよ

;枝の回転力を求める手続きを定義。
(define (moment branch)
  (let ((len (branch-length branch))
        (structure (branch-structure branch)))
    (cond ((number? structure)
           (* len structure))
          ((balanced? structure)
           (* len (mobile-weight structure)))
          (else
            #f))))

;釣り合い状況を確認
(define (balanced? mobile)
  ;左の回転力と右の回転力を求め、それらが等しく、かつ
  (let ((left (moment (left-branch mobile)))
        (right (moment (right-branch mobile))))
    (and (eq? left right)
         (not (eq? left #f)))))

(balanced? mobile-1)

(define left-3 (make-branch 3 4))
(define right-3 (make-branch 2 6))
(define mobile-3 (make-mobile left-3 right-3))
(define left-4 (make-branch 10 1))
(define right-4 (make-branch 1 mobile-3))
(define mobile-4 (make-mobile left-4 right-4))

(balanced? mobile-3)
(balanced? mobile-4)

;; d. 構成子が
(define (make-mobile left right)
  (cons left right))

(define (make-branch len structure)
  (cons len structure))

;; と変更したときに，新しい表現に対応するにはプログラムをどのくらい変更する必要があるか

; right-branchとbranch-structure をかえる．cadrをcdrにする
