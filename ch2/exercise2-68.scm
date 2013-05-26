(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol char tree)
  (if (leaf? tree)
      (if (equal? (symbol-leaf tree) char)
	  '()
	  (error "encode error: symbol not found" char))
      (if (equal? (symbol-leaf (left-branch tree)) char )
	  '(0)
	  (append '(1) (encode-symbol char (right-branch tree))))))