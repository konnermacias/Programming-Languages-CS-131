;#lang racket
;(require htdp/testing)
;(require racket/trace)
; recursively compare each head elm and continue until
; list is empty
(define (recurse LDA SVM)
	(if (or (null? LDA) (null? SVM))
		empty
		(cons (expr-compare (car LDA) (car SVM)) (recurse (cdr LDA) (cdr SVM)))
	)
)


; simple checks
; 1. Are they equal
; 2. Special #f #t and #t #f cases
; 3. Else (if % LDA SVM) 
(define (check LDA SVM)
	(cond
	    [(equal? LDA SVM) LDA]
	  	[(and (equal? #t LDA) (equal? #f SVM)) '%]
  	    [(and (equal? #f LDA) (equal? #t SVM)) '(not %)]
  	    [else `(if % ,LDA ,SVM)]
  	)
)

; Handles case when both car's are equal
; 
; if any head elm has special type, then send to check
; o/w -> these are not special and we should go thru rest of list
(define (expr-handler LDA SVM)
	(if (or
		(equal? (car LDA) 'quote) (equal? (car SVM) 'quote)
		(equal? (car LDA) 'if) (equal? (car SVM) 'if)
		(equal? (car LDA) 'let) (equal? (car SVM) 'let)
		(equal? (car LDA) 'lambda) (equal? (car SVM) 'lambda)
		)
	  	(check LDA SVM)
	  	(recurse LDA SVM)
	)
)

(define (glue LDA SVM)
	(string->symbol
		(string-append
			(symbol->string LDA)
			"!"
			(symbol->string SVM)
		)
	)
)

; recurse until we find a matching elm
; incremenet index-count each time
(define (get-index L elm index)
	(if (equal? (car L) elm)
		index 
		(get-index (cdr L) elm (+ index 1))
	)
)


; helper function to check-let
; returns megadata including a list of:
; a!b (Bind), a's (LDA2), and b's (SVM2).
(define (let-recurse LDA1 SVM1 Bind LDA2 SVM2)
	(cond 
		[(or (null? LDA1) (null? SVM1)) `(,Bind ,LDA2 ,SVM2)]
		; check if first elms aren't equal a from ((a c)) b from ((b d))
		; -> if not -> glue a!b
		[(not (equal? (caar LDA1) (caar SVM1)))
		 (let-recurse
		 	(cdr LDA1) 
		 	(cdr SVM1) 
		 	(cons (glue (caar LDA1) (caar SVM1)) Bind) 
		 	(cons (caar LDA1) LDA2) 
		 	(cons (caar SVM1) SVM2)
		 )]
		 ; recurse
		[else (let-recurse (cdr LDA1) (cdr SVM1) Bind LDA2 SVM2)]
	)
)

; substitutes all cases within x which should have glued in place
(define (subs x y elm glued) 
	(cond
		[(null? x) empty]
		; recurse if fst elm of x is a list
		[(list? (car x))
			(cons
				; attach
				(subs (car x) y elm glued)
				(subs (cdr x) y elm glued)
			)
		]
		; are we looking at a symbol which needs to be sub?
		[(member (car x) elm) 
			; substitute similar to subs-init
			(cons 
				(let 
					((index (get-index elm (car x) 0)))
					(list-ref glued index)
				)
				(subs (cdr x) y elm glued)
			)]
		; recurse otherwise
		[else (cons (car x) (subs (cdr x) y elm glued))]
	)
)

; substitutes only fst elm of each pair in pairs
; pairs ex: ((a 1) (f a))
(define (subs-init pairs elm glued)
	(cond 
		[(null? pairs) empty]
		[(member (caar pairs) elm)
		; substitute first elm and continue with others
		 (cons
		 	(let 
		 		; capture index of elm
		 		((index (get-index elm (caar pairs) 0)))
		 		; make (b!c 2)
		 		(cons (list-ref glued index) (cdar pairs))
		 	)
		 	; continue with others (f a) etc. 
		 	(subs-init (cdr pairs) elm glued)
		 )]
		[else (cons (car pairs) (subs-init (cdr pairs) elm glued))]
	)
)

(define (check-let LDA SVM)
	; if second elm's lengths are equal then we can progress
	(if (= (length (cadr LDA)) (length (cadr SVM)))
		; MegaData will contain 
		(let ((MegaData (let-recurse (cadr LDA) (cadr SVM) empty empty empty)))
			(cons 'let
				(expr-compare
					(cons
						; (car MD) -> Bind Inst.
						; (cadr MD) -> LDA Inst.
						; (caddr MD) -> SVM Inst.
						(subs-init (cadr LDA) (cadr MegaData) (car MegaData))
						(subs (cddr LDA) (cddr SVM) (cadr MegaData) (car MegaData))
					)
					(cons
						(subs-init (cadr SVM) (caddr MegaData) (car MegaData))
						(subs (cddr SVM) (cddr LDA) (caddr MegaData) (car MegaData))
					)
				)
			)
		)
		`(if % ,LDA ,SVM)
	)
)

(define (lambda-recurse LDA1 SVM1 Bind LDA2 SVM2)
	(cond
		[(and (null? LDA1) (null? SVM1)) `(,Bind ,LDA2 ,SVM2)]
		[(not (equal? (car LDA1) (car SVM1)))
		(lambda-recurse
			(cdr LDA1) 
			(cdr SVM1) 
			(cons 
				(glue (car LDA1) (car SVM1))
				Bind
			) 
			(cons 
				(car LDA1) LDA2) 
				(cons (car SVM1) SVM2)
			)]
		[else (lambda-recurse (cdr LDA1) (cdr SVM1) Bind LDA2 SVM2)]
	)
)


(define (check-lambda LDA SVM)
	(if (= (length (cadr LDA)) (length (cadr SVM)))
		(let ((MegaData (lambda-recurse (cadr LDA) (cadr SVM) empty empty empty))) 
		 	(cons 
		 		'lambda
		 		(expr-compare
	 				(subs (cdr LDA) (cdr SVM) (cadr MegaData) (car MegaData)) 
	 				(subs (cdr SVM) (cdr SVM) (caddr MegaData) (car MegaData))
 				)
 			)
		)
		`(if % ,LDA ,SVM)
	)
)

;  Check whether head elm is special
;  -> Check handle quote, let, and lambda
;  o/w -> recurse through lists
(define (spec-expr-handler LDA SVM)
	(cond
		[(equal? (car LDA) 'quote) (check LDA SVM)]
		[(equal? (car LDA) 'let) (check-let LDA SVM)]
		[(equal? (car LDA) 'lambda) (check-lambda LDA SVM)]
		[else (recurse LDA SVM)]
	)
)


; 1. Verify we are looking at lists and they have equal length
;    O/w -> perform checks of constants
; 2. If car vals are the same, then send to spec-expr-handler
;    O/w -> expr-handler which uses or to check if one of heads is special
;
(define (expr-compare x y)
	(cond
		[(and (list? x) (list? y)) 
			(if (= (length x) (length y))
				(if (equal? (car x) (car y))
	  				(spec-expr-handler x y)
	  				(expr-handler x y)
				)
				(check x y)
			)]
		[else (check x y)]
	)
)


(define (test-expr-compare x y)
	(let ((expr (expr-compare x y)))
    	(and
      		(equal? (eval x) (eval (cons 'let (cons '((% #t)) (list expr)))))
      		(equal? (eval y) (eval (cons 'let (cons '((% #f)) (list expr)))))
      	)
	)
)

(define test-expr-x 
 '(let ((a 1) (b 2) (c 3))
    '(
      '(1 2 3)
      #f ;2
      #f ;3
      #t ;4
      #t ;5
      '(quote (a b c)) ;6
      '(quote (a b c)) ;7
      '(cons 'z '(y x w)) ;8
      '(cons 'z '(y x w)) ;9
      '(let ((a 'b) (b 'c) (c 'e)) '(a b c)) ;10
      '(let ((a 'b) (b 'c) (c 'e)) '(a b c)) ;11
      ''(let ((b d)) b) ;12
      '((lambda (a b) (* a b)) 2 4) ;13
      '((lambda (a b) (- a b)) 2 4) ;14
      '(if a b c) ;15
      '(if c #t b) ;16
      '(list a b) ;17
      '(cons 'a '(b c)) ;18
      '(* 1 2 (/ 3 4)) ;19
    )
  )
)

(define test-expr-y
 '(let ((a 1) (b 2) (c 3))
    '(
    	'(1 3 2) ;1
        #f ;2
        #f ;3
        #t ;4
        #t ;5
        '(quote (a b d)) ;6
        '(quote (a c b)) ;7
        '(cons 'a '(y x w)) ;8
        '(cons 'z '(y z w)) ;9
        '(let ((a 'x) (b 'y) (c 'z)) '(a b c)) ;10
        '(let ((a 'b) (p 'c) (c 'e)) '(a p c)) ;11
        ''(let ((p q)) p) ;12
        '((lambda (a c) (* a c)) 2 4) ;13
        '((lambda (a b) (- b a)) 2 4) ;14
        '(if a c b) ;15
        '(if d #t b) ;16
    	'(list b a) ;17
    	'(cons 'b '(a c)) ;18
    	'(* 3 4 (/ 1 2)) ;19
    )
  )
)