(import (scheme base) (scheme small) (chibi assert))

; https://www.nct9.ne.jp/m_hiroi/func/abcscm01.html
;1
(define (cubic x) (* x (* x x)))
(assert (= (cubic 7) 343))

;2
(define (half x) (/ x 2))
(assert (= (half 8) 4))
(assert (= (half 7) 3.5))

;3
(define (medium x y) (/ (+ x y) 2))
(assert (= (medium 7 2) 4.5))

;4
(define (square-med x y)
	(/
		(+
			(* x x)
			(* y y)
		)
		2
	)
)
(assert (= (square-med 2 6) 20))

;5
(define (sum n) (/ (* n (+ n 1)) 2))
(assert (= (sum 7) 28))

; https://www.nct9.ne.jp/m_hiroi/func/abcscm02.html
;1
(define
	xs
	(list
		'a
		(cons 'b '())
		(cons (cons 'c '()) '())
		(cons
			(cons
				(cons 'd '())
				'()
			)
			'()
		)
	)
)
(display xs)
(newline)

(define
	ys
	(list
		(list 'a 'b 'c)
		(list 'd 'e 'f)
		(list 'g 'h 'i)
	)
)
(display ys)
(newline)

(define
	zs
	(list (cons 'a 'b) (cons 'c 'd) (cons 'e 'f))
)
(display zs)
(newline)

; https://www.nct9.ne.jp/m_hiroi/func/abcscm03.html

