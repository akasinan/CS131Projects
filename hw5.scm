;Return #t if obj is an empty listdiff, #f otherwise.
(define (null-ld? obj)
	(if (or (null? obj) (not (pair? obj))) #f
		(eq? (car obj) (cdr obj))
	)
)
;Return #t if obj is a listdiff, #f otherwise.
(define (listdiff? obj)
	(if (null-ld? obj) #t
		(if (or (null? obj) (not (pair? obj)) (not (pair? (car obj)))) #f
		(listdiff? (cons (cdr (car obj)) (cdr obj))))
	)
)
;Return a listdiff whose first element is obj and whose remaining elements are 
;listdiff. (Unlike cons, the last argument cannot be an arbitrary object; it must 
;be a listdiff.)
(define (cons-ld obj listdiff)
	(if (listdiff? listdiff)
		(cons (cons obj (car listdiff)) (cdr listdiff)) (error "It is an error")
	)
)
;Return the first element of listdiff. It is an error if listdiff has no elements. 
;("It is an error" means the implementation can do anything it likes when this
;happens, and we won't test this case when grading.)
(define (car-ld listdiff)
	(if (and (listdiff? listdiff) (not (null-ld? listdiff)))
		(car (car listdiff)) (error "It is an error")
	)
)
;Return a listdiff containing all but the first element of listdiff. It is an error
;if listdiff has no elements.
(define (cdr-ld listdiff)
	(if (and (listdiff? listdiff) (not (null-ld? listdiff)))
		(cons (cdr (car listdiff)) (cdr listdiff)) (error "listdiff has no elements")
	)
)
;Return a newly allocated listdiff of its arguments.
(define (listdiff obj . args)
	(cons (cons obj args) '())
)
;Return the length of listdiff.
(define (length-ld listdiff)
	(define (length-ld-tl listdiff accumulator)
		(if (listdiff? listdiff)
			(if (null-ld? listdiff) accumulator (length-ld-tl (cdr-ld listdiff) (+ accumulator 1)))
			(error "It is an error")
		)
	)
	(length-ld-tl listdiff 0)
)
;Return a listdiff consisting of the elements of the first listdiff followed by the 
;elements of the other listdiffs. The resulting listdiff is always newly allocated, 
;except that it shares structure with the last argument. (Unlike append, the last 
;argument cannot be an arbitrary object; it must be a listdiff.)
(define (append-ld listdiff . args)
	(if (null? args) listdiff (apply append-ld (cons (append (take (car listdiff) (length-ld listdiff))
		(car (car args))) (cdr (car args)))
	(cdr args)))
)
;Return listdiff, except with the first k elements omitted. If k is zero, return 
;listdiff. It is an error if k exceeds the length of listdiff.
(define (list-tail-ld listdiff k)
	(if (and (listdiff? listdiff) (> k (length-ld listdiff)))
		(error "k exceeds the length of listdiff")
	(if (and (listdiff? listdiff) (= k 0)) listdiff
		(list-tail-ld (cdr-ld listdiff) (- k 1)))
	)
)
;Return a listdiff that represents the same elements as list.
(define (list->listdiff list)
	(if (list? list)
		(apply listdiff (car list) (cdr list))
		(error "It is an error")
	)
)
;Return a list that represents the same elements as listdiff.
(define (listdiff->list listdiff)
	(if (listdiff? listdiff)
		(take (car listdiff) (length-ld listdiff))
		(error "It is an error")
	)
)
;Return a Scheme expression that, when evaluated, will return a copy of listdiff, 
;that is, a listdiff that has the same top-level data structure as listdiff. Your 
;implementation can assume that the argument listdiff contains only booleans, 
;characters, numbers, and symbols.
(define (expr-returning listdiff)
	(if (listdiff? listdiff)
		`(cons ',(take (car listdiff) (length-ld listdiff)) '())
		(error "It is an error")
	)
)