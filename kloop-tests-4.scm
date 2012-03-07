(begin-tests "with-clause"

(test-equal 0 (loop with x = 0 do (return x)))

(test-equal '(3 2 1)
       (let ((s '()))
	 (loop with x = 1 for a from x to 3 by 1 do (push a s)) s))
(test-equal '(1 3 6)
       (loop with a = 1 
	     with b = (+ a 2) 
	     with c = (+ b 3)
	     return (list a b c)))
(test-equal '(1 2 3)
       (loop with a = 1 
	     and b = 2 
	     and c = 3
	     return (list a b c)))
(test-equal '(1 7 13)
	    (let ((a 5)
		  (b 10))
	      (loop with a = 1
		    and b = (+ a 2)
		    and c = (+ b 3)
		    return (list a b c))))
(test-equal '(0.0 0 0.0)
	    (loop with (a b c) of-type (float integer float)
		  return (list a b c)))
(test-equal '(0.0 0.0 0.0)
	    (loop with (a b c) of-type float 
		  return (list a b c)))

(test-equal '(0 1 2)
	    (flet ((triple () (values 0 1 2)))
		  (loop with (a b c) = (multiple-value-list (triple))
			do (return (list a b c)))))

(test-equal '(0 1 2)
	    (flet ((triple () (values 0 '(1) 2)))
		  (loop with (a (b) c) = (multiple-value-list (triple))
			do (return (list a b c)))))
(test-equal '(0 1 2 #f)
	    (flet ((triple () (values 0 '(0 1 2) 2)))
		  (loop with (a (nil b) c d) = (multiple-value-list (triple))
			do (return (list a b c d)))))

(test-equal '(0 1 2)
	    (flet ((triple () (values 0 1 2)))
		  (loop with (a b c) fixnum = (multiple-value-list (triple))
			do (return (list a b c)))))
(test-equal '(0 1 2)
	    (flet ((triple () (values 0 '(1) 2)))
		  (loop with (a (b) c) of-type (fixnum (fixnum) fixnum) =
			(multiple-value-list (triple))
			do (return (list a b c)))))

)

(begin-tests "binding"

;;;; I think the spec doesn't say exactly what should happen for the
;;;; following cases.  In particular section 6.1.1.4 "Expanding Loop
;;;; Forms" says:
;;;;
;;;;   "A variable is thus sometimes bound to a meaningless value of
;;;;   the correct type, and then later in the prologue it is set to
;;;;   the true initial value by using setq."
;;;;
;;;; My interpretation of this sentence is that portable code shouldn't
;;;; make assumptions about the value of a variable in the init forms.

(test-equal '((0 0) (1 -1) (2 -2) (3 -3) (4 -4) (5 -5))
	    (loop for a from 0 upto 5
		  for b from a downto -5
		  collect (list a b)))

(test-equal '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0))
	    (loop for a from 0 upto 5
		  with x = a
		  collect (list a x)))

(test-equal '((1 1) (3 1))
	    (loop for (a b) in '((1 2) (3 4))
		  with c = a
		  collect (list a c)))

;; the init form for for-as-equals is defined by the spec.
(test-equal '((1 1) (3 4))
	    (loop for (a b) in '((1 2) (3 4))
		  for c = a then b
		  collect (list a c)))

(test-equal '((1 1) (3 1))
	    (loop for (a b) in '((1 2) (3 4))
		  for c = a then c
		  collect (list a c)))

)
