
(begin-tests "simple-loop"

(test-equal #f (loop (return #f)))
(test-equal #t (loop (return #t)))

(test-equal "return 0" 
	    0 (let ((x 10))
		(loop (if (= x 0) 
			  (return x)
			  (decf x)))))
(test-equal "return #t"
	    #t (let ((x 10))
		 (loop (when (= x 0) 
			 (return #t))
		       (decf x))))

(test-equal "return void" 
	    '()
	    (multiple-value-list
	     (let ((x 10))
	       (loop (when (= x 0) 
		       (return))
		     (decf x)))))

(test-equal '() 
	    (let ((stack '(0 1 2))) 
	      (loop (when (null? stack) 
		      (return #f)) 
		    (pop stack))
	      stack))

(test-equal '(1 2 3) 
	    (multiple-value-list 
	     (let ((x 0))
	       (loop (when (= x 0) 
		       (return (values 1 2 3)))
		     (decf x)))))

(test-equal 3 
	    (let ((i 0)) 
	      (loop (incf i) 
		    (when (= i 3) 
		      (return i)))))


)


(begin-tests "arithmetic"

(test-equal '(3 2 1)
	    (let ((stack '()))
	      (loop for i from 1 to 3 by 1 do (push i stack))
	      stack))

(test-equal '(3 2 1 0)
	    (let ((stack '()))
	      (loop for i below 4 do (push i stack))
	      stack))

(test-equal "step by 2" '(10 8 6 4 2 0)
	    (let ((stack '()))
	      (loop for i to 10 by 2 do (push i stack))
	      stack))

(test-expect-fail "step by 0.2")
(test-equal "step by 0.2" 
	    '(1.0 0.8 0.6 0.4 0.2 0)
	    (let ((stack '()))
	      (loop for f to 1.0 by 0.2 do (push f stack))
	      stack))

(test-equal "step in sync" 
	    '((2 . 7) (1 . 6) (0 . 5))
	    (let ((stack '()))
	      (loop for i below 3
		    for j from 5
		    do (push (cons i j) stack))
	      stack))

(begin-tests "for-as-arithmetic-up with 3 forms"
(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a from 1 to 3 by 1 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a from 1 by 1 to 3 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a to 3 by 1 from 1 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a to 3 from 1 by 1 do (push a s)) s))

(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a by 1 to 3 from 1 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 from 1 to 3 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 to 3 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 by 1 to 3 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a to 3 by 1 upfrom 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a to 3 upfrom 1 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 to 3 upfrom 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 upfrom 1 to 3 do (push a s)) s))


(test-equal '(3 2 1)
	    (let ((s '())) (loop for a from 1 upto 3 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a from 1 by 1 upto 3 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upto 3 by 1 from 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upto 3 from 1 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 upto 3 from 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 from 1 upto 3 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 upto 3 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 by 1 upto 3 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upto 3 by 1 upfrom 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upto 3 upfrom 1 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 upto 3 upfrom 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 upfrom 1 upto 3 do (push a s)) s))


(test-equal '(3 2 1)
	    (let ((s '())) (loop for a from 1 below 4 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a from 1 by 1 below 4 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a below 4 by 1 from 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a below 4 from 1 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 below 4 from 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 from 1 below 4 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 below 4 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 by 1 below 4 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a below 4 by 1 upfrom 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a below 4 upfrom 1 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 below 4 upfrom 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a by 1 upfrom 1 below 4 do (push a s)) s))

)

(begin-tests "for-as-arithmetic-up with 2 forms"

(test-equal '(3 2 1) (let ((s '())) (loop for a from 1 to 3 do (push a s)) s))
(test-equal '(3 2 1) (let ((s '())) (loop for a to 3 from 1 do (push a s)) s))

(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a upfrom 1 to 3 do (push a s)) s))
(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a to 3 upfrom 1 do (push a s)) s))


(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a from 1 upto 3 do (push a s)) s))
(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a upto 3 from 1 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 upto 3 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upto 3 upfrom 1 do (push a s)) s))


(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a from 1 below 4 do (push a s)) s))
(test-equal '(3 2 1) 
	    (let ((s '())) (loop for a below 4 from 1 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a upfrom 1 below 4 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a below 4 upfrom 1 do (push a s)) s))


(test-equal '(3 2 1 0) (let ((s '())) (loop for a to 3 by 1 do (push a s)) s))
(test-equal '(3 2 1 0) (let ((s '())) (loop for a by 1 to 3 do (push a s)) s))

(test-equal '(3 2 1 0) 
	    (let ((s '())) (loop for a upto 3 by 1 do (push a s)) s))
(test-equal '(3 2 1 0) 
	    (let ((s '())) (loop for a by 1 upto 3 do (push a s)) s))

(test-equal '(3 2 1 0)
	    (let ((s '())) (loop for a below 4 by 1 do (push a s)) s))
(test-equal '(3 2 1 0)
	    (let ((s '())) (loop for a by 1 below 4 do (push a s)) s))

(test-equal 4 (let ((s '(1 2 3)))
		(loop for a from 1 by 1 do (if (null? s) (return a) (pop s)))))
(test-equal 4 (let ((s '(1 2 3)))
		(loop for a by 1 from 1 do (if (null? s) (return a) (pop s)))))

(test-equal 4 (let ((s '(1 2 3)))
		(loop for a upfrom 1 by 1 
		      do (if (null? s) (return a) (pop s)))))
(test-equal 4 (let ((s '(1 2 3)))
		(loop for a by 1 upfrom 1 
		      do (if (null? s) (return a) (pop s)))))


)

(begin
(test-begin "for-as-arithmetic-up with 1 form")
(test-equal 4 (let ((s '(1 2 3)))
		(loop for a from 1 do (if (null? s) (return a) (pop s)))))
(test-equal 4 (let ((s '(1 2 3)))
		(loop for a upfrom 1 do (if (null? s) (return a) (pop s)))))

(test-equal '(3 2 1 0)
	    (let ((s '())) (loop for a to 3 do (push a s)) s))
(test-equal '(3 2 1 0)
	    (let ((s '())) (loop for a upto 3 do (push a s)) s))
(test-equal '(3 2 1 0)
	    (let ((s '())) (loop for a below 4 do (push a s)) s))

(test-equal 3 (let ((s '(1 2 3)))
		(loop for a by 1 do (if (null? s) (return a) (pop s)))))

(test-end "for-as-arithmetic-up with 1 form")
)

(begin
(test-begin "for-as-arithmetic-downto with 3 forms")
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a from 3 downto 1 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a from 3 by 1 downto 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downto 1 by 1 from 3 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downto 1 from 3 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 from 3 downto 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 downto 1 from 3 do (push a s)) s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a from 3 above 0 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a from 3 by 1 above 0 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a above 0 by 1 from 3 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a above 0 from 3 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 from 3 above 0 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 above 0 from 3 do (push a s)) s))

(test-end "for-as-arithmetic-downto with 3 forms")

)

(begin
(test-begin "for-as-arithmetic-downto with 2 forms")
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a from 3 downto 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downto 1 from 3 do (push a s)) s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a from 3 above 0 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a above 0 from 3 do (push a s)) s))

(test-end "for-as-arithmetic-downto with 2 forms")
)

(begin
(test-begin "for-as-arithmetic-downfrom with 3 forms")
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 to 1 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 by 1 to 1 do (push a s)) s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a to 1 by 1 downfrom 3 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a to 1 downfrom 3 by 1 do (push a s)) s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 to 1 downfrom 3 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 downfrom 3 to 1 do (push a s)) s))


(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 downto 1 by 1 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 by 1 downto 1 do (push a s))
		 s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downto 1 by 1 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downto 1 downfrom 3 by 1 do (push a s))
		 s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 downto 1 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 downfrom 3 downto 1 do (push a s))
		 s))


(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 above 0 by 1 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 by 1 above 0 do (push a s))
		 s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a above 0 by 1 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a above 0 downfrom 3 by 1 do (push a s))
		 s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 above 0 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a by 1 downfrom 3 above 0 do (push a s))
		 s))

(test-end "for-as-arithmetic-downfrom with 3 forms")
)

(begin-tests "for-as-arithmetic-downfrom with 2 forms"
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 to 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a to 1 downfrom 3 do (push a s)) s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 downto 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downto 1 downfrom 3 do (push a s)) s))

(test-equal '(1 2 3)
	    (let ((s '())) (loop for a downfrom 3 above 0 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop for a above 0 downfrom 3 do (push a s)) s))


(test-equal 0 (let ((s '(0 1 2)))
		(loop for a downfrom 3 by 1 
		      do (if (null? s) (return a) (pop s)))))
(test-equal 0 (let ((s '(0 1 2)))
		(loop for a by 1 downfrom 3 
		      do (if (null? s) (return a) (pop s)))))

)

(begin-tests "for-as-arithmetic-downfrom with 1 form"
(test-equal 0 (let ((s '(0 1 2)))
		(loop for a downfrom 3 do (if (null? s) (return a) (pop s)))))

)

(begin-tests "for-as-arithmetic form evaluation"
(test-equal '(10 8 6 4 2)
	    (let ((s '()))
	      (loop for a from (+ 1 1) upto (+ 4 6) by (+ 1 1) do (push a s))
	      s))
)

(begin-tests "for-as-arithmetic form evaluation order"
(test-equal '(11 9 7 5 3 1)
	    (let ((x 0)
		  (s '()))
	      (loop for a from (incf x) upto (+ (incf x) 10) by x 
		    do (push a s))
	      s))

(test-equal '(11 9 7 5 3 1)
	    (let ((x 0) (s '()))
	      (loop for a from (incf x) by (incf x) upto (+ x 10) 
		    do (push a s))
	      s))

(test-equal '(12 11 10 9 8 7 6 5 4 3 2)
	    (let ((x 0) (s '()))
	      (loop for a by (incf x) from (incf x) upto (+ x 10)
		    do (push a s))
	      s))

(test-equal '(12 11 10 9 8 7 6 5 4 3)
	    (let ((x 0) (s '()))
	      (loop for a by (incf x) upto (+ (incf x) 10) from (incf x)
		    do (push a s))
	      s))
)

(begin-tests "for-as-arithmetic type"
(test-equal '(3 2 1)
	    (let ((s '())) (loop for a t from 1 to 3 by 1 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '())) (loop for a fixnum from 1 to 3 by 1 do (push a s)) s))

(test-equal '(3.0 2.0 1.0)
	    (let ((s '())) 
	      (loop for a float from 1.0 to 3.0 by 1.0 do (push a s))
	      s))


(test-equal '(3 2 1)
	    (let ((s '()))
	      (loop for a of-type t from 1 to 3 by 1 do (push a s))
	      s))

(test-equal '(3 2 1)
	    (let ((s '()))
	      (loop for a of-type fixnum from 1 to 3 by 1 do (push a s)) s))

(test-equal '(3.0 2.0 1.0)
	    (let ((s '()))
	      (loop for a of-type float from 1.0 to 3.0 by 1.0 do (push a s))
	      s))

(test-equal '(3 2 1)
	    (let ((s '()))
	      (loop for a of-type number from 1 to 3 by 1 do (push a s)) s))

(test-equal '(3 2 1)
	    (let ((s '()))
	      (loop for a of-type integer from 1 to 3 by 1 do (push a s)) s))

)


(begin-tests "for-as-arithmetic misc"
(test-equal '(10 5 0)
	    (let ((s '())) (loop for a from 0 upto 10 by 5 do (push a s)) s))

(test-equal '(9 6 3 0)
	    (let ((s '())) (loop for a from 0 upto 10 by 3 do (push a s)) s))

(test-equal '(0 -1 -2 -3)
	    (let ((s '())) (loop for a from -3 upto 0 do (push a s)) s))

(test-equal '(-3 -2 -1 0)
	    (let ((s '())) (loop for a downfrom 0 to -3 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a from 1 to 3 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a upfrom 1 to 3 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a from 1 upto 3 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a upfrom 1 upto 3 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a from 1 below 4 by 1 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a upfrom 1 below 4 by 1 do (push a s)) s))
(test-equal '(3 2 1) (let ((s '())) (loop as a from 1 to 3 do (push a s)) s))
(test-equal '(3 2 1) (let ((s '())) (loop as a upfrom 1 to 3 do (push a s)) s))
(test-equal '(3 2 1) (let ((s '())) (loop as a from 1 upto 3 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a upfrom 1 upto 3 do (push a s)) s))
(test-equal '(3 2 1) (let ((s '())) 
		       (loop as a from 1 below 4 do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '())) (loop as a upfrom 1 below 4 do (push a s)) s))
(test-equal '(3 2 1 0) (let ((s '())) (loop as a to 3 by 1 do (push a s)) s))
(test-equal '(3 2 1 0) (let ((s '())) (loop as a upto 3 by 1 do (push a s)) s))
(test-equal '(3 2 1 0)
	    (let ((s '())) (loop as a below 4 by 1 do (push a s)) s))

(test-equal 4 (let ((s '(1 2 3)))
		(loop as a from 1 by 1 do (if (null? s) (return a) (pop s)))))
(test-equal 4 (let ((s '(1 2 3)))
		(loop as a upfrom 1 by 1 
		      do (if (null? s) (return a) (pop s)))))
(test-equal 4 (let ((s '(1 2 3)))
		(loop as a from 1 do (if (null? s) (return a) (pop s)))))
(test-equal '(3 2 1 0) (let ((s '())) (loop as a to 3 do (push a s)) s))
(test-equal 3 (let ((s '(1 2 3)))
		(loop as a by 1 do (if (null? s) (return a) (pop s)))))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a from 3 downto 1 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a from 3 above 0 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a from 3 downto 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a from 3 above 0 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a downfrom 3 to 1 by 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a to 1 by 1 downfrom 3 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a by 1 to 1 downfrom 3 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a downfrom 3 downto 1 by 1 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a downto 1 by 1 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a by 1 downto 1 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a downfrom 3 above 0 by 1 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a above 0 by 1 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a by 1 above 0 downfrom 3 do (push a s))
		 s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a downfrom 3 to 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a downfrom 3 downto 1 do (push a s)) s))
(test-equal '(1 2 3)
	    (let ((s '())) (loop as a downfrom 3 above 0 do (push a s)) s))
(test-equal 0 (let ((s '(0 1 2)))
		(loop as a downfrom 3 by 1 
		      do (if (null? s) (return a) (pop s)))))
(test-equal 0 (let ((s '(0 1 2)))
		(loop as a downfrom 3 do (if (null? s) (return a) (pop s)))))
(test-equal '(0) (let ((s '())) (loop for a from 0 upto 0 do (push a s)) s))
(test-equal #f (loop for a upfrom 0 below 0))
;;(test-equal '() (loop for a upfrom 10 to -10 collect a))
(test-equal '(1 2/3 1/3)
	    (let ((s '()))
	      (loop for a from 1/3 upto 1 by 1/3 do (push a s))
	      s))
(test-equal '(5/3 4/3 1 2/3 1/3)
	    (let ((s '()))
	      (loop for a of-type rational from 1/3 upto 5/3 by 1/3 
		    do (push a s))
	      s))
(test-equal '(2 1 0)
	    (let ((s '())) (loop for a fixnum below 3 do (push a s)) s))
(test-equal '(2 1 0)
	    (let ((s '())) (loop for a of-type fixnum below 3 
				 do (push a s)) s))
(test-equal '(2 1 0)
	    (let ((s '())) (loop for a of-type (integer 0 2)
			  below 3 do (push a s)) s))

)

)


(begin-tests "for-as-in-list"
(test-equal #f (loop for a in '()))
(test-equal '(2 1 0)
	    (let ((s '())) (loop for a in '(0 1 2) do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '()))
	      (loop for a in (let ((i 0))
			       ;; let* to enforce evaluation order
			       (let* ((x (incf i))
				      (y (incf i))
				      (z (incf i)))
				 (list x y z)))
		    do (push a s))
	      s))
(test-error (loop for a in '(0 1 . 2)))
(test-equal '(3 2 1 0)
	      (let ((s '()))
		(loop for a in '(0 1 2 3) by cdr do (push a s))
		s))
(test-equal '(2 0)
	    (let ((s '()))
	      (loop for a in '(0 1 2 3) by cddr do (push a s))
	      s))

(test-expect-fail "in-list by cdddr '(3)")
(test-equal "in-list by cdddr '(3)" 
	    '(3 0)
	    (let ((s '()))
	      (loop for a in '(0 1 2 3) by cdddr do (push a s))
	      s))
(test-equal '(0)
	    (let ((s '()))
	      (loop for a in '(0 1 2 3) by cddddr do (push a s))
	      s))
(test-equal '(2 1 0) 
	    (let ((s '())) (loop for a t in '(0 1 2) do (push a s)) s))
(test-equal '(2 1 0)
	    (let ((s '())) (loop for a of-type t in '(0 1 2) do (push a s)) s))
(test-equal '(2 1 0) 
	    (let ((s '())) 
	      (loop for a fixnum in '(0 1 2) do (push a s))
	      s))
(test-equal '(2 1 0) 
	    (let ((s '()))
	      (loop for a of-type fixnum in '(0 1 2) do (push a s))
	      s))
(test-equal '(2 1 0) 
	    (let ((s '())) 
	      (loop for a of-type t in '(0 1 2) do (push a s))
	      s))
(test-equal '(2.0 1.0 0.0) 
	    (let ((s '()))
	      (loop for a float in '(0.0 1.0 2.0) do (push a s))
	      s))
(test-equal '(2.0 1.0 0.0) 
	    (let ((s '())) 
	      (loop for a of-type float in '(0.0 1.0 2.0)
		    do (push a s))
	      s))

)

(begin-tests "for-as-on-list"
(test-equal #f (loop for a on '()))
(test-equal '((2) (1 2) (0 1 2))
	    (let ((s '())) (loop for a on '(0 1 2) do (push a s)) s))
(test-equal '(3 2 1)
	    (let ((s '()))
	      (loop for a on (let ((i 0))
			       ;; let* to enforce evaluation order
			       (let* ((x (incf i))
				      (y (incf i))
				      (z (incf i)))
				 (list x y z)))
		    do (push (car a) s))
	      s))
(test-equal '((1 . 2) (0 1 . 2))
	    (let ((s '())) (loop for a on '(0 1 . 2) do (push a s)) s))
(test-equal '((3) (2 3) (1 2 3) (0 1 2 3))
	    (let ((s '()))
	      (loop for a on '(0 1 2 3) by cdr do (push a s))
	      s))
(test-equal '((2 3) (0 1 2 3))
	    (let ((s '()))
	      (loop for a on '(0 1 2 3) by cddr do (push a s))
	      s))

(test-expect-fail "on-list by (cdddr '(3))")
(test-equal "on-list by (cdddr '(3))"
	    '((3) (0 1 2 3))
	    (let ((s '()))
	      (loop for a on '(0 1 2 3) by cdddr do (push a s))
	      s))
(test-equal '((0 1 2 3))
	    (let ((s '()))
	      (loop for a on '(0 1 2 3) by cddddr do (push a s))
	      s))
(test-equal '((2) (1 2) (0 1 2))
	    (let ((s '())) (loop for a t on '(0 1 2) do (push a s)) s))
(test-equal '((2) (1 2) (0 1 2))
	    (let ((s '())) (loop for a of-type t on '(0 1 2) do (push a s)) s))
(test-equal '((2) (1 2) (0 1 2))
	    (let ((s '())) (loop for a of-type list on '(0 1 2) do (push a s))
		 s))

(test-equal '((0 1 2 3))
	    (let ((s '()))
	      (loop for a on '(0 1 2 3) by (lambda (arg) (cddddr arg))
		    do (push a s))
	      s))

)

(begin-tests "for-as-across"
(test-equal #f (loop for a across ""))
(test-equal '() (let ((s '())) (loop for a across "" do (push a s)) s))
(test-equal '(#\c #\b #\a)
	    (let ((s '())) (loop for a across "abc" do (push a s)) s))
(test-equal '(z y x)
	    (let ((s '())) (loop for a across '#(x y z) do (push a s)) s))
;;(test-equal '(1 0 1 0)
;;	    (let ((s '())) (loop for a across #*0101 do (push a s)) s))
(test-equal '(#\c #\b #\a)
	    (let ((s '())) (loop for a t across "abc" do (push a s)) s))
(test-equal '(#\c #\b #\a)
	    (let ((s '())) (loop for a of-type t across "abc" do (push a s)) s))
(test-equal '(#\c #\b #\a)
	    (let ((s '())) (loop for a of-type character across "abc"
				 do (push a s)) s))
(test-equal '(#\c #\b #\a)
	    (let ((s '())) (loop for a of-type base-char across "abc"
				 do (push a s)) s))
(test-equal '(2.0 1.0 0.0)
	    (let ((s '())) (loop for a float across '#(0.0 1.0 2.0)
				 do (push a s)) s))
(test-equal '(2.0 1.0 0.0)
	    (let ((s '())) (loop for a of-type float across '#(0.0 1.0 2.0)
				 do (push a s)) s))
(test-equal '(2 1 0)
	    (let ((s '())) (loop for a fixnum across '#(0 1 2)
				 do (push a s)) s))
(test-equal '(2 1 0)
	    (let ((s '())) (loop for a of-type fixnum across '#(0 1 2)
				 do (push a s)) s))
)

;;(test-equal "simple-collect" '(0 1 2) (loop for a below 3 collect a))
;;(test-equal "simple-append" '(0 0 1 1 2 2) 
;;	    (loop for a below 3 append (list a a)))
;;
(test-equal "simple-destructuring" 
	    '(a b c)
	    (loop for (x y) in '((1 a) (2 b) (3 c))
		  collect y))

(begin-tests "for-as-equals-then"

(test-equal 2
	    (let ((i 3)) 
	      (loop for a = 0 then (+ 1 a)
		    do (when (zerop (decf i)) (return a)))))

(test-equal '(2 1 0)
	    (let ((s '())) 
	      (loop for a = '(0 1 2) then (cdr a)
		    do (if (pair? a) (push (car a) s) (return s)))))


(test-equal '(2 1 0)
	    (let ((s '())) (loop with i = 0 for x = i
				 do (when (= i 3) (return))
				 (push x s) (incf i)) s))

(test-equal '(3 2 1 0)
	    (let ((s '()))
	      (loop for i = 0 then (+ 1 i) do (push i s) 
		    when (= i 3) return #t)
	      s))

(test-equal '(3 2 1 0)
	    (let ((s '()))
	      (loop for i fixnum = 0 then (+ 1 i) do (push i s)
		    when (= i 3) return #t)
	      s))

(test-equal '(3 2 1 0)
	    (let ((s '()))
	      (loop for i of-type fixnum = 0 then (+ 1 i) do (push i s)
		    when (= i 3) return #t)
	      s))

(test-equal '(3.0 2.0 1.0 0.0)
	    (let ((s '()))
	      (loop for i float = 0.0 then (+ 1 i) do (push i s)
		    when (= i 3.0) return #t)
	      s))
(test-equal '(3.0 2.0 1.0 0.0)
	    (let ((s '()))
	      (loop for i of-type float = 0.0 then (+ 1 i) do (push i s)
		    when (= i 3.0) return #t)
	      s))
(test-equal '(3.0 2.0 1.0 0.0)
	    (let ((s '()))
	      (loop for i t = 0.0 then (+ 1 i) do (push i s)
		    when (= i 3.0) return #t)
	      s))
(test-equal '(3.0 2.0 1.0 0.0)
	    (let ((s '()))
	      (loop for i of-type t = 0.0 then (+ 1 i) do (push i s)
		    when (= i 3.0) return #t)
	      s))

(test-equal #t 
	    (let ((chars '(#\a #\b #\c #\d)))
	      (loop for c = (pop chars) unless (pair? chars) return #t)))

(test-equal #t
	    (let ((chars '(#\a #\b #\c #\d)))
	      (loop for c of-type character = (pop chars) 
		    unless (pair? chars) return #t)))
(test-equal #t
	    (let ((chars '(#\a #\b #\c #\d)))
	      (loop for c of-type base-char = (pop chars) 
		    unless (pair? chars) return #t)))

(test-equal '(3 2 1 0)
	    (let ((s '()))
	      (loop for i of-type (integer 0 3) = 0 then (+ 1 i) do (push i s)
		    when (= i 3) return #t)
	      s))


(test-equal '(0 1 2 1 2 3 2 3 4)
	    (flet ((triple (n) (values n (+ n 1) (+ n 2))))
		  (loop for i from 0 upto 2
			for (a b c) = (multiple-value-list (triple i))
			append `(,a ,b ,c))))


(test-equal '(0 1 2 1 2 3 2 3 4)
	    (flet ((triple (n) (values n `(,(+ n 1)) `((,(+ n 2))))))
		  (loop for i from 0 upto 2
			for (a (b) ((c))) = (multiple-value-list (triple i))
			append `(,a ,b ,c))))

(test-equal '(0 10 11 12 13 20 21 22 
		1 11 12 13 14 21 22 23 
		2 12 13 14 15 22 23 24)
	    (flet ((triple (n) 
			   (values n
				   `(,(+ n 10) ,(+ n 11) ,(+ n 12) ,(+ n 13))
				   `(,(+ n 20) ,(+ n 21) ,(+ n 22)))))
		  (loop for i from 0 upto 2
			for (a (b0 b1 b2 b3) (c0 c1 c2)) 
			= (multiple-value-list (triple i))
			append `(,a ,b0 ,b1 ,b2 ,b3 ,c0 ,c1 ,c2))))


(test-equal 
 '(0 10 11 12 13 200 210 211 212 220
     1 11 12 13 14 201 211 212 213 221
     2 12 13 14 15 202 212 213 214 222)
 (flet ((triple (n) 
		(values n
			`(,(+ n 10) ,(+ n 11) ,(+ n 12) ,(+ n 13))
			`(,(+ n 200)
			  (,(+ n 210) ,(+ n 211) ,(+ n 212) ,(+ n 213))
			  ,(+ n 220)))))
       (loop for i from 0 upto 2
	     for (a (b0 b1 b2 b3) (c0 (c10 c11 c12) c2)) =
	     (multiple-value-list (triple i))
	     append `(,a ,b0 ,b1 ,b2 ,b3 ,c0 ,c10 ,c11 ,c12 ,c2))))

)

;;(test-equal '(0 1 2) (loop for a from 0 below 3 collect a))

