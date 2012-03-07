
(begin-tests "numeric-accumulation-clauses"

(begin-tests "count"

(test-equal 5 (loop for a from 1 upto 10 counting (evenp a)))
(test-equal 10 (loop for a downfrom 10 above 0 count a))
(test-equal 10 (loop for a downfrom 10 above 0 counting a))
(test-equal #f (loop for a downfrom 10 above 0 count a into x))
(test-equal #f (loop for a downfrom 10 above 0 counting a into x))
(test-equal 10 (loop for a downfrom 10 above 0 count a into x 
		     finally (return x)))
(test-equal 10 (loop for a downfrom 10 above 0 counting a into x 
		     finally (return x)))
(test-equal 6 (loop for a in '(#f a #f #f b #f c d e #f #f #f #f f)
		    when a count it))
(test-equal 6 (loop for a in '(#f a #f #f b #f c d e #f #f #f #f f)
		    when a counting it))
(test-equal #f (loop for a in '(#f a #f #f b #f c d e #f #f #f #f f)
		     when a count it into x))
(test-equal #f (loop for a in '(#f a #f #f b #f c d e #f #f #f #f f)
		     when a counting it into x))
(test-equal 6 (loop for a in '(#f a #f #f b #f c d e #f #f #f #f f)
		    when a count it into x finally (return x)))
(test-equal 6 (loop for a in '(#f a #f #f b #f c d e #f #f #f #f f)
		    when a counting it into x finally (return x)))
(test-equal 5 (loop for i in '(a b #f c #f d e) count i))
)

(begin-tests "sum"
(test-equal 55 (loop for a to 10 sum a))
(test-equal 55 (loop for a to 10 summing a))
(test-equal 55 (loop for a in '(0 #f 1 #f 2 3 #f 4 5 6 7 #f 8 9 10 #f)
		     if a sum it))
(test-equal 55 (loop for a in '(0 #f 1 #f 2 3 #f 4 5 6 7 #f 8 9 10 #f)
		     if a summing it))
(test-equal #t
	    (loop for a to 10
		  sum a into sum
		  if (oddp a) sum a into odd
		  else sum a into even
		  finally (return (= sum (+ odd even)))))
(test-equal #t
	    (loop for a to 10
		  summing a into sum
		  if (oddp a) sum a into odd
		  else summing a into even
		  finally (return (= sum (+ odd even)))))
(test-equal (loop for a downfrom 5 to 1
		  summing a) 15)
(test-equal #f (loop for a downfrom 5 to 1
		     summing a into n))    ;; not return automatically

(test-equal 14
	    (loop for i from 1 to 4
		  sum i fixnum
		  count #t fixnum))
)


(begin-tests "maximize"
(test-equal 5 (loop for i in '(2 1 5 3 4) maximize i))
(test-equal 9 (loop for a in '(0 5 9) maximize a))
(test-equal 9 (loop for a in '(0 5 9) maximizing a))
(test-equal 9 (loop for a in '(0 9 5) maximize a))
(test-equal 9 (loop for a in '(0 9 5) maximizing a))
(test-equal 9 (loop for a in '(9 0 5) maximize a))
(test-equal 9 (loop for a in '(9 0 5) maximizing a))
(test-equal 9 (loop for a in '(9 0 9 5) maximize a))
(test-equal 9 (loop for a in '(9 0 9 5) maximizing a))
(test-equal 9
	    (let ((l '()))
	      (loop (when (= (car (push (random 10) l)) 9) (return)))
	      (loop for a in l maximize a)))
(test-equal 9
	    (let ((l '()))
	      (loop (when (= (car (push (random 10) l)) 9) (return)))
	      (loop for a in l maximizing a)))
(test-equal 99
	    (let ((l '()))
	      (loop (when (= (car (push (random 100) l)) 99) (return)))
	      (loop for a in l maximize a)))
(test-equal 99
	    (let ((l '()))
	      (loop (when (= (car (push (random 100) l)) 99) (return)))
	      (loop for a in l maximizing a)))
(test-equal 999
	    (let ((l '()))
	      (loop (when (= (car (push (random 1000) l)) 999) (return)))
	      (loop for a in l maximize a)))
(test-equal 999
	    (let ((l '()))
	      (loop (when (= (car (push (random 1000) l)) 999) (return)))
	      (loop for a in l maximizing a)))
(test-equal #f (loop for a in '(0 5 9) maximize a into max))
(test-equal #f (loop for a in '(0 5 9) maximizing a into max))
(test-equal 9 (loop for a in '(0 5 9) maximize a into max 
		    finally (return max)))
(test-equal 9 (loop for a in '(0 5 9) maximizing a into max 
		    finally (return max)))
(test-equal 9 (loop for a in '(0 5 9) maximize a into max of-type integer
		    finally (return max)))
(test-equal 9 (loop for a in '(0 5 9) maximizing a into max of-type integer
		    finally (return max)))
(test-equal 9.0 (loop for a in '(0.0 5.0 9.0) maximize a into max float
		      finally (return max)))
(test-equal 9.0 (loop for a in '(0.0 5.0 9.0) maximizing a into max float
		      finally (return max)))
(test-equal 6.0
	    (let ((series '(1.2 4.3 5.7)))
	      (loop for v in series maximize (round v) of-type fixnum)))
)


(begin-tests "minimize"
(test-equal 1 (loop for i in '(2 1 5 3 4) minimize i))
(test-equal 0 (loop for a in '(0 5 9) minimize a))
(test-equal 0 (loop for a in '(0 5 9) minimizing a))
(test-equal 0 (loop for a in '(9 5 0) minimize a))
(test-equal 0 (loop for a in '(9 5 0) minimizing a))
(test-equal 0 (loop for a in '(9 0 5) minimize a))
(test-equal 0 (loop for a in '(9 0 5) minimizing a))
(test-equal 0 (loop for a in '(9 0 9 0 5 0) minimizing a))
(test-equal 0 (loop for a in '(9 0 9 0 5 0) minimizing a))
(test-equal 1 (loop for a in '(1 5 9) minimize a))
(test-equal 1 (loop for a in '(1 5 9) minimizing a))
(test-equal 1 (loop for a in '(9 5 1) minimize a))
(test-equal 1 (loop for a in '(9 5 1) minimizing a))
(test-equal 1 (loop for a in '(9 1 5) minimize a))
(test-equal 1 (loop for a in '(9 1 5) minimizing a))
(test-equal 1 (loop for a in '(9 1 9 1 5 1) minimizing a))
(test-equal 1 (loop for a in '(9 1 9 1 5 1) minimizing a))
(test-equal 0
	    (let ((l '()))
	      (loop (when (zerop (car (push (random 10) l))) (return)))
	      (loop for a in l minimize a)))

(test-equal 0 
	    (let ((l '()))
	      (loop (when (zerop (car (push (random 10) l))) (return)))
	      (loop for a in l minimizing a)))
(test-equal 0 
	    (let ((l '()))
	      (loop (when (zerop (car (push (random 100) l))) (return)))
	      (loop for a in l minimize a)))
(test-equal 0 
	    (let ((l '()))
	      (loop (when (zerop (car (push (random 100) l))) (return)))
	      (loop for a in l minimizing a)))
(test-equal 0 
	    (let ((l '()))
	      (loop (when (zerop (car (push (random 1000) l))) (return)))
	      (loop for a in l minimize a)))
(test-equal 0 
	    (let ((l '()))
	      (loop (when (zerop (car (push (random 1000) l))) (return)))
	      (loop for a in l minimizing a)))
(test-equal #f (loop for a in '(0 5 9) minimize a into min))
(test-equal #f (loop for a in '(0 5 9) minimizing a into min))
(test-equal 0 (loop for a in '(0 5 9) minimize a into min 
		    finally (return min)))
(test-equal 0 (loop for a in '(0 5 9) minimizing a into min 
		    finally (return min)))
(test-equal 0 (loop for a in '(0 5 9) minimize a into min of-type integer
		    finally (return min)))
(test-equal 0 (loop for a in '(0 5 9) minimizing a into min of-type integer
		    finally (return min)))
(test-equal 0.0 (loop for a in '(0.0 5.0 9.0) minimize a into min float
		      finally (return min)))
(test-equal 0.0 (loop for a in '(0.0 5.0 9.0) minimizing a into min float
		      finally (return min)))
(test-equal 1.0
	    (let ((series '(1.2 4.3 5.7)))
	      (loop for v of-type float in series
		    minimize (round v) into result of-type fixnum
		    finally (return result))))
)
	     

(test-equal 60 (loop for a upto 10 summing a when (oddp a) counting it))
(test-equal 220 (loop for a upto 10
		      for b downfrom 20
		      sum a
		      summing b))
(test-equal 60 (loop for a upto 10
		     summing a into sum
		     when (oddp a) counting it into sum
		     finally (return sum)))
(test-equal 21 (loop for a in '(a 1 b 3 c 4 5 x 2 y z)
		     if (and (numberp a) a) summing it
		     else counting 1))


(test-equal 5 (loop for a from 3 to 5 maximizing a minimizing a))
(test-equal 3 (loop for a upto 3 for b from 6 downto 3 maximize a minimize b))
(test-equal '(-3 3)
	    (loop for a in '(0 -1 1 -2 2 -3 3)
		  maximize a into plus
		  minimize a into minus
		  finally (return (list minus plus))))

(test-equal '((0 1 2 3 4 5 6 7 8 9) (45 10 9 0))
	    (let ((val '()))
	      (list-lr (loop for a below 10
			     collecting a
			     summing a into sum
			     counting a into count
			     maximizing a into max
			     minimizing a into min
			     finally (setq val (list sum count max min)))
		       val)))
(test-equal 'ok (loop for a below 3 collecting a
		      finally (return 'ok)))
(test-equal '((0 1 2) #t)
	    (let ((flag #f))
	      (list-lr (loop for a below 3 collecting a
			     finally (setq flag #t))
		       flag)))
(test-equal 'ok (loop for a below 3 appending (list a)
		      finally (return 'ok)))
(test-equal 'ok (loop for a below 3 nconcing (list a)
		      finally (return 'ok)))


)
