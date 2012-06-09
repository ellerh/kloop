
(begin-tests "conditional-clauses"

(test-equal '(#f 5 5)
	    (let ((odd 0)
		  (even 0))
	      (list-lr
	       (loop for a from 1 upto 10
		     if (oddp a) do (incf odd) else do (incf even) end)
	       odd 
	       even)))

(test-equal '(#f 5 5 5 6)
	      (let ((odd+ 0) (even+ 0) (odd- 0) (even- 0))
		(list-lr
		 (loop for a from -10 upto 10
		       if (oddp a) if (> a 0) do (incf odd+) 
		       else do (incf odd-) end
		       else if (> a 0) do (incf even+) 
		       else do (incf even-))
		 odd+ even+ odd- even-)))

(test-equal '(#f 5 5 5 5)
	    (let ((odd+ 0) (even+ 0) (odd- 0) (even- 0))
	      (list-lr (loop for a from -10 upto 10
			  unless (zerop a)
			  if (oddp a)
		 	  if (> a 0) do (incf odd+) else do (incf odd-) end
			  else
			  if (> a 0) do (incf even+) else do (incf even-))
		    odd+ even+ odd- even-)))

(test-equal '(#f 5 5 5 5)
	    (let ((odd+ 0) (even+ 0) (odd- 0) (even- 0))
	      (list-lr (loop for a from -10 upto 10
			  if (not (zerop a))
			  when (oddp a)
			  unless (< a 0) do (incf odd+) 
			  else do (incf odd-) end
			  else
			  unless (<= a 0) do (incf even+)
			  else do (incf even-))
		    odd+ even+ odd- even-)))

;;(handler-bind ((simple-error #'(lambda (c) (declare (ignore c)) (continue))))
;;  (eq 'continued
;;      (loop for item in '(1 2 3 a 4 5)
;;	    when (not (numberp item))
;;	    return (or (cerror 
;;			"ignore this error" "non-numeric value: ~s" item)
;;		       'continued))

(test-equal '((1 2345 323 235) (324 2 4 252))
	    (loop for i in '(1 324 2345 323 2 4 235 252)
		  when (oddp i) collect i into odd-numbers
		  else			; I is even.
		  collect i into even-numbers
		  finally
		  (return (list odd-numbers even-numbers))))

(test-equal '(4 5 6)
	    (loop for i in '(1 2 3 4 5 6)
		  when (and (> i 3) i)
		  collect it))

(test-equal (loop for i in '(1 2 3 4 5 6)
		  when (and (> i 3) i)
		  return it) 4)

(test-equal 
 '((3.0 9.8) (0 4 5) (apple orange banana))
 (let ((l '(0 3.0 apple 4 5 9.8 orange banana)))
   (loop for i in l
	 when (numberp i)
	   when (floatp i)
	     collect i into float-numbers
	   else			; Not (floatp i)
	    collect i into other-numbers
	  else			; Not (numberp i)
	    when (symbolp i) 
	       collect i into symbol-list
	     else			; Not (symbolp i)
	       do 
	       (error #f "found a funny value in list ~S, value ~S~%" l i)
	 finally (return (list float-numbers other-numbers symbol-list)))))

(test-equal '(1 3) (loop for i below 5 if (oddp i) collecting i))
(test-equal '(1 3) (loop for i below 5 when (oddp i) collecting i))

(test-equal '((0) 1 (2) 3 (4))
	    (loop for i below 5
		  if (oddp i) collecting i else collecting (list i)))
(test-equal '((0) 1 (2) 3 (4))
	    (loop for i below 5
		  when (oddp i) collecting i else collecting (list i)))
(test-equal '(1 3) (loop for i below 5 unless (evenp i) collecting i))
(test-equal '((0) 1 (2) 3 (4))
	    (loop for i below 5
		  unless (evenp i) collecting i else collecting (list i)))

(test-equal '(1 3) (loop for i below 5 if (oddp i) collecting i end))
(test-equal '(1 3) (loop for i below 5 when (oddp i) collecting i end))
(test-equal '((0) 1 (2) 3 (4))
	    (loop for i below 5
		  if (oddp i) collecting i else collecting (list i) end))
(test-equal '((0) 1 (2) 3 (4))
	    (loop for i below 5
		  when (oddp i) collecting i else collecting (list i) end))
(test-equal '(1 3) (loop for i below 5 unless (evenp i) collecting i end))
(test-equal '((0) 1 (2) 3 (4))
	    (loop for i below 5
		  unless (evenp i) collecting i else collecting (list i) end))

(test-equal '("0-0" "0-1")
	    (loop for (a b) in '((0 0) (0 1))
		  if (zerop a) if (zerop b) collect "0-0" else collect "0-1"))
(test-equal '("0-0" "0-1")
	    (loop for (a b) in '((0 0) (0 1))
		  when (zerop a) 
		  if (zerop b) collect "0-0" else collect "0-1"))
(test-equal '("0-1" "1-X" "1-X")
	    (loop for (a b) in '((0 0) (0 1) (1 0) (1 1))
		  if (zerop a) if (= b 1) collect "0-1" end
		  else collect "1-X"))
(test-equal '("0-1" "1-X" "1-X")
	    (loop for (a b) in '((0 0) (0 1) (1 0) (1 1))
		  when (zerop a) if (= b 1) collect "0-1" end
		  else collect "1-X"))
(test-equal '("0-0" "0-1")
	    (loop for (a b) in '((0 0) (0 1))
		  unless (not (zerop a)) 
		  if (zerop b) collect "0-0" else collect "0-1"))
(test-equal '("0-1" "1-X" "1-X")
	    (loop for (a b) in '((0 0) (0 1) (1 0) (1 1))
		  unless (not (zerop a)) if (= b 1) collect "0-1" end
		  else collect "1-X"))

(test-equal '(x0-0-0 x0-0-1 x0-1-0 x0-1-1 x1-0-0 x1-0-1 x1-1-0 x1-1-1)
	    (loop for (a b c) in '((0 0 0) (0 0 1)
				   (0 1 0) (0 1 1)
				   (1 0 0) (1 0 1)
				   (1 1 0) (1 1 1))
		  if (zerop a)
		  if (zerop b)
		  if (zerop c) collect 'x0-0-0 else collect 'x0-0-1
		  else if (zerop c) collect 'x0-1-0 else collect 'x0-1-1
		  else if (zerop b)
		  if (zerop c) collect 'x1-0-0 else collect 'x1-0-1
		  else if (zerop c) collect 'x1-1-0 else collect 'x1-1-1))

(test-equal '(((0) 1 (2) 3 (4) 5 (6) 7 (8) 9) 25 20)
	    (loop for a below 10
		  if (oddp a) collect a into bag and sum a into odd
		  else collect (list a) into bag and sum a into even
		  finally (return (list bag odd even))))

(test-equal '(0 1 (1) ((1)) 2 3 (3) ((3)) 4 5 (5) ((5)) 
		6 7 (7) ((7)) 8 9 (9) ((9)))
	    (loop for a below 10
		  if (oddp a)
		  collect a and collect (list a) and collect (list (list a))
		  else collect a))

(test-equal '((1 (1) 3 (3) 5 (5) 7 (7) 9 (9))
	      5 -5)
	    (let ((c0 0) (c1 0))
	      (list-lr 
	       (loop for a below 10
		     when (oddp a)
		     collect a and do (incf c0) (decf c1) and collect (list a))
	       c0
	       c1)))

)

(begin-tests "return-clause"
(test-equal 0 (loop return 0))

(test-equal 1 (loop for a from 0 below 3 when (and (oddp a) a) return it))
(test-equal 'ok (loop for a in '(#f #f ok #f ok2) when a return it))
(test-equal 'ok (loop with a = 'ok if a return it else return #f))
(test-equal '(0 1 2) (multiple-value-list (loop return (values 0 1 2))))
(test-equal '(#t #f)
	    (let ((flag #f))
	      (list-lr
	       (loop for a below 3 when (oddp a) return #t 
		     finally (setq flag #t))
	       flag)))
(test-equal '(3 0)
	    (loop for a in '(0 1 2 3) 
		  and b in '(3 2 1 0)
		  if (and (oddp a) a)
		  if (and (evenp b) b)
		  when (and (= (* a b) 0) (list a b))
		  return it))
)
