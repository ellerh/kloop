

(begin-tests "initial-final-clause"

(test-equal 0 (loop initially (return 0)))
(test-equal 0 (loop repeat 2 finally (return 0)))
(test-equal 1 (loop with x = 0 initially (incf x) return x))
(test-equal 2
	    (loop with x = 0 for a from 0 below 3
		  initially (incf x) finally (return (incf x))))
(test-equal 3
	    (loop with x = 0 for a from 0 below 3
		  initially (incf x) (incf x) finally (return (incf x))))
(test-equal 3
	    (loop with x = 0 for a from 0 upto 3
		  initially (incf x) finally (incf x) (return (incf x))))
(test-equal 4
	    (loop with x = 0 for a from 0 upto 3
		  initially (incf x) (incf x) 
		  finally (incf x) (return (incf x))))
(test-equal 7
	    (loop with x = 0 for a from 0 below 3
		  do (incf x)
		  initially (incf x) (incf x) 
		  finally (incf x) (return (incf x))))

(test-equal (loop with x = 2
		  initially (setq x (* x 3))
		  for i below 3
		  initially (setq x (* x 5))
		  do (incf x i)
		  finally (return x)) 33)
(test-equal '(initially0 initially1 initially2 initially3
			 body0 body1 body0 body1
			 finally0 finally1 finally2 finally3)
	    (loop with x = '()
		  repeat 2
		  initially (push 'initially0 x)
		  finally (push 'finally0 x)
		  initially (push 'initially1 x)
		  finally (push 'finally1 x)
		  do (push 'body0 x)
		  finally (push 'finally2 x) (push 'finally3 x)
		  finally (return (reverse x))
		  initially (push 'initially2 x) (push 'initially3 x)
		  do (push 'body1 x)))

)

(begin-tests "do-clause"

(test-equal '(0 1 2)
	    (loop with i = 3
		  with stack = '()
		  do (when (zerop i) (loop-finish))
		  (decf i)
		  (push i stack)
		  finally (return stack)))

(test-equal '(0 1 2)
	    (loop with i = 3
		  with stack = '()
		  doing (when (zerop i) (loop-finish))
		  (decf i)
		  (push i stack)
		  finally (return stack)))
(test-equal 10 (loop with x = 10 do (return x)))
(test-equal 10 (loop with x = 10 doing (return x)))
(test-equal 2 (loop with x = 0 do (incf x) doing (incf x) (return x)))
(test-equal 2 (loop with x = 0 do (incf x) doing (incf x) do (return x)))
(test-equal 2 (loop with x = 0 do (incf x) (incf x) doing (return x)))
(test-equal 4 (loop with x = 0 do (incf x) (incf x) (incf x) 
		    doing (incf x) (return x)))
)
