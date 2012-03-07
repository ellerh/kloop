(begin-tests "numeric-accumulation-clauses 2"


(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing it fixnum))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing it of-type fixnum))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a summing it float))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a summing it of-type float))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing it of-type number))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing it of-type (integer 0)))

(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a fixnum))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a of-type fixnum))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a summing a float))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a summing a of-type float))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a of-type number))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a of-type (integer 0)))

(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a into sum fixnum finally (return sum)))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a into sum of-type fixnum 
		     finally (return sum)))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a summing a into sum float finally (return sum)))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a summing a into sum of-type float 
		       finally (return sum)))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a into sum of-type number 
		     finally (return sum)))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a summing a into sum of-type (integer 0) 
		     finally (return sum)))


(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum it fixnum))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum it of-type fixnum))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a sum it float))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a sum it of-type float))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum it of-type number))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum it of-type (integer 0)))

(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a fixnum))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a of-type fixnum))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a sum a float))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a sum a of-type float))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a of-type number))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a of-type (integer 0)))

(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a into sum fixnum finally (return sum)))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a into sum of-type fixnum 
		     finally (return sum)))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a sum a into sum float finally (return sum)))
(test-equal 10.0 (loop for a in '(#f 1.0 #f 2.0 #f 3.0 #f 4.0)
		       when a sum a into sum of-type float 
		       finally (return sum)))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a into sum of-type number 
		     finally (return sum)))
(test-equal 10 (loop for a in '(#f 1 #f 2 #f 3 #f 4)
		     when a sum a into sum of-type (integer 0) 
		     finally (return sum)))

(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a fixnum))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a of-type fixnum))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a of-type integer))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a of-type (integer 0)))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a of-type number))

(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a into x fixnum finally (return x)))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a into x of-type fixnum finally (return x)))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a into x of-type integer finally (return x)))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a into x of-type (integer 0) finally (return x)))
(test-equal 7 (loop for a in '(#f a #f b #f c #f d e #f f g #f #f #f #f)
		    counting a into x of-type number finally (return x)))

(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) maximize a fixnum))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) maximize a of-type fixnum))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximize a float))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximize a of-type float))
(test-equal 99.0 (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
		       maximize a of-type real))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) maximize a of-type (integer 0)))


(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) maximize a into max fixnum
		     finally (return max)))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3)
		     maximize a into max of-type fixnum
		     finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximize a into max float finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximize a into max of-type float finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
		       maximize a into max of-type real finally (return max)))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3)
		     maximize a into max of-type (integer 0)
		     finally (return max)))

(test-equal 99 (loop for a in '(3 #f 5 8 #f 0 #f 7 7 99 3) 
		     when a maximize it fixnum))
(test-equal 99 (loop for a in '(#f 3 #f 5 #f 8 0 7 7 #f 99 #f 3)
		     when a maximize it of-type fixnum))
(test-equal 99.0 (loop for a in '(3.0 #f 5.0 8.0 0.0 #f #f #f #f 7.0 
				      #f 7.0 99.0
				      #f 3.0 #f #f #f)
		       when a maximize it float))
(test-equal 99.0 (loop for a in '(#f #f #f #f #f 3.0 #f 5.0 8.0 0.0
				     #f #f #f 7.0 7.0 #f #f 99.0 3.0)
		       when a maximize it of-type float))
(test-equal 99.0 (loop for a in '(3.0 5.0 #f #f 2.2 #f #f 8.0 0
				      #f #f 3/5 #f #f 7.0 7 99 3.0)
		       when a maximize it of-type real))
(test-equal 99 (loop for a in '(3 #f #f 5 8 0 #f #f 7 7 99 #f #f 3)
		     when a maximize a of-type (integer 0)))

(test-equal 99 (loop for a in '(3 #f 5 8 #f 0 #f 7 7 99 3)
		     when a maximize it into max fixnum
		     finally (return max)))
(test-equal 99 (loop for a in '(#f 3 #f 5 #f 8 0 7 7 #f 99 #f 3)
		     when a 
		     maximize it into max of-type fixnum finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 #f 5.0 8.0 0.0 #f #f #f #f 7.0 
				      #f 7.0 99.0
				      #f 3.0 #f #f #f)
		       when a maximize it into max float finally (return max)))
(test-equal 99.0 (loop for a in '(#f #f #f #f #f 3.0 #f 5.0 8.0 0.0
				     #f #f #f 7.0 7.0 #f #f 99.0 3.0)
		       when a maximize it into max of-type float finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 #f #f 2.2 #f #f 8.0 0
				      #f #f 3/5 #f #f 7.0 7 99 3.0)
		       when a 
		       maximize it into max of-type real finally (return max)))
(test-equal 99 (loop for a in '(3 #f #f 5 8 0 #f #f 7 7 99 #f #f 3)
		     when a maximize it into max of-type (integer 0)
		     finally (return max)))



(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) maximizing a fixnum))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) maximizing a of-type fixnum))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximizing a float))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximizing a of-type float))
(test-equal 99.0 (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
		       maximizing a of-type real))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) 
		     maximizing a of-type (integer 0)))


(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) 
		     maximizing a into max fixnum
		     finally (return max)))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3) 
		     maximizing a into max of-type fixnum
		     finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximizing a into max float finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
		       maximizing a into max of-type float 
		       finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
		       maximizing a into max of-type real 
		       finally (return max)))
(test-equal 99 (loop for a in '(3 5 8 0 7 7 99 3)
		     maximizing a into max of-type (integer 0)
		     finally (return max)))

(test-equal 99 (loop for a in '(3 #f 5 8 #f 0 #f 7 7 99 3) when a 
		     maximizing it fixnum))
(test-equal 99 (loop for a in '(#f 3 #f 5 #f 8 0 7 7 #f 99 #f 3)
		     when a maximizing it of-type fixnum))
(test-equal 99.0 (loop for a in '(3.0 #f 5.0 8.0 0.0 #f #f #f #f 
				      7.0 #f 7.0 99.0
				      #f 3.0 #f #f #f)
		       when a maximizing it float))
(test-equal 99.0 (loop for a in '(#f #f #f #f #f 3.0 #f 5.0 8.0 0.0
				     #f #f #f 7.0 7.0 #f #f 99.0 3.0)
		       when a maximizing it of-type float))
(test-equal 99.0 (loop for a in '(3.0 5.0 #f #f 2.2 #f #f 8.0 0
				      #f #f 3/5 #f #f 7.0 7 99 3.0)
		       when a maximizing it of-type real))
(test-equal 99 (loop for a in '(3 #f #f 5 8 0 #f #f 7 7 99 #f #f 3)
		     when a maximizing a of-type (integer 0)))

(test-equal 99 (loop for a in '(3 #f 5 8 #f 0 #f 7 7 99 3)
		     when a maximizing it into max fixnum
		     finally (return max)))
(test-equal 99 (loop for a in '(#f 3 #f 5 #f 8 0 7 7 #f 99 #f 3)
		     when a maximizing it into max of-type fixnum 
		     finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 #f 5.0 8.0 0.0 #f #f #f #f
				      7.0 #f 7.0 99.0
				      #f 3.0 #f #f #f)
		       when a maximizing it into max float 
		       finally (return max)))
(test-equal 99.0 (loop for a in '(#f #f #f #f #f 3.0 #f 5.0 8.0 0.0
				     #f #f #f 7.0 7.0 #f #f 99.0 3.0)
		       when a maximizing it into max of-type float 
		       finally (return max)))
(test-equal 99.0 (loop for a in '(3.0 5.0 #f #f 2.2 #f #f 8.0 0
				      #f #f 3/5 #f #f 7.0 7 99 3.0)
		       when a maximizing it into max of-type real 
		       finally (return max)))
(test-equal 99 (loop for a in '(3 #f #f 5 8 0 #f #f 7 7 99 #f #f 3)
		     when a maximizing it into max of-type (integer 0)
		     finally (return max)))


(test-equal 3 (loop for a in '(3 5 8 4 7 7 99 3) minimize a fixnum))
(test-equal 3 (loop for a in '(3 5 8 4 7 7 99 3) minimize a of-type fixnum))
(test-equal 3.0 (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) minimize a float))
(test-equal 3.0 (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) 
		      minimize a of-type float))
(test-equal 3.0 (loop for a in '(5.0 8 7 3 7.0 3.0 99.0 1000) 
		      minimize a of-type real))
(test-equal 5 (loop for a in '(6 5 8 7 7 99) minimize a of-type (integer 0)))

(test-equal 3 (loop for a in '(5 8 4 7 7 99 3) minimize a into min fixnum
		    finally (return min)))
(test-equal 3 (loop for a in '(5 8 4 7 7 99 3) 
		    minimize a into min of-type fixnum
		    finally (return min)))
(test-equal 3.0 (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0) 
		      minimize a into min float
		      finally (return min)))
(test-equal 3.0 (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0)
		      minimize a into min of-type float finally (return min)))
(test-equal 3.0 (loop for a in '(5.0 8 4.0 31/3 7.0 7 99.0 3.0)
		      minimize a into min of-type real finally (return min)))
(test-equal 5 (loop for a in '(6 5 8 7 7 99) 
		    minimize a into min of-type (integer 0)
		    finally (return min)))

(test-equal 3 (loop for a in '(#f 5 8 #f #f 7 7 #f 99 3) 
		    when a minimize it fixnum))
(test-equal 3 (loop for a in '(#f 5 8 #f #f 7 7 #f 99 3)
		    when a minimize it of-type fixnum))
(test-equal 3.0 (loop for a in '(#f 5.0 8.0 #f #f 7.0 7.0 #f 99.0 3.0)
		      when a minimize it float))
(test-equal 3.0 (loop for a in '(#f 5.0 8.0 #f #f 7.0 7.0 #f 99.0 3.0)
		      when a minimize it of-type float))
(test-equal 3.0 (loop for a in '(#f 5.0 8.0 #f #f 7.0 7.0 #f 99.0 3.0)
		    when a minimize it of-type real))
(test-equal 3 (loop for a in '(#f 5 8 #f #f 7 7 #f 99 3)
		    when a minimize it of-type (integer 0)))
(test-equal -99 (loop for a in '(#f -5 8 #f #f 7 7 #f -99 3)
		      when a minimize it of-type (integer)))


(test-equal 3 (loop for a in '(3 5 8 4 7 7 99 3) minimizing a fixnum))
(test-equal 3 (loop for a in '(3 5 8 4 7 7 99 3) minimizing a of-type fixnum))
(test-equal 3.0 (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) minimizing a float))
(test-equal 3.0 (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) 
		      minimizing a of-type float))
(test-equal 3.0 (loop for a in '(5.0 8 7 3 7.0 3.0 99.0 1000)
		      minimizing a of-type real))
(test-equal 5 (loop for a in '(6 5 8 7 7 99) minimizing a of-type (integer 0)))

(test-equal 3 (loop for a in '(5 8 4 7 7 99 3) minimizing a into min fixnum
		    finally (return min)))
(test-equal 3 (loop for a in '(5 8 4 7 7 99 3)
		    minimizing a into min of-type fixnum
		    finally (return min)))
(test-equal 3.0 (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0)
		      minimizing a into min float
		      finally (return min)))
(test-equal 3.0 (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0)
		      minimizing a into min of-type float 
		      finally (return min)))
(test-equal 3.0 (loop for a in '(5.0 8 4.0 31/3 7.0 7 99.0 3.0)
		      minimizing a into min of-type real finally (return min)))
(test-equal 5 (loop for a in '(6 5 8 7 7 99) 
		    minimizing a into min of-type (integer 0)
		    finally (return min)))

(test-equal 3 (loop for a in '(#f 5 8 #f #f 7 7 #f 99 3) 
		    when a minimizing it fixnum))
(test-equal 3 (loop for a in '(#f 5 8 #f #f 7 7 #f 99 3)
		    when a minimizing it of-type fixnum))
(test-equal 3.0 (loop for a in '(#f 5.0 8.0 #f #f 7.0 7.0 #f 99.0 3.0)
		      when a minimizing it float))
(test-equal 3.0 (loop for a in '(#f 5.0 8.0 #f #f 7.0 7.0 #f 99.0 3.0)
		      when a minimizing it of-type float))
(test-equal 3.0 (loop for a in '(#f 5.0 8.0 #f #f 7.0 7.0 #f 99.0 3.0)
		      when a minimizing it of-type real))
(test-equal 3 (loop for a in '(#f 5 8 #f #f 7 7 #f 99 3)
		    when a minimizing it of-type (integer 0)))
(test-equal -99 (loop for a in '(#f -5 8 #f #f 7 7 #f -99 3)
		      when a minimizing it of-type (integer)))

(test-equal 'ok (loop for i from 0 upto 10 summing i finally (return 'ok)))
(test-equal 'ok (loop for i in '(#f #f 3 #f 5 #f 6)
		      counting i finally (return 'ok)))
(test-equal 'ok (loop for i in '(#f #f 3 #f 5 #f 6)
		      when i maximizing it finally (return 'ok)))
(test-equal 'ok (loop for i in '(#f #f 3 #f 5 #f 6)
		      when i minimizing it finally (return 'ok)))


)
