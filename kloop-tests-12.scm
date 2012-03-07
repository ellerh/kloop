

(begin-tests "misc"
(test-equal 4 
	    (loop for (item . x) of-type (t . fixnum) 
		  in '((A . 1) (B . 2) (C . 3))
		  unless (eq? item 'B) sum x))
(test-equal '((a b c d) (b c d) (c d) (d))
	    (loop for sublist on '(a b c d) collect sublist))
(test-equal '(1 2 3) (loop for (item) on '(1 2 3) collect item))
(test-equal '(1 11 21 31 41)
	    (loop for item = 1 then (+ item 10)
		  for iteration from 1 to 5
		  collect item))
(test-equal '(((0 0) (0 1)) ((1 0) (1 1)) ((2 0) (2 1)))
	    (loop for i below 3 collecting (loop for j below 2 collecting (list i j))))
(test-equal 0 (loop for i from -10 upto 0 maximizing i))
(test-equal '(0 -10)
	    (loop for i from -10 upto 0 maximizing i into max minimizing i into min
		  finally (return (list max min))))
(test-equal '(#\B #\D #\F)
	    (loop for c across "aBcDeFg" 
		  when (and (char-upper-case? c) c) collecting it))
(test-equal '(0 1 2)
	    (loop named my-loop for i below 3 collect i into x
		  finally (return-from my-loop x)))
(test-equal '(0 1 2)
	    (loop named nil for i below 3 collect i into x
		  finally (return-from nil x)))
;;(test-expect-fail "return-from nil")
;;(test-equal "return-from nil"
;;	    '(0 1 2)
;;	    (loop for i below 3 collect i into x
;;		  finally (return-from nil x)))
(test-equal '(0 1 2)
	    (loop for i below 3 collect i into x
		  finally (return x)))
(test-equal '((10 1 j 100) (9 2 k 99) (8 3 l 98) (7 4 m 97) (6 5 n 96)
	      (5 6 o 95)   (4 7 p 94) (3 8 q 93) (2 9 r 92) (1 10 s 91))
	    (loop for a from 10 above 0
		  for b in '(1 2 3 4 5 6 7 8 9 10)
		  for c on '(j k l m n o p q r s)
		  for d = 100 then (- d 1)
		  collect (list a b (car c) d)))

(test-equal '(1000 10 1 j 100)
	    (loop with e = 0
		  for a from 10 above 0
		  for b in '(1 2 3 4 5 6 7 8 9 10)
		  for c on '(j k l m n o p q r s)
		  for d = 100 then (- d 1)
		  append (list a b (car c) d) into values
		  initially (setq e 1000)
		  repeat 1
		  finally (return (cons e values))))
(test-equal '(1000 10 1 j 100 9 2 k 99)
	    (loop with e = 0
		  for a from 10 above 0
		  for b in '(1 2 3 4 5 6 7 8 9 10)
		  for c on '(j k l m n o p q r s)
		  for d = 100 then (- d 1)
		  append (list a b (car c) d) into values
		  initially (setq e 1000)
		  repeat 2
		  finally (return (cons e values))))

(test-equal '(0 10 20 30 40 50 60 70 80 90 100)
	    (loop for a from 0 upto 100 by 2
		  repeat 1000
		  when (zerop (mod a 10)) collect a))


)

(begin-tests "it"
(test-equal '(x 0 y 0 z 0)
	    (let ((it '0))
	      (loop for a in '(#f x y #f z) when a collect it and collect it)
	      ))
  
(test-equal '(x 0 0 y 0 0 z 0 0)
	    (let ((it '0))
	      (loop for a in '(x #f y #f z #f)
		    if a collect it end
		    collect it)))
)
