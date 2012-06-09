

(begin-tests "list-accumulation-clauses"

(begin-tests "collect"

(test-equal '(0 1 2) (loop for a from 0 below 3 collect a))
(test-equal '(0 1 2) (loop for a from 0 below 3 collecting a))
(test-equal '(0 1 2 3 4) (loop for a in '(#f 0 #f #f 1 2 #f 3 #f 4)
			       when a collect it))
(test-equal '(0 1 2 3 4) (loop for a in '(#f 0 #f #f 1 2 #f 3 #f 4)
			       when a collecting it))
(test-equal '(0 1 2 3 4)
	    (loop for a in '(#f 0 #f #f 1 2 #f 3 #f 4)
		  when a collect it into bag
		  finally (return bag)))
(test-equal '(0 1 2 3 4)
	    (loop for a in '(#f 0 #f #f 1 2 #f 3 #f 4)
		  when a collecting it into bag
		  finally (return bag)))
(test-equal '((1 3 5 7 9) (0 2 4 6 8))
	    (loop for a below 10
		  if (oddp a) collect a into odd else collect a into even end
		  finally (return (list odd even))))
(test-equal '(0 2 1 0 1 1 0 2 0)
	    (loop for a below 3
		  for b on '(2 1 0)
		  collecting a
		  appending b))



(test-equal 15 (loop for i of-type fixnum in '(1 2 3 4 5) sum i))
(test-equal 22.4 
	    (let ((series '(1.2 4.3 5.7)))
	      (loop for v in series sum (* 2.0 v))))
(test-equal '((1 3 5 7 9) 25)
	    (loop for a below 10
		  if (oddp a) collect a into odd and sum a into sum
		  finally (return (list odd sum))))

(test-equal '((1 3 5 7 9) 25 (0 2 4 6 8) 20)
	    (loop for a below 10
		  if (oddp a) collect a into odd and sum a into odd-sum
		  else collect a into even and sum a into even-sum
		  end
		  finally (return (list odd odd-sum even even-sum))))

(test-equal '(bird turtle horse cat)
	    (loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
		  when (symbolp i) collect i))
(test-equal '(0 0 1 1 2 2)
	    (loop for i below 3
		  for j upto 2
		  collecting i
		  collecting j))
(test-equal '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0)
	    (loop for a from -10 upto 0
		  collecting a))
(test-equal #f (loop for a from -10 upto 0
		     collecting a into list))    ;; not return automatically


)

(begin-tests "append"

(test-equal '((0 1 2) (0) (1) (2))
	     (let* ((zero (list 0))
		    (one (list 1))
		    (two (list 2))
		    (list (list zero one two)))
	       (list-lr (loop for a in list append a)
			zero 
			one 
			two )))
(test-equal '(1 2) (loop for a in '(#f (1) #f (2)) when a append a))
(test-equal '(1 2) (loop for a in '(#f (1) #f (2)) when a appending a))
(test-equal #f (loop for a in '(#f (1) #f (2)) when a append a into x))
(test-equal #f (loop for a in '(#f (1) #f (2)) when a appending a into x))
(test-equal '(1 2) (loop for a in '(#f (1) #f (2)) when a append a into x
			 finally (return x)))
(test-equal '(1 2) (loop for a in '(#f (1) #f (2)) when a appending a into x
			 finally (return x)))
(test-equal '(1 2) (loop for a in '(#f (1) #f (2)) when a append it))
(test-equal '(1 2) (loop for a in '(#f (1) #f (2)) when a appending it))
(test-equal '(1 2 3 4 3 4)
	    (loop for a on (list 0 1 2 3 4) when (oddp (car a)) append a))
(test-equal '(1 2 3 4 3 4)
	    (loop for a on (list 0 1 2 3 4) when (oddp (car a)) appending a))
(test-equal '(a b (c)) (loop for x in '((a) (b) ((c))) append x))

)

(begin-tests "nconc"

(test-equal '((0 1 2 3) ((0 1 2 3) (1 2 3) (2 3) (3)))
	    (let ((l (list (list 0) (list 1) (list 2) (list 3))))
	      (list-lr (loop for a in l nconc a)
		       l)))
(test-equal '((0 1 2 3) ((0 1 2 3) (1 2 3) (2 3) (3)))
	    (let ((l (list (list 0) (list 1) (list 2) (list 3))))
	      (list-lr (loop for a in l nconcing a)
		       l)))
(test-equal '((0 1 2 3) (#f (0 1 2 3) #f #f (1 2 3) (2 3) #f (3) #f))
	    (let ((l (list #f (list 0) #f #f (list 1) (list 2) 
			   #f (list 3) #f)))
	      (list-lr (loop for a in l when a nconc it)
		       l)))

(test-equal '((0 1 2 3) (#f (0 1 2 3) #f #f (1 2 3) (2 3) #f (3) #f))
	    (let ((l (list #f (list 0) #f #f (list 1) (list 2) 
			   #f (list 3) #f)))
	      (list-lr (loop for a in l when a nconcing it)
		       l)))

(test-equal #f (loop for a in (list (list (list 0) (list 1) (list 2) (list 3)))
		     nconc a into x))
(test-equal #f (loop for a in (list (list (list 0) (list 1) (list 2) (list 3)))
		     nconcing a into x))

(test-equal '((0 1 2 3) ((0 1 2 3) (1 2 3) (2 3) (3)))
	    (let ((l (list (list 0) (list 1) (list 2) (list 3))))
	      (list-lr (loop for a in l nconc a into x finally (return x))
		       l)))

(test-equal '((0 1 2 3) ((0 1 2 3) (1 2 3) (2 3) (3)))
	    (let ((l (list (list 0) (list 1) (list 2) (list 3))))
	      (list-lr (loop for a in l nconcing a into x finally (return x))
		       l)))


(test-equal '(a (c))
	    (loop for i upfrom 0 as x in '(a b (c))
		  nconc (if (evenp i) (list x) '())))

)

(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in '(0 3 6)
		  for b in '((1) (4) (7))
		  for c in (copy-tree '((2) (5) (8)))
		  collecting a
		  appending b
		  nconcing c))
(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in '(0 3 6)
		  for b in (copy-tree '((1) (4) (7)))
		  for c in (list (list 2) (list 5) (list 8))
		  collecting a
		  nconcing b
		  appending c))
(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in '((0) (3) (6))
		  for b in (copy-tree '((1) (4) (7)))
		  for c in '(2 5 8)
		  appending a
		  nconcing b
		  collecting c))
(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in '((0) (3) (6))
		  for b in '(1 4 7)
		  for c in (copy-tree '((2) (5) (8)))
		  appending a
		  collecting b
		  nconcing c))
(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in (copy-tree '((0) (3) (6)))
		  for b in '(1 4 7)
		  for c in '((2) (5) (8))
		  nconcing a
		  collecting b
		  appending c))
(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in (copy-tree '((0) (3) (6)))
		  for b in '((1) (4) (7))
		  for c in '(2 5 8)
		  nconcing a
		  appending b
		  collecting c))
(test-equal '(0 1 2 3 4 5 6 7 8 9 10)
	    (loop for a in '(0 6)
		  for b in '((1 2 3) (7 8 9))
		  for c in (copy-tree '((4 5) (10)))
		  collect a
		  append b
		  nconc c))
(test-equal '() 
	    (loop for a in '()
		  for b in '((1 2 3) (7 8 9))
		  for c in (copy-tree '((4 5) (10)))
		  collect a
		  append b
		  nconc c))
(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in '(0 3 6)
		  for b in '((1) (4) (7))
		  for c in (copy-tree '((2) (5) (8)))
		  collecting a into list
		  appending b into list
		  nconcing c into list
		  finally (return list)))
(test-equal '(0 1 2 3 4 5 6 7 8)
	    (loop for a in '(0 3 6)
		  for b in '(1 4 7)
		  for c in (copy-tree '((2) (5) (8)))
		  collect a collect b nconc c))

)
