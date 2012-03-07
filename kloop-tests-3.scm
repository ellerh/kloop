
(begin-tests "for-as and preposition"

(test-equal '(2 1 0)
	    (let ((s '()))
	      (loop for a from 1 upto 3 and x = 0 then a
		    do (push x s))
	      s))
(test-equal '(3 2 1 0)
	    (let ((s '()))
	      (loop for a from 0 upto 3
		    for x = 0 then a
		    do (push x s))
	      s))
(test-equal '((2 2 2) (1 1 1) (0 0 0))
	    (let ((i 4)
		  (s '()))
	      (loop for a = 0 then (+ 1 a)
		    for b = 0 then a
		    for c = 0 then b
		    do (when (zerop (decf i)) (return))
		    (push (list a b c) s))
	      s))
(test-equal '((3 2 1) (2 1 0) (1 0 0) (0 0 0))
	    (let ((i 5)
		  (s '()))
	      (loop for a = 0 then (+ 1  a) and b = 0 then a and c = 0 then b
		    do (when (zerop (decf i)) (return))
		    (push (list a b c) s))
	      s))

(test-equal '(3 2 1 0)
	    (let ((s '())) 
	      (loop for a in '(0 1 2 3) for x = a do (push x s)) s))

(test-equal '(2 1 0 100)
	    (let ((s '())) (loop for a in '(0 1 2 3) and x = 100 then a
				 do (push x s)) s))
(test-equal '(3 2 1 0)
	    (let ((s '())) (loop for a on '(0 1 2 3) for x = (car a)
				 do (push x s)) s))
(test-equal '(2 1 0 100)
	    (let ((s '())) (loop for a on '(0 1 2 3) and x = 100 then (car a)
				 do (push x s)) s))
(test-equal '(3 2 1 0)
	    (let ((s '())) (loop for a across '#(0 1 2 3) for x = a
				 do (push x s)) s))
(test-equal '(2 1 0 100)
	    (let ((s '())) (loop for a across '#(0 1 2 3) and x = 100 then a
				 do (push x s)) s))
(test-equal '((1 nil) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))
	    (loop for x from 1 to 10 
		  for y = 'nil then x 
		  collect (list x y)))
(test-equal '((1 nil) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))
	    (loop for x from 1 to 10 
		  and y = 'nil then x 
		  collect (list x y)))
(test-equal 280 (loop for a upfrom 0 upto 9
		      and b downfrom 9 downto 0
		      and c from 0 to 9
		      and d from 10 above 0
		      and e below 10
		      and f to 9
		      summing (+ a b c d e f)))
(test-equal '((1 0 -1 -2 -3 -4) (2 2 2 2 2 2) (3 3 3 3 3 3) (4 4 4 4 4 4)
	      (5 5 5 5 5 5) (6 6 6 6 6 6) (7 7 7 7 7 7) (8 8 8 8 8 8)
	      (9 9 9 9 9 9))
	    (loop for a from 1 upto 9
		  as b = 0 then a
		  as c = -1 then b
		  as d = -2 then c
		  as e = -3 then d
		  as f = -4 then e
		  collecting (list a b c d e f)))
(test-equal '((1 0 -1 -2 -3 -4) (2 1 0 -1 -2 -3) (3 2 1 0 -1 -2) (4 3 2 1 0 -1)
	      (5 4 3 2 1 0) (6 5 4 3 2 1) (7 6 5 4 3 2) (8 7 6 5 4 3) 
	      (9 8 7 6 5 4))
	    (loop for a from 1 upto 9
		  and b = 0 then a
		  and c = -1 then b
		  and d = -2 then c
		  and e = -3 then d
		  and f = -4 then e
		  collecting (list a b c d e f)))
(test-equal '((1 0 -1 -2 -3 -4) (9 8 7 6 5 4) (2 1 0 -1 -2 -3) (8 9 8 7 6 5)
	      (3 2 1 0 -1 -2)
	      (7 8 9 8 7 6) (4 3 2 1 0 -1) (6 7 8 9 8 7) (5 4 3 2 1 0) 
	      (5 6 7 8 9 8)
	      (6 5 4 3 2 1) (4 5 6 7 8 9) (7 6 5 4 3 2) (3 4 5 6 7 8)
	      (8 7 6 5 4 3)
	      (2 3 4 5 6 7) (9 8 7 6 5 4) (1 2 3 4 5 6))
	    (loop for a from 1 upto 9
		  and b = 0 then a
		  and c = -1 then b
		  and d = -2 then c
		  and e = -3 then d
		  and f = -4 then e
		  for i from 9 downto 1
		  and j = 8 then i
		  and k = 7 then j
		  and l = 6 then k
		  and m = 5 then l
		  and n = 4 then m
		  collecting (list a b c d e f)
		  collecting (list i j k l m n)))

(test-equal '(2 1)
	    (let ((s '()))
	      (loop for a on (progn (push 1 s) '(0 1 2))
		    and b across (progn (push 2 s) "abc"))
	      s))

)

(begin-tests "ambiguous cases"
(test-equal '((0 5) (1 4) (2 3) (3 2) (4 1) (5 0))
	    (let ((a 5))
	      (loop for a from 0 upto 5
		    and b from a downto 0
		    collect (list a b))))

(test-equal '((0 outer))
	    (let ((a 'outer))
	      (loop for a from 0 upto 5
		    and b in (list a)
		    collect (list a b))))

(test-equal '((0 a) (1 b) (2 c))
	    (let ((b 0))
	      (loop for a from b upto 5
		    and b in '(a b c)
		    collecting (list a b))))
)


