
(begin-tests "termination-test-clauses"

(begin-tests "while/until"
(test-equal #f (loop with x = '(a b c d) while x do (pop x)))
(test-equal '(3 2 1 0)
	    (loop with stack = '() and x = '(0 1 2 3)
		  while (pair? x)
		  do (push (pop x) stack) finally (return stack)))
(test-equal '(3 2 1 0)
	    (loop with stack = '() and x = '(0 1 2 3)
		  until (null? x) do (push (pop x) stack) 
		  finally (return stack)))
(test-equal '(6 a b c d e f)
	    (let ((stack '(a b c d e f)))
	      (loop for item = (length stack) then (pop stack)
		    collect item
		    while (pair? stack))))
(test-equal '(3 5)
	    (loop for i fixnum from 3
		  when (oddp i) collect i
		  while (< i 5)))
(test-equal '(0 0 1 2 2 3 4 4 5 6)
	    (loop for a below 10
		  when (and (evenp a) a) collect it
		  while (< a 6)
		  collect a))
(test-equal '(0 0 1 2 2 3 4 4 5 6)
	    (loop for a below 10
		  when (and (evenp a) a) collect it
		  until (>= a 6)
		  collect a))
(test-equal '(0 0 0 1 1 2 2 2 3 3 4 4)
	    (loop for a below 10
		  when (and (evenp a) a) collect it
		  while (< a 6)
		  collect a
		  until (>= a 4)
		  collect a))

)

(begin-tests "repeat"
(test-equal 3 (loop with x = 0 repeat 3 do (incf x) finally (return x)))
(test-equal 1000 (loop repeat 1000 counting 1))
(test-equal #f (loop repeat 3))
(test-equal #f (loop repeat 0))
(test-equal '(#f #f)
	    (let ((body-flag #f))
	      (list-lr
	       (loop repeat 0 do (setq body-flag #t))
	       body-flag)))
(test-equal 1 (let ((x 0)) (loop repeat (incf x) sum x)))
(test-equal 4 (let ((x 1)) (loop repeat (incf x) sum x)))
(test-equal 9 (let ((x 2)) (loop repeat (incf x) sum x)))
(test-equal 16 (let ((x 3)) (loop repeat (incf x) sum x)))
(test-equal #f (loop repeat -15 return #t))
(test-equal '(#f #f)
	    (let ((body-flag #f))
	      (list-lr 
	       (loop repeat -10 do (setq body-flag #t))
	       body-flag)))
(test-equal '(1 2)
	    (let ((eval-count 0)
		  (loop-count 0))
	      (loop repeat (progn (incf eval-count) 2) do (incf loop-count))
	      (list-lr eval-count
		       loop-count)))
(test-equal '(1 0)
	    (let ((eval-count 0)
		  (loop-count 0))
	      (loop repeat (progn (incf eval-count) 0) do (incf loop-count))
	      (list-lr eval-count
		       loop-count)))
(test-equal '(1 0)
	    (let ((eval-count 0)
		  (loop-count 0))
	      (loop repeat (progn (incf eval-count) -100) do (incf loop-count))
	      (list-lr eval-count
		       loop-count)))
)

(begin-tests "always"
(test-equal #t (loop for i from 0 to 10 always (< i 11)))
(test-equal #t (loop for a in '() always (oddp a)))
(test-equal #f (loop for a in '(0 1 2) always (oddp a)))
(test-equal #t (loop for a in '(1 3 5) always (oddp a)))
(test-equal '(#f #f)
	    (let ((flag #f))
	      (list-lr 
	       (loop for i from 0 to 10 always (< i 5)
		     finally (setq flag #t) (return #t))
	       flag)))
(test-equal 'ok (loop for i below 3 always (numberp i) finally (return 'ok)))
(test-equal #t (loop repeat 3 always #t))
;;(handler-case (macroexpand ' (loop for i from 0 upto 10
;;                             always (integerp i)
;;                             collect i))
;;  (program-error () t)
;;  (error () nil)
;;  (:no-error (&rest rest) (declare (ignore rest)) nil))

(test-equal #t (loop for i below 4 always i))
)

(begin-tests "never"
(test-equal #t (loop for i from 0 to 10 never (> i 11)))
(test-equal #t (loop for a in '() never (oddp a)))
(test-equal #f (loop for a in '(0 1 2) never (oddp a)))
(test-equal #t (loop for a in '(1 3 5) never (evenp a)))
(test-equal #f (loop never #t finally (return #t)))
(test-equal '(#f #f)
	    (let ((flag #f))
	      (list-lr (loop for a below 3 never (oddp a)
			     finally (setq flag #t) (return #t))
		       flag)))
(test-equal 'ok (loop for i below 3 never (pair? i) finally (return 'ok)))
(test-equal #t (loop repeat 3 never #f))

;;(handler-case (macroexpand '(loop for i from 0 upto 10
;;                             never (integerp i)
;;                             append (list i)))
;;  (program-error () t)
;;  (error () nil)
;;  (:no-error (&rest rest) (declare (ignore rest)) nil))

(test-equal #f (loop for i below 4 never i))

)

(begin-tests "thereis"
(test-equal #f (loop for a in '(0 2 4) thereis (oddp a)))
(test-equal 11 (loop for i from 0 thereis (and (> i 10) i)))
(test-equal 'someone (loop thereis 'someone))
(test-equal 'got-here
	    (loop for i from 1 to 10
		  thereis (> i 11)
		  finally (return 'got-here)))
(test-equal '(#f 4)
	    (let ((count 0))
	      (list-lr
	       (loop for a below 10 for b in '(#f #f #f #f c)
		     always (< a 8)
		     never b
		     do (incf count))
	       count)))
(test-equal 'found-it! 
	    (loop for a in '(#f #f #f found-it! #f #f)
		  for b from 10 downto 0
		  never (< b 0)
		  thereis a))
(test-equal 4 (loop for i in '(1 2 3 4 5 6)
		    thereis (and (> i 3) i)))
(test-equal '(1 #f)
	    (let ((flag #f))
	      (list-lr
	       (loop for a below 3
		     thereis (and (oddp a) a)
		     finally (setq flag #t))
	       flag)))
(test-equal 'ok (loop for i below 3 thereis (pair? i) finally (return 'ok)))
(test-equal #f (loop repeat 3 thereis #f))
;;(handler-case (macroexpand '(loop for i from 0 upto 10
;;                             thereis (integerp i)
;;                             nconc (list i)))
;;  (program-error () t)
;;  (error () nil)
;;  (:no-error (&rest rest) (declare (ignore rest)) nil))

)

)

(begin-tests "name-clause"
(test-equal #t (loop named bar do (return-from bar #t)))
(test-equal #t (loop named outer 
		     do (loop named inner 
			      do (return-from outer #t)
			      do (return-from inner #f)
			      )))
)

(begin-tests "loop-finish"
(test-equal "loop-finish (fails because not yet correctly scoped)"
	    21
	    (loop for i from 10
		  sum i
		  do (when (= i 11) (loop-finish))))
)
