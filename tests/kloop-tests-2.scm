
(begin-tests "for-as-hash"

(test-equal 
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-key of table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-key of table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-keys of table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-keys of table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-key in table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-key in table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-keys in table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-keys in table do (push k s))
   (set-difference s '(k0 k1 k2))))

(test-equal "hask-key using hash-value"
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-key of table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-key of table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-keys of table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-keys of table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-key in table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-key in table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being each hash-keys in table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k being the hash-keys in table using (hash-value v)
	 do (push (list k v) s))
   (set-difference s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-value of table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-value of table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-values of table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-values of table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-value in table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-value in table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-values in table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-values in table do (push v s))
   (set-exclusive-or s '(v0 v1 v2))))

(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-value of table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-value of table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-values of table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-values of table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-value in table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))



(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-value in table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being each hash-values in table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for v being the hash-values in table using (hash-key k)
	 do (push (list k v) s))
   (set-exclusive-or s '((k0 v0) (k1 v1) (k2 v2)) :test equal)))



(test-equal "hash-table + destructuring (requires equal-hash 1)"
 '()
 (let ((table (make-hash-table :test 'equal))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v))
	 '((k0 k00) (k1 k11) (k2 k22)) '((v0 v00) (v1 v11) (v2 v22)))
   (loop for (k kk) being each hash-key of table do (push (list k kk) s))
   (set-exclusive-or s '((k0 k00) (k1 k11) (k2 k22)) :test equal)))

;;(test-equal
;; '()
;; (let ((table (make-hash-table :test 'equal))
;;       (s '()))
;;   (mapc (lambda (k v) (setf (gethash k table) v))
;;	 '((k0 k00) (k1 k11) (k2 k22)) '((v0 v00) (v1 v11) (v2 v22)))
;;   (loop :for (k kk) :being :each :hash-key :of table :using (hash-value (v vv))
;;	 do (push (list k kk v vv) s))
;;   (null (set-exclusive-or s
;;			   '((k0 k00 v0 v00) (k1 k11 v1 v11) (k2 k22 v2 v22))
;;			   :test equal)))
;;
;; (let ((table (make-hash-table :test 'equal))
;;       (s '()))
;;   (mapc (lambda (k v) (setf (gethash k table) v))
;;	 '((k0 k00) (k1 k11) (k2 k22)) '((v0 v00) (v1 v11) (v2 v22)))
;;   (loop :for (v vv) :being :each :hash-value :of table :using (hash-key (k kk))
;;	 do (push (list k kk v vv) s))
;;   (set-exclusive-or s
;;		     '((k0 k00 v0 v00) (k1 k11 v1 v11) (k2 k22 v2 v22))
;;		     :test equal)))


(test-equal
 '()
 (let ((table (make-hash-table))
       (s '()))
   (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
   (loop for k of-type symbol being each hash-key of table do (push k s))
   (set-exclusive-or s '(k0 k1 k2))))

(test-equal "hash-table + destructuring (requires equal-hash 2)"
 '()
 (let ((table (make-hash-table :test 'equal))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v))
	'((k0 k00) (k1 k11) (k2 k22)) '((v0 v00) (v1 v11) (v2 v22)))
  (loop for (k kk) of-type symbol being each hash-key of table
	do (push (list k kk) s))
  (set-exclusive-or s '((k0 k00) (k1 k11) (k2 k22)) :test equal)))

(test-equal "hash-table + destructuring (requires equal-hash 3)"
 '()
 (let ((table (make-hash-table :test 'equal))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v))
	'((k0 k00) (k1 k11) (k2 k22)) '((v0 v00) (v1 v11) (v2 v22)))
  (loop for (k kk) of-type (symbol symbol) being each hash-key of table
	do (push (list k kk) s))
  (set-exclusive-or s '((k0 k00) (k1 k11) (k2 k22)) :test equal)))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(0 1 2) '(v0 v1 v2))
  (loop for k fixnum being each hash-key of table do (push k s))
  (set-exclusive-or s '(0 1 2))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(0 1 2) '(v0 v1 v2))
  (loop for k of-type fixnum being each hash-key of table do (push k s))
  (set-exclusive-or s '(0 1 2))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(0.0 1.0 2.0) '(v0 v1 v2))
  (loop for k float being each hash-key of table do (push k s))
  (set-exclusive-or s '(0.0 1.0 2.0))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(0.0 1.0 2.0) '(v0 v1 v2))
  (loop for k of-type float being each hash-key of table do (push k s))
  (set-exclusive-or s '(0.0 1.0 2.0))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(0.0 1.0 2.0) '(v0 v1 v2))
  (loop for k t being each hash-key of table do (push k s))
  (set-exclusive-or s '(0.0 1.0 2.0))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(0.0 1.0 2.0) '(v0 v1 v2))
  (loop for k of-type t being each hash-key of table do (push k s))
  (set-exclusive-or s '(0.0 1.0 2.0))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(#\a #\b #\c) '(v0 v1 v2))
  (loop for k of-type character being each hash-key of table do (push k s))
  (set-exclusive-or s '(#\a #\b #\c))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
  (loop for v t being each hash-value of table do (push v s))
  (set-exclusive-or s '(v0 v1 v2))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
  (loop for v of-type t being each hash-value of table do (push v s))
  (set-exclusive-or s '(v0 v1 v2))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
  (loop for v of-type symbol being each hash-value of table do (push v s))
  (set-exclusive-or s '(v0 v1 v2))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(0 1 2))
  (loop for v fixnum being each hash-value of table do (push v s))
  (set-exclusive-or s '(0 1 2))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(0 1 2))
  (loop for v of-type (integer 0 2) being each hash-value of table
	do (push v s))
  (set-exclusive-or s '(0 1 2))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(0.0 1.0 2.0))
  (loop for v float being each hash-value of table do (push v s))
  (set-exclusive-or s '(0.0 1.0 2.0))))

(test-equal
 '()
 (let ((table (make-hash-table))
      (s '()))
  (mapc (lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(#\a #\b #\c))
  (loop for v of-type base-char being each hash-value of table do (push v s))
  (set-exclusive-or s '(#\a #\b #\c))))

)
