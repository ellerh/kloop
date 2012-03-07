
(define (glength vector)
  (cond ((vector? vector) (vector-length vector))
	((string? vector) (string-length vector))
	((bytevector? vector) (bytevector-length vector))
	(#t (error 'glength "not-a-vector" vector))))

(define (gvref vector i)
  (cond ((vector? vector) (vector-ref vector i))
	((string? vector) (string-ref vector i))
	((bytevector? vector) (bytevector-u8-ref vector i))
	(#t (error 'gvref "not-a-vector" vector))))
