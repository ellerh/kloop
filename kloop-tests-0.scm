
(define-syntax pop
  (syntax-rules ()
    ((_ place)
     (let ((top (car place)))
       (set! place (cdr place))
       top))))

(define-syntax push
  (syntax-rules ()
    ((_ value place)
     (let ((l (cons value place)))
       (set! place l)
       l))))

(define-syntax incf
  (syntax-rules ()
    ((_ place)
     (incf place 1))
    ((_ place n)
     (let ((new (+ place n)))
       (set! place new)
       new))))

(define-syntax decf
  (syntax-rules ()
    ((_ place) (incf place -1))
    ((_ place n) (incf place (- n)))))

(define-syntax multiple-value-list
  (syntax-rules ()
    ((_ exp)
     (call-with-values (lambda () exp) list))))

(define-syntax flet 
  (syntax-rules ()
    ((_ ((fun (vars ...) fbody ...) ...) lbody ...)
     (let ((fun (lambda (vars ...) fbody ...)) ...)
       lbody ...))))

(define-syntax progn
  (syntax-rules ()
    ((_ forms ...) (begin forms ...))))

(define-syntax setq 
  (syntax-rules ()
    ((_ var val) 
     (let ((tmp val))
       (set! var tmp)
       tmp))))

(define zerop zero?)
(define oddp odd?)
(define evenp even?)
(define numberp number?)
(define floatp flonum?)
(define symbolp symbol?)

(define-syntax make-hash-table
  (syntax-rules (:test equal quote)
    ((_) (make-eqv-hashtable))
    ((_ :test (quote equal))
     (make-hashtable equal-hash equal?))))

(define mapc for-each)

(define (reduce fun init list) (fold-left fun init list))

(define (set-difference-eqv set1 set2)
  (reduce (lambda (set e) (remv e set))
	  set1 set2))

(define (set-difference-equal set1 set2)
  (reduce (lambda (set e) (remove e set))
	  set1 set2))

(define-syntax set-difference 
  (syntax-rules (:test equal)
    ((_ set1 set2) (set-difference-eqv set1 set2))
    ((_ set1 set2 :test equal) (set-difference-equal set1 set2))))

(define (set-exclusive-or-eqv set1 set2)
  (reduce (lambda (set e) 
	    (cond ((and (memv e set1) (memv e set2))
		   set)
		  (#t (cons e set))))
	  '() (append set1 set2)))

(define (set-exclusive-or-equal set1 set2)
  (reduce (lambda (set e) 
	    (cond ((and (member e set1) (member e set2))
		   set)
		  (#t (cons e set))))
	  '() (append set1 set2)))

(define-syntax set-exclusive-or
  (syntax-rules (:test equal)
    ((_ set1 set2) (set-exclusive-or-eqv set1 set2))
    ((_ set1 set2 :test equal) (set-exclusive-or-equal set1 set2))))

(define-syntax setf 
  (syntax-rules (gethash)
    ((_ (gethash key tab) value)
     (hashtable-set! tab key value))))

;; like LIST but enforce left-to-right evaluation.
(define-syntax list-lr
  (syntax-rules ()
    ((_) '())
    ((_ x y ...) (let* ((tmp x)) (cons tmp (list-lr y ...))))))

(define (copy-tree x)
  (cond ((pair? x) 
	 (cons (copy-tree (car x))
	       (copy-tree (cdr x))))
	(#t x)))

(define random-state 0)
(define (random modulo)
  (set! random-state (mod (+ random-state 1) #xffffff))
  (mod random-state modulo))

(define-syntax begin-tests
  (syntax-rules ()
    ((_ name body ...)
     (begin (test-begin name)
	    body ...
	    (test-end name)))))
