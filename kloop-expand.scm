;;; kloop-expand.scm --- the expander function for the loop macro
;;
;; This file was written by Helmut Eller and has been placed in the
;; public domain.
;;
;;;
;;
;; The macro produces essentially output that looks so:
;;
;;  (let (<with-bindings>)
;;    (let (<initialze iteration-variales>)
;;      (let loop (<iteration-variales>)
;;        (let ((finish (lambda (<iteration-variales>) <finally-code>)))
;;          <body>
;;          (let (<bind iteration-variales to next value>)
;;            (if <end-test> (finish <iteration-variales>))
;;            (loop <iteration-variales>)))))
;;
;; Note that we don't use set! to update iteration-variables; instead
;; the variables are rebound. (Due to call/cc, set! requires
;; heap-allocated locations and it would be inefficient. Variables
;; that are never set! can be stack allocated.)
;;
;; In a first step, input is transformed into a simple IR.  The IR
;; consists of multiple "segments" that correspond to the above code;
;; one segment binds the WITH-variables one segments initializes
;; iteration-variables and so on.  Each segment is a list of
;; "nodes". There are 4 kinds of nodes: bind, stmt, if-node, and
;; test-nodes.  Bind-nodes bind variables like let; stmt-nodes are
;; used for side effects e.g. do-clauses; if-nodes are used to
;; represent if-clauses; test-nodes are used to handle termination
;; tests.
;;
;; In the second step, the IR segments are then converted to output
;; expression.
;;
;; Many of the parsing functions here are named like the clauses in
;; the spec[*].  Those parse the input and generate IR as side effect.
;;
;; [*] http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
;;


;;; Code



;; The Scheme world can't agree on a syntax to define structs, oops,
;; records.  So everybody needs to re-invent the wheel.
(define-syntax defstruct
  (lambda (form)
    (define (to-string x)
      (syntax-case x ()
	(_ (symbol? x) (symbol->string x))
	(_ (identifier? x) (symbol->string (syntax->datum x)))
	(_ (string? x) x)))
    (define (symconc . syms)
      (string->symbol (apply string-append (map to-string syms))))
    (define (parse-slots forms idx)
      (syntax-case forms ()
	(() '())
	((name more ...) (symbol? (syntax->datum #'name))
	 (cons (list idx #'name)
	       (parse-slots #'(more ...) (+ 1 idx))))))
    (syntax-case form ()
      ((_ name slots ...)
       (let* ((slots (parse-slots #'(slots ...) 1))
	      (len (length slots))
	      (slot-names (map (lambda (s) (car (cdr s))) slots))
	      (stxconc (lambda xs (datum->syntax #'name (apply symconc xs))))
	      (constructor (stxconc 'make- #'name))
	      (predicate (stxconc #'name '?))
	      (check #`(or (#,predicate name)
			   (r6rs-error 'check-struct-type
				       #,(string-append "expected type: "
							(to-string #'name))
				       name))))
	 #`(begin
	     (define (#,constructor . #,slot-names)
	       (vector 'name #,@slot-names))
	     (define (#,predicate obj)
	       (and (vector? obj)
		    (= (vector-length obj) #,(+ 1 len))
		    (eq? (vector-ref obj 0) 'name)))
	     #,@(map (lambda (slot)
		       (define (reader slot-name)
			 (stxconc #'name '- slot-name))
		       (define (writer slot-name)
			 (stxconc (reader slot-name) '- 'set!))
		       (apply (lambda (idx slot-name)
				#`(begin
				    (define (#,(reader slot-name) name)
				      #,check
				      (vector-ref name '#,idx))
				    (define (#,(writer slot-name) name value)
				      #,check
				      (vector-set! name '#,idx value))))
			      slot))
		     slots)))))))

(define-syntax defsynrules
  (syntax-rules ()
    ((_ name (literals ...) clauses ...)
     (define-syntax name
       (syntax-rules (literals ...) clauses ...)))))

(defsynrules defsyncase  ()
  ((_ name (arg literals ...) clauses ...)
   (define-syntax name
     (lambda (arg)
       (syntax-case arg (literals ...) clauses ...)))))

;; As I'm heretic enough to implement loop in Scheme I can just as
;; well use mutation during expansion time.  setf/push/pop make life
;; quite a bit easier.

(defsyncase setf (form)
  ((id (reader obj args ...) value)
   (let ((writer (string->symbol
		  (string-append (symbol->string (syntax->datum #'reader))
				 "-set!"))))
     #`(#,(datum->syntax #'id writer) obj args ... value)))
  ((_ var value) (identifier? #'var)
   #`(set! var value)))

(defsynrules push ()
  ((_ val (reader obj))
   (let ((o obj))
     (setf (reader o) (cons val (reader o)))))
  ((_ val var)
   (set! var (cons val var))))

(defsynrules pop ()
  ((_ (reader obj))
   (let* ((o obj)
	  (list (reader o)))
     (setf (reader o) (cdr list))
     (car list)))
  ((id val var)
   (let ((list var))
     (set! var (cdr list))
     (car list))))

;; appendf is similar to push, but instead of pushing a single value
;; we push a list of values to a place.
(defsynrules appendf ()
  ((_ prefix (reader obj))
   (let* ((p prefix)
	  (o obj))
     (setf (reader o) (append p (reader o))))))

;; The ctx packs up the different parts of the loop.
(defstruct ctx 
  src	    ; the input source form (not modified)
  id 	    ; the identifier of the macro (i.e. 'loop)
  forms     ; the list of input forms (modified during parsing)
  cont 	    ; the identifier of the return continuation
  name 	    ; if we have a named-clause this is the name; #f otherwise
  finish    ; the identifier for the 'finish-loop macro
  epilog    ; the identifier for the epilogue lambda
  finally   ; segment for finally forms
  with 	    ; segment to setup initialize accumulators etc.
  head 	    ; segment before the loop
  vars 	    ; list of identifiers that are rebound on each iteration
  neck 	    ; segment before body (currently not used)
  body 	    ; segment of the loop body
  tail 	    ; segment that binds loop vars for the next iteration
  accus	    ; alist of accumulators
  )

;; helper struct to parse for-as-arithmetic clauses
(defstruct range ctx var type init limit test step by)

;; The IR nodes:
(defstruct bind vars vals)
(defstruct stmt form)
(defstruct ifnode test true false vars it)
(defstruct testnode while ctx finally?)

(define loop-error
  (case-lambda
   ((ctx msg) (loop-error ctx msg (ctx-forms ctx)))
   ((ctx msg form) (syntax-violation #f msg (ctx-src ctx) form))))

(define (ctx-pop-form ctx)
  (pop (ctx-forms ctx)))

;; Return the next input form.  (Implicitly accesses variable 'ctx.)
(defsyncase form (exp)
  ((id)
   (let ((ctx-id (datum->syntax #'id 'ctx)))
     #`(ctx-pop-form #,ctx-id))))


;; Dispatch on the next input token. Implicitly accesses variable 'ctx.
;; Used like so:
;; (token-case
;;  ((a) a-case)        ; selected if token is 'a
;;  ((b c) b-c-case)    ; selected if token is 'b or 'c
;;  ((#f x y) x-y-case) ; selected if token is 'x or 'y doesn't consume input
;;  (#t else)           ; selected if nothing else matches
;; 
(defsyncase token-case (form)
  ((id)
   (let ((ctx-id (datum->syntax #'id 'ctx)))
     #`(loop-error #,ctx-id "invalid token")))
  ((id ((token0 tokens ...) body ...) clauses ...)
   #`(if (#,(datum->syntax #'id 'token) token0 tokens ...)
	 (begin (values) body ...)
	 (id clauses ...)))
  ((_ (#t body ...)) #'(begin (values) body ...)))

;; helper macro
(defsyncase token (form)
  ((some-id drop? token0 tokens ...) (boolean? (syntax->datum #'drop?))
   (let ((ctx-id (datum->syntax #'some-id 'ctx)))
     #`(syntax-case (ctx-forms #,ctx-id) ()
	 ((car . _) (and (identifier? #'car)
			 (memq (syntax->datum #'car) '(token0 tokens ...)))
	  (begin
	    (when drop?
	      (ctx-pop-form #,ctx-id))
	    #t))
	 (_ #f))))
  ((some-id tokens ...) #'(some-id #t tokens ...)))


;; Create a bind-node for a single variable.
(define (bind1 var val)
  (make-bind (list var) (list val)))

;; Craete a stmt node.
(define (stmt form)
  (make-stmt form))

;; Craete a stmt node.
(define (make-test ctx while finally?)
  (make-testnode while ctx finally?))

;; Create a temporary identifier. NAME is a hint for debugging.
(define (temp name) 
  (car (generate-temporaries (list name))))

;; Is X a constant?
(define (constant? x)
  (syntax-case x (quote)
    ((quote _) #t)
    ((_ ...) #f)
    (x (identifier? #'x) #f)
    (x
     (let ((x (syntax->datum #'x)))
       (or (number? x) (string? x) (char? x) (boolean? x) (vector? x))))))

;; Return a temporary for FORM, if if it's constant FORM itself.
(define (maybe-temp ctx form name)
  (cond ((constant? form)
	 form)
	(#t
	 (let ((tmp (temp name)))
	   (push (bind1 tmp form) (ctx-head ctx))
	   tmp))))


;;; for-as-arithmetic

;; The from/to/by parts must be evaluated in the order as in the source.
;; So the initialization often create temporaries.

(define (range-error range msg form)
  (loop-error (range-ctx range) msg form))

(define (range-temp range form name) (maybe-temp (range-ctx range) form name))

(define (range-init-set!-checked range form)
  (cond ((range-init range)
	 (range-error range "multiple from/upfrom/downfrom clauses" form))
	(#t
	 (range-init-set! range (range-temp range form 'init-tmp)))))

(define (range-limit-set!-checked range form)
  (cond ((range-limit range)
	 (range-error range "multiple to/upto/below/downto/above clauses"
		      form))
	(#t
	 (range-limit-set! range (range-temp range form 'limit-tmp)))))

(define (range-step-set!-checked range step)
  (cond ((not (range-step range)) (range-step-set! range step))
	((not (eq? (syntax->datum (range-step range)) (syntax->datum step)))
	 (range-error range
		      "conflicting stepping direction in for-as-arithmetic"
		      step))))

(define (range-test-set!-checked range test)
  (cond ((not (range-test range)) (range-test-set! range test))
	((not (bound-identifier=? (range-test range) test))
	 (range-error range "conflicting upto/below/downto/above clauses"
		      test))))

(define (for-from range init step)
  (range-init-set!-checked range init)
  (and step (range-step-set!-checked range step)))

(define (for-to range limit step test)
  (range-limit-set!-checked range limit)
  (and step (range-step-set!-checked range step))
  (and test (range-test-set!-checked range test)))

(define (for-by range by)
  (cond ((range-by range)
	 (range-error range "multiple for-by" by))
	(#t
	 (range-by-set! range (range-temp range by 'by-tmp)))))

(define (emit-arithmetic ctx range)
  (let* ((var (range-var range))
	 (init (or (range-init range) 0))
	 (limit (range-limit range))
	 (step (or (range-step range) #'+))
	 (by (or (range-by range) #'1))
	 (op (if limit
		 (or (range-test range)
		     (syntax-case step (- +)
		       (+ #'<=)
		       (- #'>=)))
		 #f))
	 (num (temp 'num-tmp))
	 (test (make-test ctx #`(#,op #,num #,limit) #t)))
    (push (bind1 num init) (ctx-head ctx))
    (when limit (push test (ctx-head ctx)))
    (push num (ctx-vars ctx))
    (push (bind1 num #`(#,step #,num #,by)) (ctx-tail ctx))
    (when limit (push test (ctx-tail ctx)))
    (bind1 var num)))

(define (for-as-arithmetic-subclause ctx var type)
  (let ((range (make-range ctx var type #f #f #f #f #f)))
    (do ((done #f done))
	(done #f)
      (token-case
       ((from)		(for-from range (form) #f))
       ((upfrom)	(for-from range (form) #'+))
       ((downfrom)	(for-from range (form) #'-))
       ((to)		(for-to range (form) #f #f))
       ((upto)		(for-to range (form) #'+ #'<=))
       ((below)		(for-to range (form) #'+ #'<))
       ((downto)	(for-to range (form) #'- #'>=))
       ((above)		(for-to range (form) #'- #'>))
       ((by)		(for-by range (form)))
       (#t (set! done #t))))
    (emit-arithmetic ctx range)))


;; Join to bind-nodes to one big bind-node.
(define (join-binds bind1 bind2)
  (make-bind (append (bind-vars bind1) (bind-vars bind2))
	     (append (bind-vals bind1) (bind-vals bind2))))

;; Generate for destructuring-bind.  Return 2 values: a segment that
;; binds auxiliary variables and as second value the final bind-node.
(define (d-bind var val)
  (syntax-case var ()
    (simple-var (identifier? var)
		(values '() (bind1 var val)))
    (#f (values '() (make-bind '() '())))
    ((x . y)
     (let ((tmp0 '()))
       (let ((val (cond ((identifier? val) val)
			(#t (let ((tmp (temp 'tmp)))
			      (push (bind1 tmp val) tmp0)
			      tmp)))))
	 (assert (list? tmp0))
	 (let-values (((tmps1 bind1) (d-bind #'x #`(if (null? #,val)
						       #f
						       (car #,val)))))
	 (assert (list? tmps1))
	   (syntax-case #'y ()
	     (() (values (append tmps1 tmp0) bind1))
	     (_ (let-values (((tmps2 bind2) (d-bind #'y #`(if (null? #,val)
							      '()
							      (cdr #,val)))))
		  (values (append tmps2 tmps1 tmp0)
			  (join-binds bind1 bind2)))))))))))



(define (for-as-in-list ctx var type)
  (let* ((init (token-case
		((in) (form))))
	 (step (token-case
		((by) (form))
		(#t #f)))
	 (init (maybe-temp ctx init 'list-tmp))
	 (step (if step (maybe-temp ctx step 'stepfn-tmp) #'cdr))
	 (lvar (temp 'list-var)))
    (push lvar (ctx-vars ctx))
    (push (bind1 lvar init) (ctx-head ctx))
    (let-values (((test) (make-test ctx #`(not (null? #,lvar)) #t))
		 ((tmps bind) (d-bind var #`(car #,lvar))))
      (push test (ctx-head ctx))
      (appendf tmps (ctx-head ctx))
      (push (bind1 lvar #`(#,step #,lvar)) (ctx-tail ctx))
      (push test (ctx-tail ctx))
      (appendf tmps (ctx-tail ctx))
      bind)))

(define (for-as-on-list ctx var type)
  (let* ((init (token-case
		((on) (form))))
	 (step (token-case
		((by) (form))
		(#t #f)))
	 (init (maybe-temp ctx init 'list-tmp))
	 (step (if step (maybe-temp ctx step 'stepfn-tmp) #'cdr))
	 (lvar (temp 'list-var)))
    (push lvar (ctx-vars ctx))
    (push (bind1 lvar init) (ctx-head ctx))
    (let-values (((test) (make-test ctx #`(pair? #,lvar) #t))
		 ((tmps bind) (d-bind var lvar)))
      (push test (ctx-head ctx))
      (appendf tmps (ctx-head ctx))
      (push (bind1 lvar #`(#,step #,lvar)) (ctx-tail ctx))
      (push test (ctx-tail ctx))
      (appendf tmps (ctx-tail ctx))
      bind)))

(define (for-as-across ctx var type)
  (let* ((init (token-case
		((across) (form))))
	 (vec (temp 'vec-tmp))
	 (len (temp 'len-tmp))
	 (i (temp 'i)))
    (push (bind1 vec init) (ctx-head ctx))
    (push (bind1 len #`(glength #,vec)) (ctx-head ctx))
    (push (bind1 i #'0) (ctx-head ctx))
    (push i (ctx-vars ctx))
    (let-values (((test) (make-test ctx #`(< #,i #,len) #t))
		 ((tmps bind) (d-bind var #`(gvref #,vec #,i))))
      (push test (ctx-head ctx))
      (appendf tmps (ctx-head ctx))
      (push (bind1 i #`(+ #,i 1)) (ctx-tail ctx))
      (push test (ctx-tail ctx))
      (appendf tmps (ctx-tail ctx))
      bind)))


(define (for-as-equals-then ctx var type)
  (let ((init (token-case
	       ((=) (form)))))
    (let-values (((then? then) (token-case
				((then) (values #t (form)))
				(#t (values #f #f)))))
      (cond (then?
	     (let ((val (temp 'tmp)))
	       (push (bind1 val init) (ctx-head ctx))
	       (let-values (((tmps bind) (d-bind var val)))
		 (appendf tmps (ctx-head ctx))
		 (push val (ctx-vars ctx))
		 (push (bind1 val then) (ctx-tail ctx))
		 (appendf tmps (ctx-tail ctx))
		 bind)))
	    (#t
	     (let* ((val (temp 'tmp)))
	       (let-values (((vbind) (bind1 val init))
			    ((tmps bind) (d-bind var val)))
		 (push vbind (ctx-head ctx))
		 (appendf tmps (ctx-head ctx))
		 (push val (ctx-vars ctx))
		 (push vbind (ctx-tail ctx))
		 (appendf tmps (ctx-tail ctx))
		 bind)))))))

;; This could probably written more elegantly.
(define (for-hash ctx htab key ktype val vtype)
  (cond ((not val)
	 (let ((vec (temp 'hashkeys))
	       (len (temp 'hashcount))
	       (i (temp 'key-idx)))
	   (push (bind1 vec #`(hashtable-keys #,htab)) (ctx-head ctx))
	   (push (bind1 len #`(vector-length #,vec)) (ctx-head ctx))
	   (push (bind1 i #'0) (ctx-head ctx))
	   (push i (ctx-vars ctx))
	   (let-values (((test) (make-test ctx #`(< #,i #,len) #t))
			((tmps bind) (d-bind key #`(vector-ref #,vec #,i))))
	     (push test (ctx-head ctx))
	     (appendf tmps (ctx-head ctx))
	     (push (bind1 i #`(+ #,i 1)) (ctx-tail ctx))
	     (push test (ctx-tail ctx))
	     (appendf tmps (ctx-tail ctx))
	     bind)))
	((not key)
	 (let ((vec (temp 'hashvalues))
	       (len (temp 'hashcount))
	       (i (temp 'val-idx)))
	   (push (bind1 vec
			#`(let-values (((ks vs) (hashtable-entries #,htab)))
			    vs))
		 (ctx-head ctx))
	   (push (bind1 len #`(vector-length #,vec)) (ctx-head ctx))
	   (push (bind1 i #'0) (ctx-head ctx))
	   (push i (ctx-vars ctx))
	   (let-values (((test) (make-test ctx #`(< #,i #,len) #t))
			((tmps bind) (d-bind val #`(vector-ref #,vec #,i))))
	     (push test (ctx-head ctx))
	     (appendf tmps (ctx-head ctx))
	     (push (bind1 i #`(+ #,i 1)) (ctx-tail ctx))
	     (push test (ctx-tail ctx))
	     (appendf tmps (ctx-tail ctx))
	     bind)))
	(#t
	 ;; FIXME: avoid cons for values
	 (let ((econs (temp 'hashentries))
	       (vvec (temp 'hashvalues))
	       (kvec (temp 'hashkeys))
	       (len (temp 'hashcount))
	       (i (temp 'idx)))
	   (push (bind1 econs
			#`(let-values (((ks vs) (hashtable-entries #,htab)))
			    (cons ks vs)))
		 (ctx-head ctx))
	   (push (bind1 kvec #`(car #,econs)) (ctx-head ctx))
	   (push (bind1 vvec #`(cdr #,econs)) (ctx-head ctx))
	   (push (bind1 len #`(vector-length #,kvec)) (ctx-head ctx))
	   (push (bind1 i #'0) (ctx-head ctx))
	   (push i (ctx-vars ctx))
	   (let-values (((test) (make-test ctx #`(< #,i #,len) #t))
			((ktmps kbind) (d-bind key #`(vector-ref #,kvec #,i)))
			((vtmps vbind) (d-bind val #`(vector-ref #,vvec #,i))))
	     (push test (ctx-head ctx))
	     (appendf ktmps (ctx-head ctx))
	     (appendf vtmps (ctx-head ctx))
	     (push (bind1 i #`(+ #,i 1)) (ctx-tail ctx))
	     (push test (ctx-tail ctx))
	     (appendf ktmps (ctx-tail ctx))
	     (appendf vtmps (ctx-tail ctx))
	     (join-binds kbind vbind))))))

(define (for-as-hash ctx var type)
  (token-case ((being))) (token-case ((each the)))
  (token-case
   ((hash-key hash-keys)
    (token-case ((in of)))
    (let ((htab (form)))
      (token-case
       ((using)
	(syntax-case (form) ()
	  ((hash-value val-var) (and (identifier? #'hash-value)
				     (eq? (syntax->datum #'hash-value)
					  'hash-value))
	   (for-hash ctx htab var type #'val-var #f))))
       (#t (for-hash ctx htab var type #f #f)))))
   ((hash-value hash-values)
    (token-case ((in of)))
    (let ((htab (form)))
      (token-case
       ((using)
	(syntax-case (form) ()
	  ((hash-key key-var) (and (identifier? #'hash-key)
				   (eq? (syntax->datum #'hash-key)
					'hash-key))
	   (for-hash ctx htab #'key-var #f var type))))
       (#t (for-hash ctx htab #f #f var type)))))))

(define (d-var-spec ctx) (form))

(define (var ctx) (d-var-spec ctx))

(define (maybe-type-spec ctx)
  (token-case
   ((of-type) (form))
   ((t nil fixnum float) #f)
   (#t #f)))

(define (for-as-subclause ctx)
  (let* ((var (var ctx))
	 (type (maybe-type-spec ctx)))
    (token-case
     ((#f from upfrom downfrom to upto downto below above by)
      (for-as-arithmetic-subclause ctx var type))
     ((#f in)
      (for-as-in-list ctx var type))
     ((#f on)
      (for-as-on-list ctx var type))
     ((#f across)
      (for-as-across ctx var type))
     ((#f =)
      (for-as-equals-then ctx var type))
     ((#f being)
      (for-as-hash ctx var type)))))

(define (for-as-clause ctx)
  (let loop ((bind (for-as-subclause ctx)))
    (assert (bind? bind))
    (token-case
     ((and)
      (loop (join-binds bind (for-as-subclause ctx))))
     (#t
      (push bind (ctx-head ctx))
      (appendf (bind-vars bind) (ctx-vars ctx))
      (push bind (ctx-tail ctx))))))


;; with-clauses can create variables with no initialization form. For those
;; we need to choose some default value.

(define (default-value-for-type type)
  (syntax-case type ()
    (#f #f)
    (name (identifier? #'name)
	  (let ((name (syntax->datum #'name)))
	    (case name
	      ((fixnum integer) 0)
	      ((float) 0.0)
	      (else (r6rs-error 'default-value-for-type
				"Unknown type name" name)))))))

(define (default-value var type)
  (let ((car-type (lambda (x)
		    (syntax-case x  ()
		      (_ (identifier? x) x)
		      ((car . _) #'car)
		      (#f #f))))
	(cdr-type (lambda (x)
		    (syntax-case x ()
		      (_ (identifier? x) x)
		      ((_ . cdr) #'cdr)
		      (#f #f)))))
    (syntax-case var ()
      (_ (identifier? var)
	 (default-value-for-type type))
      (() '())
      ((x . y)
       `(,(default-value #'x (car-type type)) .
	 ,(default-value #'y (cdr-type type)))))))

(define (with-clause ctx)
  (letrec ((with (lambda (ctx)
		   (let* ((var (var ctx))
			  (type (maybe-type-spec ctx))
			  (init (token-case
				 ((=) (form))
				 (#t #`(quote #,(default-value var type))))))
		     (let-values (((tmps bind) (d-bind var init)))
		       (appendf tmps (ctx-head ctx))
		       bind)))))
    (let loop ((bind (with ctx)))
      (token-case
       ((and) (loop (join-binds bind (with ctx))))
       (#t (push bind (ctx-head ctx)) #f)))))



;; Is FORM a compound form?
(define (compound? form)
  (syntax-case form ()
    ((x . y) #t)
    (_ #f)))

(define (compound-form ctx)
  (syntax-case (ctx-forms ctx) ()
    ((car  . _) (compound? #'car)
     (form))))

;; Return the all the compound forms.
(define (compound+ ctx)
  (let ((first (compound-form ctx)))
    (cons first
	  (if (syntax-case (ctx-forms ctx) ()
		((car . _) (compound? #'car))
		(_ #f))
	      (compound+ ctx)
	      '()))))

(define (initially ctx)
  (for-each (lambda (form) (push (stmt form) (ctx-head ctx)))
	    (compound+ ctx)))

(define (finally ctx)
  (for-each (lambda (form) (push (stmt form) (ctx-finally ctx)))
	    (compound+ ctx)))

(define (initial-final ctx)
  (token-case
   ((initially) (initially ctx))
   ((finally) (finally ctx))))

(define (variable-clause* ctx)
  (do ((done #f done))
      (done)
    (token-case
     ((for as) (for-as-clause ctx))
     ((with) (with-clause ctx))
     ((#f initially finally) (initial-final ctx))
     (#t (set! done #t)))))

(define (do-clause ctx)
  (for-each (lambda (form) (push (stmt form) (ctx-body ctx)))
	    (compound+ ctx)))

(define (unconditional ctx)
  (token-case
   ((do doing) (do-clause ctx))
   ((return) (return-clause ctx (form)))))


;; Info about accumulators are store ctx-accus slot.  It is an alist
;; that maps names to accu structs.  #f is used to refer to the
;; default accu.

;; We use tail-concing for collect and friends.
(defstruct list-accu head tail result)

;; Numbers are accumulated into single variable.  For some cases
;; (e.g. maximize) we create an additional variable that indicates
;; whether the accu was initialized or not.
(defstruct num-accu var flag)

;; bool-accus are used for thereis/always/never.
(defstruct bool-accu var)

;; Find the accu with name NAME.
(define (get-accu ctx name)
  (let* ((test (lambda (key)
		 (cond ((eq? key name))
		       ((or (eq? key #f) (eq? name #f)) #f)
		       (#t (eq? (syntax->datum key) (syntax->datum name))))))
	 (probe (assp test (ctx-accus ctx))))
    (if probe (cdr probe) #f)))

;; Associate the accu ACCU with name NAME.
(define (put-accu ctx name accu)
  (assert (not (get-accu ctx name)))
  (push (cons name accu) (ctx-accus ctx)))

;; Get or create a list-accu for NAME.
(define (get-list-accu ctx name)
  (let ((accu (get-accu ctx name)))
    (cond ((not accu)
	   (let* ((head (temp 'accu-head))
		  (tail (temp 'accu-tail))
		  (accu (make-list-accu head tail #`(cdr #,head))))
	     (push (bind1 head #`(cons #f '())) (ctx-with ctx))
	     (push (bind1 tail head) (ctx-with ctx))
	     (push tail (ctx-vars ctx))
	     (when name
	       (push (bind1 name #''()) (ctx-with ctx))
	       (push name (ctx-vars ctx))
	       (push (bind1 name #`(cdr #,head)) (ctx-tail ctx)))
	     (put-accu ctx name accu)
	     accu))
	  ((list-accu? accu) accu)
	  (#t (loop-error ctx "conflicting accumulation clause")))))

;; Get or create a num-accu for NAME.
(define (get-num-accu ctx name needs-flag?)
  (let ((accu (get-accu ctx name)))
    (cond ((not accu)
	   (let* ((num (or name (temp 'num-accu)))
		  (flag (if needs-flag? (temp 'num-flag) #f))
		  (accu (make-num-accu num flag)))
	     (push (bind1 num #''0) (ctx-with ctx))
	     (when flag (push (bind1 flag #''#f) (ctx-with ctx)))
	     (put-accu ctx name accu)
	     (push num (ctx-vars ctx))
	     (when flag (push flag (ctx-vars ctx)))
	     accu))
	  ((num-accu? accu) accu)
	  (#t (loop-error ctx "conflicting accumulation clause")))))

;; Get or create a bool-accul.  Bool-accus are never named.
(define (get-bool-accu ctx init)
  (let ((accu (get-accu ctx #f)))
    (cond ((not accu)
	   (let* ((bool (temp 'bool-accu))
		  (accu (make-bool-accu bool)))
	     (push (bind1 bool init) (ctx-with ctx))
	     (put-accu ctx #f accu)
	     (push bool (ctx-vars ctx))
	     accu))
	  ((bool-accu? accu) accu)
	  (#t (loop-error ctx "conflicting accumulation clause")))))

(define (collect-clause ctx form into)
  (let* ((tail (list-accu-tail (get-list-accu ctx into))))
    (push (bind1 tail #`(let ((new-tail (list #,form)))
			  (set-cdr! #,tail new-tail)
			  new-tail))
	  (ctx-body ctx))))

(define (append-clause ctx form into)
  (let* ((tail (list-accu-tail (get-list-accu ctx into))))
    (push (bind1 tail #`(let copy ((l #,form)
				   (tail #,tail))
			  (cond ((null? l) tail)
				(#t (let ((new-tail (list (car l))))
				      (set-cdr! tail new-tail)
				      (copy (cdr l) new-tail))))))
	  (ctx-body ctx))))

(define (nconc-clause ctx form into)
  (let* ((tail (list-accu-tail (get-list-accu ctx into))))
    (push (bind1 tail #`(let ((l #,form))
			  (cond ((null? l) #,tail)
				(#t
				 (set-cdr! #,tail l)
				 (let last ((l l))
				   (cond ((null? (cdr l))  l)
					 (#t (last (cdr l)))))))))
	  (ctx-body ctx))))

(define (sum-clause ctx form into type)
  (let* ((num (num-accu-var (get-num-accu ctx into #f))))
    (push (bind1 num #`(+ #,num #,form)) (ctx-body ctx))))

(define (count-clause ctx form into type)
  (let* ((num (num-accu-var (get-num-accu ctx into #f))))
    (push (bind1 num #`(if #,form (+ #,num 1) #,num)) (ctx-body ctx))))

(define (max-clause ctx form into type max)
  (let* ((accu (get-num-accu ctx into #t))
	 (num (num-accu-var accu))
	 (flag (num-accu-flag accu))
	 (tmp (temp 'val)))
    (push (bind1 num #`(let ((#,tmp #,form))
			 (if #,flag (#,max #,num #,tmp) #,tmp)))
	  (ctx-body ctx))
    (push (bind1 flag #''#t) (ctx-body ctx))))

(define (maybe-into ctx)
  (token-case
   ((into)
    (let ((form (form)))
      (syntax-case form ()
	(_ (identifier? form) form))))
   (#t #f)))

(define (list-accumulation ctx)
  (let ((frob (lambda (fun)
		(let* ((form (form))
		       (name (maybe-into ctx)))
		  (fun ctx form name)))))
    (token-case
     ((collect collecting) (frob collect-clause))
     ((append appending) (frob append-clause))
     ((nconc nconcing) (frob nconc-clause)))))

(define (numeric-accumulation ctx)
  (let ((frob (lambda (fun)
		(let* ((form (form))
		       (into (maybe-into ctx))
		       (type (maybe-type-spec ctx)))
		  (fun ctx form into type))))
	(min/max (lambda (max)
		   (lambda (ctx form into type)
		     (max-clause ctx form into type max)))))
    (token-case
     ((sum summing) (frob sum-clause))
     ((count counting) (frob count-clause))
     ((maximize maximizing) (frob (min/max #'max)))
     ((minimize minimizing) (frob (min/max #'min))))))

(define (return-clause ctx form)
  (push (stmt #`(call-with-values (lambda () #,form)
		  #,(ctx-cont ctx)))
	(ctx-body ctx)))

(define (selectable-clause ctx)
  (token-case
   ((#f collect collecting append appending nconc nconcing)
    (list-accumulation ctx))
   ((#f sum summing count counting maximize maximizing minimize minimizing)
    (numeric-accumulation ctx))
   ((return)
    (return-clause ctx (form)))
   ((#f if when unless)
    (conditional ctx))
   ((#f do doing)
    (unconditional ctx))))

;; Macro to dispatch on predicates.  E.g.
;; (fcase <value>
;;  (integer? x) ; selected if (integer? <value>) returns true.
;;  (symbol? y))
(defsynrules fcase ()
  ((_ val (predicate body ...) ...)
   (let ((val val))
     (cond ((predicate val) body ...) ...
	   (#t (r6rs-error 'fcase
			   "no-matching-clause" '(predicate ...) val))))))

(define (reduce fun init list)
  (cond ((null? list) init)
	(#t (reduce fun (fun init (car list)) (cdr list)))))

(define (union set1 set2)
  (reduce (lambda (set e)
	    (cond ((memq e set) set)
		  (#t (cons e set))))
	  set1
	  set2))

;; Collect all variables that get bound in the segment BODY.
(define (gather-vars body)
  (reduce (lambda (vars node)
	    (fcase node
		   (stmt? vars)
		   (bind? (union vars (bind-vars node)))
		   (ifnode? (union vars (ifnode-vars node)))
		   (testnode? vars)))
	  '()
	  body))

;; Are we at a conditional claus that uses the "it" variable?
(define (uses-it? ctx)
  (syntax-case (ctx-forms ctx) ()
    ((x y . __)
     (and (eq? (syntax->datum #'y) 'it)
	  (memq (syntax->datum #'x)
		'(return collect collecting append appending nconc nconcing
			 sum summing count counting
			 maximize maximizing minimize minimizing)))
     #'y)
    (_ #f)))

;; If-clauses are translated like so:
;;
;; (let ((joinpoint (lambda (<vars>) <rest>)))
;;   (if <test>
;;     (begin <true-segment>
;;            (<joinpoint> <vars>)
;;     (begin <false-segment>
;;            (<joinpoint> <vars>)))
;;
;; <rest> is the code that comes after the if-clause.  <vars> is the
;; union of variables that are bound in <true-segment> and
;; <false-segment>.  The lambda "joinpoint" is used to properly bind
;; variables in <rest>, e.g. if a collect clause occurs only in one
;; arm of the if-clause.
(define (if-clause ctx test)
  (let* ((old-body (ctx-body ctx))
	 (and-selectable-clause* (lambda ()
				   (let loop ()
				     (token-case
				      ((and) (selectable-clause ctx) (loop))
				      (#t #f)))))
	 (it (if (uses-it? ctx) (temp 'it) #f)))
    (when it
      (let ((car (ctx-pop-form ctx)))
	(token-case ((it)))
	(push it (ctx-forms ctx))
	(push car (ctx-forms ctx))))
    (ctx-body-set! ctx '())
    (selectable-clause ctx)
    (and-selectable-clause*)
    (let* ((true-block (ctx-body ctx))
	   (true-vars (gather-vars true-block)))
      (ctx-body-set! ctx '())
      (token-case
       ((else)
	(selectable-clause ctx)
	(and-selectable-clause*))
       (#t #f))
      (token-case
       ((end))
       (#t #f))
      (let* ((false-block (ctx-body ctx))
	     (false-vars (gather-vars false-block)))
	(ctx-body-set! ctx old-body)
	(push (make-ifnode test true-block false-block
			   (union true-vars false-vars)
			   it)
	      (ctx-body ctx))))))

(define (conditional ctx)
  (token-case
   ((if when)
    (if-clause ctx (form)))
   ((unless)
    (if-clause ctx #`(not #,(form))))))

(define (repeat-clause ctx)
  (let* ((init (maybe-temp ctx (form) 'repeat-init))
	 (i (temp 'i))
	 (test (make-test ctx #`(> #,i 0) #t)))
    (push (bind1 i init) (ctx-head ctx))
    (push test (ctx-head ctx))
    (push i (ctx-vars ctx))
    (push (bind1 i #`(- #,i 1)) (ctx-tail ctx))
    (push test (ctx-tail ctx))))

(define (while-clause ctx form)
  (push (make-test ctx form #t) (ctx-body ctx)))

(define (always-clause ctx form not?)
  (let ((bool (bool-accu-var (get-bool-accu ctx #t))))
    (push (bind1 bool (if not?
			  #`(not #,form)
			  #`(if #,form #t #f)))
	  (ctx-body ctx))
    (push (make-test ctx bool #f) (ctx-body ctx))))

(define (thereis-clause ctx form)
  (let ((bool (bool-accu-var (get-bool-accu ctx #f))))
    (push (bind1 bool form) (ctx-body ctx))
    (push (make-test ctx #`(not #,bool) #f) (ctx-body ctx))))

(define (termination-test ctx)
  (token-case
   ((repeat) (repeat-clause ctx))
   ((while) (while-clause ctx (form)))
   ((until) (while-clause ctx #`(not #,(form))))
   ((always) (always-clause ctx (form) #f))
   ((never) (always-clause ctx (form) #t))
   ((thereis) (thereis-clause ctx (form)))
   ))

(define (main-clause* ctx)
  (do () ((null? (ctx-forms ctx)))
    (token-case
     ((#f do doing return)
      (unconditional ctx))
     ((#f collect collecting append appending nconc nconcing)
      (list-accumulation ctx))
     ((#f sum summing count counting maximize maximizing minimize minimizing)
      (numeric-accumulation ctx))
     ((#f unless if when)
      (conditional ctx))
     ((#f repeat while until always never thereis)
      (termination-test ctx))
     ((#f initially finally) (initial-final ctx)))))

(define (maybe-name-clause ctx)
  (token-case
   ((named)
    (let ((name (form)))
      (assert (identifier? name))
      (ctx-name-set! ctx name)))
   (#t #f)))

;; Turn a list of nodes into s-expression. TAIL is the s-expression
;; that should be executed after NODES.
(define (rebuild-block nodes tail)
  (reduce (lambda (tail node)
	    (fcase node
		   (stmt? #`(begin #,(stmt-form node) #,tail))
		   (bind? #`(let (#,@(map (lambda (var val) #`(#,var #,val))
					  (bind-vars node)
					  (bind-vals node)))
			      #,tail))
		   (ifnode?
		    (let* ((vars (ifnode-vars node))
			   (join (temp 'joinpoint))
			   (k #`(#,join #,@vars))
			   (it (ifnode-it node)))
		      #`(let ((#,join (lambda (#,@vars) #,tail))
			      #,@(if it #`((#,it #,(ifnode-test node))) #'()))
			  (if #,(or it (ifnode-test node))
			      #,(rebuild-block (ifnode-true node) k)
			      #,(rebuild-block (ifnode-false node) k)))))
		   (testnode?
		    (let ((ctx (testnode-ctx node)))
		      #`(if #,(testnode-while node)
			    #,tail
			    #,(if (testnode-finally? node)
				  #`(#,(ctx-epilog ctx) #,@(ctx-vars ctx))
				  #`(#,(ctx-cont ctx) #,(result-form ctx))))))
		   ))
	  tail
	  nodes))

(define (result-form ctx)
  (let ((accu (get-accu ctx #f)))
    (fcase accu
	   (not #f)
	   (list-accu? (list-accu-result accu))
	   (num-accu? (num-accu-var accu))
	   (bool-accu? (bool-accu-var accu)))))

;; (return) without arguments returns (values) to avoid those dreaded
;; "unreachable code" errors in Kawa.
(define (bind-return loop-id name k body)
  (cond ((not name)
	 (let ((return-id (datum->syntax loop-id 'return)))
	   #`(call/cc
	      (lambda (#,k)
		(let-syntax ((#,return-id
			      (syntax-rules ()
				((_ exp)
				 (call-with-values (lambda () exp) #,k))
				((_)
				 (#,k)))))
		  #,body)))))
	(#t
	 (let ((return-from (datum->syntax loop-id 'return-from)))
	   #`(call/cc
	      (lambda (#,k)
		(let-syntax ((#,return-from
			      (lambda (src)
				(syntax-case src ()
				  ((_ tag exp) (eq? (syntax->datum #'tag)
						    '#,name)
				   #`(call-with-values (lambda () exp)
				       #,#'#,k))
				  ((_ tag) (eq? (syntax->datum #'tag) '#,name)
				   #`(#,#'#,k))
				  ((_ . stuff)
				   #`(#,#'#,return-from . stuff))))))
		  #,body)))))))

(define (extended-loop src)
  (syntax-case src ()
    ((loop-id forms ...)
     (let* ((finish-id (datum->syntax #'loop-id 'loop-finish))
	    (epilog (temp 'epilog))
	    (cont (temp 'k))
	    (ctx (make-ctx src #'loop-id #'(forms ...) cont #f
			   finish-id epilog
			   '() '() '() '() '() '() '() '())))
       (maybe-name-clause ctx)
       (variable-clause* ctx)
       (main-clause* ctx)
       (emit-loop ctx)))))

(define (emit-loop ctx)
  (bind-return
   (ctx-id ctx) (ctx-name ctx) (ctx-cont ctx)
   (rebuild-block
    (ctx-with ctx)
    #`(let-syntax ((#,(ctx-epilog ctx)
		    (syntax-rules ()
		      ((_ . args) (#,(ctx-cont ctx) #,(result-form ctx))))))
	#,(rebuild-block
	   (ctx-head ctx)
	   #`(let loop (#,@(map (lambda (var) #`(#,var #,var))
				(ctx-vars ctx)))
	       #,(bind-loop-finish
		  ctx
		  (rebuild-block
		   (ctx-neck ctx)
		   (rebuild-block
		    (ctx-body ctx)
		    (rebuild-block
		     (ctx-tail ctx)
		     #`(loop #,@(ctx-vars ctx))))))))))))

;; FIXME: finish-loop doesn't work properly yet.
(define (bind-loop-finish ctx body)
  #`(let ((#,(ctx-epilog ctx)
	   (lambda (#,@(ctx-vars ctx))
	     #,(rebuild-block
		(ctx-finally ctx)
		#`(#,(ctx-cont ctx) #,(result-form ctx))))))
      (let-syntax ((#,(ctx-finish ctx)
		    (syntax-rules ()
		      ((_) (#,(ctx-epilog ctx) #,@(ctx-vars ctx))))))
	#,body)))

;; A simple-loop just binds the return continuation begins to loop.
(define (simple-loop form)
  (syntax-case form ()
    ((loop-id forms ...)
     (bind-return
      #'loop-id #f #'k
      #`(let loop ()
	  (begin forms ...)
	  (loop))))))

;; Is this a "simple" or an "extended" loop?
(define (loop-simple? forms)
  (syntax-case forms ()
    (() #t)
    ((car . _) (compound? #'car) #t)
    (_ #f)))

(define (loop-expand src)
  (syntax-case src ()
    ((loop-id forms ...)
     (let ((forms #'(forms ...)))
       (cond ((loop-simple? forms)
	      (simple-loop src))
	     (#t
	      (extended-loop src)))))))


;; tests

;; (define-syntax loop loop-expand)
;; (require 'testing)
;; (load "kloop-tests.scm")

;; (loop for a from 3 for b from 0 do (return (values a b)))
;; (loop for i below 3 for j from 5 do (format #t "~a~a\n" i j))

;; (define (foo list) (loop for a in list do (format #t "~s\n" a)))
;; (loop for a downfrom 3 to 1 by 1 do (format #t "~a\n" a))
;; (loop for a donfrom 3 to 1 by 1 do (format #t "~a\n" a))

;; (expand '(loop for a upfrom 1 to 3 by 1 do (display a)))
;; (expand ' (loop for a from 1 below 10 do (return)))
;; (require 'syntax-utils)
#|
(expand ' (loop for i in '(1 2 3 4 5 6)
		  when (and (> i 3) i)
		  collect it))

(loop-expand #' (loop for a below 10
		if (odd? a) collect a into bag and sum a into odd
		else collect (list a) into bag and sum a into even
		finally (return (list bag odd even))))
|#
;; (expand '(loop for a in '(0 1  2)))
;; (expand '(loop for a below 3 collect a))
;; (expand '(loop for a = 1 then 2 do (display a)))
;; (expand '(token-case ((for as) (for-as-clause ctx))))
;; (unquote
;; (tests)

