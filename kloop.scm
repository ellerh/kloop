
(module-export loop 
	       ;; re-exported from runtime but shouldn't be needed
	       gvref glength 
	       )

(import (rnrs lists))
(import (rnrs hashtables))
(import (kloop-runtime))

(define (syntax-violation who message form #!optional subform)
  (let* ((in (if who 
		 (format "~a" who)
		 (syntax-case form ()
		   ((x . _) (syntax->datum #'x))
		   (x (syntax->datum #'x)))))
	 (fsrc (syntax->datum form))
	 (src (if subform 
		  (format "~s in ~s" (syntax->datum subform) fsrc)
		  fsrc)))
    (error (format "Invalid syntax in ~a: ~a ~a" in message src))))

(define-syntax assert 
  (syntax-rules ()
    ((_ exp) (unless exp (error "assertion failed" 'exp)))))

(include-relative "kloop-expand.scm")

(define-syntax loop loop-expand)
