#!r6rs
(library (kloop-r6rs include)
    (export include)
    (import (rnrs))

 (define-syntax include
   (lambda (form)
     (define (read-file filename ctx)
       (call-with-input-file filename
	 (lambda (port)
	   (let loop ((result '()))
	     (let ((x (read port)))
	       (cond ((eof-object? x) (reverse result))
		     (#t (loop (cons (datum->syntax ctx x) result)))))))))
     (syntax-case form ()
       ((id filename)
	#`(begin #,@(read-file (syntax->datum #'filename) #'id))))))

 )
