#!r6rs
(library (kloop-r6rs loop)
    (export loop)
    (import (rnrs)
	    (for (only (kloop-r6rs loop-expand) loop-expand) expand))

 (define-syntax loop loop-expand)
 )
