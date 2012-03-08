#!r6rs
(library (kloop-r6rs loop-expand)
    (export loop-expand)
    (import (kloop-r6rs include)
	    (for (kloop-r6rs loop-runtime) (meta -1))
	    (for (rnrs mutable-pairs) (meta -1))
	    (for (rename (rnrs) (error r6rs-error))
		 run expand (meta -1)))

 (include "kloop-expand.scm")

 )
