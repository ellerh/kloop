
(import (only (kloop) loop gvref glength))

(import (rnrs lists)
	(rnrs hashtables)
	(gnu kawa slib testing))

(define flonum? gnu.math.DFloNum?)

(test-begin "kloop-tests")

(include "kloop-tests-0.scm")

(include "kloop-tests-1.scm")
(include "kloop-tests-2.scm")
(include "kloop-tests-3.scm")
(include "kloop-tests-4.scm")
(include "kloop-tests-5.scm")
(include "kloop-tests-6.scm")
(include "kloop-tests-7.scm")
(include "kloop-tests-8.scm")
(include "kloop-tests-9.scm")
(include "kloop-tests-10.scm")
;;(include "kloop-tests-11.scm")
(include "kloop-tests-12.scm")

(test-end "kloop-tests")
