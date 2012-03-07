
all: kloop.jar

KAWA := kawa

kloop.jar: kloop.scm kloop-expand.scm kloop-runtime.scm
	$(KAWA) -d classes/ -C kloop-runtime.scm
	CLASSPATH=classes $(KAWA) -d classes/ -C kloop.scm
	jar cf $@ -C classes/ .

test: kloop.jar
	CLASSPATH=kloop.jar $(KAWA) -f run-tests.scm
	petite --libdirs . kloop-r6rs/tests.scm
	IKARUS_LIBRARY_PATH=. ikarus kloop-r6rs/tests.scm
	plt-r6rs ++path . kloop-r6rs/tests.scm
	larceny -r6rs -path . -program kloop-r6rs/tests.scm

clean:
	rm -fv kloop.jar
	rm -frv classes/
