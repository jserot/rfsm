include ../etc/platform

makefiles:
	@for i in std/{single,multi}/*; do (cd $$i; $(RFSMMAKE) -with-test-targets -no-confirm -lib $(LIBDIR) -compiler $(RFSMC) -o Makefile ./main.pro) done

clean:
	@for i in std/{single,multi}/*; do (cd $$i; make clean) done

clobber:
	@for i in std/{single,multi}/*; do (cd $$i; make clobber) done
