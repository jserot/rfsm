test: 
	@for i in {single,multi}/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make dot && make -s clean)); done
	@for i in {single,multi}/*/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make dot && make -s clean)); done

clean: 
	@for i in {single,multi}/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make -s clean) || true); done
	@for i in {single,multi}/*/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make -s clean) || true); done

clobber: 
	@for i in {single,multi}/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make -s clobber) || true); done
	@for i in {single,multi}/*/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make -s clobber) || true); done
