clean: 
	for i in {single,multi}/*; do (test -d $$i && test -e $$i/Makefile && (echo "Cleaning in $$i"; cd $$i; make -s clean) || true); done
	for i in {single,multi}/*/*; do (test -d $$i && test -e $$i/Makefile && (echo "Cleaning in $$i"; cd $$i; make -s clean) || true); done

clobber: 
	@for i in {single,multi}/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make -s clobber) || true); done
	@for i in {single,multi}/*/*; do (test -d $$i && test -e $$i/Makefile && (cd $$i; make -s clobber) || true); done
