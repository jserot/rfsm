include VERSION
include config

PACKNAME=rfsm

INSTALLED = src/lib/_build/*.{mli,cmi,cma} 
ifeq ($(BUILD_NATIVE),yes)
	INSTALLED += src/lib/_build/*.{cmx,cmxa,a}
endif

QMAKE_MACOS = /Developer/Qt5.2.1/5.2.1/clang_64/bin/qmake 
QMAKE_WIN = C:/Qt/Qt5.8.0/5.8/mingw53_32/bin/qmake.exe
MAKE_WIN = C:/Qt/Qt5.8.0/Tools/mingw530_32/bin/mingw32-make

.PHONY: compiler lib gui clean test doc

all: 		lib compiler libs gui doc

compiler:
			(cd src/compiler; make)

lib:
			(cd src/lib; make byte)
ifeq ($(BUILD_NATIVE),yes)
			(cd src/lib; make native)
endif

gui:
	cat src/gui/builtin_options.txt src/compiler/options_spec.txt > src/gui/options_spec.txt
ifeq ($(PLATFORM), win32)
	(cd src/gui; $(QMAKE_WIN) -spec win32-g++ rfsm.pro; $(MAKE_WIN))
else
	(cd src/gui; $(QMAKE_MACOS) -spec macx-clang CONFIG+=x86_64 rfsm.pro; make)
endif

libs:
	(cd lib/systemc; make)
	(cd lib/vhdl; make)

doc: 
	(cd src/lib; make doc)
	rm -f doc/lib/*
	cp src/lib/rfsm.docdir/* doc/lib
	(cd doc/um; make; cp rfsm.pdf ..)
	pandoc -o CHANGELOG.txt CHANGELOG.md
	pandoc -o README.txt README.md

test:
	echo "Testing"

clean:
			(cd src/lib; make clean)
			(cd src/compiler; make clean)
			(cd src/gui; make clean)
			(cd lib; make clean)
			(cd examples; make clean)
			(cd doc/um; make clean)
			\rm -f doc/lib/*
			\rm -f doc/*.*

clobber: 
			(cd src/lib; make clobber)
			(cd src/compiler; make clobber)
			(cd src/gui; make clean)
			(cd lib; make clobber)
			(cd examples; make clobber)
			(cd doc/um; make clobber)
			\rm -f doc/lib/*
			\rm -f doc/*.*
			\rm -f *~

install-lib: 
	@echo "Installing $(PACKNAME) in $(INSTALL_LIBDIR)"
	rm -rf $(INSTALL_LIBDIR)/$(PACKNAME)
	ocamlfind install -destdir $(INSTALL_LIBDIR) $(PACKNAME) META $(INSTALLED)

install-doc:
	@echo "Installing $(PACKNAME) documentation in $(INSTALL_DOCDIR)"
	rm -rf $(INSTALL_DOCDIR)/$(PACKNAME)
	mkdir $(INSTALL_DOCDIR)/$(PACKNAME)
	cp -r doc/lib/*.{html,css} $(INSTALL_DOCDIR)/$(PACKNAME)

uninstall-lib:
	@echo "Removing $(PACKNAME) from $(INSTALL_LIBDIR)"
	rm -rf $(INSTALL_LIBDIR)/$(PACKNAME)

uninstall-doc:
	@echo "Removing $(PACKNAME) doc from $(INSTALL_DOCDIR)"
	rm -rf $(INSTALL_DOCDIR)/$(PACKNAME)

DISTDIR=/tmp/rfsm-$(VERSION)-source

source-dist: 
	@echo "** Cleaning"
	make clobber
	rm -rf $(DISTDIR)
	@echo "** Creating $(DISTDIR)"
	mkdir -p $(DISTDIR)
	mkdir -p $(DISTDIR)/compiler
	mkdir -p $(DISTDIR)/lib
	mkdir -p $(DISTDIR)/lib/etc
	mkdir -p $(DISTDIR)/lib/systemc
	mkdir -p $(DISTDIR)/lib/vhdl
	mkdir -p $(DISTDIR)/examples
	mkdir -p $(DISTDIR)/man
	mkdir -p $(DISTDIR)/doc
#	@echo "** Building doc and man pages"
#	(cd doc/um; $(MAKE) all)
#	(cd doc/primer; $(MAKE) all)
#	(cd man; $(MAKE) all)
	@echo "** Copying files"
	cp lib/etc/* $(DISTDIR)/lib/etc
	cp lib/systemc/{*.h,Makefile} $(DISTDIR)/lib/systemc
	cp lib/vhdl/{*.vhd,Makefile} $(DISTDIR)/lib/vhdl
	cp -r examples $(DISTDIR)/examples
#	cp man/*.$(MANEXT) $(DISTDIR)/man
#	cp man/Makefile.public $(DISTDIR)/man/Makefile
#	cp doc/um/caph-lrm.pdf doc/primer/caph-primer.pdf $(DISTDIR)/doc
#	cp doc/Makefile.public $(DISTDIR)/doc/Makefile
#	cp configure CHANGES README KNOWN-BUGS LICENSE VERSION $(DISTDIR)
	cp configure ReadMe VERSION $(DISTDIR)
	cp -r compiler/{*.ml,*.mli,Makefile,*.mly,*.mll,_tags,TAGS} $(DISTDIR)/compiler
	cp -r gui $(DISTDIR)/gui
#	cp dist/source/{Makefile,INSTALL} $(DISTDIR)
	@echo "** Creating archive $(DISTDIR).tar.gz"
	(cd /tmp; tar -zcvf rfsm-$(VERSION)-source.tar.gz rfsm-$(VERSION)-source)

MACOS_DIST=/tmp/rfsm

macos-dist:
	@echo "** Cleaning"
	make clobber
	@echo "** Configuring for MacOS distribution"
	./configure -platform macos -dot "dot" -dotviewer "open -a Graphviz" -vcdviewer "open -a gtkwave" -txtviewer "open"
	@echo "** Building"
	(cd src/compiler; make)
	(cd src/gui; make)
	(cd doc/um; make; cp rfsm.pdf ..)
	make macos-install
	make macos-installer

macos-install:
	@echo "** Installing in $(MACOS_DIST)"
	rm -rf $(MACOS_DIST)
	mkdir $(MACOS_DIST)
	cp -r src/gui/rfsm.app $(MACOS_DIST)/Rfsm.app
	cp src/compiler/main.native $(MACOS_DIST)/Rfsm.app/Contents/MacOS/rfsmc
	cp ./dist/macos/rfsm.ini $(MACOS_DIST)/Rfsm.app/Contents/MacOS
	cp ./dist/macos/INSTALL $(MACOS_DIST)/INSTALL
#	mkdir $(MACOS_DIST)/doc
#	cp -r doc/lib $(MACOS_DIST)/doc
	cp  doc/rfsm.pdf $(MACOS_DIST)/UserManual.pdf
	mkdir $(MACOS_DIST)/examples
	mkdir $(MACOS_DIST)/examples/{single,multi}
	cp -r examples/single/{chrono,gensig,doubleimp,mousectlr,mul8} $(MACOS_DIST)/examples/single
	cp -r examples/multi/ctrmod8 $(MACOS_DIST)/examples/multi
	cp {CHANGELOG.txt,KNOWN-BUGS,LICENSE,README.txt,VERSION} $(MACOS_DIST)

RFSM_VOLUME=Rfsm-$(VERSION)

macos-installer:
	@echo "** Creating disk image"
	rm -f /tmp/Rfsm.dmg
	hdiutil create -size 32m -fs HFS+ -volname "$(RFSM_VOLUME)" /tmp/Rfsm.dmg
	hdiutil attach /tmp/Rfsm.dmg
	cp -r $(MACOS_DIST)/Rfsm.app /Volumes/$(RFSM_VOLUME)
	ln -s /Applications /Volumes/$(RFSM_VOLUME)/Applications
	cp -r $(MACOS_DIST)/examples /Volumes/$(RFSM_VOLUME)/Examples
#	cp -r $(MACOS_DIST)/doc /Volumes/$(RFSM_VOLUME)/Documentation
	cp $(MACOS_DIST)/{CHANGELOG.txt,KNOWN-BUGS,LICENSE,README.txt,VERSION,INSTALL,UserManual.pdf} /Volumes/$(RFSM_VOLUME)
	hdiutil detach /Volumes/$(RFSM_VOLUME)
	hdiutil convert /tmp/Rfsm.dmg -format UDZO -o /tmp/Rfsm_ro.dmg
	mv /tmp/Rfsm_ro.dmg /tmp/Rfsm.dmg
	@echo "** Done. Disk image is /tmp/Rfsm.dmg"

WIN_SRC_DIR=~/Desktop/SF1/Caml

win32-pre:
	@echo "** Preparing Windows version.."
	@echo "** Cleaning source directory.."
	make clobber
	@echo "Building documentation"
	(cd doc/um; make; cp rfsm.pdf ..)
	@echo "** Copying source tree"
	if [ -d $(WIN_SRC_DIR)/rfsm ]; then rm -rf $(WIN_SRC_DIR)/rfsm.bak; mv $(WIN_SRC_DIR)/rfsm $(WIN_SRC_DIR)/rfsm.bak; fi
	(cd ..; cp -r working $(WIN_SRC_DIR)/rfsm)
	@echo "** Done"
	@echo "** Now, make win32-{gui,compiler} from Windows"

win32-compiler:
	@echo "***********************************************************************"
	@echo "**** WARNING: this make step must be invoked from a [Cygwin] shell ****"
	@echo "***********************************************************************"
	@echo "** Building compiler"
	make compiler
	@echo "** Done"

win32-gui:
	@echo "******************************************************************************"
	@echo "**** WARNING: this make step must be invoked from a [mingw32(MSYS)] shell ****"
	@echo "******************************************************************************"
	./configure -platform win32 -dot "/C/Program Files/Graphviz/bin/dot.exe" -dotviewer "/C/Program Files/Graphviz/bin/dotty.exe" -vcdviewer "/C/Program Files/gtkwave/bin/gtkwave.exe"
	@echo "** Building GUI"
	make gui
	@echo "** Done"

WIN_INSTALL_DIR=./build

win32-install:
	@echo "** Installing in $(WIN_INSTALL_DIR)"
	rm -rf $(WIN_INSTALL_DIR)
	mkdir $(WIN_INSTALL_DIR)
	cp ./src/gui/release/rfsm.exe $(WIN_INSTALL_DIR)
	mkdir $(WIN_INSTALL_DIR)/bin
	cp ./src/compiler/_build/main.native $(WIN_INSTALL_DIR)/bin/rfsmc.exe
	cp ../caph/caphy-dlls/{Qt5Core,Qt5Gui,Qt5Widgets,libgcc_s_dw2-1,libstdc++-6,libwinpthread-1}.dll $(WIN_INSTALL_DIR)
	mkdir $(WIN_INSTALL_DIR)/platforms
	cp ../caph/caphy-dlls/qwindows.dll $(WIN_INSTALL_DIR)/platforms
	cp {CHANGELOG.txt,KNOWN-BUGS,LICENSE,README.txt,VERSION} $(WIN_INSTALL_DIR)
	cp ./dist/windows/FIRST.TXT $(WIN_INSTALL_DIR)
	cp ./dist/windows/icons/*.{bmp,ico} $(WIN_INSTALL_DIR)
	mkdir $(WIN_INSTALL_DIR)/doc
	cp  doc/rfsm.pdf $(WIN_INSTALL_DIR)/doc
	mkdir $(WIN_INSTALL_DIR)/examples
	mkdir $(WIN_INSTALL_DIR)/examples/{single,multi}
	cp -r examples/single/{chrono,gensig,doubleimp,mousectlr,mul8} $(WIN_INSTALL_DIR)/examples/single
	cp -r examples/multi/ctrmod8 $(WIN_INSTALL_DIR)/examples/multi
	@echo "Done"

win32-installer:
	@echo "** Building self-installer"
	/C/Program\ Files/Inno\ Setup\ 5/iscc ./dist/windows/RfsmSetup.iss

