include VERSION
include config

PACKNAME=rfsm

LIB_INSTALLED = \
  src/lib/_build/*.mli \
  src/lib/_build/*.cmi \
  src/lib/_build/*.cma 
BIN_INSTALLED = src/compiler/rfsmc
ifeq ($(BUILD_NATIVE),yes)
	LIB_INSTALLED += \
      src/lib/_build/*.cmx \
      src/lib/_build/*.cmxa \
      src/lib/_build/*.a
    BIN_INSTALLED += src/compiler/rfsmc.opt
endif

QMAKE_MACOS = /Developer/Qt5.2.1/5.2.1/clang_64/bin/qmake 
QMAKE_WIN = C:/Qt/Qt5.8.0/5.8/mingw53_32/bin/qmake.exe
MAKE_WIN = C:/Qt/Qt5.8.0/Tools/mingw530_32/bin/mingw32-make

.PHONY: compiler lib gui clean test doc install dist opam opam-doc

all: 		lib compiler libs gui doc

opam: lib compiler opam-doc

lib:
			(cd src/lib; make byte)
ifeq ($(BUILD_NATIVE),yes)
			(cd src/lib; make native)
endif

compiler:
			(cd src/compiler; make byte)
			mv src/compiler/main.byte src/compiler/rfsmc
ifeq ($(BUILD_NATIVE),yes)
			(cd src/compiler; make native)
			mv src/compiler/main.native src/compiler/rfsmc.opt
endif

gui:
ifeq ($(BUILD_GUI),yes)
	cat src/gui/builtin_options.txt src/compiler/options_spec.txt > src/gui/options_spec.txt
ifeq ($(PLATFORM), win32)
	(cd src/gui; $(QMAKE_WIN) -spec win32-g++ rfsm.pro; $(MAKE_WIN))
else
	(cd src/gui; $(QMAKE_MACOS) -spec macx-clang CONFIG+=x86_64 rfsm.pro; make)
endif
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

opam-doc: 
	(cd src/lib; make doc)
	rm -f doc/lib/*
	cp src/lib/rfsm.docdir/* doc/lib
	(cd doc/um; make; cp rfsm.pdf ..)

clean:
	(cd src/lib; make clean)
	(cd src/compiler; make clean)
	(cd src/gui; make clean)
	(cd lib; make clean)
	(cd examples; make clean)
	(cd doc/um; make clean)
	rm -f doc/lib/*

clobber: 
	(cd src/lib; make clobber)
	(cd src/compiler; make clobber)
	(cd src/gui; make clean)
	(cd lib; make clobber)
	(cd examples; make clobber)
	(cd doc/um; make clobber)
	rm -f doc/lib/*
	\rm -f src/gui/rfsm.app/Contents/MacOS/rfsm
	\rm -f *~

install:
	mkdir -p $(INSTALL_LIBDIR)
	cp -r lib/ml $(INSTALL_LIBDIR)
	cp -r lib/etc $(INSTALL_LIBDIR)
	mkdir -p $(INSTALL_LIBDIR)/systemc
	cp lib/systemc/*.{o,h} $(INSTALL_LIBDIR)/systemc
	mkdir -p $(INSTALL_LIBDIR)/vhdl
	cp lib/vhdl/*.vhd $(INSTALL_LIBDIR)/vhdl
	mkdir -p $(INSTALL_BINDIR)
	cp src/compiler/rfsmc $(INSTALL_BINDIR)
ifeq ($(BUILD_NATIVE),yes)
	cp src/compiler/rfsmc.opt $(INSTALL_BINDIR)
endif
ifeq ($(BUILD_GUI),yes)
	cp src/gui/rfsm $(INSTALL_BINDIR)/rfsm
endif
	mkdir -p $(INSTALL_DOCDIR)
	cp -r doc/lib $(INSTALL_DOCDIR)
	cp -r doc/um/rfsm.pdf $(INSTALL_DOCDIR)/rfsm-manual.pdf

install-opam: 
	@echo "Installing $(PACKNAME) in $(INSTALL_LIBDIR)"
	rm -rf $(INSTALL_LIBDIR)/$(PACKNAME)
	ocamlfind install -destdir $(INSTALL_LIBDIR) $(PACKNAME) META $(LIB_INSTALLED)
	@echo "Installing rfsmc in $(INSTALL_BINDIR)"
	cp $(BIN_INSTALLED) $(INSTALL_BINDIR)
	@echo "Installing $(PACKNAME) documentation in $(INSTALL_DOCDIR)"
	rm -rf $(INSTALL_DOCDIR)/$(PACKNAME)
	mkdir $(INSTALL_DOCDIR)/$(PACKNAME)
	cp doc/lib/*.html doc/lib/*.css $(INSTALL_DOCDIR)/$(PACKNAME)
	@echo "Installing emacs mode in $(INSTALL_EMACSDIR)"
	mkdir -p $(INSTALL_EMACSDIR)
	cp lib/etc/rfsm-mode.el $(INSTALL_EMACSDIR)

uninstall-opam:
	@echo "Removing $(PACKNAME) from $(INSTALL_LIBDIR)"
	rm -rf $(INSTALL_LIBDIR)/$(PACKNAME)
	@echo "Removing rfsmc from $(INSTALL_BINDIR)"
	rm -f $(INSTALL_BINDIR)/rfsmc $(INSTALL_BINDIR)/rfsmc.opt
	@echo "Removing $(PACKNAME) doc from $(INSTALL_DOCDIR)"
	rm -rf $(INSTALL_DOCDIR)/$(PACKNAME)
	@echo "Removing emacs mode from $(INSTALL_EMACSDIR)"
	rm -f $(INSTALL_EMACSDIR)/rfsm-mode.el

DISTDIR=/tmp/rfsm-$(VERSION)-source

source-dist: 
	@echo "** Cleaning"
	make clobber
	@echo "** Creating $(DISTDIR)"
	rm -rf $(DISTDIR)
	mkdir -p $(DISTDIR)
	@echo "** Copying files"
	cp -r {lib,src,doc} $(DISTDIR)
	mkdir -p $(DISTDIR)/examples
	cp -r examples/{single,multi,Makefile} $(DISTDIR)/examples
	cp configure CHANGELOG.md README.md KNOWN-BUGS LICENSE VERSION INSTALL Makefile $(DISTDIR)
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
	make doc
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
	mkdir $(MACOS_DIST)/doc
#	cp -r doc/lib $(MACOS_DIST)/doc
	cp  doc/um/rfsm.pdf $(MACOS_DIST)/doc/rfsm-manual.pdf
	mkdir $(MACOS_DIST)/examples
	mkdir $(MACOS_DIST)/examples/{single,multi}
	cp -r examples/single $(MACOS_DIST)/examples
	cp -r examples/multi $(MACOS_DIST)/examples
	cp {CHANGELOG.txt,KNOWN-BUGS,LICENSE,README.txt} $(MACOS_DIST)

RFSM_VOLUME=Rfsm-$(VERSION)

macos-installer:
	@echo "** Creating disk image"
	rm -f /tmp/Rfsm.dmg
	hdiutil create -size 32m -fs HFS+ -volname "$(RFSM_VOLUME)" /tmp/Rfsm.dmg
	hdiutil attach /tmp/Rfsm.dmg
	cp -r $(MACOS_DIST)/Rfsm.app /Volumes/$(RFSM_VOLUME)
	ln -s /Applications /Volumes/$(RFSM_VOLUME)/Applications
	cp -r $(MACOS_DIST)/examples /Volumes/$(RFSM_VOLUME)/Examples
	cp -r $(MACOS_DIST)/doc /Volumes/$(RFSM_VOLUME)/Documentation
	cp $(MACOS_DIST)/{CHANGELOG.txt,KNOWN-BUGS,LICENSE,README.txt,INSTALL} /Volumes/$(RFSM_VOLUME)
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
	cp {CHANGELOG.txt,KNOWN-BUGS,LICENSE,README.txt} $(WIN_INSTALL_DIR)
	cp ./dist/windows/FIRST.TXT $(WIN_INSTALL_DIR)
	cp ./dist/windows/icons/*.{bmp,ico} $(WIN_INSTALL_DIR)
	mkdir $(WIN_INSTALL_DIR)/doc
	cp  doc/rfsm.pdf $(WIN_INSTALL_DIR)/doc
	mkdir $(WIN_INSTALL_DIR)/examples
	mkdir $(WIN_INSTALL_DIR)/examples/{single,multi}
	cp -r examples/single $(WIN_INSTALL_DIR)/examples
	cp -r examples/multi $(WIN_INSTALL_DIR)/examples
	@echo "Done"

win32-installer:
	@echo "** Building self-installer"
	/C/Program\ Files/Inno\ Setup\ 5/iscc ./dist/windows/RfsmSetup.iss

# Targets for building and deploying distribution

TMPDIR=/tmp
DISTNAME=rfsm
DISTDIR=$(TMPDIR)/rfsm
EXCLUDES=--exclude .git --exclude .gitignore --exclude .DS_Store
TARBALL=$(DISTNAME).tar

dist: 
	@make -f Makefile clean
	@rm -rf $(DISTDIR)
	@mkdir $(DISTDIR)
	@echo "** Copying files into $(DISTDIR)"
	(rsync --quiet -avz $(EXCLUDES) . $(DISTDIR))
	@ echo "** Creating tarball"
	@(cd $(TMPDIR); tar cf $(TARBALL) $(DISTNAME); gzip -f $(TARBALL))
	@ echo "** File $(TMPDIR)/$(TARBALL).gz is ready."
	echo "archive: \"http://cloud.ip.univ-bpclermont.fr/~serot/rfsm/dist/rfsm.tar.gz\"" > url
	echo "checksum: \""`md5 -q $(TMPDIR)/$(TARBALL).gz`"\"" >> url
	@echo "Created file ./url"

export:
	ncftpput -u serot ftp.ip.uca.fr /home/www/rfsm/dist $(TMPDIR)/$(TARBALL).gz

