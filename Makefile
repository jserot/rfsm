ifeq ($(OS),Windows_NT)
  CLEAN_CMD = cmd /c "if exist _build rmdir /s /q _build"
else
  CLEAN_CMD = dune clean
endif

ifeq ($(OS),Windows_NT)
all: host std_guest 
else
all: host std_guest doc
endif

host:
	make -C src/host

all_guests: std_guest other_guests

std_guest:
	make -C src/guests/std

OTHER_GUESTS := core mini simple szdints szvars

other_guests:
	$(foreach guest,$(OTHER_GUESTS),make -C $(guest);)

install:
	dune build @install

doc: doc.html doc.pdf

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc.html:
	dune build @doc
	cp -r -p _build/default/_doc/_html/* ./docs
	chmod -R u+w ./docs

doc.pdf:
	(cd ./docs/user_manual; make)
	(cd ./docs/ref_manual; make)

ifeq ($(OS),Windows_NT)
clean:
	cmd /c "if exist _build rmdir /s /q _build"
	make -C examples clean
clobber: clean
	del /Q *~
else
clean:
	dune clean
	make -C examples clean
clobber: clean
	rm -f *~
endif
