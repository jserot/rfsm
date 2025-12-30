SUBDIRS := systemc vhdl

all:
	$(foreach dir,$(SUBDIRS),make -C $(dir);)

clean:
	$(foreach dir,$(SUBDIRS),make -C $(dir) clean;)

clobber: clean
	$(foreach dir,$(SUBDIRS),make -C $(dir) clobber;)

