include ../etc/platform

SUBDIRS := $(wildcard std/single/* std/multi/*)
CLEAN_SUBDIRS := $(filter $(wildcard %/Makefile), $(SUBDIRS:%=%/Makefile))
CLEAN_DIRS := $(patsubst %/Makefile,%,$(CLEAN_SUBDIRS))

clean:
	$(foreach dir,$(CLEAN_DIRS),make -C $(dir) clean;)

clobber:
	$(foreach dir,$(CLEAN_DIRS),make -C $(dir) clobber;)

MAKEFILES_DIRS := $(filter $(wildcard %/main.pro), $(SUBDIRS:%=%/main.pro))
MAKEFILES_DIRS := $(patsubst %/main.pro,%,$(MAKEFILES_DIRS))

ifneq ($(OS),Windows_NT)
makefiles:
	$(foreach dir,$(MAKEFILES_DIRS),\
		$(RFSMMAKE) -with-test-targets -no-confirm -lib $(LIBDIR) -compiler $(RFSMC) -o $(dir)/Makefile $(dir)/main.pro;)
endif

