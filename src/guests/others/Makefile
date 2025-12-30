SUBIDRS := simplel szdints szvars

all:
	$(foreach dir,$(SUBDIRS),$(MAKE) -C $(dir);)

clean:
	$(foreach dir,$(SUBDIRS),$(MAKE) -C $(dir) clean;)
