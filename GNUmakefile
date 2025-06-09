include ./config.mk

main    := $(or $(wildcard $(ARCH).S),$(wildcard $(ARCH).asm))
obj     := $(basename $(main)).o
sources := $(main) $(prelude)

.PHONY:
.PHONY: clean

all: 1x

clean:
	$(RM) 1x *.core *.lst *.o
	$(MAKE) --directory=bootstrap $@

install: 1x
	install -d $(PREFIX)/bin
	install $^ $(PREFIX)/bin

bootstrap/%: bootstrap/%.m4 ; $(MAKE) --directory=$(dir $@) $(notdir $@)

nargs = -d$(SYSTEM) -dLIBC=$(LIBC) -dPRELUDE=$(prelude) -f$(format) -g
ifeq ($(SYSTEM),Darwin)
	format  = macho64
else
	format  = elf64
	listing = -Wa,-a=$(basename $1).lst
endif

%.o: %.asm ; nasm $(nargs) -l$(basename $<).lst -o $@ $<

%.o: CFLAGS += -D$(SYSTEM) -D'PRELUDE="$(prelude)"'
%.o: %.S ; $(CROSS_COMPILE)$(CC) $(CFLAGS) $(call listing,$<) -c -o $@ $<

$(obj): $(sources)
1x: $(obj) ; $(CROSS_COMPILE)$(CC) $(CFLAGS) -o $@ $^
