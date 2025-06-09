include ../config.mk

all: $(TARGET).cf
clean: ; $(RM) *-*.cf

%.cf: %.cf.m4 10_intro.cf 40_posix.cf 50_bootstrap.cf ; m4 $< > $@
%-aarch64.cf: 20_aarch64.cf
%-x86_64.cf: 20_x86_64.cf
darwin-%.cf: 30_darwin.cf 35_bsd.cf
freebsd-%.cf: 30_freebsd.cf 35_bsd.cf
linux-%.cf:  30_linux.cf
