include ../config.mk
prelude  := ../$(prelude)

DISK      ?= /dev/$(LOOP)
PARTITION ?= $(DISK)p1

BOARD  := board.cf
boot   := boot
kernel := kernel.img
sdcard := sdcard.img

ifeq ($(SYSTEM),FreeBSD)
DOAS   := doas
ATTACH := $(DOAS) mdconfig -a -t vnode -f
DETACH := $(DOAS) mdconfig -d -u
MOUNT  := $(DOAS) mount -t msdos
NEWFS  := $(DOAS) newfs_msdos -A
else
DOAS   := sudo
ATTACH := $(DOAS) losetup --show --find
DETACH := $(DOAS) losetup --detach
MOUNT  := $(DOAS) mount -t
NEWFS  := $(DOAS) mkfs.vfat
endif

.PHONY:
.PHONY: _format _kernel _kformat _partition _sdcard all clean help kernel sdcard

all: 1x
help:
	@echo "clean       :: Cleanup generated files"
	@echo "distclean   :: Cleanup both generated and downloaded files"
	@echo "help        :: Show this help"
	@echo "kernel      :: Create disk image for writing to EMMC"
	@echo "sdcard      :: Create SD card image with /boot partition"

clean:
	$(RM) 1x $(kernel) $(sdcard) *.elf *.html *.o
	$(MAKE) --directory=../bootstrap $@

../bootstrap/%: ../bootstrap/%.m4 ; $(MAKE) --directory=$(dir $@) $(notdir $@)

%.o: %.S ; $(CROSS_COMPILE)$(CC) $(CFLAGS) -D'BOARD="$(BOARD)"' -D'PRELUDE="$(prelude)"' -o $@ $<
aarch64.o: aarch64.S $(BOARD) $(prelude)
board.o: board.S
vpath aarch64.S ..

# Compile to ELF
1x.elf: CFLAGS += -c
1x.elf: aarch64.o board.o
	$(CROSS_COMPILE)ld -nostdlib -T board.ld -o $@ $^

# Extract ELF to flat binary
1x: 1x.elf ; $(CROSS_COMPILE)objcopy -O binary $< $@

# Not included b/c of incompatible license.
idblock.bin rk3566-pinenote-v1.2.dtb rk3566-quartz64-a.dtb rk356x_spl_loader_v1.13.112.bin uboot.img:
	@echo See README.adoc for instructions to obtain $@
	@false

# Use FAT for /boot
_format:
	$(NEWFS) $(PARTITION)
	$(DETACH) $(LOOP)

_kformat: PARTITION := $(DISK)
_kformat: _format

$(kernel):
	truncate -s 32M $@
	$(MAKE) LOOP=$$($(ATTACH) $@) _kformat

_partition:
ifeq ($(SYSTEM),FreeBSD)
	$(DOAS) gpart create -s gpt $(LOOP)
	$(DOAS) gpart add -b 32768 -t "!0fc63daf-8483-4772-8e79-3d69d8477de4" $(LOOP)
else
	$(DOAS) sdfisk --lock $(LOOP) < partitions.txt
endif
	$(DOAS) dd of=$(DISK) if=idblock.bin seek=64
	$(DOAS) dd of=$(DISK) if=uboot.img   seek=16384
	$(DETACH) $(LOOP)

$(sdcard): idblock.bin uboot.img
	truncate -s 32M $@
	$(MAKE) LOOP=$$($(ATTACH) $@) _partition
	$(MAKE) LOOP=$$($(ATTACH) $@) _format

# Mount /boot partition locally to prepare files
$(boot):
	mkdir -p $@
	$(MOUNT) $(PARTITION) $@

$(boot)/1xforth $(boot)/extlinux: $(boot)
	$(DOAS) mkdir -p $@

$(boot)/1xforth/1x: 1x $(boot)/1xforth
	$(DOAS) cp $< $@

$(boot)/1xforth/rk3566-quartz64-a.dtb: rk3566-quartz64-a.dtb $(boot)/1xforth
	$(DOAS) cp $< $@

$(boot)/extlinux/extlinux.conf: extlinux.conf $(boot)/extlinux
	$(DOAS) cp $< $@

_sdcard: $(boot)/1xforth/1x $(boot)/1xforth/rk3566-quartz64-a.dtb $(boot)/extlinux/extlinux.conf
	$(DOAS) umount $(boot)
	$(DETACH) $(LOOP)
	rmdir $(boot)

_kernel: PARTITION := $(DEVICE)
_kernel: _sdcard

# Prepare kernel.img for rkdeveloptool
kernel: $(kernel) 1x ; $(MAKE) LOOP=$$($(ATTACH) $<) _kernel

# Create SD card image with /boot partition
sdcard: $(sdcard) 1x ; $(MAKE) LOOP=$$($(ATTACH) $<) _sdcard
