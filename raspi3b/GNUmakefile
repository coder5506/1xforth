include ../config.mk
prelude  := ../$(prelude)

# Download location for Raspberry Pi firmware
FIRMWARE  ?= https://github.com/raspberrypi/firmware/raw/master/boot

DISK      ?= $(LOOP)
PARTITION ?= $(DISK)p1

BOARD  := board.cf
boot   := boot
cache  := .cache
sdcard := sdcard.img

.PHONY:
.PHONY: _format _partition _sdcard all clean distclean help sdcard

all: kernel8.img
help:
	@echo "clean       :: Cleanup generated files"
	@echo "distclean   :: Cleanup both generated and downloaded files"
	@echo "help        :: Show this help"
	@echo "kernel8.img :: (default) Build a bootable kernel image"
	@echo "sdcard      :: Create SD card image with /boot partition"

clean:
	$(RM) 1x $(sdcard) *.img *.elf *.html *.o
	$(MAKE) --directory=../bootstrap $@

distclean: clean ; $(RM) -R $(cache)

../bootstrap/%: ../bootstrap/%.m4 ; $(MAKE) --directory=$(dir $@) $(notdir $@)

%.o: %.S ; $(CROSS_COMPILE)$(CC) $(CFLAGS) -D'BOARD="$(BOARD)"' -D'PRELUDE="$(prelude)"' -o $@ $<
aarch64.o: aarch64.S $(BOARD) $(prelude)
board.o: board.S
vpath aarch64.S ..

# Compile to ELF
kernel8.elf: CFLAGS += -c
kernel8.elf: aarch64.o board.o
	$(CROSS_COMPILE)ld -nostdlib -T board.ld -o $@ $^

# Extract ELF to flat binary
kernel8.img: kernel8.elf
	$(CROSS_COMPILE)objcopy -O binary $< $@

_partition:
	sudo sfdisk --lock $(LOOP) < partitions.txt
	sudo losetup --detach $(LOOP)

# Use FAT for /boot
_format:
	sudo mkfs.vfat -F32 $(PARTITION)
	sudo losetup --detach $(LOOP)

$(sdcard):
	truncate --size=68M $@
	$(MAKE) LOOP=$$(sudo losetup --show --find $@) _partition
	$(MAKE) LOOP=$$(sudo losetup --show --partscan --find $@) _format

# Mount /boot partition locally to prepare files
$(boot):
	mkdir --parents $@
	sudo mount $(PARTITION) $@

# Required binary blobs
firmware := \
	COPYING.linux       \
	LICENSE.broadcom    \
	bcm2710-rpi-3-b.dtb \
	bootcode.bin        \
	fixup_cd.dat        \
	start_cd.elf
firmware := $(addprefix $(boot)/,$(firmware))

# Download required firmware
.PRECIOUS: $(cache)/%
$(cache)/%:
	mkdir --parents $(dir $@)
	curl $(FIRMWARE)/$(notdir $@) --location --silent --remote-name --output-dir $(dir $@)

# Copy firmware to /boot partition
$(boot)/%: $(cache)/% $(boot) ; sudo cp $< $@

# Copy kernel to /boot partition
$(boot)/config.txt:  config.txt  $(boot) ; sudo cp $< $@
$(boot)/kernel8.img: kernel8.img $(boot) ; sudo cp $< $@

# Install kernel and all required firmware to SD card
_sdcard: $(boot)/config.txt $(boot)/kernel8.img $(firmware)
	sudo umount $(boot)
	sudo losetup --detach $(LOOP)
	rmdir $(boot)

# Create SD card image with /boot partition
sdcard: $(sdcard) config.txt kernel8.img
	$(MAKE) LOOP=$$(sudo losetup --show --partscan --find $<) _sdcard
