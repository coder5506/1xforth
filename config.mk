# Standardize ARCH by indirecting "uname -m"
aarch64 := aarch64
amd64   := x86_64
arm64   := aarch64
x86_64  := x86_64


CROSS_COMPILE ?=
ARCH    ?= $($(shell uname -m))
SYSTEM  ?= $(shell uname -o)
HOST    ?= $(shell echo "$(SYSTEM)-$(ARCH)" | tr A-Z a-z)
TARGET  ?= $(HOST)

LIBC    ?= $(findstring Darwin,$(SYSTEM))
ASFLAGS := -g $(if $(LIBC),,-ffreestanding -nostdlib -static)
CFLAGS  := $(ASFLAGS)
PREFIX  ?= $(HOME)/.local

prelude := bootstrap/$(TARGET).cf
