/* Link for Quartz64 Model A */
SECTIONS
{
	. = 0xa80000;  /* ${kernel_addr_r} in U-Boot */

	.text : { KEEP(*(.text.boot)) *(.text .text.*) }
	.rodata : { *(.rodata .rodata.*) }
	PROVIDE(_data = .);
	.data : { *(.data .data.*) }
	.bss (NOLOAD) : {
		. = ALIGN(16);
		__bss_start = .;
		*(.bss .bss.*)
		__bss_end = .;
	}
	_end = .;

	/DISCARD/ : { *(.comment) *(.gnu*) *(.note*) *(.eh_frame*) }
}
