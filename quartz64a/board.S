// Copyright (c) 2022,2023 Eric Sessoms
// See license at end of file

.arch	armv8-a


// Copied from aarch64.S
.set	BUFFER_SIZE, 4096		// 7 blocks + metadata
.set	USER_SIZE, 8192			// 1024 variables
RSP	.req	sp			// Return stack pointer
U	.req	x21			// User pointer


	.global	main
	.section .text
main:	mov	U, RSP			// Save initial stack frame
	sub	U, U, #(BUFFER_SIZE + USER_SIZE)
	and	U, U, #-4096
	adr	x0, __bss_end		// Allocate dictionary
	add	x0, x0, #4095
	and	x0, x0, #-4096
	adr	x1, WDICTIONARY
	str	x0, [x1]
	adr	x1, XDICTIONARY
	str	x0, [x1]
	b	cold			// Relay into cold start


.set	PAGE_SHIFT, 12
.set	PAGE_FLAGS, ((PAGE_SHIFT - 10) / 2)
.set	BASE_FLAGS, 1
.set	HEAD_FLAGS, ((PAGE_FLAGS << 1) | (BASE_FLAGS << 3))

	.global	_start
	.section .text.boot
_start:	add	x13, x18, #0x16		// u32 "MZ" PE/COFF signature
	b	start			// u32 Executable code
	.dc.a	0x80000			// u64 Image load offset
	.dc.a	(_end - _start)		// u64 Effective image size
	.dc.a	HEAD_FLAGS		// u64 Kernel flags
	.dc.a	0			// u64 Reserved
	.dc.a	0			// u64 Reserved
	.dc.a	0			// u64 Reserved
	.ascii	"ARM\x64"		// u32 Magic
	.dc.l	0			// u32 PE/COFF offset
start:	adr	x3, _start		// Place stacks below code
	mov	RSP, x3
	adr	x3, __bss_start		// Clear BSS
	adr	x4, __bss_end
1:	stp	xzr, xzr, [x3], #16
	cmp	x3, x4
	blo	1b
	b	main


// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to
// whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall
// be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
// KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
