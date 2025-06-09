// Copyright (c) 2022,2023 Eric Sessoms
// See license at end of file

.arch	armv8-a


// System interface (portable?)
.set	MAP_PRIVATE, 2
.set	PROT_EXEC,  4
.set	PROT_WRITE, 2
.set	PROT_READ,  1
.set	stdin,  0
.set	stdout, 1

#ifdef Darwin
#define main _main
#define weak weak_definition
.set	MAP_ANONYMOUS, 0x1000
.set	MAP_JIT,       0x0800
.set	SYS_exit,   1
.set	SYS_mmap, 197
.set	SYS_read,   3
.set	SYS_write,  4
.set	SYS_SVC, 0x80
SYS_REG	.req	x16			// System call register
.macro	ADRL	reg, sym
	adrp	\reg, \sym@page
	add	\reg, \reg, \sym@pageoff
.endm
#else
#define const section .rodata
.set	MAP_ANONYMOUS, 0x20
.set	MAP_JIT,          0
.set	SYS_exit,  93
.set	SYS_mmap, 222
.set	SYS_read,  63
.set	SYS_write, 64
.set	SYS_SVC, 0
SYS_REG	.req	x8
.macro	ADRL	reg, sym
	adr	\reg, \sym
.endm
#endif


// Memory map, from high-memory to low
// - Process information block
// - Block buffers
// - User variables
// - Parameter stack
// - Return stack
// - Terminal input buffer
// - Pad
// - User dictionary
// - System dictionary
// - System variables
// - Pre-compiled Forth

.set	BUFFER_SIZE, 4096		// 7 blocks + metadata
.set	USER_SIZE, 8192			// 1024 variables
.set	DATA_SIZE, 4096			// 512 cells ought to be enough for anybody
.set	RETURN_SIZE, 8192		// 512 cells, 16-byte alignment

// i.e., one hugepage
.set	DICT_SIZE, (2*1024*1024)	// Almost 2MB for definitions
.set	WORDLIST_SIZE, (2048*16)	// 1792 forth headers
.set	MACRO_SIZE, (256*16)		//  256 macro headers


// Registers
// - X0  parameter 1 / result 1 / scratch
// - X1  parameter 2 / result 2
// - X2  parameter 3
// - X3  parameter 4
// - X4  parameter 5
// - X5  parameter 6
// - X6  parameter 7
// - X7  parameter 8
// - X8  [alias XR] indirect return value address
// - X9  caller saved
// - X10 caller saved
// - X11 caller saved
// - X12 caller saved
// - X13 caller saved
// - X14 caller saved
// - X15 caller saved
// - X16 [alias IP0, intra-procedure-call scratch register]
// - X17 [alias IP1, intra-procedure-call scratch register]
// - X18 [platform register]
// - X19 S (second on stack)
// - X20 T (top of stack)
// - X21 U (user pointer)
// - X22 W (anonymous local variable)
// - X23 X (anonymous local variable)
// - X24 Y (anonymous local variable)
// - X25 Z (anonymous local variable)
// - X26 A (anonymous local variable)
// - X27 B (anonymous local variable)
// - X28 callee saved
// - X29 [alias FP] PSP (parameter stack pointer)
// - X30 [alias LR] R (top of return stack, link register)
// - X31 [alias SP] RSP (return stack pointer)

PSP	.req	fp			// Parameter stack pointer
RSP	.req	sp			// Return stack pointer

R	.req	lr			// Top of return stack
S	.req	x19			// Second on stack
SW	.req	w19			// 32-bit "word" name for S
T	.req	x20			// Top of stack
TW	.req	w20			// 32-bit "word" name for T
U	.req	x21			// User pointer
W	.req	x22			// Anonymous local variable
X	.req	x23			// Anonymous local variable
Y	.req	x24			// Anonymous local variable
Z	.req	x25			// Anonymous local variable
A	.req	x26			// Anonymous local variable
B	.req	x27			// Anonymous local variable


// Conveniences

// Second-on-stack is no longer needed
.macro	NIP
	ldr	S, [PSP], #8
.endm

// Top-of-stack is no longer needed
.macro	DROP1
	mov	T, S
	NIP
.endm

// Top two items on stack are no longer needed
.macro	DROP2
	ldp	T, S, [PSP], #16
.endm

// Pop parameter stack into REG
.macro	POP1	reg
	mov	\reg, T
	DROP1
.endm

// Same effect as "POP1 \reg1 ; POP1 \reg2"
.macro	POP2	reg1, reg2
	mov	\reg1, T
	mov	\reg2, S
	ldp	T, S, [PSP], #16
.endm

// Restore the link register
.macro	POPLR	reg
	ldp	R, \reg, [RSP], #16
.endm

// Pop return stack into REG
.macro	POPRSP	reg
	mov	\reg, R
	ldr	R, [RSP], #16
.endm

// Push REG onto parameter stack
.macro	PUSH1	reg
	str	S, [PSP, #-8]!
	mov	S, T
	mov	T, \reg
.endm

// Same effect as "PUSH1 \reg1 ; PUSH1 \reg2"
.macro	PUSH2	reg1, reg2
	stp	T, S, [PSP, #-16]!
	mov	S, \reg1
	mov	T, \reg2
.endm

// Save the link register on the return stack.
// REG can save a second register "for free", to preserve alignment.
.macro	PUSHLR	reg
	stp	R, \reg, [RSP, #-16]!
.endm

// Push REG onto return stack
.macro	PUSHRSP	reg
	str	R, [RSP, #-16]!
	mov	R, \reg
.endm


.macro	CALL	name
	str	R, [RSP, #-16]!
	bl	\name
.endm

.macro	RETURN
	POPRSP	x2
	ret	x2
.endm


// We define our colors to match their ANSI escape codes. For no
// necessary reason, it just makes them easier to remember.
//
// The colors themselves are not important, we're only
// interested in their meaning, but the color names provide a
// convenient handle with which to refer to the meaning.
//
// This gives us a consistent vocabulary.
//
// Notes:
// - Bit #3 (8) is used as an "alternate" flag.

.set	BLACK, 0			// Data
.set	RED, 1				// Label
.set	GREEN, 2			// Compile
.set	YELLOW, 3			// Execute
.set	BLUE, 4				// Format
.set	MAGENTA, 5			// Retain
.set	CYAN, 6				// Postpone
.set	WHITE, 7			// Comment
.set	ALT, 8				// Alternative usage


// User variables

// Address of user variable
.macro	UADDR	reg, var
	add	\reg, U, #\var
.endm

// Value of user variable
.macro	UFETCH	reg, var
	UADDR	\reg, \var
	ldr	\reg, [\reg]
.endm

// Declare user variable
.set	UP, 0				// Next available offset
.macro	USER	var
	.set	\var, UP
	.set	UP, (UP + 8)
.endm

USER	BASE				// Current number base for I/O
USER	BLK				// Current input
USER	COLOR				// Color of current input word
USER	CURRENT				// Active wordlist for new definitions
USER	DP				// Dictionary pointer
USER	FORTH				// Head of "forth" wordlist
USER	IN				// Bytes consumed in current buffer
USER	MACRO				// Head of "macro" wordlist
USER	NUM_TIB				// Bytes used in terminal buffer
USER	SOURCE_ID			// Identifies source for text interpreter
USER	SPAN				// Number of bytes stored by EXPECT
USER	STATE				// Color of previous input word
USER	SYNC				// Dictionary location of last cache sync
USER	USE				// Current source file

// Remember initial dictionary allocation
USER	DICT_START			// Start of dictionary allocation
USER	FORTH_START			// Start of "forth" wordlist
USER	MACRO_START			// Start of "macro" wordlist

// Collect usage profile
USER	DICT_MAX			// Maximum dictionary usage
USER	RP_MIN				// Maximum return stack usage
USER	SP_MIN				// Maximum parameter stack usage


.macro	SP0	reg			// Base of parameter stack
	mov	\reg, U
.endm
.macro	RP0	reg			// Base of return stack
	sub	\reg, U, #DATA_SIZE
.endm
.macro	TIB	reg			// Terminal input buffer
	sub	\reg, U, #(DATA_SIZE + RETURN_SIZE)
.endm
.macro	HERE	reg			// Next available location in dictionary
	UFETCH	\reg, DP
.endm

// Reserves dictionary space to compile a literal
.macro	WORD	reg
	HERE	\reg
	add	\reg, \reg, #(28 + 7)
	and	\reg, \reg, #-8
.endm

// Reserves dictionary space to read a word
.macro	PAD	reg
	WORD	\reg
	add	\reg, \reg, #128
.endm


// System variables

	.global	ENVIRON			// Devicetree pointer on devices
	.global	WDICTIONARY		// Writable dictionary
	.global	XDICTIONARY		// Executable dicitionary
	.bss
	.balign	8
ARGC:		.ds.d	1		// Number of command-line arguments
ARGV:		.ds.d	1		// Array of command-line arguments
ATEXIT:		.ds.d	1		// Atexit function from startup
BOOTSTRAP:	.ds.d	1		// Indicates that bootstrap has run
ENVIRON:	.ds.d	1		// Array of environment variables
WDICTIONARY:	.ds.d	1		// Initial dictionary allocation
XDICTIONARY:	.ds.d	1		// Executable dictionary for JIT


// In all stack comments below, I use a prefixed comma (",") to
// call attention to T or S in register.

// Inline-able primitives
	.text

// Arbitrary, but we're inlining words that
// 1. Are no larger than a call (so no more than 7 instructions
//    or 28 bytes).  We want to avoid the overhead on anything
//    smaller than a call.
// 2. Don't overwrite any callee-save registers.  So we don't
//    have to juggle register reassignment.
// 3. Don't make any calls and don't access any variables, to
//    ensure the word is relocatable.
// 4. Don't exit early.  We look for a RET to mark the end of
//    the definition.

// ! ( ,x ,a-addr) Write 8-byte value to ADDR
store:	str	S, [T]
	DROP2
	RETURN

// #tib ( -- ,a-addr) Holds number of bytes used in TIB
num_tib:
	UADDR	x0, NUM_TIB
	PUSH1	x0
	RETURN

// * ( ,n1 ,n2 -- ,n1*n2)
star:	mul	T, T, S
	NIP
	RETURN

// + ( ,n1 ,n2 -- ,n1+n2)
plus:	add	T, T, S
	NIP
	RETURN

// +! ( ,n ,addr)
plus_store:
	ldr	x0, [T]
	add	x0, x0, S
	str	x0, [T]
	DROP2
	RETURN

// , ( ,x) Append 8-byte value to dictionary, does not align
comma:	UADDR	x1, DP
	ldr	x0, [x1]
	str	T, [x0], #8
	DROP1
	str	x0, [x1]
	RETURN

// - ( ,n1 ,n2 -- ,n1-n2)
minus:	sub	T, S, T
	NIP
	RETURN

// 0< ( ,n -- ,f)
negative:
	tst	T, T
	csetm	T, MI
	RETURN

// 0<= ( ,n -- ,f)
non_positive:
	tst	T, T
	csetm	T, LE
	RETURN

// 0<> ( ,n -- ,f)
non_zero:
	tst	T, T
	csetm	T, NE
	RETURN

// 0= ( ,n -- ,f)
zerop:	tst	T, T
	csetm	T, EQ
	RETURN

// 0> ( ,n -- ,f)
positive:
	tst	T, T
	csetm	T, GT
	RETURN

// 0>= ( ,n -- ,f)
non_negative:
	tst	T, T
	csetm	T, PL
	RETURN

// 1+ ( ,n -- ,n+1)
increment:
	add	T, T, #1
	RETURN

// 1- ( ,n -- ,n-1)
decrement:
	sub	T, T, #1
	RETURN

// 2* ( ,n -- ,n*2)
two_star:
	lsl	T, T, #1
	RETURN

// 2/ ( ,n -- ,n/2)
// Division, implemented as shift, preserves sign
two_div:
	asr	T, T, #1
	RETURN

// 8* ( ,n -- ,n*8)
eight_star:
	lsl	T, T, #3
	RETURN

// 8+ ( ,n -- ,n+8)
eight_plus:
	add	T, T, #8
	RETURN

// 8- ( ,n -- ,n-8)
eight_minus:
	sub	T, T, #8
	RETURN

// 8/ ( ,n -- ,n/8)
// Division, implemented as shift, preserves sign
eight_div:
	asr	T, T, #3
	RETURN

// < ( ,n1 ,n2 -- ,f)
less:	cmp	S, T
	csetm	T, LT
	ldr	S, [PSP], #8
	RETURN

// <= ( ,n1 ,n2 -- ,f)
not_greater:
	cmp	S, T
	csetm	T, LE
	ldr	S, [PSP], #8
	RETURN

// <> ( ,n1 ,n2 -- ,f)
not_equal:
	cmp	S, T
	csetm	T, NE
	ldr	S, [PSP], #8
	RETURN

// = ( ,n1 ,n2 -- ,f)
equal:	cmp	S, T
	csetm	T, EQ
	ldr	S, [PSP], #8
	RETURN

// > ( ,n1 ,n2 -- ,f)
greater:
	cmp	S, T
	csetm	T, GT
	ldr	S, [PSP], #8
	RETURN

// >= ( ,n1 ,n2 -- ,f)
not_less:
	cmp	S, T
	csetm	T, GE
	ldr	S, [PSP], #8
	RETURN

// >in ( -- ,a-addr)
// Holds address of next unparsed byte in current block
input_pointer:
	UADDR	x0, IN
	PUSH1	x0
	RETURN

// @ ( ,a-addr -- ,x) Read 8-byte value from ADDR
fetch:	ldr	T, [T]
	RETURN

// abs ( ,n -- ,n') Absolute value of signed integer N
abs:	tst	T, T
	csneg	T, T, T, PL
	RETURN

// aligned ( ,addr -- ,a-addr)
// Adjust address to align with the next cell boundary
aligned:
	add	T, T, #7
	and	T, T, #-8
	RETURN

// allot ( ,n)
// Allocate N bytes from the dictionary.  May be negative.
allot:	UADDR	x0, DP
	ldr	x1, [x0]
	add	x1, x1, T
	str	x1, [x0]
	DROP1
	RETURN

// and ( ,n1 ,n2 -- ,n1&n2)
_and:	and	T, S, T
	NIP
	RETURN

// ashift ( ,n ,u -- ,n>>u)
// Shift N right U%64 positions, replicating the sign-bit.
// See also RSHIFT.
ashift:	asr	T, S, T
	NIP
	RETURN

// base ( -- ,a-addr) Holds current number base
base:	str	S, [PSP, #-8]!
	mov	S, T
	UADDR	T, BASE
	RETURN

// blk ( -- ,a-addr) Holds number of current input block
blk:	str	S, [PSP, #-8]!
	mov	S, T
	UADDR	T, BLK
	RETURN

// c! ( ,c ,c-addr) Write one byte to ADDR
c_store:
	strb	SW, [T]
	ldp	T, S, [PSP], #16
	RETURN

// c, ( ,c) Append one-byte to dictionary
c_comma:
	UADDR	x1, DP
	ldr	x0, [x1]
	strb	TW, [x0], #1
	DROP1
	str	x0, [x1]
	RETURN

// c@ ( ,c-addr -- ,c) Read one-byte from ADDR, zero-extend
c_fetch:
	ldrb	TW, [T]
	RETURN

// decimal ( --) Read and display numbers in base-10
decimal:
	mov	x0, 10
	UADDR	x1, BASE
	str	x0, [x1]
	RETURN

// defer ( --) Compile stub code to call deferred word
defer:	movz	x0, #0
	movk	x0, #0, LSL #16
	movk	x0, #0, LSL #32
	movk	x0, #0, LSL #48
	br	x0
	RETURN

// dp ( -- ,a-addr) Holds address of next free byte in dictionary
dp:	str	S, [PSP, #-8]!
	mov	S, T
	UADDR	T, DP
	RETURN

// drop ( ,x1 ,x2 -- ,x1) Remove top-of-stack
drop:	tst	T, T			// Supports "drop if"
	DROP1
	.set	drop_len, (. - drop)
	RETURN

// dup ( ,x -- ,x ,x) Duplicate top-of-stack
dup:	str	S, [PSP, #-8]!
	mov	S, T
	RETURN

// execute ( ,xt) Run code at given address
execute:
	POP1	x0
	PUSHLR	xzr
	blr	x0
	RETURN

// forth ( --) Add new words to "forth" wordlist
forth:	UADDR	x0, FORTH
	UADDR	x1, CURRENT
	str	x0, [x1]
	RETURN

// here ( -- ,c-addr) Address of next free byte in dictionary
here:	str	S, [PSP, #-8]!
	mov	S, T
	HERE	T
	RETURN

// hex ( --) Read and display numbers in base-16
hex:	mov	x0, 16
	UADDR	x1, BASE
	str	x0, [x1]
	RETURN

// invert ( ,n -- ,~n) Ones-complement negation
invert:	mvn	T, T
	RETURN

// lshift ( ,n ,u -- ,n<<u)
lshift:	lsl	T, S, T
	NIP
	RETURN

// macro ( --) Add new words to "macro" wordlist
macro:	UADDR	x0, MACRO
	UADDR	x1, CURRENT
	str	x0, [x1]
	RETURN

// max ( ,n1 ,n2 -- ,n-max)
max:	cmp	S, T
	csel	T, S, T, GT
	ldr	S, [PSP], #8
	RETURN

// min ( n1 ,n2 -- ,n-min)
min:	cmp	S, T
	csel	T, S, T, LT
	ldr	S, [PSP], #8
	RETURN

// negate ( ,n -- ,-n) Twos-complement negation
negate:	neg	T, T
	RETURN

// nip ( ,x1 ,x2 -- ,x2) Discard second-on-stack
nip:	NIP
	RETURN

// nop ( --)
_nop:	nop
	RETURN

// or ( ,n1 ,n2 -- ,n1|n2)
_or:	orr	T, S, T
	NIP
	RETURN

// over ( ,x1 ,x2 -- x1 ,x2 ,x1) Duplicate second-on-stack
over:	str	S, [PSP, #-8]!
	mov	x0, S
	mov	S, T
	mov	T, x0
	RETURN

// pop ( S: -- ,x / R: ,x --) Take top item off return stack
pop:	str	S, [PSP, #-8]!
	mov	S, T
	POPRSP	T
	RETURN

// push ( S: ,x -- / R: -- ,x) Add new item on return stack
push:	PUSHRSP	T
	DROP1
	RETURN

// rshift ( ,n ,u -- ,n>>>u)
// Replaces the sign-bit with zero.  See also ashift.
rshift:	lsr	T, S, T
	NIP
	RETURN

// source-id ( -- ,n) Describes source for text interpreter
source_id:
	str	S, [PSP, #-8]!
	mov	S, T
	UFETCH	T, SOURCE_ID
	RETURN

// source-id! ( ,n)
source_id_store:
	UADDR	x0, SOURCE_ID
	str	T, [x0]
	DROP1
	RETURN

// span ( -- ,a-addr) Holds number of bytes read by expect
span:	str	S, [PSP, #-8]!
	mov	S, T
	UADDR	T, SPAN
	RETURN

// swap	 ( ,x1 ,x2 -- ,x2 ,x2)
swap:	mov	x0, S
	mov	S, T
	mov	T, x0
	RETURN

// swap- ( ,x1 ,x2 -- ,x2-x1)
swap_minus:
	sub	T, T, S
	NIP
	RETURN

// tib ( -- ,a-addr) Address of terminal input buffer
tib:	TIB	x0
	PUSH1	x0
	RETURN

// tuck ( ,x1 ,x2 -- x2 ,x1 ,x2)
tuck:	str	T, [PSP, #-8]!
	RETURN

// u+ ( n1 ,n2 ,n3 -- ,n1+n3 ,n2) Add to second-on-stack
under_plus:
	POP1	x0
	add	S, S, x0
	RETURN

// up ( -- ,a-addr) Holds address of next available offset in user area
up:	str	S, [PSP, #-8]!
	mov	S, T
	UFETCH	T, UP
	RETURN

// use ( -- ,a-addr) Holds address of current source file
use:	str	S, [PSP, #-8]!
	mov	S, T
	UADDR	T, USE
	RETURN

// w! ( ,w ,w-addr) Write 4-byte word to ADDR
w_store:
	str	SW, [T]
	ldp	T, S, [PSP], #16
	RETURN

// w@ ( ,w-addr -- ,w) Read 4-byte word from ADDR, zero-extend
w_fetch:
	ldr	TW, [T]
	RETURN

// xchg ( ,x1 ,addr -- ,x2)
xchg:	ldr	x0, [T]
	str	S, [T]
	ldr	S, [PSP], #8
	mov	T, x0
	RETURN

// xor ( ,n1 ,n2 -- ,n1^n2)
_xor:	eor	T, S, T
	ldr	S, [PSP], #8
	RETURN

// Words defined before this point may be inlined
inline_before:


// Builtin words

// ' ( "<spaces>name" -- xt) Compilation address of NAME
	.text
tick:	CALL	tword
	b	findf

// Hardware division truncates, which is mathematically
// incorrect.  This routine corrects the results such that:
//   D  = d * q + r
//   0 <= r < |d|
//
// Even if you don't care about the mathematics, this is
// necessary to ensure consistency with other primitive
// operations.  When d is a power of 2 (2^n):
//   q = D asr n
//   r = D and (2^n-1)
//
// IN  x0 :: quotient
//     x1 :: remainder
//     x2 :: divisor
// OUT x0 :: adjusted quotient
//     x1 :: adjusted remainder
	.text
fix_euclidean:
	tst	x1, x1			// Remainder negative?
	bpl	2f
	tst	x2, x2			// Divisor positive?
	bmi	1f
	add	x1, x1, x2		// r < 0, d > 0
	sub	x0, x0, #1
	b	2f
1:	sub	x1, x1, x2		// r < 0, d < 0
	add	x0, x0, #1
2:	RETURN


// IN  x0 :: low-64 bits of dividend
//     x1 :: high-64 bits of dividend
//     x2 :: divisor
// OUT x0 :: floored quotient
//     x1 :: floored remainder
	.text
unsigned_division:
	cbz	x2, 3f			// Attempted divide by zero
	cmp	x1, x2			// Quotient will not fit in 64 bits
	bhs	3f

	// Divide by repeated subtraction.
	// N.B., OK to shift at top of loop, x1 is always less
	// than x2, otherwise we would overflow.
	mov	x3, #64			// Loop for 64 bit shifts
1:	lsr	x4, x0, #63		// Shift x1:x0 left
	lsl	x1, x1, #1
	orr	x1, x1, x4
	lsl	x0, x0, #1
	subs	x4, x1, x2		// Can we get a 1?
	blo	2f
	mov	x1, x4
	orr	x0, x0, #1
2:	subs	x3, x3, #1
	bne	1b
	RETURN
3:	// Signal error with q = r = INT_MAX
	mov	x0, #1
	lsl	x0, x0, #63
	sub	x0, x0, #1
	mov	x1, x0
	RETURN


// IN  x0 :: low-64 bits of dividend
//     x1 :: high-64 bits of dividend
//     x2 :: divisor
// OUT x0 :: truncated quotient
//     x1 :: truncated remainder
	.text
signed_division:
	PUSHRSP	x28
	mov	x28, xzr		// Sign
	tst	x1, x1			// Is dividend negative?
	bpl	1f
	eor	x28, x28, #1		// Record sign
	mvn	x1, x1			// Two's complement negation.
	mvn	x0, x0
	adds	x0, x0, #1
	adc	x1, x1, xzr
1:	tst	x2, x2			// Is divisor negative?
	bpl	2f
	eor	x28, x28, #1		// -1^2 == 1
	neg	x2, x2
2:	CALL	unsigned_division
	cbz	x28, 3f			// Should result be negative?
	neg	x0, x0
	neg	x1, x1

	// Don't call fix_euclidean here, because we no longer
	// know if divisor was originally negative.
3:	POPRSP	x28
	RETURN


// */mod ( n1 ,n2 ,n3 -- ,n-rem ,n-quot)
	.text
star_div_mod:
	PUSHRSP	x28
	POP1	x28
	smulh	x1, S, T		// Product in x1:x0
	mul	x0, S, T
	mov	x2, x28
	CALL	signed_division
	mov	x2, x28
	CALL	fix_euclidean
	mov	S, x1
	mov	T, x0
	POPRSP	x28
	RETURN

// /mod ( ,n1 ,n2 -- ,n-rem ,n-quot)
	.text
div_mod:
	sdiv	x0, S, T		// Truncated quotient
	mul	x1, x0, T
	sub	x1, S, x1		// Truncated remainder
	CALL	fix_euclidean
	mov	S, x1			// n-rem
	mov	T, x0			// n-quot
	RETURN

	.text
use_block:
	UADDR	x1, BLK			// Select block
	str	x0, [x1]
	UADDR	x0, IN			// Beginning of block
	str	xzr, [x0]
	CALL	decimal
	CALL	forth
	mov	x0, #WHITE		// Reset to white
	UADDR	x1, COLOR
	str	x0, [x1]
	UADDR	x1, STATE
	str	x0, [x1]
	RETURN

// Convert x-dictionary address to dictionary address
	.text
xp_to_dp:
	ADRL	x2, XDICTIONARY
	ldr	x2, [x2]
	cmp	x0, x2
	blo	1f
	add	x3, x2, DICT_SIZE
	cmp	x0, x3
	bhs	1f
	ADRL	x1, WDICTIONARY
	ldr	x1, [x1]
	sub	x1, x1, x2
	add	x0, x0, x1
1:	RETURN

// <xp
	.text
from_xp:
	mov	x0, T
	CALL	xp_to_dp
	mov	T, x0
	RETURN

// Convert dictionary address to x-dictionary address
	.text
dp_to_xp:
	ADRL	x2, WDICTIONARY
	ldr	x2, [x2]
	cmp	x0, x2
	blo	1f
	add	x3, x2, DICT_SIZE
	cmp	x0, x3
	bhs	1f
	ADRL	x1, XDICTIONARY
	ldr	x1, [x1]
	sub	x1, x1, x2
	add	x0, x0, x1
1:	RETURN

// >xp
	.text
to_xp:	mov	x0, T
	CALL	dp_to_xp
	mov	T, x0
	RETURN

// >body ( ,xt -- ,a-addr)
// Move from compilation address to parameter field
	.text
to_body:
	ldr	x0, =(call_len + 7)
	add	T, T, x0
	and	T, T, #-8
	RETURN

// align ( --) Adjust HERE to next cell boundary
	.text
align:	UADDR	x0, DP
	ldr	x1, [x0]
	add	x1, x1, #3
	and	x1, x1, #-4
	tst	x1, #7
	beq	1f
	ldr	w2, =0xd903201f		// nop
	str	w2, [x1], #4
1:	str	x1, [x0]
	RETURN

// argc ( -- ,u) Number of command-line arguments
	.text
argc:	ADRL	x0, ARGC
	ldr	x0, [x0]
	PUSH1	x0
	RETURN

// argv ( -- ,a-addr) Array of command-line arguments
	.text
argv:	ADRL	x0, ARGV
	ldr	x0, [x0]
	PUSH1	x0
	RETURN

// block ( ,u -- ,addr) Address of requested block
	.text
block:	lsl	T, T, #9
	UFETCH	x0, USE
	add	T, T, x0
	RETURN

// bye ( --) Exit forth
	.text
bye:	ADRL	x0, ATEXIT	// Call any registered finalizer
	ldr	x0, [x0]
	cbz	x0, 1f
	PUSHLR	xzr
	blr	x0
	POPLR	xzr
1:	eor	x0, x0, x0		// error_code
	mov	SYS_REG, #SYS_exit
	svc	#SYS_SVC		// Does not return

// compare ( c-addr1 u-count1 ,c-addr2 ,u-count2 -- ,n)
	.text
compare:
	ldp	x1, x0, [PSP], #16	// u-count1, c-addr1
	mov	x2, S			// c-addr2
	mov	x3, T			// u-count2
	DROP2
	mov	x4, x1			// min
	cmp	x3, x4
	bhs	1f
	mov	x4, x3
1:	cbz	x4, 4f			// Compare bytes
	ldrb	w6, [x0], #1
	ldrb	w7, [x2], #1
	cmp	w6, w7
	blo	2f
	bhi	3f
	sub	x4, x4, 1
	b	1b
2:	mov	x0, -1			// <
	PUSH1	x0
	RETURN
3:	mov	x0, 1			// >
	PUSH1	x0
	RETURN
4:	cmp	x1, x3			// Compare lengths
	blo	5f
	bhi	6f
	PUSH1	xzr			// =
	RETURN
5:	mov	x0, -1			// <
	PUSH1	x0
	RETURN
6:	mov	x0, 1			// >
	PUSH1	x0
	RETURN

// count ( ,c-addr1 -- ,c-addr2 ,u)
// Translate counted string to string and count
	.text
count:	str	S, [PSP, #-8]!
	mov	S, T			// c-addr
	ldrb	TW, [S], #1		// u
	cmp	T, #128
	blo	2f
	and	x0, T, #127		// Number of length bytes
	eor	T, T, T			// Init length to zero
	cbz	x0, 2f			// Guard against zero length
1:	ldrb	w1, [S], #1
	orr	T, x1, T, LSL #8
	subs	x0, x0, #1
	bne	1b
2:	RETURN

// char ( "<spaces>name" -- c)
	.text
char:	CALL	tword
	CALL	count
	DROP1
	b	c_fetch

// Rewind instruction sync
	.text
rewind:	CALL	xp_to_dp
	UADDR	x2, SYNC
	ldr	x1, [x2]
	cmp	x0, x1
	bhs	1f
	mov	x1, x0
1:	and	x1, x1, #-64
	str	x1, [x2]
	RETURN

// defer! ( ,xt1 ,xt2) Set XT2 to execute XT1
	.text
defer_store:
	mov	x0, T			// Rewind instruction sync
	CALL	rewind
	mov	x0, T			// Work in writable dictionary
	CALL	xp_to_dp
	mov	T, x0
	add	x0, T, #16		// Alter 4 MOV instructions
1:	ldr	w1, [T]			// Read MOV
	bfi	w1, SW, #5, #16		// Set address bits
	ror	S, S, #16
	str	w1, [T], #4
	cmp	T, x0
	bne	1b
	DROP2
	b	sync			// Sync updated instructions

// defer@ ( ,xt1 -- ,xt2) XT1 is set to execute XT2
	.text
defer_fetch:
	add	x0, T, #16		// Decode 4 MOV instructions
	eor	x1, x1, x0
1:	ldr	w2, [T], #4		// Read MOV
	bfxil	x1, x2, #5, #16		// Copy out address bits
	ror	x1, x1, #16
	cmp	T, x0
	bne	1b
	mov	T, x1
	RETURN

// environ ( -- ,a-addr) Array of environment variables
	.text
environ:
	ADRL	x0, ENVIRON
	ldr	x0, [x0]
	PUSH1	x0
	RETURN

// erase ( ,c-addr ,u-count   ) Populate bytes with 0
// fill  (  c-addr ,u-count ,c) Populate bytes with c
	.text
erase:	mov	x0, xzr			// c
	b	1f
fill:	POP1	x0			// c
1:	cbz	T, 2f			// u-count
	strb	w0, [S], #1		// c-addr
	sub	T, T, 1
	b	1b
2:	DROP2
	RETURN


	// LSB Huffman codes for ASCII 32 to 126.
	.const
	.balign 4
_huffman:
	.dc.w	           0b00,  2	// SPACE
	.dc.w	    0b000011111,  9 	// !
	.dc.w	     0b00110111,  8 	// "
	.dc.w	     0b10110111,  8 	// #
	.dc.w	     0b01110111,  8 	// $
	.dc.w	  0b00111111111, 11 	// %
	.dc.w	0b0111111111111, 13 	// &
	.dc.w	     0b11110111,  8 	// '
	.dc.w	      0b0001011,  7 	// (
	.dc.w	      0b1001011,  7 	// )
	.dc.w	    0b100011111,  9 	// *
	.dc.w	     0b00001111,  8 	// +
	.dc.w	      0b0101011,  7 	// ,
	.dc.w	        0b00110,  5 	// -
	.dc.w	      0b1101011,  7 	// .
	.dc.w	    0b010011111,  9 	// /
	.dc.w	       0b011101,  6 	// 0
	.dc.w	      0b0011011,  7 	// 1
	.dc.w	      0b1011011,  7 	// 2
	.dc.w	     0b10001111,  8 	// 3
	.dc.w	    0b110011111,  9 	// 4
	.dc.w	    0b001011111,  9 	// 5
	.dc.w	    0b101011111,  9 	// 6
	.dc.w	   0b0101111111, 10 	// 7
	.dc.w	    0b011011111,  9 	// 8
	.dc.w	    0b111011111,  9 	// 9
	.dc.w	      0b0111011,  7 	// :
	.dc.w	     0b01001111,  8 	// ;
	.dc.w	   0b1101111111, 10 	// <
	.dc.w	     0b11001111,  8 	// =
	.dc.w	      0b1111011,  7 	// >
	.dc.w	    0b000111111,  9 	// ?
	.dc.w	     0b00101111,  8 	// @
	.dc.w	        0b10110,  5 	// A -- encodes as lowercase
	.dc.w	      0b1000111,  7 	// B
	.dc.w	        0b01110,  5 	// C
	.dc.w	        0b11110,  5 	// D
	.dc.w	         0b0010,  4 	// E
	.dc.w	        0b00001,  5 	// F
	.dc.w	       0b111101,  6 	// G
	.dc.w	       0b000011,  6 	// H
	.dc.w	        0b10001,  5 	// I
	.dc.w	   0b0011111111, 10 	// J
	.dc.w	     0b11101111,  8 	// K
	.dc.w	        0b01001,  5 	// L
	.dc.w	       0b100011,  6 	// M
	.dc.w	        0b11001,  5 	// N
	.dc.w	        0b00101,  5 	// O
	.dc.w	       0b010011,  6 	// P
	.dc.w	   0b1011111111, 10 	// Q
	.dc.w	        0b10101,  5 	// R
	.dc.w	        0b01101,  5 	// S
	.dc.w	         0b1010,  4 	// T
	.dc.w	       0b110011,  6 	// U
	.dc.w	      0b0100111,  7 	// V
	.dc.w	      0b1100111,  7 	// W
	.dc.w	      0b0010111,  7 	// X
	.dc.w	      0b1010111,  7 	// Y
	.dc.w	    0b010111111,  9 	// Z
	.dc.w	     0b10101111,  8 	// [
	.dc.w	      0b0000111,  7	// "\\"
	.dc.w	     0b01101111,  8 	// ]
	.dc.w	 0b011111111111, 12 	// ^
	.dc.w	    0b100111111,  9 	// _
	.dc.w	0b1111111111111, 13 	// `
	.dc.w	        0b10110,  5 	// a
	.dc.w	      0b1000111,  7 	// b
	.dc.w	        0b01110,  5 	// c
	.dc.w	        0b11110,  5 	// d
	.dc.w	         0b0010,  4 	// e
	.dc.w	        0b00001,  5 	// f
	.dc.w	       0b111101,  6 	// g
	.dc.w	       0b000011,  6 	// h
	.dc.w	        0b10001,  5 	// i
	.dc.w	   0b0011111111, 10 	// j
	.dc.w	     0b11101111,  8 	// k
	.dc.w	        0b01001,  5 	// l
	.dc.w	       0b100011,  6 	// m
	.dc.w	        0b11001,  5 	// n
	.dc.w	        0b00101,  5 	// o
	.dc.w	       0b010011,  6 	// p
	.dc.w	   0b1011111111, 10 	// q
	.dc.w	        0b10101,  5 	// r
	.dc.w	        0b01101,  5 	// s
	.dc.w	         0b1010,  4 	// t
	.dc.w	       0b110011,  6 	// u
	.dc.w	      0b0100111,  7 	// v
	.dc.w	      0b1100111,  7 	// w
	.dc.w	      0b0010111,  7 	// x
	.dc.w	      0b1010111,  7 	// y
	.dc.w	    0b010111111,  9 	// z
	.dc.w	    0b110111111,  9 	// {
	.dc.w	  0b10111111111, 11 	// |
	.dc.w	    0b001111111,  9 	// }
	.dc.w	  0b01111111111, 11 	// ~
	.dc.w	              0,  0

// huffman ( -- ,w-addr) Table of LSB Huffman codes
	.text
	.balign	4
huffman:
	str	S, [PSP, #-8]!
	mov	S, T
	ADRL	T, _huffman
	RETURN

// decode-char ( ,bits ,len -- ,char ,consumed)
// Decode single character
	.text
decode_char:
	ADRL	x0, _huffman		// Start of table
1:	ldrb	w1, [x0, #2]		// Length of entry
	cbz	w1, 3f			// Valid?
	cmp	w1, TW			// Sufficient bits?
	bhi	2f
	mov	w2, #1			// Mask
	lsl	w2, w2, w1
	sub	w2, w2, #1
	and	w2, w2, SW
	ldrh	w3, [x0]
	cmp	w2, w3			// Failed?
	beq	4f
2:	add	x0, x0, #4
	b	1b
3:	mov	S, #-1			// char
	eor	T, T, T			// consumed
	b	5f
4:	mov	S, x0
	ADRL	x0, _huffman
	sub	S, S, x0
	lsr	S, S, #2
	add	S, S, #32		// char
	mov	T, x1			// consumed
5:	RETURN

// move-bits ( x-hi ,x-lo ,n -- ,x-hi' ,x-lo')
// Shift low-order bits from x-hi into x-lo
	.text
move_bits:
	POP1	x0			// n
	mov	x1, S			// x-hi
	lsr	S, S, x0		// x-hi'
	lsr	T, T, x0
	neg	x0, x0
	add	x0, x0, #64
	lsl	x1, x1, x0
	orr	T, T, x1		// x-lo'
	RETURN

// decode ( ,x-hi ,x-lo -- ,c-addr) Decode to counted string
	.text
decode:	stp	x25, x26, [RSP, #-16]!
	stp	x27, x28, [RSP, #-16]!
	and	w28, TW, #0x0f		// Bytes used
	sub	w28, w28, #1
	lsl	w28, w28, #3		// Bits available
	mov	x0, #8
	PUSH1	x0			// n
	CALL	move_bits		// Drop length byte
	POP2	x27, x26		// x-lo, x-hi
	WORD	x25			// Destination
1:	PUSH2	x27, x28		// bits, len
	CALL	decode_char
	POP2	x1, x0			// consumed, char
	cbz	w1, 2f			// Success?
	cmp	w0, #32			// Space is terminator
	bls	2f
	strb	w0, [x25, #1]!		// Save decoded char
	PUSH2	x26, x27		// x-hi, x-lo
	PUSH1	x1			// n
	CALL	move_bits
	POP2	x27, x26		// x-lo, x-hi
	b	1b
2:	mov	x0, x25
	WORD	x25
	sub	x0, x0, x25
	strb	w0, [x25]		// Write length
	PUSH1	x25			// c-addr
	ldp	x27, x28, [RSP], #16
	ldp	x25, x26, [RSP], #16
	RETURN

// decode-number ( ,x-hi ,x-lo -- ,n ,u-junk)
// Read number from Huffman-encoded word
	.text
decode_number:
	CALL	decode
	CALL	count
	CALL	signed
	NIP				// Discard c-addr
	RETURN

// encode ( ,c-addr ,u -- ,x-hi ,x-lo)
// Represent string as two LSB Huffman-encoded words
	.text
encode:	eor	x1, x1, x1		// x-hi
	eor	x2, x2, x2		// x-lo
	mov	x0, #8			// Count bits used
	POP2	x3, x4			// u, c-addr
	ADRL	x5, _huffman		// Code table
1:	cmp	x0, #104		// Space available?
	bhs	3f
	cbz	x3, 3f			// Input available?
	ldrb	w6, [x4], #1		// Next byte
	sub	x3, x3, #1
	subs	x6, x6, #32		// Valid character?
	ble	3f
	cmp	x6, #95
	bge	3f
	mov	x7, #4
	mul	x7, x7, x6
	add	x7, x7, x5
	mov	x8, x7
	ldrh	w7, [x7]		// Bits
	add	x8, x8, #2
	ldrb	w8, [x8]		// Length
	cmp	x0, #64			// First word?
	bhs	2f
	lsl	x7, x7, x0
	orr	x2, x2, x7
	add	x0, x0, x8
	cmp	x0, #64			// Crossing word boundary?
	bls	1b
	mov	x7, #4
	mul	x7, x7, x6
	add	x7, x7, x5
	ldrh	w7, [x7]		// Reload bits
	sub	x0, x0, #64		// Num unencoded bits
	eor	x0, x0, x8
	eor	x8, x8, x0
	eor	x0, x0, x8
	sub	x0, x0, x8		// Num encoded bits
	lsr	x7, x7, x0		// Discard encoded bits
	mov	x0, #64
2:	lsl	x7, x7, x0		// Second word
	orr	x1, x1, x7
	add	x0, x0, x8
	b	1b
3:	add	x0, x0, #7		// Bytes used
	lsr	x0, x0, #3		// (Include length byte itself)
	orr	x2, x2, x0		// (Low nibble)
	PUSH2	x1, x2			// Push in LSB order
	RETURN


// Save user variable on return stack
.macro	SAVE	var
	UFETCH	x0, \var
	PUSHRSP	x0
.endm

// Restore user variable from return stack
.macro	RESTORE	var
	POPRSP	x0
	UADDR	x1, \var
	str	x0, [x1]
.endm

// load ( ,u) Read from indicated source block
	.text
load:	SAVE	BASE
	SAVE	BLK
	SAVE	COLOR
	SAVE	IN
	SAVE	STATE
	POP1	x0			// u
	CALL	use_block
	UFETCH	x0, BLK			// Binary input?
	cbz	x0, 1f
	CALL	binterpret
	b	2f
1:	CALL	tinterpret
2:	RESTORE	STATE
	RESTORE	IN
	RESTORE	COLOR
	RESTORE	BLK
	RESTORE	BASE
	RETURN


// Restore user variable from dictionary
.macro	RESTOR	var
	UADDR	x2, \var
	ldr	x0, [x1], #8
	str	x0, [x2]
.endm

// ( --) Run-time code for marker
	.text
_marker:
	POPRSP	x1			// Address of parameter block
	add	x1, x1, #7
	and	x1, x1, #-8
	RESTOR	CURRENT			// Restore context from parameter block
	RESTOR	DP
	RESTOR	FORTH
	RESTOR	MACRO
	RESTOR	UP
	CALL	sync			// Synchronize instruction cache
	RETURN

// Save user variable in dictionary
.macro	SAV	var
	UFETCH	x0, \var
	PUSH1	x0
	CALL	comma
.endm

// marker ( --) Define restore point
	.text
marker:	adr	x0, _marker		// Compile run-time code
	PUSH1	x0
	CALL	compile_comma
	CALL	align			// Align parameter block
	SAV	CURRENT			// Save context
	SAV	DP
	SAV	FORTH
	SAV	MACRO
	SAV	UP
	RETURN

// move ( src ,dst ,count) Copy COUNT bytes from SRC to DST
	.text
move:	POP2	x2, x1			// count, dst
	POP1	x0			// src
	tst	x2, x2			// count > 0?
	ble	5f
	cmp	x1, x0
	blo	1f
	bhi	3f
	RETURN				// Nowhere to go
1:	cbz	x2, 2f			// Low-to-high
	ldrb	w3, [x0], #1
	strb	w3, [x1], #1
	sub	x2, x2, 1
	b	1b
2:	RETURN
3:	add	x0, x0, x2		// High-to-low
	add	x1, x1, x2
4:	cbz	x2, 5f
	ldrb	w3, [x0, #-1]!
	strb	w3, [x1, #-1]!
	sub	x2, x2, 1
	b	4b
5:	RETURN

// pad ( -- ,a-addr)
// Reserve space at end of dictionary to read a word.
	.text
pad:	str	S, [PSP, #-8]!
	mov	S, T
	PAD	T
	RETURN

// source ( -- ,c-addr ,u) Current interpreter source
	.text
source:	UFETCH	x0, BLK			// Text input?
	cbnz	x0, 2f
	UFETCH	x0, SOURCE_ID		// From the terminal?
	cbnz	x0, 1f
	TIB	x0
	UFETCH	x1, NUM_TIB
	b	3f
1:	UFETCH	x0, USE			// File buffer
	UFETCH	x1, SPAN
	b	3f
2:	lsl	x0, x0, #9		// Block input
	UFETCH	x1, USE
	add	x0, x0, x1
	mov	x1, #512
3:	PUSH2	x0, x1			// c-addr, u
	RETURN

// within ( n1 ,n2 ,n3 -- ,f) n2 <= n1 < n3
	.text
within:	POP2	x3, x2			// n3, n2
	mov	x1, T			// n1
	eor	T, T, T			// false
	subs	x1, x1, x2		// n1 - n2
	blt	1f
	sub	x3, x3, x2		// n3 - n2
	subs	x3, x3, x1		// n3 - n1
	csinv	T, xzr, xzr, LE		// false if n3 <= n1
1:	RETURN


// Primitive I/O

	.const
	.balign	4
_emit_name:	.dc.b	4
		.ascii	"emit"
	.balign	4
_expect_name:	.dc.b	6
		.ascii	"expect"
	.balign	4
_key_name:	.dc.b	3
		.ascii	"key"
	.balign	4
_query_name:	.dc.b	5
		.ascii	"query"
	.balign	4
_type_name:	.dc.b	4
		.ascii	"type"

	.bss
	.balign	8
_emit:		.ds.d	1
_expect:	.ds.d	1
_key:		.ds.d	1
_query:		.ds.d	1
_type:		.ds.d	1

// ( ,c) Output a single character
	.text
	.balign	4
builtin_emit:
	sub	RSP, RSP, #16
	mov	x2, #1			// count
	mov	x1, RSP			// buf
	strb	TW, [x1]
	DROP1
	mov	x0, #stdout		// fd
	mov	SYS_REG, #SYS_write
	PUSHLR	xzr
	svc	#SYS_SVC
	POPLR	xzr
	add	RSP, RSP, #16
	RETURN

// emit ( ,c) Output a single character
	.text
emit:	ADRL	x0, _emit
	ldr	x1, [x0]
	cbnz	x1, 1f
	ADRL	x1, _emit_name
	PUSH1	x1
	CALL	find
	POP1	x1
	ADRL	x0, _emit
	str	x1, [x0]
1:	br	x1

// ( -- ,c) Read one byte of input
	.text
builtin_key:
	sub	RSP, RSP, #16
	mov	x2, #1			// count
	mov	x1, RSP			// buf
	mov	x0, #stdin		// fd
	mov	SYS_REG, #SYS_read
	PUSHLR	xzr
	svc	#SYS_SVC
	POPLR	xzr
	ldrb	w1, [RSP]
	add	RSP, RSP, #16
	tst	x0, x0
	ble	bye			// End program on EOF
	PUSH1	x1
	RETURN

// key ( -- ,c) Read one byte of input
	.text
key:	ADRL	x0, _key
	ldr	x1, [x0]
	cbnz	x1, 1f
	ADRL	x1, _key_name
	PUSH1	x1
	CALL	find
	POP1	x1
	ADRL	x0, _key
	str	x1, [x0]
1:	br	x1

// ( ,c-addr ,+n) Receive bytes into memory
	.text
builtin_expect:
	stp	x27, x28, [RSP, #-16]!
	POP2	x27, x28		// n, c-addr
	PUSHRSP	x28			// Save starting address
	CALL	key			// Exit program on EOF
	POP1	x0
	tst	x0, x0
	bmi	bye
	subs	x27, x27, 1		// Reserve space for terminator
	ble	2f
1:	cmp	w0, 10
	beq	2f
	strb	w0, [x28], #1
	subs	x27, x27, 1
	beq	2f
	CALL	key
	POP1	x0
	tst	x0, x0
	bpl	1b
2:	strb	wzr, [x28]		// Append terminator
	POPRSP	x0			// Restore starting address
	sub	x28, x28, x0		// Number of bytes read
	UADDR	x1, SPAN
	str	x28, [x1]
	ldp	x27, x28, [RSP], #16
	RETURN

// expect ( ,c-addr ,+n) Receive bytes into memory
	.text
expect:	ADRL	x0, _expect
	ldr	x1, [x0]
	cbnz	x1, 1f
	ADRL	x1, _expect_name
	PUSH1	x1
	CALL	find
	POP1	x1
	ADRL	x0, _expect
	str	x1, [x0]
1:	br	x1

// ( --) Read one-line of input into terminal buffer
	.text
builtin_query:
	TIB	x0
	mov	x1, #128
	PUSH2	x0, x1			// c-addr, n
	CALL	expect
	UFETCH	x0, SPAN
	UADDR	x1, NUM_TIB
	str	x0, [x1]
	UADDR	x0, IN			// Reset used count
	str	xzr, [x0]
	RETURN

// query ( --) Read one-line of input into terminal buffer
	.text
query:	ADRL	x0, _query
	ldr	x1, [x0]
	cbnz	x1, 1f
	ADRL	x1, _query_name
	PUSH1	x1
	CALL	find
	POP1	x1
	ADRL	x0, _query
	str	x1, [x0]
1:	br	x1

// ( ,c-addr ,u) Output counted string
	.text
builtin_type:
	POP2	x2, x1			// count, buf
	mov	x0, #stdout		// fd
	mov	SYS_REG, #SYS_write
	PUSHLR	xzr
	svc	#SYS_SVC
	POPLR	xzr
	RETURN

// type ( ,c-addr ,u) Output counted string
	.text
type:	ADRL	x0, _type
	ldr	x1, [x0]
	cbnz	x1, 1f
	ADRL	x1, _type_name
	PUSH1	x1
	CALL	find
	POP1	x1
	ADRL	x0, _type
	str	x1, [x0]
1:	br	x1


// Words

// We represent each word with 12-bytes.  Byte 0 is the length
// of the word, which can range from 1 to 127, and bytes 1-11
// contain up to the first 11-characters of the word, padded out
// with NULs.

// word ( -- ,x-hi ,x-lo) Read one word from input, updating COLOR
	.text
bword:	UFETCH	x2, BLK			// Find source block
	lsl	x2, x2, #9
	UFETCH	x3, USE
	add	x2, x2, x3
	PUSHRSP	x2			// Preserve block address
	UFETCH	x3, IN			// Input pointer
	add	x2, x2, x3
	eor	w1, w1, w1		// Clear return value
	add	x2, x2, #3		// Ensure alignment
	and	x2, x2, #-4
1:	ldr	w0, [x2], #4		// Read 32-bit word
	cbz	w0, 8f			// Done if NULL
	and	w3, w0, #0xfff0		// Done if FF
	cmp	w3, #0x0c40
	bne	2f
	eor	w0, w0, w0		// Clear return value
	b	8f
2:	eor	w3, w3, w3
	bfxil	w3, w0, #4, #4		// Extract color
	UADDR	x4, COLOR		// Save color
	str	x3, [x4]
	cmp	w3, #BLUE		// Skip blue words
	beq	1b
	cmp	w3, #WHITE		// Skip white words
	bne	4f
	sub	x2, x2, #3		// Length excludes tag
	bfxil	w3, w0, #0, #4		// 4-bit length?
	cbnz	w3, 3f
	add	x2, x2, #1		// Length excludes itself
	bfxil	w3, w0, #8, #8		// 8-bit length?
	cmp	w3, #128
	blo	3f
	add	x2, x2, #2
	rev32	x3, x0			// 16-bit big-endian length
	bfxil	w3, w3, #0, #16
3:	add	x2, x2, x3
	add	x2, x2, #3
	and	x2, x2, #-4
	b	1b

	// Read numbers
4:	cmp	w3, RED			// Red is never a number
	beq	7f
	ands	w3, w3, #8		// Binary number?
	beq	7f
	mov	x1, #-1			// Flag result as number
	bfxil	w3, w0, #0, #4		// Extract length
	cmp	w3, #2			// 16-bit number?
	bne	5f
	lsr	w0, w0, #16
	b	8f
5:	cmp	w3, #4			// 32-bit number?
	bne	6f
	ldr	w0, [x2], #4
	b	8f
6:	ldp	w0, w1, [x2], #8	// 64-bit-number
	orr	x0, x0, x1, lsl #32
	b	8f

	// Read encoded word
7:	and	w0, w0, #0xffffff0f	// Strip color from return
	bfxil	w3, w0, #0, #4		// Extract length
	cmp	w3, #4			// More than 4 bytes?
	bls	8f
	ldr	w1, [x2], #4		// Read next 4 bytes
	orr	x0, x0, x1, LSL #0x20
	eor	w1, w1, w1		// Clear high-order bits
	cmp	w3, #8			// More than 8 bytes?
	bls	8f
	ldr	w1, [x2], #4

8:	POPRSP	x3			// Update input pointer
	sub	x2, x2, x3
	UADDR	x3, IN
	str	x2, [x3]
	PUSH2	x1, x0			// x-hi, x-lo
	RETURN

// word ( -- ,a-addr) Read one word from input, updating COLOR
	.text
tword:	CALL	source
	POP2	xzr, x0			// u (discard), c-addr
	PUSHRSP	x0			// Preserve original block address
	UFETCH	x1, IN
	add	x0, x0, x1		// Source
	WORD	x1			// Destination
	stp	xzr, xzr, [x1]		// Clear buffer
	str	xzr, [x1, #16]
	UFETCH	x2, COLOR		// Keep COLOR cached in register
	and	w2, w2, #7
1:	ldrb	w3, [x0], #1		// Skip leading whitespace
	cbz	w3, 5f			// NUL marks end of block
	cmp	w3, #32
	bls	1b
2:	cmp	w3, #127		// Accept only 7-bit US-ASCII
	blo	3f
	beq	1b
	cmp	w3, #0x90		// Look for color codes
	bhs	1b
	and	w2, w3, #0x0f		// Save 4-bits of color
	UADDR	x4, COLOR
	str	x2, [x4]
	and	w2, w2, #7
	b	1b
3:	cmp	w2, #WHITE		// Skip comments
	beq	1b
4:	strb	w3, [x1, #1]!		// Accumulate characters
	ldrb	w3, [x0], #1		// Read next
	cmp	w3, #32
	bls	5f
	cmp	w3, #127
	blo	4b
5:	cbnz	w3, 6f			// Try not to walk off end
	sub	x0, x0, #1
6:	POPRSP	x2			// Update used count
	sub	x0, x0, x2
	UADDR	x2, IN
	str	x0, [x2]
	WORD	x0			// Find length of word
	sub	x1, x1, x0
	strb	w1, [x0]		// Write length byte
	PUSH1	x0			// Return counted string
	RETURN


// in  x0 :: character
//     x1 :: base
// out x0 :: value, 0 <= value < base, or -1
	.text
digit:	subs	w0, w0, #48		// '0' .. '9'
	blo	2f
	cmp	w0, #10
	blo	1f
	sub	w0, w0, #7		// 'A' .. 'Z'
	cmp	w0, #10
	blo	2f
	cmp	w0, #36
	blo	1f
	sub	w0, w0, #32		// 'a' .. 'z'
	cmp	w0, #10
	blo	2f
1:	cmp	w0, w1			// In range?
	blo	3f
2:	mov	x0, #-1			// Not a valid digit
3:	RETURN

// ( u1 ,c-addr1 ,u-count -- u2 ,c-addr2 ,u-junk)
	.text
natural:
	stp	x27, x28, [RSP, #-16]!
	ldr	x27, [PSP]		// u1
	UADDR	x0, BASE
	ldrb	w28, [x0]
1:	cbz	T, 4f			// u-count
	ldrb	w0, [S]
	cmp	w0, '#'			// Decimal literal?
	bne	2f
	mov	x28, #10
	add	S, S, #1
	subs	T, T, #1
	beq	4f
	b	3f
2:	cmp	w0, '$'			// Hex literal?
	bne	3f
	mov	x28, #16
	add	S, S, #1
	subs	T, T, #1
	beq	4f
3:	mov	x1, x28
	ldrb	w0, [S]
	CALL	digit
	tst	x0, x0
	bmi	4f
	madd	x27, x27, x28, x0
	add	S, S, #1		// Consume digit
	sub	T, T, #1
	b	3b
4:	str	x27, [PSP]		// u2
	ldp	x27, x28, [RSP], #16
	RETURN

// ( ,c-addr1 ,u-count -- n ,c-addr2 ,u-junk)
	.text
signed:	PUSHRSP	x28
	mov	x28, 1
	str	xzr, [PSP, #-8]!	// u
	cbz	T, 1f			// Got input?
	ldrb	w0, [S]			// Look for sign
	cmp	w0, '-'
	bne	1f
	mov	x28, #-1
	add	S, S, #1		// Consume sign
	sub	T, T, #1
1:	CALL	natural
	ldr	x0, [PSP]		// u
	mul	x0, x0, x28
	str	x0, [PSP]		// n
	POPRSP	x28
	RETURN

// number ( ,c-addr -- ,n ,u-junk) Read number from counted string
	.text
number:	CALL	count
	CALL	signed
	NIP				// Discard c-addr
	RETURN


// Dictionary lookup and navigation

// A wordlist is stored as a packed linear sequence of 16-byte
// headers.  The first 12-bytes hold the length-prefixed name of
// the word, and the remaining 4-bytes contain the signed offset
// to the word in the dictionary.

// Words can be hidden by zeroing their length byte in the
// wordlist.  An actual zero-length word (all NULs) is a link
// field.  If not NULL, it's address points to the next wordlist
// in the chain.

// find ( ,x-hi ,x-lo -- ,xt) Lookup word in current wordlist
	.text
bfind:	POP2	x1, x2			// x-lo, x-hi
	UFETCH	x0, CURRENT		// Load current wordlist
	ldr	x0, [x0]
1:	ldp	x3, x4, [x0]		// Compare names
	cmp	x3, x1
	ccmp	w4, w2, #0, EQ
	beq	3f
	cbz	w3, 2f			// More in this list?
	sub	x0, x0, #16		// Continue with next entry
	b	1b
2:	ldr	x0, [x0, #8]		// Move to next list
	cbz	x0, 4f			// Another list?
	b	1b
3:	ldrsw	x1, [x0, #12]		// Read code field
	add	x0, x0, x1
	CALL	dp_to_xp		// Address executable dictionary
4:	PUSH1	x0			// xt
	RETURN

// find ( ,c-addr -- ,xt) Lookup word in current wordlist
	.text
find:	CALL	count
	CALL	encode
	b	bfind

// findf ( ,x-hi ,x-lo -- ,xt) Lookup word in "forth" wordlist
	.text
bfindf:	UADDR	x0, CURRENT
	ldr	x1, [x0]
	stp	x0, x1, [RSP, #-16]!
	UADDR	x1, FORTH
	str	x1, [x0]
	CALL	bfind
	ldp	x0, x1, [RSP], #16
	str	x1, [x0]
	RETURN

// ( ,c-addr -- ,xt) Lookup word in "forth" wordlist
	.text
findf:	CALL	count
	CALL	encode
	b	bfindf

// findm ( ,x-hi ,x-lo -- ,xt) Lookup word in "macro" wordlist
	.text
bfindm:	UADDR	x0, CURRENT
	ldr	x1, [x0]
	stp	x0, x1, [RSP, #-16]!
	UADDR	x1, MACRO
	str	x1, [x0]
	CALL	bfind
	ldp	x0, x1, [RSP], #16
	str	x1, [x0]
	RETURN

// ( ,c-addr -- ,xt) Lookup word in "macro" wordlist
	.text
findm:	CALL	count
	CALL	encode
	b	bfindm


// >name ( ,xt -- ,nt) Move from dictionary entry to wordlist entry
	.text
to_name:
	mov	x0, T			// xt
	CALL	xp_to_dp
	UFETCH	T, FORTH		// Load "forth" wordlist
1:	ldrsw	x2, [T, #12]		// Decode relative address
	add	x1, T, x2
	cmp	x0, x1			// Compare code address
	beq	3f
	ldr	x1, [T]			// More in this list?
	cbz	x1, 2f
	sub	T, T, #16		// Continue with next entry
	b	1b
2:	ldr	T, [T, #8]		// Move to next list
	cbnz	T, 1b
3:	RETURN


// Code generation

	.text
call_tmpl:				// Template instructions for a call
	movz	x0, #0			// Immediate in bits 20..5
	movk	x0, #0, LSL #16
	movk	x0, #0, LSL #32
	movk	x0, #0, LSL #48
	str	R, [RSP, #-16]!
	blr	x0
	.set	call_len, (. - call_tmpl)

// ( ,w-addr) Emit instructions to call word given by w-addr
	.text
_call:	adr	x1, call_tmpl		// Source
	ldp	w2, w3, [x1], #8	// Copy 4 MOV instructions
	ldp	w4, w5, [x1], #8
	ldp	w6, w7, [x1]		// Copy STR, BLR
	POP1	x0
	bfi	w2, w0, #5, #16		// Set address bits
	ror	x0, x0, #16
	bfi	w3, w0, #5, #16
	ror	x0, x0, #16
	bfi	w4, w0, #5, #16
	ror	x0, x0, #16
	bfi	w5, w0, #5, #16
	UADDR	x1, DP
	ldr	x0, [x1]		// Destination
	stp	w2, w3, [x0], #8	// Write 6 instructions
	stp	w4, w5, [x0], #8
	stp	w6, w7, [x0], #8
	str	x0, [x1]		// Update DP
	RETURN


// ( ,w-addr) Copy instructions for a word inline
	.text
inline:	HERE	x0			// Destination
	adr	x1, 3f			// "mov x2, R"  marks end of definition
	ldr	w1, [x1]
1:	ldr	w2, [T], #4		// Read instruction
	cmp	w2, w1			// Not a RETURN?
	beq	2f
	str	w2, [x0], #4		// Write instruction
	b	1b
2:	DROP1
	UADDR	x1, DP			// Update DP
	str	x0, [x1]
3:	RETURN


// compile, ( ,w-addr) Call or inline word, as appropriate
	.text
compile_comma:
	adr	x0, inline_before
	cmp	T, x0			// Can we inline it?
	blo	inline
	b	_call


// Builtin macros

// ; ( --) Exit a definition
	.text
exit:	stp	x27, x28, [RSP, #-16]!
	HERE	x27			// Examine prior word
	adr	x1, call_tmpl
	ldp	w2, w3, [x27, #-8]	// Compare STR, BLR
	ldp	w4, w5, [x1, #16]
	cmp	w2, w4			// Does it look like a call?
	ccmp	w3, w5, #0, EQ
	bne	4f
	sub	x27, x27, #24		// Beginning of call sequence
	eor	x28, x28, x28		// Extract target address
1:	ldr	w2, [x27], #4
	cmp	w2, w4			// STR marks end of loop (little-endian)
	beq	2f
	bfxil	x28, x2, #5, #16	// Copy out address bits
	ror	x28, x28, #16
	b	1b
2:	PUSH1	x28			// Verify target address
	CALL	to_name
	POP1	x0
	cbnz	x0, 3f
	ADRL	x0, XDICTIONARY
	ldr	x0, [x0]
	cmp	x28, x0
	blo	4f
	add	x0, x0, DICT_SIZE
	cmp	x28, x0
	bhs	4f
3:	sub	x27, x27, #4		// Delete STR
	mov	x0, x27			// Rewind instruction sync
	CALL	rewind
	ldr	w2, =0xd61f0000		// BR X0
	str	w2, [x27], #4		// Replace tail call with jump
	b	5f
4:	HERE	x27			// Not a tail call, add a RETURN
	adr	x1, 6f			// RETURN
	ldp	w2, w3, [x1], #8	// Copy 3 instructions
	stp	w2, w3, [x27], #8
	ldr	w2, [x1]
	str	w2, [x27], #4
5:	UADDR	x1, DP			// Update DP
	str	x27, [x1]
	ldp	x27, x28, [RSP], #16
6:	RETURN

// +if ( S: ,x -- ,x / C: -- ,addr) Execute body when X >= 0
// -if ( S: ,x -- ,x / C: -- ,addr) Execute body when X <  0
// 0if ( S: ,x -- ,x / C: -- ,addr) Execute body when X == 0
//  if ( S: ,x -- ,x / C: -- ,addr) Execute body when X <> 0
	.text
plus_if:
	ldr	w0, =0x54000004		// bmi
	b	1f
minus_if:
	ldr	w0, =0x54000005		// bpl
	b	1f
zero_if:
	ldr	w0, =0x54000001		// bne
	b	1f
nzero_if:
	ldr	w0, =0x54000000		// beq
1:	UFETCH	x1, DP			// Look for "drop if"
	sub	x7, x1, #12
	ldp	w2, w3, [x7]
	adr	x4, drop
	ldp	w5, w6, [x4]
	cmp	w2, w5
	ccmp	w3, w6, #0, EQ
	bne	2f
	ldr	w2, [x7, #8]		// Compare 12 bytes
	ldr	w5, [x4, #8]
	cmp	w2, w5
	beq	3f			// Not found?
2:	ldr	w2, =0xea14029f		// tst T, T
	str	w2, [x1], #4
3:	PUSH1	x1			// Save location of jump instruction
	str	w0, [x1], #4
	UADDR	x0, DP			// Update DP
	str	x1, [x0]
	RETURN

// then ( S: -- / C: ,addr --)
	.text
then:	UADDR	x0, DP
	ldr	x1, [x0]		// Destination
	ldr	w2, =0xd503201f		// Pad with NOP
	str	w2, [x1], #4
	str	x1, [x0]
	sub	x1, x1, T		// Compute jump offset
	asr	x1, x1, #2		// Encode for 4-byte instruction size
	ldr	w2, [T]			// Update jump instruction
	bfi	w2, w1, #5, #19
	str	w2, [T]
	POP1	x0			// Rewind instruction sync
	b	rewind


// Interpreter

// (literal) ( -- ,x) Runtime code for literal
	.text
_literal:
	str	S, [PSP, #-8]!
	mov	S, T
	movz	T, #0
	movk	T, #0, LSL #16
	movk	T, #0, LSL #32
	movk	T, #0, LSL #48
	.set	_literal_len, (. - _literal)

// literal ( C: ,x -- / S: -- ,x) Compile literal
	.text
	.balign	4
literal:
	UADDR	x0, DP
	ldr	x1, [x0]		// Destination
	adr	x2, _literal		// Source
	ldr	x3, =_literal_len	// Limit
	add	x3, x2, x3
	ldr	w4, [x2], #4		// Copy STR
	str	w4, [x1], #4
	ldr	w4, [x2], #4		// Copy MOV
	str	w4, [x1], #4
	ldr	w4, [x2], #4		// Copy MOVZ
	bfi	w4, TW, #5, #16		// Populate immediate data
	ror	T, T, #16
	str	w4, [x1], #4		// Write MOVZ
	eor	x5, x5, x5
1:	cmp	x2, x3			// Copy up to 3 MOVK instructions
	beq	2f
	ldr	w4, [x2], #4
	bfxil	x5, T, #0, #16		// Inspect next 16-bits
	ror	T, T, #16
	cbz	x5, 1b			// Don't bother to encode zeros
	bfi	w4, w5, #5, #16		// Populate immediate data
	str	w4, [x1], #4		// Write MOVK
	b	1b
2:	str	x1, [x0]		// Update DP
	DROP1
	RETURN


// Synchronize data and instruction caches
// See Arm Architecture Reference Manual for A-Profile Architecture
//   Section B2.2.25, Page B2-156 (Aug 19, 2022)
//   Concurrent modification and execution of instructions

	.text
update_xdict:
	PUSHLR	x28
#ifdef Darwin
	eor	x0, x0, x0		// Disable write protection
	bl	_pthread_jit_write_protect_np
#endif
	UFETCH	x0, SYNC		// Compute count
	UFETCH	x28, DP
	subs	x28, x28, x0
	ble	1f
	PUSH1	x0			// src
	CALL	dp_to_xp		// dst
	PUSH1	x0
	PUSH1	x28			// count
	CALL	move
1:
#ifdef Darwin
	mov	x0, 1			// Enable write protection
	bl	_pthread_jit_write_protect_np
#endif
	POPLR	x28
	RETURN

	.text
sync:	CALL	update_xdict
	UFETCH	x0, SYNC
	CALL	dp_to_xp
	UFETCH	x1, SYNC
	UFETCH	x2, DP
1:	cmp	x1, x2
	bhs	2f
	dc	cvau, x0		// Clean data cache
	dsb	ish			// Ensure visibility of cleaned data
	ic	ivau, x0		// Invalidate instruction cache
	dsb	ish
	isb				// Synchronize fetched instruction stream
	add	x0, x0, #64
	add	x1, x1, #64
	b	1b
2:	and	x2, x2, #-64		// Remember last sync point
	UADDR	x1, SYNC
	str	x2, [x1]
	RETURN


// Available error messages
	.const
	.balign	4
error_msg:	.ascii	" ? "
		.set	error_len, (. - error_msg)
	.balign	4
uo_msg:		.ascii	"user area exceeded\n"
		.set	uo_len, (. - uo_msg)
	.balign	4
su_msg:		.ascii	"stack underflow\n"
		.set	su_len, (. - su_msg)
	.balign	4
so_msg:		.ascii	"stack overflow\n"
		.set	so_len, (. - so_msg)
	.balign	4
ru_msg:		.ascii	"return stack underflow\n"
		.set	ru_len, (. - ru_msg)
	.balign	4
ro_msg:		.ascii	"return stack overflow\n"
		.set	ro_len, (. - ro_msg)
	.balign	4
do_msg:		.ascii	"dictionary space exceeded\n"
		.set	do_len, (. - do_msg)
	.balign	4
fo_msg:		.ascii	"forth wordlist exceeded\n"
		.set	fo_len, (. - fo_msg)
	.balign	4
mo_msg:		.ascii	"macro wordlist exceeded\n"
		.set	mo_len, (. - mo_msg)
	.balign	4
green_err:	.ascii	"compiling\n"
		.set	green_len, (. - green_err)
	.balign	4
yellow_err:	.ascii	"executing\n"
		.set	yellow_len, (. - yellow_err)
	.balign	4
cyan_err:	.ascii	"not a macro\n"
		.set	cyan_len, (. - cyan_err)
	.balign	4
parse_msg:	.ascii	"invalid\n"
		.set	parse_len, (. - parse_msg)

// Table of error messages
	.data
	.balign	8
message_data:
	.dc.a	error_msg		//  0 Error
	.dc.a	uo_msg			//  1 User area overflow
	.dc.a	su_msg			//  2 Parameter stack underflow
	.dc.a	so_msg			//  3 Parameter stack overflow
	.dc.a	ru_msg			//  4 Return stack underflow
	.dc.a	ro_msg			//  5 Return stack overflow
	.dc.a	do_msg			//  6 Dictionary overflow
	.dc.a	fo_msg			//  7 FORTH wordlist overflow
	.dc.a	mo_msg			//  8 MACRO wordlist overflow
	.dc.a	green_err		//  9 Unable to compile
	.dc.a	yellow_err		// 10 Unable to execute
	.dc.a	cyan_err		// 11 Not a macro
	.dc.a	parse_msg		// 12 Unable to parse

	.const
message_len:
	.dc.b	error_len
	.dc.b	uo_len
	.dc.b	su_len
	.dc.b	so_len
	.dc.b	ru_len
	.dc.b	ro_len
	.dc.b	do_len
	.dc.b	fo_len
	.dc.b	mo_len
	.dc.b	green_len
	.dc.b	yellow_len
	.dc.b	cyan_len
	.dc.b	parse_len

// message ( ,n)
	.text
	.balign	4
message:
	ADRL	x0, message_data
	ldr	x0, [x0, T, lsl #3]	// c-addr
	ADRL	x1, message_len
	ldrb	w1, [x1, T]		// u-count
	DROP1
	PUSH2	x0, x1
	b	type


// error ( ,n)
	.text
error:	WORD	x0
	PUSH1	x0
	CALL	count
	CALL	type			// Context
	PUSH1	xzr
	CALL	message			// Error
	CALL	message			// Message
	b	abort			// From the top..

.macro	ERROR	num
	PUSH1	\num
	b	error
.endm

	.text
parse_error:
	DROP1
	ERROR	12


// Set STATE to the current value of COLOR
.macro	UPDATE_STATE
	UFETCH	x0, COLOR
	UADDR	x1, STATE
	str	x0, [x1]
.endm


// ( ,x-hi ,x-lo) Create new dictionary entry
	.text
bred:	UPDATE_STATE
	UFETCH	x0, CURRENT		// Allocate header
	ldr	x1, [x0]
	add	x1, x1, #16
	str	x1, [x0]
	UADDR	x0, DP			// Current location
	ldr	x2, [x0]
	add	x2, x2, #3		// Align to instruction boundary
	and	x2, x2, #-4
	str	x2, [x0]
	sub	x2, x2, x1		// Relativize address
	stp	T, S, [x1]		// Copy name to header
	str	w2, [x1, #12]		// Copy address to header
	DROP2
	RETURN

// ( ,a-addr) Create new dictionary entry
	.text
red:	CALL	count
	CALL	encode
	b	bred


// ( ,x -- ..) Transition YELLOW>GREEN compiles a literal
	.text
bgreen_literal:
	UFETCH	x0, STATE
	and	w0, w0, #7
	cmp	w0, #YELLOW
	beq	literal
	RETURN

// ( ,x-hi ,x-lo) Compile number
	.text
bgreen_number:
	PUSHRSP	x28
	POP2	x28, xzr		// x-lo, x-hi (discarded)
	CALL	bgreen_literal		// Transition?
	UPDATE_STATE
	PUSH1	x28			// Compile number
	POPRSP	x28
	b	literal

// ( ,x-hi ,x-lo) Compile word
	.text
bgreen:	stp	x27, x28, [RSP, #-16]!
	POP2	x27, x28		// x-lo, x-hi
	CALL	bgreen_literal		// Transition?
	UPDATE_STATE

	PUSH2	x28, x27		// x-hi, x-lo
	CALL	bfindm			// Macro?
	POP1	x0			// xt
	cbz	x0, 1f
	mov	x28, x0
	CALL	sync			// Ensure executable
	mov	x0, x28
	ldp	x27, x28, [RSP], #16
	br	x0			// Execute it

1:	PUSH2	x28, x27		// x-hi, x-lo
	CALL	bfindf			// Word?
	cbz	T, 2f			// xt
	ldp	x27, x28, [RSP], #16
	b	compile_comma		// Compile it

2:	DROP1				// Discard result of bfindf
	PUSH2	x28, x27		// x-hi, x-lo
	ldp	x27, x28, [RSP], #16
	CALL	decode_number		// Number?
	POP1	x0			// u-junk
	cbz	x0, literal		// Compile it

	// None of the above?
	DROP1				// Discard result of decode_number
	ERROR	9

// ( ,a-addr) Compile word
	.text
green:	PUSHRSP	x28
	POP1	x28
	CALL	bgreen_literal		// Transition?
	UPDATE_STATE

1:	PUSH1	x28			// Macro?
	CALL	findm
	POP1	x0			// xt
	cbz	x0, 2f
	mov	x28, x0
	CALL	sync			// Ensure executable
	mov	x0, x28
	POPRSP	x28
	br	x0			// Execute it

2:	PUSH1	x28			// Is this a word?
	CALL	findf
	cbz	T, 3f
	POPRSP	x28
	b	compile_comma		// Compile word into definition

3:	DROP1				// Drop result of word search
	PUSH1	x28
	CALL	number			// Is this a number?
	POP1	x0
	cbnz	x0, 4f
	POPRSP	x28
	b	literal			// Compile number into definition

	// None of the above
4:	DROP1				// Drop bad number
	POPRSP	x28
	ERROR	9


// ( ,x-hi ,x-lo -- ,x-lo) Execute number
	.text
byellow_number:
	UPDATE_STATE
	NIP
	RETURN

// ( ,x-hi ,x-lo -- ..) Execute word
	.text
byellow:
	stp	x27, x28, [RSP, #-16]!
	mov	x27, T			// x-lo
	mov	x28, S			// x-hi
	UPDATE_STATE

	CALL	bfindf			// Word?
	POP1	x0			// xt
	cbz	x0, 1f
	mov	x28, x0
	CALL	sync			// Ensure executable
	mov	x0, x28
	ldp	x27, x28, [RSP], #16
	br	x0			// Execute it

1:	PUSH2	x28, x27		// x-hi, x-lo
	ldp	x27, x28, [RSP], #16
	CALL	decode_number		// Number?
	POP1	x0			// u-junk
	cbnz	x0, 2f
	RETURN				// Now in T

	// Neither?
	DROP1				// Discard result of decode_number
	ERROR	10

// ( ,a-addr) Execute word
	.text
yellow:	PUSHRSP	x28
	mov	x28, T
	UPDATE_STATE
	CALL	findf			// Word?
	POP1	x0			// xt
	cbz	x0, 1f
	mov	x28, x0
	CALL	sync			// Ensure executable
	mov	x0, x28
	POPRSP	x28
	br	x0			// Execute it
1:	PUSH1	x28			// Number?
	CALL	number
	POP1	x0			// u-junk
	cbnz	x0, 2f
	POPRSP	x28
	RETURN				// T now holds parsed number
2:	DROP1				// Discard result of number
	POPRSP	x28
	ERROR	10


// ( ,x-hi ,x-lo) Postpone number
	.text
bcyan_number:
	UPDATE_STATE
	NIP
	CALL	literal			// Postpone it
	adr	x0, literal
	PUSH1	x0
	b	compile_comma

// ( ,x-hi ,x-lo) Postpone word
	.text
bcyan:	stp	x27, x28, [RSP, #-16]!
	mov	x27, S			// x-hi
	mov	x28, T			// x-lo
	UPDATE_STATE

	CALL	bfindm			// Macro?
	cbz	T, 1f			// xt
	ldp	x27, x28, [RSP], #16
	b	compile_comma		// Compile it

1:	DROP1				// Discard result of bfindm
	PUSH2	x27, x28		// x-hi, x-lo
	CALL	bfindf			// Word?
	cbz	T, 2f			// xt
	ldp	x27, x28, [RSP], #16
	CALL	literal			// Postpone it
	adr	x0, compile_comma
	PUSH1	x0
	b	compile_comma

2:	DROP1				// Discard result of bfindf
	PUSH2	x27, x28		// x-hi, x-lo
	ldp	x27, x28, [RSP], #16
	CALL	decode_number
	POP1	x0			// u-junk
	cbnz	x0, 3f
	CALL	literal			// Postpone it
	adr	x0, literal
	PUSH1	x0
	b	compile_comma

3:	DROP1				// Discard result of decode_number
	ERROR	11

// ( ,a-addr) Postpone word
	.text
cyan:	PUSHRSP	x28
	UPDATE_STATE
	mov	x28, T
	CALL	findm			// Is this a macro?
	cbz	T, 1f
	POPRSP	x28
	b	compile_comma		// Compile address of macro
1:	DROP1				// Drop result of macro search
	PUSH1	x28
	CALL	findf			// Is this a word?
	cbz	T, 2f
	CALL	literal			// Compile instruction to compile word
	adr	x0, compile_comma
	PUSH1	x0
	POPRSP	x28
	b	compile_comma
2:	DROP1				// Drop result of word search
	PUSH1	x28
	CALL	number			// Is this a number?
	POP1	x0
	cbnz	x0, 3f
	CALL	literal			// Compile instruction to compile literal
	adr	x0, literal
	PUSH1	x0
	POPRSP	x28
	b	compile_comma
3:	DROP1				// Drop bad number
	POPRSP	x28
	ERROR	11


// Check stack, dictionary limits.
	.text
check:
	// Check user variables
	UFETCH	x0, UP			// Writing past end?
	cmp	x0, #USER_SIZE
	bhi	1f

	// Check parameter stack
	cmp	PSP, U			// Underflow?
	bhi	2f
	SP0	x0
	sub	x0, x0, #16
	ldr	x1, [x0]		// Sentinel?
	cmp	x1, x0
	bne	2f
	sub	x0, x0, #DATA_SIZE	// Overflow?
	cmp	PSP, x0
	blo	3f

	// Check return stack
	mov	x2, RSP			// Underflow?
	cmp	x2, x0
	bhs	4f
	RP0	x0
	sub	x0, x0, #16
	ldr	x1, [x0]		// Sentinel?
	cmp	x1, x0
	bne	4f
	sub	x0, x0, #RETURN_SIZE	// Overflow?
	cmp	x2, x0
	blo	5f

	// Check dictionary
	UFETCH	x0, DP
	UFETCH	x1, MACRO_START
	add	x1, x1, #DICT_SIZE
	cmp	x0, x1			// Writing past end of dictionary?
	bhi	6f
	UFETCH	x0, FORTH
	UFETCH	x1, DICT_START
	cmp	x0, x1			// Overwriting dictionary entries?
	bhs	7f
	UFETCH	x0, MACRO
	UFETCH	x1, FORTH_START
	cmp	x0, x1
	bhs	8f
	RETURN

1:	ERROR	1
2:	ERROR	2
3:	ERROR	3
4:	ERROR	4
5:	ERROR	5
6:	ERROR	6
7:	ERROR	7
8:	ERROR	8


// Handle current word according to its color
	.data
	.balign 8
bdispatch:
	.dc.a	parse_error		// BLACK is not used
	.dc.a	bred
	.dc.a	bgreen
	.dc.a	byellow
	.dc.a	parse_error		// BLUE should never get here
	.dc.a	parse_error		// MAGENTA is not used
	.dc.a	bcyan
	.dc.a	parse_error		// WHITE should never get here
	.dc.a	parse_error		// BLACK is not used
	.dc.a	bred
	.dc.a	bgreen_number
	.dc.a	byellow_number
	.dc.a	parse_error		// BLUE should never get here
	.dc.a	parse_error		// MAGENTA is not used
	.dc.a	bcyan_number
	.dc.a	parse_error		// WHITE should never get here

// Binary interpreter
	.text
binterpret:
	CALL	bword			// Loop over each word of buffer
	cbnz	T, 1f			// Got a word?
	cmp	S, -1			// Special case for number 0
	bne	2f
1:	UFETCH	x1, COLOR
	ADRL	x0, bdispatch
	ldr	x0, [x0, x1, LSL #3]
	str	R, [RSP, #-16]!
	blr	x0			// Dispatch on current color
	CALL	check
	b	binterpret
2:	DROP2				// Discard result of bword
	RETURN


// Handle current word according to its color
	.data
	.balign	8
dispatch:
	.dc.a	parse_error		// BLACK is not used
	.dc.a	red
	.dc.a	green
	.dc.a	yellow
	.dc.a	parse_error		// BLUE should never get here
	.dc.a	parse_error		// MAGENTA is not used
	.dc.a	cyan
	.dc.a	parse_error		// WHITE should never get here

// ( --) Interpret source text or command
	.text
tinterpret:
	CALL	tword			// Loop over each word of input
	ldrb	w0, [T]			// Got a word?
	cbz	w0, 1f
	UADDR	x0, COLOR
	ldrb	w1, [x0]
	and	w1, w1, #7		// Ignore ALT bit
	ADRL	x0, dispatch
	ldr	x0, [x0, x1, lsl #3]
	PUSHLR	xzr
	blr	x0			// Dispatch on current color
	CALL	check
	b	tinterpret
1:	DROP1				// Discard result of tword
	RETURN


// Collect usage profile.
	.text
usage:	UADDR	x1, SP_MIN		// Parameter stack usage
	ldr	x0, [x1]
	cmp	PSP, x0
	bhs	1f
	str	PSP, [x1]
1:	mov	x2, RSP			// Return stack usage
	UADDR	x1, RP_MIN
	ldr	x0, [x1]
	cmp	x2, x0
	bhs	2f
	str	x2, [x1]
2:	UFETCH	x2, DP			// Dictionary usage
	UADDR	x1, DICT_MAX
	ldr	x0, [x1]
	cmp	x2, x0
	bls	3f
	str	x2, [x1]
3:	RETURN


// Include bootstrap code
	.const
bootstrap_forth:
	.incbin	PRELUDE
#ifdef BOARD
	.incbin	BOARD
#endif
	.dc.b	0x83			// (yellow)
	.ascii	"main"
	.set	bootstrap_len, (. - bootstrap_forth)
	.dc.b	0

	.text
	.balign	4
bootstrap:
	UADDR	x0, USE			// Use bootstrap as current source
	ADRL	x1, bootstrap_forth
	str	x1, [x0]
	UADDR	x0, BLK			// Block 0
	str	xzr, [x0]
	UADDR	x0, IN			// Byte 0
	str	xzr, [x0]
	UADDR	x0, SOURCE_ID
	mov	x1, #-1
	str	x1, [x0]
	UADDR	x0, SPAN
	mov	x1, bootstrap_len
	str	x1, [x0]
	RP0	x0			// Setup return stack
	sub	x0, x0, #16
	mov	RSP, x0
	str	x0, [RSP]		// Sentinel
	CALL	tinterpret		// Interpret bootstrap source
	b	abort			// Return to warm start

	.text
encode_wordlist:
	PUSHRSP	x28
	mov	x28, x0
1:	ldrb	w0, [x28]
	cbz	x0, 2f			// More?
	add	x1, x28, #1
	PUSH2	x1, x0			// c-addr, u
	CALL	encode
	POP2	x0, x1			// x-lo, x-hi
	str	x0, [x28]		// 12 bytes
	str	w1, [x28, #8]
	sub	x28, x28, #16
	b	1b
2:	POPRSP	x28
	RETURN

	.text
encode_wordlists:
	ADRL	x0, builtin_macro
	CALL	encode_wordlist
	ADRL	x0, builtin_forth
	CALL	encode_wordlist
	RETURN

// quit ( --) Main loop of outer interpreter
	.text
quit:	UADDR	x0, USE			// No current source file
	str	xzr, [x0]
	UADDR	x0, SOURCE_ID		// Read from terminal
	str	xzr, [x0]
	mov	x0, xzr
	CALL	use_block
	ADRL	x0, BOOTSTRAP		// Once only..
	ldr	x1, [x0]
	cbnz	x1, 1f
	mov	x1, #-1
	str	x1, [x0]
	CALL	encode_wordlists
	b	bootstrap		// Execute Forth bootstrap
1:	RP0	x0			// Setup return stack
	sub	x0, x0, #16
	mov	RSP, x0
	str	x0, [RSP]		// Sentinel
	CALL	query
	CALL	tinterpret
	CALL	usage
	b	1b


// abort ( --)
// Warm start sets up parameter stack and execution environment
	.text
abort:	SP0	PSP			// Setup parameter stack
	sub	PSP, PSP, #16
	mov	x0, #-1
	stp	PSP, x0, [PSP]		// Sentinel
	DROP2				// T, S can never be empty
	UADDR	x0, FORTH		// Initialize search order
	UADDR	x1, CURRENT
	str	x0, [x1]
	b	quit			// Enter main loop


// Cold start sets up system data structures
	.global	cold
	.text
cold:	mov	x0, #UP			// Allocate user area
	add	x0, x0, #8
	UADDR	x1, UP
	str	x0, [x1]

	ADRL	x0, WDICTIONARY		// Subdivide dictionary space
	ldr	x0, [x0]
	UADDR	x1, MACRO_START
	str	x0, [x1]
	add	x1, x0, #MACRO_SIZE
	UADDR	x2, FORTH_START
	str	x1, [x2]
	add	x1, x0, #WORDLIST_SIZE
	UADDR	x2, DICT_START
	str	x1, [x2]
	UADDR	x2, DP
	str	x1, [x2]		// Next available
	UADDR	x2, SYNC
	and	x1, x1, #-64
	str	x1, [x2]

	// Link wordlists
	UFETCH	x0, MACRO_START
	UADDR	x1, MACRO		// Point to allocated wordlist
	str	x0, [x1]
	ADRL	x1, builtin_macro	// Link to builtin wordlist
	stp	xzr, x1, [x0]

	UFETCH	x0, FORTH_START
	UADDR	x1, FORTH		// Point to allocated wordlist
	str	x0, [x1]
	ADRL	x1, builtin_forth	// Link to builtin wordlist
	stp	xzr, x1, [x0]

	// Initialize usage tracking
	UADDR	x1, SP_MIN		// Parameter stack usage
	SP0	x0
	str	x0, [x1]
	UADDR	x1, RP_MIN		// Return stack usage
	RP0	x0
	str	x0, [x1]
	UADDR	x1, DICT_MAX		// Dictionary usage
	UFETCH	x0, DP
	str	x0, [x1]

	b	abort			// Run the interpreter


	.global	main
	.weak	main
	.text
main:	ADRL	x3, ARGC		// Save process startup information
	str	x0, [x3]
	ADRL	x3, ARGV
	str	x1, [x3]
	ADRL	x3, ENVIRON
	str	x2, [x3]
	mov	U, RSP			// Save initial stack frame
	sub	U, U, #(BUFFER_SIZE + USER_SIZE)
	and	U, U, #-4096

	// Allocate executable dictionary
	eor	x5, x5, x5		// offset
	mov	x4, #-1			// db
	mov	x3, #(MAP_JIT | MAP_ANONYMOUS | MAP_PRIVATE)	// flags
	mov	x2, #(PROT_EXEC | PROT_WRITE | PROT_READ)	// prot
	mov	x1, #DICT_SIZE		// len
	eor	x0, x0, x0		// addr
	mov	SYS_REG, #SYS_mmap
	svc	#SYS_SVC
	ADRL	x1, XDICTIONARY
	str	x0, [x1]

	// Allocate writable dictionary
#ifdef Darwin
	eor	x5, x5, x5		// offset
	mov	x4, #-1			// db
	mov	x3, #(MAP_ANONYMOUS | MAP_PRIVATE)	// flags
	mov	x2, #(PROT_WRITE | PROT_READ)		// prot
	mov	x1, #DICT_SIZE		// len
	eor	x0, x0, x0		// addr
	mov	SYS_REG, #SYS_mmap
	svc	#SYS_SVC
#endif
	ADRL	x1, WDICTIONARY
	str	x0, [x1]

	b	cold			// Relay into cold start


// When not linking the C runtime
#ifndef Darwin
	.global	_start
	.weak	_start
	.text
_start:	ADRL	x1, ATEXIT		// Save process startup information
	str	x0, [x1]		// To be called at exit
	ldr	x0, [RSP], #8		// argc
	mov	x1, RSP			// argv
	mov	x2, RSP
	add	x2, x2, x0, lsl #3
	add	x2, x2, #8		// environ
	b	main
#endif


// Define dictionary headers for all builtin words
.macro	HEADER	name, code
1:	.dc.b	len\@			// Length of name
2:	.ascii	"\name"			// Up to 11 bytes of name
	.set	len\@, (. - 2b)
	.ds.b	(11 - len\@)		// Padding
	.dc.l	(\code - 1b)		// Offset to code
.endm

// End of a wordlist
.macro	TAIL
	.ds.b	16
.endm

	.data
	.balign	16
	TAIL
	HEADER	"then",		then
	HEADER	"literal",	literal
	HEADER	"if",		nzero_if
	HEADER	"compile,",	compile_comma
	HEADER	";",		exit
	HEADER	"0if",		zero_if
	HEADER	"-if",		minus_if
builtin_macro:
	HEADER	"+if",		plus_if

	TAIL
	HEADER	"%type%",	builtin_type
	HEADER	"%query%",	builtin_query
	HEADER	"%key%",	builtin_key
	HEADER	"%expect%",	builtin_expect
	HEADER	"%emit%",	builtin_emit
	HEADER	"xor",		_xor
	HEADER	"xchg",		xchg
	HEADER	"word",		tword
	HEADER	"within",	within
	HEADER	"w@",		w_fetch
	HEADER	"w!",		w_store
	HEADER	"use",		use
	HEADER	"up",		up
	HEADER	"u+",		under_plus
	HEADER	"type",		type
	HEADER	"tuck",		tuck
	HEADER	"tib",		tib
	HEADER	"swap-",	swap_minus
	HEADER	"swap",		swap
	HEADER	"span",		span
	HEADER	"source-id!",	source_id_store
	HEADER	"source-id",	source_id
	HEADER	"source",	source
	HEADER	"rshift",	rshift
	HEADER	"query",	query
	HEADER	"push",		push
	HEADER	"pop",		pop
	HEADER	"pad",		pad
	HEADER	"over",		over
	HEADER	"or",		_or
	HEADER	"nop",		_nop
	HEADER	"nip",		nip
	HEADER	"negate",	negate
	HEADER	"natural",	natural
	HEADER	"move",		move
	HEADER	"min",		min
	HEADER	"max",		max
	HEADER	"marker",	marker
	HEADER	"macro",	macro
	HEADER	"lshift",	lshift
	HEADER	"load",		load
	HEADER	"key",		key
	HEADER	"invert",	invert
	HEADER	"huffman",	huffman
	HEADER	"hex",		hex
	HEADER	"here",		here
	HEADER	"forth",	forth
	HEADER	"find",		find
	HEADER	"fill",		fill
	HEADER	"expect",	expect
	HEADER	"execute",	execute
	HEADER	"erase",	erase
	HEADER	"environ",	environ
	HEADER	"encode",	encode
	HEADER	"emit",		emit
	HEADER	"dup",		dup
	HEADER	"drop",		drop
	HEADER	"dp",		dp
	HEADER	"defer@",	defer_fetch
	HEADER	"defer!",	defer_store
	HEADER	"defer",	defer
	HEADER	"decode",	decode
	HEADER	"decimal",	decimal
	HEADER	"count",	count
	HEADER	"compare",	compare
	HEADER	"char",		char
	HEADER	"c@",		c_fetch
	HEADER	"c,",		c_comma
	HEADER	"c!",		c_store
	HEADER	"bye",		bye
	HEADER	"block",	block
	HEADER	"blk",		blk
	HEADER	"base",		base
	HEADER	"ashift",	ashift
	HEADER	"argv",		argv
	HEADER	"argc",		argc
	HEADER	"and",		_and
	HEADER	"allot",	allot
	HEADER	"aligned",	aligned
	HEADER	"align",	align
	HEADER	"abs",		abs
	HEADER	"abort",	abort
	HEADER	"@",		fetch
	HEADER	">xp",		to_xp
	HEADER	">name",	to_name
	HEADER	">in",		input_pointer
	HEADER	">body",	to_body
	HEADER	">=",		not_less
	HEADER	">",		greater
	HEADER	"=",		equal
	HEADER	"<xp",		from_xp
	HEADER	"<>",		not_equal
	HEADER	"<=",		not_greater
	HEADER	"<",		less
	HEADER	"8/",		eight_div
	HEADER	"8-",		eight_minus
	HEADER	"8+",		eight_plus
	HEADER	"8*",		eight_star
	HEADER	"2/",		two_div
	HEADER	"2*",		two_star
	HEADER	"1-",		decrement
	HEADER	"1+",		increment
	HEADER	"0>=",		non_negative
	HEADER	"0>",		positive
	HEADER	"0=",		zerop
	HEADER	"0<>",		non_zero
	HEADER	"0<=",		non_positive
	HEADER	"0<",		negative
	HEADER	"/mod",		div_mod
	HEADER	"-",		minus
	HEADER	",",		comma
	HEADER	"+!",		plus_store
	HEADER	"+",		plus
	HEADER	"*/mod",	star_div_mod
	HEADER	"*",		star
	HEADER	"'",		tick
	HEADER	"#tib",		num_tib
builtin_forth:
	HEADER	"!",		store


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
