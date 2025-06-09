;; Copyright (c) 2022,2023 Eric Sessoms
;; See license at end of file


;; System interface
%define start _start
MAP_ANON	equ	0x1000
MAP_PRIVATE	equ	2

%ifdef Darwin
%ifdef LIBC
%define main _main
%else
%define start start
%endif
%define _SYS(nr) (0x2000000 + nr)
SYS_exit	equ	_SYS(1)
SYS_mmap	equ	_SYS(197)
SYS_read	equ	_SYS(3)
SYS_write	equ	_SYS(4)
%endif

%ifdef FreeBSD
SYS_exit	equ	1
SYS_mmap	equ	477
SYS_read	equ	3
SYS_write	equ	4
%endif

%ifdef Linux
SYS_exit	equ	60
SYS_mmap	equ	9
SYS_read	equ	0
SYS_write	equ	1
MAP_ANON	equ	0x20
%endif


;; Memory map, from high-memory to low
;; - Process information block
;; - Block buffers
;; - User variables
;; - Parameter stack
;; - Return stack
;; - Terminal input buffer
;; - Pad
;; - User dictionary
;; - System dictionary
;; - System variables
;; - Pre-compiled Forth

BUFFER_SIZE	equ	4096		; 7 blocks + metadata
USER_SIZE	equ	8192		; 1024 variables
DATA_SIZE	equ	4096		; 512 cells ought to be enough for anybody
RETURN_SIZE	equ	8192		; 1024 cells

;; i.e., one hugepage
DICT_SIZE	equ	(2*1024*1024)	; Almost 2MB for definitions
WORDLIST_SIZE	equ	(2048*16)	; 1792 forth headers
MACRO_SIZE	equ	(256*16)	; 256 macro headers


;; Registers
;; - RAX result, scratch
;; - RCX parameter 4
;; - RDX parameter 3
;; - RBX T (top of stack)
;; - RSP RP (return stack pointer)
;; - RBP SP (parameter stack pointer)
;; - RSI parameter 2
;; - RDI parameter 1
;; - R8  B (anonymous local variable, parameter 5)
;; - R9  A (anonymous local variable, parameter 6)
;; - R10 Z (anonymous local variable, caller saved)
;; - R11 Y (anonymous local variable, caller saved)
;; - R12 X (anonymous local variable)
;; - R13 W (anonymous local variable)
;; - R14 U (user pointer)
;; - R15 [global offset table]
;; - FS  [thread-local storage]

%define SP rbp				; Parameter stack pointer
%define RP rsp				; Return stack pointer

;; Pseudo-registers
;; - R  top of return stack
;; - S  second on stack

%define T  rbx				; Top of stack
%define TW ebx				; 32-bit "word" name for T
%define TH bx				; 16-bit "halfword" name for T
%define TC bl				;  8-bit "char" name for T
%define U  r14				; User pointer
%define W  r13				; Anonymous local variable
%define X  r12				; Anonymous local variable
%define Y  r11				; Anonymous local variable, caller saved
%define Z  r10				; Anonymous local variable, caller saved
%define A  r9				; Anonymous local variable, parameter 6
%define B  r8				; Anonymous local variable, parameter 5


;; Conveniences

;; Second-on-stack is no longer needed
%macro	NIP 0
	add	SP, 8
%endmacro

;; Top-of-stack is no longer needed
%macro	DROP1 0
	mov	T, [SP]
	NIP
%endmacro

;; Top two items on stack are no longer needed
%macro	DROP2 0
	mov	T, [SP + 8]
	add	SP, 16
%endmacro

;; Pop parameter stack into ARG
%macro	POP1	1
	mov	%1, T
	DROP1
%endmacro

;; Same effect as "POP1 \arg1 ; POP1 \arg2"
%macro	POP2	2
	mov	%1, T
	mov	%2, [SP]
	DROP2
%endmacro

;; Push ARG onto parameter stack
%macro	PUSH1	1
	sub	SP, 8
	mov	[SP], T
	mov	T, %1
%endmacro

;; Same effect as "PUSH1 \arg1 ; PUSH1 \arg2"
%macro	PUSH2	2
	mov	[SP -  8], T
	mov	[SP - 16], %1
	sub	SP, 16
	mov	T, %2
%endmacro


;; We define our colors to match their ANSI escape codes. For no
;; necessary reason, it just makes them easier to remember.
;;
;; The colors themselves are not important, we're only
;; interested in their meaning, but the color names provide a
;; convenient handle with which to refer to the meaning.
;;
;; This gives us a consistent vocabulary.
;;
;; Notes:
;; - Bit #3 (8) is used as an "alternate" flag.

BLACK	equ	0			; Data
RED	equ	1			; Label
GREEN	equ	2			; Compile
YELLOW	equ	3			; Execute
BLUE	equ	4			; Format
MAGENTA	equ	5			; Retain
CYAN	equ	6			; Postpone
WHITE	equ	7			; Comment
ALT	equ	8			; Alternative usage


;; User variables

;; Address of user variable
%macro	UADDR	2
	lea	%1, [U + %2]
%endmacro

;; Value of user variable
%macro	UFETCH	2
	mov	%1, [U + %2]
%endmacro

;; Set value of user variable
%macro	USTORE	2
	mov	[U + %1], %2
%endmacro

;; Declare user variable
%assign	UP 0				; Next available offset
%macro	USER	1
	%assign	%1 UP
	%assign	UP (UP + 8)
%endmacro

USER	BASE				; Current number base for I/O
USER	BLK				; Current input
USER	COLOR				; Color of current input word
USER	CURRENT				; Active wordlist for new definitions
USER	DP				; Dictionary pointer
USER	FORTH				; Head of "forth" wordlist
USER	IN				; Bytes consumed in current buffer
USER	MACRO				; Head of "macro" wordlist
USER	NUM_TIB				; Bytes used in input buffer
USER	SPAN				; Number of bytes stored by EXPECT
USER	STATE				; Color of previous input word
USER	SOURCE_ID			; Identifies source for text interpreter
USER	USE				; Current source file

;; Remember initial dictionary allocation
USER	DICT_START			; Start of dictionary allocation
USER	FORTH_START			; Start of "forth" wordlist
USER	MACRO_START			; Start of "macro" wordlist

;; Collect usage profile
USER	DICT_MAX			; Maximum dictionary usage
USER	RP_MIN				; Maximum return stack usage
USER	SP_MIN				; Maximum parameter stack usage


%macro	SP0	1			; Base of parameter stack
	mov	%1, U
%endmacro
%macro	RP0	1			; Base of return stack
	lea	%1, [U - DATA_SIZE]
%endmacro
%macro	TIB	1			; Terminal input buffer
	lea	%1, [U - (DATA_SIZE + RETURN_SIZE)]
%endmacro
%macro	HERE	1			; Next available location in dictionary
	UFETCH	%1, DP
%endmacro

;; Reserves dictionary space to compile a literal
%macro	WORD	1
	HERE	%1
	add	%1, 32
%endmacro

;; Reserves dictionary space to read a word
%macro	PAD	1
	HERE	%1
	add	%1, 160
%endmacro


;; System variables

	section .bss
	alignb	8
ARGC:		resq	1		; Number of command-line arguments
ARGV:		resq	1		; Array of command-line arguments
ATEXIT:		resq	1		; Atexit function from startup
BOOTSTRAP:	resq	1		; Indicates that bootstrap has run
DICTIONARY:	resq	1		; Initial dictionary allocation
ENVIRON:	resq	1		; Array of environment variables


;; In all stack comments below, I use a prefixed comma (",") to
;; call attention to T in register.

;; Inline-able primitives
	section	.text

;; We'll inline words that:
;; 1. Are arbitrarily no larger than (literal), so no more than
;;    18 bytes of object code.
;; 2. Don't overwrite any callee-save registers, so we don't
;;    have to juggle register reassignment.
;; 3. Don't make any calls and don't access any variables, to
;;    ensure the word is relocatable.
;; 4. Don't contain the byte value "C3" (RET), which we look for
;;    to mark the end of the definition.

;; ! ( x ,a-addr) Write 8-byte value to ADDR
store:	mov	rax, [SP]
	mov	[T], rax
	DROP2
	ret

;; #tib ( -- ,a-addr) Holds number of bytes used in TIB
num_tib:
	SUB	SP, 8
	mov	[SP], T
	UADDR	T, NUM_TIB
	ret

;; * ( n1 ,n2 -- ,n1*n2)
star:	imul	T, [SP]
	NIP
	ret

;; + ( n1 ,n2 -- ,n1+n2)
plus:	add	T, [SP]
	NIP
	ret

;; +! ( n ,addr)
plus_store:
	mov	rax, [T]
	add	rax, [SP]
	mov	[T], rax
	DROP2
	ret

;; - ( n1 ,n2 -- ,n1-n2)
minus:	POP1	rcx
	sub	T, rcx
	ret

;; 0< ( ,n -- ,f)
negative:
	sar	T, 63
	ret

;; 0<= ( ,n -- ,f)
non_positive:
	mov	rax, T
	xor	TW, TW
	test	rax, rax
	jg	.pos
	not	T
.pos:	ret

;; 0<> ( ,n -- ,f)
non_zero:
	test	T, T
	jz	.zero
	xor	TW, TW
	not	T
.zero:	ret

;; 0= ( ,n -- ,f)
zerop:	test	T, T
	jz	.zero
	xor	TW, TW
	not	T
.zero:	not	T
	ret

;; 0> ( ,n -- ,f)
positive:
	mov	rax, T
	xor	TW, TW
	test	rax, rax
	jle	.np
	not	T
.np:	ret

;; 0>= ( ,n -- ,f)
non_negative:
	sar	T, 63
	not	T
	ret

;; 1+ ( ,n -- ,n+1)
increment:
	mov	rcx, T			; "inc T" can't be inlined.
	inc	rcx
	mov	T, rcx
	ret

;; 1- ( ,n -- ,n-1)
decrement:
	dec	T
	ret

;; 2* ( ,n -- ,n*2)
two_star:
	shl	T, 1
	ret

;; 2/ ( ,n -- ,n/2)
;; Division, implemented as shift, preserves sign
two_div:
	sar	T, 1
	ret

;; 8* ( ,n -- ,n*8)
eight_star:
	shl	T, 3
	ret

;; 8+ ( ,n -- ,n+8)
eight_plus:
	mov	rcx, T
	add	rcx, 8
	mov	T, rcx
	ret

;; 8- ( ,n -- ,n-8)
eight_minus:
	sub	T, 8
	ret

;; 8/ ( ,n -- ,n/8)
;; Division, implemented as shift, preserves sign
eight_div:
	sar	T, 3
	ret

;; < ( n1 ,n2 -- ,f)
less:	mov	rcx, T			; n2
	mov	T, [SP]			; n1
	sub	T, rcx			; n1 - n2
	sar	T, 63			; n1 < n2
	add	SP, 8
	ret

;; <= ( n1 ,n2 -- ,f)
not_greater:
	sub	T, [SP]			; n2 -  n1
	sar	T, 63			; n2 <  n1
	not	T			; n2 >= n1
	add	SP, 8
	ret				; n1 <= n2

;; <> ( n1 ,n2 -- ,f)
not_equal:
	sub	T, [SP]			; n2 - n1
	jz	.eq			; n2 = n1
	xor	TW, TW
	not	T
.eq:	add	SP, 8
	ret

;; <xp
from_xp:
	ret

;; = ( n1 ,n2 -- ,f)
equal:	sub	T, [SP]			; n2 - n1
	jz	.eq			; n2 = n1
	xor	TW, TW
	not	T
.eq:	not	T
	add	SP, 8
	ret

;; > ( n1 ,n2 -- ,f)
greater:
	sub	T, [SP]			; n2 - n1
	sar	T, 63			; n2 < n1
	add	SP, 8
	ret				; n1 > n2

;; >in ( -- ,a-addr)
;; Holds address of next unparsed byte in current block
input_pointer:
	sub	SP, 8
	mov	[SP], T
	UADDR	T, IN
	ret

;; >xp
to_xp:	ret

;; @ ( ,addr -- ,x) Read 8-byte value from address
fetch:	mov	T, [T]
	ret

;; abs ( ,n -- ,n') Absolute value
_abs:	test	T, T
	jns	.ns
	neg	T
.ns:	ret

;; allot ( ,n)
;; Allocate bytes from dictionary, may be negative
allot:	POP1	rax
	add	[U + DP], rax
	ret

;; and ( n1 ,n2 -- ,n1&n2) Bitwise and
_and:	and	T, [SP]
	add	SP, 8
	ret

;; ashift ( n ,u -- ,n>>u)
;; Shift N right U%64 positions, replicating the sign-bit
ashift:
	mov	cl, TC
	mov	T, [SP]
	add	SP, 8
	sar	T, cl
	ret

;; base ( -- ,a-addr) Holds current number base
base:	sub	SP, 8
	mov	[SP], T
	UADDR	T, BASE
	ret

;; blk ( -- ,a-addr) Holds number of current input block
blk:	sub	SP, 8
	mov	[SP], T
	UADDR	T, BLK
	ret

;; c! ( c ,addr) Write 1-byte value to address
c_store:
	mov	rax, [SP]
	mov	[T], al
	DROP2
	ret

;; c@ ( ,addr -- ,c) Read 1-byte value from address, zero-extend
c_fetch:
	movzx	TW, byte [T]
	ret

;; decimal ( --) Read and display numbers in base-10
decimal:
	mov	qword [U + BASE], 10
	ret

;; defer ( --) Compile stub code to call a deferred word
defer:	mov	rax, strict qword 0
	jmp	rax
	ret

;; defer! ( xt1 ,xt2) Set XT2 to execute XT1
defer_store:
	mov	rax, [SP]
	mov	[T + 2], rax
	DROP2
	ret

;; defer@ ( ,xt1 -- ,xt2) XT1 is set to execute XT2
defer_fetch:
	mov	T, [T + 2]
	ret

;; dp ( -- ,a-addr) Holds address of next free byte in dictionary
dp:	sub	SP, 8
	mov	[SP], T
	UADDR	T, DP
	ret

;; drop ( x1 ,x2 -- ,x1) Remove top-of-stack
drop:	add	SP, 8
	test	T, T			; Supports "drop if"
	mov	T, [SP - 8]
drop_len	equ	($ - drop)
	ret

;; dup ( ,x -- x ,x) Duplicate top-of-stack
dup:	sub	SP, 8
	mov	[SP], T
	ret

;; execute ( ,xt) Run code at given address
execute:
	POP1	rax
	call	rax
	ret

;; forth ( --) Add new words to "forth" wordlist
forth:	UADDR	rax, FORTH
	USTORE	CURRENT, rax
	ret

;; here ( -- ,c-addr) Address of next free byte in dictionary
here:	PUSH1	[U + DP]
	ret

;; hex ( --) Read and display numbers in base-16
hex:	mov	qword [U + BASE], 16
	ret

;; invert ( ,n -- ,~n) Ones-complement negation
invert:	not	T
	ret

;; lshift ( n ,u -- ,n<<u)
lshift:	mov	cl, TC
	mov	T, [SP]
	add	SP, 8
	shl	T, cl
	ret

;; macro ( --) Add new words to "macro" wordlist
macro:	UADDR	rax, MACRO
	USTORE	CURRENT, rax
	ret

;; max ( n1 ,n2 -- ,n-max)
max:	cmp	T, [SP]
	jge	.ge
	mov	T, [SP]
.ge:	add	SP, 8
	ret

;; min ( n1 ,n2 -- ,n-min)
min:	cmp	T, [SP]
	jle	.le
	mov	T, [SP]
.le:	add	SP, 8
	ret

;; negate ( ,n -- ,-n) Twos-complement negation
negate:
	neg	T
	ret

;; nip ( x1 ,x2 -- ,x2) Remove second-on-stack
nip:	add	SP, 8
	ret

;; nop ( --)
_nop:	nop
	ret

;; or ( n1 ,n2 -- ,n1|n2) Bitwise inclusive or
_or:	or	T, [SP]
	add	SP, 8
	ret

;; over ( x1 ,x2 -- x1 x2 ,x1) Duplicate second-on-stack
over:	sub	SP, 8
	mov	[SP], T
	mov	T, [SP + 8]
	ret

;; pop ( S: -- ,x / R: x --) Take top item off return stack
pop:	sub	SP, 8
	mov	[SP], T
	pop	T
	ret

;; push ( S: ,x -- / R: -- x) Add new item on return stack
push:	push	T
	mov	T, [SP]
	add	SP, 8
	ret

;; rshift ( n ,u -- ,n>>>u)
;; Replaces the sign-bit with zero.  See also ashift.
rshift:
	mov	cl, TC
	mov	T, [SP]
	add	SP, 8
	shr	T, cl
	ret

;; source-id ( -- ,n) Describes source for text interpreter
source_id:
	PUSH1	[U + SOURCE_ID]
	ret

;; source-id! ( ,n)
source_id_store:
	POP1	[U + SOURCE_ID]
	ret

;; span ( -- ,a-addr) Holds number of bytes read by expect
span:	sub	SP, 8
	mov	[SP], T
	UADDR	T, SPAN
	ret

;; swap ( x1 ,x2 -- x2 ,x2)
swap:	xchg	T, [SP]
	ret

;; swap- ( x1 ,x2 -- ,x2-x1)
swap_minus:
	sub	T, [SP]
	add	SP, 8
	ret

;; tib ( -- ,a-addr) Address of terminal input buffer
tib:	TIB	rcx
	PUSH1	rcx
	ret

;; tuck ( x1 ,x2 -- x2 x1 ,x2)
tuck:	mov	rax, [SP]
	mov	[SP], T
	sub	SP, 8
	mov	[SP], rax
	ret

;; u+ ( n1 n2 ,n3 -- n1+n3 ,n2) Add to second-on-stack
under_plus:
	add	[SP + 8], T
	DROP1
	ret

;; up ( -- ,a-addr) Holds address of next available offset in user area
up:	sub	SP, 8
	mov	[SP], T
	UADDR	T, UP
	ret

;; use ( -- ,a-addr) Holds address of current source file
use:	sub	SP, 8
	mov	[SP], T
	UADDR	T, USE
	ret

;; w! ( w ,addr) Write 4-byte value to ADDR
w_store:
	mov	eax, [SP]
	mov	[T], eax
	DROP2
	ret

;; w@ ( ,addr -- ,w) Read 4-byte value from ADDR, zero-extend
w_fetch:
	mov	TW, [T]
	ret

;; xchg ( x1 ,addr -- ,x2)
xchg:	POP1	rax
	xchg	T, [rax]
	ret

;; xor ( n1 ,n2 -- ,n1^n2) Bitwise exclusive or
_xor:	xor	T, [SP]
	NIP
	ret

;; Words defined before this point may be inlined
inline_before:


;; Builtin words

;; ' ( "<spaces>name" -- xt) Compilation address of name
	section	.text
tick:	call	_tword
	jmp	findf

;;  /mod (    n1 ,n2 -- n-rem ,n-quot)
;; */mod ( n1 n2 ,n3 -- n-rem ,n-quot)
;;
;; Hardware division truncates, which is mathematically
;; incorrect.  This routine corrects the results such that:
;;   D  = d * q + r
;;   0 <= r < |d|
;;
;; Even if you don't care about the mathematics, this is
;; necessary to ensure consistency with other primitive
;; operations.  When d is a power of 2 (2^n):
;;   q = D asr n
;;   r = D and (2^n-1)
	section	.text
div_mod:
	mov	rax, 1
	mov	[SP - 8], rax
	sub	SP, 8
star_div_mod:
	mov	rax, [SP + 8]
	imul	qword [SP]
	add	SP, 8
	idiv	T
	test	rdx, rdx		; Is remainder negative?
	jns	.2
	test	T, T			; Is divisor positive?
	js	.1
	add	rdx, T			; r < 0, d > 0
	dec	rax
	jmp	.2
.1:	sub	rdx, T			; r < 0, d < 0
	inc	rax
.2:	mov	[SP], rdx
	mov	T, rax
	ret

;; , ( ,x) Append 8-byte value to dictionary, does not align
	section	.text
comma:	call	here
	call	store
	add	qword [U + DP], 8
	ret

	section	.text
use_block:
	USTORE	BLK, rdi			; Select block
	mov	qword [U + IN], 0		; Beginning of block
	call	decimal				; Reset to decimal
	call	forth				; Reset to "forth" wordlist
	mov	qword [U + COLOR], WHITE	; Reset to white
	mov	qword [U + STATE], WHITE
	ret

;; >= ( n1 ,n2 -- ,f)
	section .text
not_less:
	mov	rax, T			; n2
	mov	T, [SP]			; n1
	sub	T, rax			; n1 -  n2
	sar	T, 63			; n1 <  n2
	not	T			; n1 >= n2
	add	SP, 8
	ret

;; aligned ( ,addr -- ,a-addr) Adjust address to next cell boundary
	section .text
aligned:
	add	T, 7
	and	T, -8
	ret

;; >body ( ,xt -- ,addr) Map code field to parameter field
	section .text
to_body:
	cmp	byte [T], 0xE8		; Call near
	je	.near
	add	T, 7			; Far call is 12-bytes
.near:	add	T, 5			; Near call is 5-bytes
	jmp	aligned

;; align ( --) Adjust HERE to next cell boundary
	section .text
_align:	UFETCH	rax, DP
.loop:	mov	rcx, rax
	and	rcx, 7
	jz	.done
	mov	byte [rax], 0x90	; nop
	inc	rax
	jmp	.loop
.done:	USTORE	DP, rax
	ret

;; argc ( -- ,u) Number of command-line arguments
	section .text
argc:	PUSH1	[rel ARGC]
	ret

;; argv ( -- ,a-addr) Array of command-line arguments
	section .text
argv:	PUSH1	[rel ARGV]
	ret

;; block ( ,u -- ,a-addr) Address of requested block
	section .text
block:	shl	T, 9			; 512-byte blocks
	add	T, qword [U + USE]
	ret

;; bye ( --) Exit forth
	section .text
bye:	mov	rax, [rel ATEXIT]	; Call any registered finalizer
	test	rax, rax
	jz	.1
	call	rax
.1:	xor	edi, edi		; error_code
	mov	eax, SYS_exit
	syscall				; Does not return

;; c, ( ,c) Append one-byte to dictionary
	section .text
c_comma:
	call	here
	call	c_store
	add	qword [U + DP], 1
	ret

;; compare ( c-addr1 u1 c-addr2 ,u2 -- ,n)
	section .text
compare:
	add	SP, 24
	mov	rcx, [SP - 16]		; u1
	cmp	rcx, T			; min
	jle	.1
	mov	rcx, T
.1:	mov	rsi, [SP - 8]		; c-addr1
	mov	rdi, [SP - 24]		; c-addr2
	repe	cmpsb
	jl	.lt			; -1
	jg	.gt			;  1
	;; Bytes equal?  Compare lengths.
	mov	rcx, [SP - 16]		; u1
	cmp	rcx, T			; u1 - u2
	jl	.lt
	jg	.gt
	xor	TW, TW			; =
	ret
.lt:	mov	T, -1			; <
	ret
.gt:	mov	T, 1			; >
	ret

;; count ( ,c-addr -- c-addr' ,u)
;; Translate counted string to string and count
	section .text
count:	movzx	eax, byte [T]
	inc	T
	cmp	eax, 128
	jb	.2
	mov	ecx, eax		; Number of length bytes
	and	ecx, 127
	xor	eax, eax		; Init to zero
	test	ecx, ecx		; Guard against zero length
	jz	.2
.1:	movzx	edx, byte [T]
	inc	T
	shl	rax, 8
	or	rax, rdx
	loop	.1
.2:	PUSH1	rax
	ret

;; char ( "<spaces>name" -- c)
	section .text
char:	call	_tword
	call	count
	call	drop
	jmp	c_fetch

;; environ ( -- ,a-addr) Array of environment variables
	section .text
environ:
	PUSH1	[rel ENVIRON]
	ret

;; erase ( c-addr ,u   ) Populate bytes with 0
;; fill  ( c-addr  u ,c) Populate bytes with C
	section .text
erase:	PUSH1	0			; c
fill:	POP1	rax			; c
	POP2	rcx, rdi		; u, c-addr
	cld
	rep	stosb
	ret


	;; LSB Huffman codes for ASCII 32 to 126.
	section .rodata
	align 4
_huffman:
	dw	           0b00,  2	; SPACE
	dw	    0b000011111,  9 	; !
	dw	     0b00110111,  8 	; "
	dw	     0b10110111,  8 	; #
	dw	     0b01110111,  8 	; $
	dw	  0b00111111111, 11 	; %
	dw	0b0111111111111, 13 	; &
	dw	     0b11110111,  8 	; '
	dw	      0b0001011,  7 	; (
	dw	      0b1001011,  7 	; )
	dw	    0b100011111,  9 	; *
	dw	     0b00001111,  8 	; +
	dw	      0b0101011,  7 	; ,
	dw	        0b00110,  5 	; -
	dw	      0b1101011,  7 	; .
	dw	    0b010011111,  9 	; /
	dw	       0b011101,  6 	; 0
	dw	      0b0011011,  7 	; 1
	dw	      0b1011011,  7 	; 2
	dw	     0b10001111,  8 	; 3
	dw	    0b110011111,  9 	; 4
	dw	    0b001011111,  9 	; 5
	dw	    0b101011111,  9 	; 6
	dw	   0b0101111111, 10 	; 7
	dw	    0b011011111,  9 	; 8
	dw	    0b111011111,  9 	; 9
	dw	      0b0111011,  7 	; :
	dw	     0b01001111,  8 	; ;
	dw	   0b1101111111, 10 	; <
	dw	     0b11001111,  8 	; =
	dw	      0b1111011,  7 	; >
	dw	    0b000111111,  9 	; ?
	dw	     0b00101111,  8 	; @
	dw	        0b10110,  5 	; A -- encodes as lowercase
	dw	      0b1000111,  7 	; B
	dw	        0b01110,  5 	; C
	dw	        0b11110,  5 	; D
	dw	         0b0010,  4 	; E
	dw	        0b00001,  5 	; F
	dw	       0b111101,  6 	; G
	dw	       0b000011,  6 	; H
	dw	        0b10001,  5 	; I
	dw	   0b0011111111, 10 	; J
	dw	     0b11101111,  8 	; K
	dw	        0b01001,  5 	; L
	dw	       0b100011,  6 	; M
	dw	        0b11001,  5 	; N
	dw	        0b00101,  5 	; O
	dw	       0b010011,  6 	; P
	dw	   0b1011111111, 10 	; Q
	dw	        0b10101,  5 	; R
	dw	        0b01101,  5 	; S
	dw	         0b1010,  4 	; T
	dw	       0b110011,  6 	; U
	dw	      0b0100111,  7 	; V
	dw	      0b1100111,  7 	; W
	dw	      0b0010111,  7 	; X
	dw	      0b1010111,  7 	; Y
	dw	    0b010111111,  9 	; Z
	dw	     0b10101111,  8 	; [
	dw	      0b0000111,  7	; "\\"
	dw	     0b01101111,  8 	; ]
	dw	 0b011111111111, 12 	; ^
	dw	    0b100111111,  9 	; _
	dw	0b1111111111111, 13 	; `
	dw	        0b10110,  5 	; a
	dw	      0b1000111,  7 	; b
	dw	        0b01110,  5 	; c
	dw	        0b11110,  5 	; d
	dw	         0b0010,  4 	; e
	dw	        0b00001,  5 	; f
	dw	       0b111101,  6 	; g
	dw	       0b000011,  6 	; h
	dw	        0b10001,  5 	; i
	dw	   0b0011111111, 10 	; j
	dw	     0b11101111,  8 	; k
	dw	        0b01001,  5 	; l
	dw	       0b100011,  6 	; m
	dw	        0b11001,  5 	; n
	dw	        0b00101,  5 	; o
	dw	       0b010011,  6 	; p
	dw	   0b1011111111, 10 	; q
	dw	        0b10101,  5 	; r
	dw	        0b01101,  5 	; s
	dw	         0b1010,  4 	; t
	dw	       0b110011,  6 	; u
	dw	      0b0100111,  7 	; v
	dw	      0b1100111,  7 	; w
	dw	      0b0010111,  7 	; x
	dw	      0b1010111,  7 	; y
	dw	    0b010111111,  9 	; z
	dw	    0b110111111,  9 	; {
	dw	  0b10111111111, 11 	; |
	dw	    0b001111111,  9 	; }
	dw	  0b01111111111, 11 	; ~
	dw	              0,  0

;; huffman ( -- ,w-addr) Table of LSB Huffman codes
	section .text
huffman:
	sub	SP, 8
	mov	[SP], T
	lea	T, [rel _huffman]
	ret

;; decode-char ( bits ,len -- char ,consumed)
;; Decode single character
	section .text
decode_char:
	POP1	rdx			; len
	lea	rsi, [rel _huffman]	; Start of table
.1:	movzx	ecx, byte [rsi + 2]	; Length of entry
	test	ecx, ecx		; Valid?
	jz	.4
	cmp	ecx, edx		; Sufficient bits?
	ja	.2
	mov	eax, 1			; Mask
	shl	eax, cl
	dec	eax
	and	rax, T
	cmp	ax, [rsi]		; Failed?
	je	.3
.2:	add	rsi, 4
	jmp	.1
.3:	mov	T, rsi
	lea	rsi, [rel _huffman]
	sub	T, rsi
	shr	T, 2
	add	T, 32			; char
	PUSH1	rcx			; consumed
	ret
.4:	mov	T, -1			; char
	xor	ecx, ecx
	PUSH1	rcx			; consumed
	ret

;; move-bits ( x-hi x-lo ,n -- x-hi' x-lo')
;; Shift low-order bits from x1 into x2
	section .text
move_bits:
	POP2	rcx, rax		; n, x-lo
	mov	rdx, T			; x-hi
	shr	T, cl			; x-hi'
	shr	rax, cl
	mov	esi, ecx
	mov	ecx, 64
	sub	ecx, esi
	shl	rdx, cl
	or	rax, rdx
	PUSH1	rax			; x-lo'
	ret

;; decode ( x-hi ,x-lo -- ,c-addr) Decode to counted string
	section .text
decode:	push	r10		; WORD buffer
	push	r11		; x-hi (high-order bits of encoding)
	push	r12		; x-lo (low-order bits of encoding)
	push	r13		; Encoding length in bits
	mov	r13, T		; Bytes used
	and	r13d, 0x0f
	dec	r13d
	shl	r13d, 3		; Bits available
	mov	eax, 8		; Drop length byte
	PUSH1	rax		; n
	call	move_bits
	POP2	r12, r11	; x-lo, h-hi
	WORD	r10		; Destination
	inc	r10		; Reserve length byte
.1:	PUSH2	r12, r13	; bits, len
	call	decode_char
	POP2	rcx, rax	; consumed, char
	test	ecx, ecx	; Success?
	jz	.2
	cmp	eax, 32		; Space character is terminator
	jle	.2
	mov	[r10], al	; Save decoded char
	inc	r10
	PUSH2	r11, r12	; x-hi, x-lo
	PUSH1	rcx		; n
	call	move_bits
	POP2	r12, r11	; x-lo, x-hi
	jmp	.1
.2:	mov	rax, r10
	WORD	r10
	sub	rax, r10
	dec	eax
	mov	[r10], al	; Write length
	PUSH1	r10		; c-addr
	pop	r13
	pop	r12
	pop	r11
	pop	r10
	ret

;; decode-number ( x1 ,x2 -- n ,u-junk)
;; Read number from Huffman-encoded word
	section .text
decode_number:
	call	decode
	call	count
	call	signed
	NIP				; Discard c-addr
	ret

;; encode ( c-addr ,u-count -- x-hi ,x-lo)
;; Represent string as two LSB Huffman-encoded words
	section .text
encode:	push	r10			; Characters remaining
	push	r11			; Current character
	push	r12			; Bit pattern of entry
	push	r13			; Bit length of entry
	xor	edx, edx		; x-hi (high-order bits of encoding)
	xor	eax, eax		; x-lo (low-order bits of encoding)
	mov	ecx, 8			; Count bits used
	POP2	r10, rsi		; u-count, c-addr
	lea	rdi, [rel _huffman]	; Code table
.1:	cmp	ecx, 104		; Space available?
	jae	.3
	test	r10d, r10d		; Input available?
	jz	.3
	movzx	r11d, byte [rsi]	; Next byte
	inc	rsi
	dec	r10d
	sub	r11d, 32		; Valid character?
	jbe	.3
	cmp	r11d, 95
	jae	.3
	movzx	r12d, word [rdi + 4*r11]	; Bits
	movzx	r13d, byte [rdi + 4*r11 + 2]	; Length
	cmp	ecx, 64			; First word?
	jae	.2
	shl	r12, cl
	or	rax, r12
	add	ecx, r13d
	cmp	ecx, 64			; Crossing word boundary?
	jbe	.1
	movzx	r12d, word [rdi + 4*r11]	; Reload bits
	sub	ecx, 64			; Num unencoded bits
	xchg	ecx, r13d
	sub	ecx, r13d		; Num encoded bits
	shr	r12d, cl		; Discard encoded bits
	mov	ecx, 64
.2:	shl	r12, cl			; Second word
	or	rdx, r12
	add	ecx, r13d
	jmp	.1
.3:	add	ecx, 7			; Bytes used
	shr	ecx, 3			; (Includes length byte itself)
	mov	al, cl			; (Low nibble)
	pop	r13
	pop	r12
	pop	r11
	pop	r10
	PUSH2	rdx, rax		; x-hi, x-lo
	ret


;; Save user variable on return stack
%macro	SAVE	1
	UFETCH	rax, %1
	push	rax
%endmacro

;; Restore user variable from return stack
%macro	RESTORE	1
	pop	rax
	USTORE	%1, rax
%endmacro

;; load ( ,u) Read from indicated source block
	section .text
load:	SAVE	BASE
	SAVE	BLK
	SAVE	COLOR
	SAVE	IN
	SAVE	STATE
	POP1	rdi			; u
	call	use_block
	UFETCH	rax, BLK		; Binary or text input?
	test	rax, rax
	jz	.1
	call	binterpret
	jmp	.2
.1:	call	tinterpret
.2:	RESTORE	STATE
	RESTORE	IN
	RESTORE	COLOR
	RESTORE	BLK
	RESTORE	BASE
	ret


;; Restore user variable from dictionary
%macro	RESTOR	1
	mov	rax, [rcx]
	add	rcx, 8
	USTORE	%1, rax
%endmacro

;; ( --) Run-time code for marker
	section .text
_marker:
	pop	rcx			; Address of parameter block
	add	rcx, 7
	and	rcx, -8
	RESTOR	CURRENT			; Restore variables from parameter block
	RESTOR	DP
	RESTOR	FORTH
	RESTOR	MACRO
	RESTOR	UP
	ret

;; Save user variable in dictionary
%macro	SAV	1
	PUSH1	[U + %1]
	call	comma
%endmacro

;; marker ( -- xt) Define a restore point
	section .text
marker:	lea	rax, [rel _marker]	; Compile run-time code
	PUSH1	rax
	call	compile_comma
	call	_align			; Align parameter block
	SAV	CURRENT
	SAV	DP
	SAV	FORTH
	SAV	MACRO
	SAV	UP
	ret

;; move ( src dst ,count) Copy COUNT bytes from SRC to DST
	section .text
move:	POP1	rcx			; count
	POP2	rdi, rsi		; dst, src
	cmp	rdi, rsi
	jb	.lth
	ja	.htl
	ret				; Nowhere to go
.lth:	cld				; Low-to-high
	rep	movsb
	ret
.htl:	add	rdi, rcx		; High-to-low
	add	rsi, rcx
	dec	rdi
	dec	rsi
	std
	rep	movsb
	cld
	ret

;; pad ( -- ,c-addr) Reserve dictionary space to read a word
	section .text
pad:	PUSH1	[U + DP]
	add	T, 160
	ret

;; source ( -- c-addr ,u) Current interpreter source
	section .text
source:	UFETCH	rax, BLK
	test	rax, rax		; Text input?
	jnz	.2
	UFETCH	rax, SOURCE_ID		; From the terminal?
	test	rax, rax
	jnz	.1
	TIB	rax			; Terminal input buffer
	UFETCH	rcx, NUM_TIB
	jmp	.3
.1:	UFETCH	rax, USE		; File buffer
	UFETCH	rcx, SPAN
	jmp	.3
.2:	shl	rax, 9			; Block input
	add	rax, [U + USE]
	mov	rcx, 512
.3:	PUSH2	rax, rcx		; c-addr, u
	ret

;; within ( n1 n2 ,n3 -- ,f) n2 <= n1 < n3
	section .text
within:	mov	rdx, T			; n3
	mov	rcx, [SP + 0]		; n2
	mov	rax, [SP + 8]		; n1
	xor	TW, TW			; false
	add	SP, 16
	sub	rax, rcx		; n1 - n2
	jl	.1
	sub	rdx, rcx		; n3 - n2
	sub	rdx, rax		; n3 - n1
	jle	.1
	not	T			; true
.1:	ret


;; Primitive I/O

	section .text
_syscall:
	push	r8			; Y, Z, A, and B are all caller-save
	push	r9
	push	r10
	push	r11
	push	rbp			; Save stack frame
	mov	rbp, rsp
	syscall
	mov	rsp, rbp		; Restore stack frame
	pop	rbp
	PUSH1	rax			; Copy result to parameter stack
	pop	r11			; Restore caller-save registers
	pop	r10
	pop	r9
	pop	r8
	ret


	section	.rodata
_emit_name	db	4, "emit"
_expect_name	db	6, "expect"
_key_name	db	3, "key"
_query_name	db	5, "query"
_type_name	db	4, "type"

	section	.bss
	alignb	8
_emit	resq	1
_expect	resq	1
_key	resq	1
_query	resq	1
_type	resq	1

;; ( ,c) Output a single character
stdout		equ	1
	section .text
builtin_emit:
	sub	RP, 8			; Allocate buffer
	mov	[RP], TC
	DROP1
	mov	edx, 1			; count
	mov	rsi, RP			; buf
	mov	edi, stdout		; fd
	mov	eax, SYS_write
	call	_syscall
	DROP1
	add	RP, 8			; Deallocate buffer
	ret

;; emit ( ,c) Output a single character
	section .text
emit:	mov	rax, [rel _emit]
	test	rax, rax
	jnz	.nz
	lea	rax, [rel _emit_name]
	PUSH1	rax
	call	find
	POP1	rax
	mov	[rel _emit], rax
.nz:	jmp	rax

;; ( -- ,c) Read one byte of input
stdin		equ	0
	section .text
builtin_key:
	sub	RP, 8			; Allocate buffer
	mov	edx, 1			; count
	mov	rsi, RP			; buf
	mov	edi, stdin		; fd
	mov	eax, SYS_read
	call	_syscall
	POP1	rax
	sub	SP, 8
	mov	[SP], T
	movzx	TW, byte [RP]
	add	RP, 8			; Deallocate buffer
	test	eax, eax		; EOF or error?
	jg	.1
	mov	T, -1
.1:	ret

;; key ( -- ,c) Read one byte of input
	section .text
key:	mov	rax, [rel _key]
	test	rax, rax
	jnz	.nz
	lea	rax, [rel _key_name]
	PUSH1	rax
	call	find
	POP1	rax
	mov	[rel _key], rax
.nz:	jmp	rax

;; ( c-addr ,+n) Receive bytes into memory
	section .text
builtin_expect:
	push	r12
	push	r13
	POP2	r13, r12		; n, c-addr
	push	r12			; Save starting address
	call	key			; Exit program on EOF
	POP1	rax
	test	eax, eax
	js	bye
	dec	r13			; Reserve space for terminator
	jle	.2
.1:	cmp	al, 10
	je	.2
	mov	[r12], al
	inc	r12
	dec	r13
	jz	.2
	call	key
	POP1	rax
	test	eax, eax
	jns	.1
.2:	mov	byte [r12], 0		; Append terminator
	pop	rax			; Restore starting address
	sub	r12, rax		; Number of bytes read
	USTORE	SPAN, r12
	pop	r13
	pop	r12
	ret

;; expect ( c-addr ,+n) Receive bytes into memory
	section .text
expect:	mov	rax, [rel _expect]
	test	rax, rax
	jnz	.nz
	lea	rax, [rel _expect_name]
	PUSH1	rax
	call	find
	POP1	rax
	mov	[rel _expect], rax
.nz:	jmp	rax

;; ( --) Read one-line of input into terminal buffer
	section .text
builtin_query:
	TIB	rax
	PUSH2	rax, 128		; c-addr, n
	call	expect
	UFETCH	rax, SPAN
	USTORE	NUM_TIB, rax
	mov	qword [U + IN], 0	; Reset used count
	ret

;; query ( --) Read one-line of input into terminal buffer
	section .text
query:	mov	rax, [rel _query]
	test	rax, rax
	jnz	.nz
	lea	rax, [rel _query_name]
	PUSH1	rax
	call	find
	POP1	rax
	mov	[rel _query], rax
.nz:	jmp	rax

;; ( c-addr ,u) Output counted string
	section .text
builtin_type:
	POP2	rdx, rsi		; count, buf
	mov	edi, stdout		; fd
	mov	eax, SYS_write
	call	_syscall
	DROP1
	ret

;; type ( c-addr ,u) Output counted string
	section .text
type:	mov	rax, [rel _type]
	test	rax, rax
	jnz	.nz
	lea	rax, [rel _type_name]
	PUSH1	rax
	call	find
	POP1	rax
	mov	[rel _type], rax
.nz:	jmp	rax


;; Words

;; We represent each word with 12-bytes.  Byte 0 is the length
;; of the word, which can range from 1 to 127, and bytes 1-11
;; contain up to the first 11-characters of the word, padded out
;; with NULs.

;; word ( -- x-hi ,x-lo) Read one word from input, updating COLOR
	section .text
bword:	UFETCH	rsi, BLK		; Find source block
	shl	rsi, 9
	add	rsi, [U + USE]
	push	rsi			; Preserve block address
	add	rsi, [U + IN]		; Input pointer
	xor	edx, edx		; Clear return value
	add	rsi, 3			; Ensure alignment
	and	rsi, -4
.1:	mov	eax, [rsi]		; Read 32-bit word
	add	rsi, 4
	test	eax, eax		; Done if NULL
	jz	.8
	mov	ecx, eax		; Done if FF
	and	ecx, 0xfff0
	cmp	ecx, 0x0c40		; 40 blue 0c formfeed
	jne	.2
	xor	eax, eax		; Clear return value
	jmp	.8
.2:	mov	ecx, eax		; Extract color
	shr	ecx, 4
	and	ecx, 0x0f
	USTORE	COLOR, rcx		; Save color
	and	ecx, 0x07
	cmp	ecx, BLUE		; Skip blue words
	je	.1
	cmp	ecx, WHITE		; Skip white words
	jne	.4
	sub	rsi, 3			; Length excludes tag
	mov	ecx, eax		; 4-bit length?
	and	ecx, 0x0f
	jnz	.3
	inc	rsi			; Length excludes itself
	mov	ecx, eax		; 8-bit length?
	shr	ecx, 8
	and	ecx, 0xff
	cmp	ecx, 128
	jb	.3
	add	rsi, 2
	mov	ecx, eax		; 16-bit big-endian length
	bswap	ecx
	and	ecx, 0xffff
.3:	add	rsi, rcx
	add	rsi, 3
	and	rsi, -4
	jmp	.1

	;; Read numbers
.4:	cmp	ecx, RED		; Red is never a number
	je	.7
	test	eax, 0x80		; Binary number?
	jz	.7
	mov	rdx, -1			; Flags result as number
	mov	ecx, eax
	and	ecx, 0x0f
	cmp	ecx, 2			; 16-bit number?
	jne	.5
	shr	eax, 16
	jmp	.8
.5:	cmp	ecx, 4			; 32-bit number?
	jne	.6
	mov	eax, [rsi]
	add	rsi, 4
	jmp	.8
.6:	mov	rax, [rsi]		; 64-bit number
	add	rsi, 8
	jmp	.8

	;; Read encoded word
.7:	and	eax, 0xffffff0f		; Strip color from return
	mov	ecx, eax
	and	ecx, 0x0f
	cmp	ecx, 4			; More than 4 bytes?
	jle	.8
	mov	edx, [rsi]		; Read next 4 bytes
	add	rsi, 4
	shl	rdx, 32
	or	rax, rdx
	xor	edx, edx		; Clear high-order bits
	cmp	ecx, 8			; More than 8 bytes?
	jle	.8
	mov	edx, [rsi]
	add	rsi, 4

.8:	mov	rcx, rsi		; Update input pointer
	pop	rsi
	sub	rcx, rsi
	USTORE	IN, rcx
	PUSH2	rdx, rax		; x-hi, x-lo
	ret


;; word ( -- ,a-addr) Read one word from input, updating COLOR
	section .text
_tword:	call	source
	DROP1
	POP1	rsi
	push	rsi			; Preserve original block address
	add	rsi, [U + IN]		; Source
	WORD	rdi			; Destination
	xor	eax, eax		; Clear buffer
	mov	[rdi + 0], rax
	mov	[rdi + 8], rax
	inc	rdi			; First byte will hold length
	UFETCH	ecx, COLOR
	and	ecx, 0x07
.1:	movzx	eax, byte [rsi]		; Skip leading whitespace
	inc	rsi
	cmp	al, 0x20
	ja	.2
	test	al, al			; NUL marks end of a block
	je	.6
	jmp	.1
.2:	cmp	al, 0x7F		; Accept only 7-bit US-ASCII
	jb	.3
	je	.1
	cmp	al, 0x90		; Look for color codes
	jae	.1
	and	al, 0x0F		; Save 4-bits of color
	USTORE	COLOR, rax
	mov	cl, al
	and	cl, 0x07
	jmp	.1
.3:	cmp	cl, WHITE		; Skip comments
	je	.1
.4:	mov	[rdi], al		; Accumulate characters
	inc	rdi
	movzx	eax, byte [rsi]		; Lookahead at next
	cmp	al, 0x20
	jbe	.5
	cmp	al, 0x7F
	jae	.5
	inc	rsi			; Consume
	jmp	.4
.5:	cmp	byte [rsi], 0		; Consume terminating space
	je	.6
	inc	rsi
.6:	mov	rax, rsi		; Update used count
	pop	rsi
	sub	rax, rsi
	USTORE	IN, rax
	mov	rax, rdi		; Find length of word
	WORD	rdi
	sub	rax, rdi
	dec	al
	and	al, 127			; Encode length byte
	mov	[rdi], al
	PUSH1	rdi			; Return counted string
	ret


;; in  RDI character
;;     RSI base
;; out RAX value, 0 <= value < base, or -1
	section .text
digit:	mov	eax, edi
	sub	eax, 48			; '0' .. '9'
	jb	.2
	cmp	eax, 10
	jb	.1
	sub	eax, 7			; 'A' .. 'Z'
	cmp	eax, 10
	jb	.2
	cmp	eax, 36
	jb	.1
	sub	eax, 32			; 'a' .. 'z'
	cmp	eax, 10
	jb	.2
.1:	cmp	eax, esi		; In range?
	jae	.2
	ret
.2:	mov	rax, -1			; Not a valid digit
	ret

;; ( u1 c-addr1 ,u-count1 -- u2 c-addr2 ,u-junk)
	section .text
natural:
	push	r11
	push	r12
	push	r13
	mov	r13, [SP + 0]		; c-addr
	mov	r12, [SP + 8]		; u
	movzx	r11d, byte [U + BASE]
.1:	test	T, T			; u-count
	jz	.4
	cmp	byte [r13], '#'		; Decimal literal?
	jne	.2
	mov	r11d, 10
	inc	r13
	dec	T
	jz	.4
	jmp	.3
.2:	cmp	byte [r13], '$'		; Hex literal?
	jne	.3
	mov	r11d, 16
	inc	r13
	dec	T
	jz	.4
.3:	mov	rsi, r11
	movzx	edi, byte [r13]
	call	digit
	test	eax, eax
	js	.4
	imul	r12, r11
	add	r12, rax
	inc	r13			; Consume digit
	dec	T
	jmp	.3
.4:	mov	[SP + 8], r12		; u'
	mov	[SP + 0], r13		; c-addr'
	pop	r13
	pop	r12
	pop	r11
	ret

;; ( c-addr ,u-count -- n c-addr' ,u-junk)
	section .text
signed:	push	r13
	mov	r13, 1
	mov	rcx, [SP]		; c-addr
	xor	eax, eax
	mov	[SP - 0], rax		; u
	mov	[SP - 8], rcx		; c-addr
	sub	SP, 8
	test	T, T			; Got input?
	jz	.1
	cmp	byte [rcx], '-'		; Look for sign
	jne	.1
	mov	r13, -1
	inc	rcx			; Consume sign
	mov	[SP], rcx
	dec	T
.1:	call	natural
	mov	rax, [SP + 8]		; u
	imul	rax, r13
	mov	[SP + 8], rax		; n
	pop	r13
	ret

;; number ( ,c-addr -- n ,u-junk) Read number from counted string
	section .text
number:	call	count
	call	signed
	NIP				; Discard c-addr
	ret


;; Dictionary lookup and navigation

;; A wordlist is stored as a packed linear sequence of 16-byte
;; headers.  The first 12-bytes hold the length-prefixed name of
;; the word, and the remaining 4-bytes contain the signed offset
;; to the word in the dictionary.

;; Words can be hidden by zeroing their length byte in the
;; wordlist.  An actual zero-length word (all NULs) is a link
;; field.  If not NULL, its address points to the next wordlist
;; in the chain.

;; find ( x-hi ,x-lo -- ,addr) Lookup word in current wordlist
	section .text
bfind:	POP2	rdi, rsi		; x-lo, x-hi
	UFETCH	rax, CURRENT		; Load current wordlist
	mov	rax, [rax]
.1:	cmp	rdi, [rax + 0]		; Compare names
	jne	.2
	cmp	esi, [rax + 8]
	je	.4
.2:	mov	ecx, [rax]		; More in this list?
	test	ecx, ecx
	jz	.3
	sub	rax, 16			; Continue with next entry
	jmp	.1
.3:	mov	rax, [rax + 8]		; Move to next list
	test	rax, rax		; Another list?
	jz	.5
	jmp	.1
.4:	movsx	rcx, dword [rax + 12]
	add	rax, rcx
.5:	PUSH1	rax			; addr
	ret

;; find ( ,a-addr -- ,xt) Lookup word in current wordlist
	section .text
find:	call	count
	call	encode
	call	bfind
	ret

;; findf ( x-hi ,x-lo -- ,addr) Lookup word in "forth" wordlist
	section .text
bfindf:	UFETCH	rcx, CURRENT
	push	rcx
	UADDR	rcx, FORTH
	USTORE	CURRENT, rcx
	call	bfind
	pop	rcx
	USTORE	CURRENT, rcx
	ret

;; ( ,a-addr -- ,xt) Lookup word in "forth" wordlist
	section .text
findf:	call	count
	call	encode
	call	bfindf
	ret

;; findm ( x-hi ,x-lo -- ,addr) Lookup word in "macro" wordlist
	section .text
bfindm:	UFETCH	rcx, CURRENT
	push	rcx
	UADDR	rcx, MACRO
	USTORE	CURRENT, rcx
	call	bfind
	pop	rcx
	USTORE	CURRENT, rcx
	ret

;; ( ,a-addr -- ,xt) Lookup word in "macro" wordlist
	section .text
findm:	call	count
	call	encode
	call	bfindm
	ret


;; >name ( ,xt -- ,nt) Move from dictionary entry to wordlist entry
	section .text
to_name:
	mov	rcx, T			; xt
	UFETCH	rax, CURRENT		; Load current wordlist
	mov	T, [rax]
.1:	mov	rdx, T			; Decode relative address
	movsx	rsi, dword [rdx + 12]
	add	rdx, rsi
	cmp	rcx, rdx		; Compare code address
	je	.3
	mov	rax, [T]		; More in this list?
	test	rax, rax
	jz	.2
	sub	T, 16			; Continue with next entry
	jmp	.1
.2:	mov	T, [T + 8]		; Move to next list
	test	T, T
	jnz	.1
.3:	ret


;; Code generation

	;; Template instructions for an absolute call.
	section .rodata
call_tmpl:
	mov	rax, strict qword 0
	call	rax
call_len	equ	($ - call_tmpl)

;; ( ,addr) Emit instructions to call word given by addr
	section .text
_call:	HERE	rdi
	mov	rax, T			; Find relative offset.
	sub	rax, rdi
	sub	rax, 5
	mov	rcx, rax		; Possible to represent in 32-bits?
	sar	rcx, 32
	test	ecx, ecx
	jz	call_relative
	not	ecx
	test	ecx, ecx
	jz	call_relative
	mov	ecx, call_len		; Compile an absolute call.
	lea	rsi, [rel call_tmpl]
	rep	movsb
	USTORE	DP, rdi
	POP1	[rdi - 10]
	ret
call_relative:
	mov	byte [rdi], 0xE8	; Call near, relative, 32-bit offset
	mov	[rdi + 1], eax
	add	rdi, 5
	USTORE	DP, rdi
	DROP1
	ret


;; ( ,addr) Copy instructions for a word inline
	section .text
inline:
	POP1	rdi			; Word is destination for search.
	push	rdi
	mov	al, 0xC3		; RET marks end of definition.
	mov	ecx, 20			; Greater than maximum length of inline.
	repne	scasb
	mov	rcx, rdi		; Search lands one-past end.
	dec	rcx
	pop	rsi			; Word is source for copy.
	sub	rcx, rsi		; Length of source.
	HERE	rdi			; Copy to end of dictionary.
	rep	movsb
	USTORE	DP, rdi
	ret


;; compile, ( ,addr) Call or inline word, as appropriate
	section .text
compile_comma:
	lea	rax, [rel inline_before]
	cmp	T, rax			; Can we inline it?
	jb	inline
	jmp	_call


;; Builtin macros

	section .text
validate_address:			; Is RDI a valid destination?
	push	r13
	mov	r13, rdi
	PUSH1	rdi
	call	to_name
	POP1	rax
	test	rax, rax
	jnz	.1
	mov	rcx, [rel DICTIONARY]
	cmp	r13, rcx
	jl	.1
	add	rcx, DICT_SIZE
	cmp	r13, rcx
	jge	.1
	dec	rax
.1:	pop	r13
	ret

;; ; ( --) Exit a definition
	section .text
exit:	HERE	rdi			; Call near, relative, 32-bit offset.
	cmp	byte [rdi - 5], 0xE8
	jne	.1
	movsx	rax, dword [rdi - 4]
	add	rdi, rax
	call	validate_address
	test	rax, rax
	jz	.1
	HERE	rdi
	mov	byte [rdi - 5], 0xE9	; Change to JMP.
	ret
.1:	HERE	rdi			; Call near, absolute, address in RAX.
	cmp	word [rdi -  2], 0xD0FF
	jne	.2
	cmp	word [rdi - 12], 0xB848
	jne	.2
	mov	rdi, [rdi - 10]
	call	validate_address
	test	rax, rax
	jz	.2
	HERE	rdi
	mov	word [rdi -  2], 0xE0FF	; Change to JMP.
	ret
.2:	HERE	rdi
	mov	byte [rdi], 0xC3	; Add a RET.
	inc	rdi
	USTORE	DP, rdi
	ret

;; +if ( S: ,x -- ,x / C: -- ,addr) Execute body when X >= 0
;; -if ( S: ,x -- ,x / C: -- ,addr) Execute body when X <  0
;; 0if ( S: ,x -- ,x / C: -- ,addr) Execute body when X == 0
;;  if ( S: ,x -- ,x / C: -- ,addr) Execute body when X <> 0
	section .text
plus_if:
	mov	eax, 0x880f		; js
	jmp	nzero_if.1
minus_if:
	mov	eax, 0x890f		; jns
	jmp	nzero_if.1
zero_if:
	mov	eax, 0x850f		; jnz
	jmp	nzero_if.1
nzero_if:
	mov	eax, 0x840f		; jz
.1:	mov	ecx, drop_len		; Look for "drop if"
	lea	rsi, [rel drop]
	HERE	rdi
	sub	rdi, rcx
	repe	cmpsb
	je	.2			; Not found?
	HERE	rdi			; test T, T
	mov	dword [rdi], 0xdb8548
	add	rdi, 3
.2:	mov	[rdi], ax
	add	rdi, 2
	PUSH1	rdi			; Location of jump offset
	mov	dword [rdi], 0		; Placeholder
	add	rdi, 4
	USTORE	DP, rdi
	ret

;; then ( S: -- / C: ,addr --)
	section	.text
then:	HERE	rax
	mov	byte [rax], 0x90	; Pad with NOP
	inc	rax
	USTORE	DP, rax
	sub	rax, T			; Compute relative offset
	sub	rax, 4
	mov	[T], eax		; Populate jump offset
	DROP1
	ret


;; Interpreter

;; (literal) ( -- ,x) Runtime code for literal
	section .rodata
lit_common:
	sub	SP, 8
	mov	[SP], T
	ret
lit_u32:
	;; 5 bytes, zero-extend 32-bit immediate
	mov	TW, 0
	ret
lit_s64:
	;; 10 bytes, 64-bit immediate
	mov	T, 0x8000000000000000
	ret

%macro	INLINE	1
	sub	SP, 8
	mov	[SP], T
	lea	T, [rel %1]
	call	inline
%endmacro

;; literal ( C: ,x -- / S: -- ,x) Compile literal
	section .text
literal:
	INLINE	lit_common
	test	T, T			; Unsigned?
	js	literal_s64
	mov	rax, 0x100000000
	cmp	T, rax			; 32-bit?
	jae	literal_s64
	INLINE	lit_u32
	HERE	rdi
	mov	[rdi - 4], TW
	DROP1
	ret
literal_s64:				; 64-bit
	INLINE	lit_s64
	HERE	rdi
	mov	[rdi - 8], T
	DROP1
	ret


;; Available error messages
	section .rodata
error_msg	db	" ? "
error_len	equ	($ - error_msg)
uo_msg		db	"user area exceeded", 10
uo_len		equ	($ - uo_msg)
su_msg		db	"stack underflow", 10
su_len		equ	($ - su_msg)
so_msg		db	"stack overflow", 10
so_len		equ	($ - so_msg)
ru_msg		db	"return stack underflow", 10
ru_len		equ	($ - ru_msg)
ro_msg		db	"return stack overflow", 10
ro_len		equ	($ - ro_msg)
do_msg		db	"dictionary space exceeded", 10
do_len		equ	($ - do_msg)
fo_msg		db	"forth wordlist exceeded", 10
fo_len		equ	($ - fo_msg)
mo_msg		db	"macro wordlist exceeded", 10
mo_len		equ	($ - mo_msg)
green_err	db	"compiling", 10
green_len	equ	($ - green_err)
yellow_err	db	"executing", 10
yellow_len	equ	($ - yellow_err)
cyan_err	db	"not a macro", 10
cyan_len	equ	($ - cyan_err)
parse_msg	db	"invalid", 10
parse_len	equ	($ - parse_msg)

;; Table of error messages
	section .data
	align	8
message_data:
	dq	error_msg		;  0 Error
	dq	uo_msg			;  1 User area overflow
	dq	su_msg			;  2 Parameter stack underflow
	dq	so_msg			;  3 Parameter stack overflow
	dq	ru_msg			;  4 Return stack underflow
	dq	ro_msg			;  5 Return stack overflow
	dq	do_msg			;  6 Dictionary overflow
	dq	fo_msg			;  7 FORTH wordlist overflow
	dq	mo_msg			;  8 MACRO wordlist overflow
	dq	green_err		;  9 Unable to compile
	dq	yellow_err		; 10 Unable to execute
	dq	cyan_err		; 11 Not a macro
	dq	parse_msg		; 12 Unable to parse

	section .rodata
message_len:
	db	error_len
	db	uo_len
	db	su_len
	db	so_len
	db	ru_len
	db	ro_len
	db	do_len
	db	fo_len
	db	mo_len
	db	green_len
	db	yellow_len
	db	cyan_len
	db	parse_len

;; message ( n)
	section .text
message:
	lea	rax, [rel message_data]
	mov	rax, [rax + 8*T]
	sub	SP, 8
	mov	[SP], rax		; c-addr
	lea	rax, [rel message_len]
	movzx	T, byte [rax + T]	; u-count
	jmp	type


;; error ( n)
	section .text
error:	WORD	rax
	PUSH1	rax
	call	count
	call	type			; Context
	PUSH1	0
	call	message			; Error
	call	message			; Message
	jmp	abort			; From the top...

%macro	ERROR	1
	PUSH1	%1
	jmp	error
%endmacro

	section .text
parse_error:
	DROP1
	ERROR	12


;; Set STATE to the current value of COLOR
%macro	UPDATE_STATE 0
	movzx	eax, byte [U + COLOR]
	USTORE	STATE, rax
%endmacro


;; ( x-hi ,x-lo) Create new dictionary entry
	section .text
bred:	UPDATE_STATE
	POP2	rdi, rsi		; x-lo, x-hi
	UFETCH	rcx, CURRENT		; Find current wordlist
	mov	rax, [rcx]		; Allocate header
	add	rax, 16
	mov	[rcx], rax
	mov	[rax +  0], rdi		; Copy name to header
	mov	[rax +  8], esi
	HERE	rcx			; Copy dictionary address to header
	sub	rcx, rax		; Relativize address
	mov	[rax + 12], ecx
	ret

;; ( ,a-addr) Create new dictionary entry
	section .text
red:	call	count
	call	encode
	jmp	bred


;; ( ,x -- ...) Transition YELLOW->GREEN compiles a literal
	section .text
bgreen_literal:
	movzx	eax, byte [U + STATE]
	and	eax, 0x07
	cmp	eax, YELLOW
	je	literal
	ret

;; ( x-hi ,x-lo)
	section .text
bgreen_number:
	push	r13
	POP2	r13, rax		; x-lo, x-hi (discarded)
	call	bgreen_literal		; Transition?
	UPDATE_STATE
	PUSH1	r13			; Compile number
	pop	r13
	jmp	literal

;; ( x-hi ,x-lo)
	section .text
bgreen:	push	r12
	push	r13
	POP2	r12, r13		; x-lo, x-hi
	call	bgreen_literal		; Transition?
	UPDATE_STATE

	PUSH2	r13, r12		; x-hi, x-lo
	call	bfindm			; Macro?
	POP1	rax			; addr
	test	rax, rax
	jz	.1
	pop	r13
	pop	r12
	jmp	rax			; Execute it

.1:	PUSH2	r13, r12		; x-hi, x-lo
	call	bfindf			; Word?
	test	T, T			; addr
	jz	.2
	pop	r13
	pop	r12
	jmp	compile_comma		; Compile it

.2:	DROP1				; Discard result of bfindf
	PUSH2	r13, r12		; x-hi, x-lo
	pop	r13
	pop	r12
	call	decode_number		; Number?
	POP1	rax			; u-junk
	test	rax, rax
	jz	literal			; Compile it

	;; None of the above?
	DROP1				; Drop bad number
	ERROR	9

;; ( ,a-addr) Compile word
	section .text
green:	push	r13
	POP1	r13

	call	bgreen_literal		; Transition?
	UPDATE_STATE

	PUSH1	r13			; Macro?
	call	findm
	POP1	rax
	test	rax, rax
	jz	.2
	pop	r13
	jmp	rax			; Execute it

.2:	PUSH1	r13
	call	findf			; Word?
	test	T, T
	jz	.3
	pop	r13
	jmp	compile_comma		; Compile it

.3:	DROP1				; Drop result of findf
	PUSH1	r13
	pop	r13
	call	number			; Number?
	POP1	rax			; u-junk
	test	rax, rax
	jz	literal			; Compile it

	;; None of the above?
	DROP1				; Drop bad number
	ERROR	9


;; ( x-hi ,x-lo -- ,x-lo)
	section .text
byellow_number:
	UPDATE_STATE
	NIP
	ret

;; ( x-hi ,x-lo -- ...)
	section .text
byellow:
	push	r12
	push	r13
	POP2	r12, r13		; x-lo, x-hi
	UPDATE_STATE

	PUSH2	r13, r12		; x-hi, x-lo
	call	bfindf			; Word?
	POP1	rax			; addr
	test	rax, rax
	jz	.1
	pop	r13
	pop	r12
	jmp	rax			; Execute it

.1:	PUSH2	r13, r12		; x-hi, x-lo
	pop	r13
	pop	r12
	call	decode_number		; Number?
	POP1	rax			; u-junk
	test	rax, rax
	jnz	.2
	ret				; Now in T

	;; Neither?
.2	DROP1				; Drop bad number.
	ERROR	10

;; ( ,a-addr -- ...)  Execute word
	section .text
yellow:	push	r13
	mov	r13, T
	UPDATE_STATE

	call	findf			; Word?
	POP1	rax			; addr
	test	rax, rax
	jz	.1
	pop	r13
	jmp	rax			; Execute it

.1:	PUSH1	r13
	pop	r13
	call	number			; Number?
	POP1	rax			; u-junk
	test	rax, rax
	jnz	.2
	ret				; Now in T

	;; Neither?
.2:	DROP1				; Drop bad number
	ERROR	10


;; ( x-hi ,x-lo)
	section .text
bcyan_number:
	UPDATE_STATE
	NIP
	call	literal			; Postpone it
	lea	rax, [rel literal]
	PUSH1	rax
	jmp	compile_comma

;; ( x-hi ,x-lo)
	section .text
bcyan:	push	r12
	push	r13
	POP2	r12, r13		; x-lo, x-hi
	UPDATE_STATE

	PUSH2	r13, r12		; x-hi, x-lo
	call	bfindm			; Macro?
	test	T, T			; addr
	jz	.1
	pop	r13
	pop	r12
	jmp	compile_comma		; Compile it

.1:	DROP1				; Discard result of bfindm
	PUSH2	r13, r12		; x-hi, x-lo
	call	bfindf			; Word?
	test	T, T			; addr
	jz	.2
	pop	r13
	pop	r12
	call	literal			; Postpone it
	lea	rax, [rel compile_comma]
	PUSH1	rax
	jmp	compile_comma

.2:	DROP1				; Discard result of bfindf
	PUSH2	r13, r12		; x-hi, x-lo
	pop	r13
	pop	r12
	call	decode_number		; Number?
	POP1	rax			; u-junk
	test	rax, rax
	jnz	.3
	call	literal			; Postpone it
	lea	rax, [rel literal]
	PUSH1	rax
	jmp	compile_comma

	;; None of the above?
.3:	DROP1				; Discard result of number
	ERROR	11

;; ( ,a-addr) Compile postponed word
	section .text
cyan:	push	r13
	mov	r13, T
	UPDATE_STATE

	call	findm			; Macro?
	test	T, T			; addr
	jz	.1
	pop	r13
	jmp	compile_comma		; Compile it

.1:	DROP1				; Discard result of findm
	PUSH1	r13
	call	findf			; Word?
	test	T, T			; addr
	jz	.2
	call	literal			; Postpone it
	lea	rax, [rel compile_comma]
	PUSH1	rax
	pop	r13
	jmp	compile_comma

.2:	DROP1				; Discard result of findf
	PUSH1	r13
	pop	r13
	call	number			; Number?
	POP1	rax			; u-junk
	test	rax, rax
	jnz	.3
	call	literal			; Postpone it
	lea	rax, [rel literal]
	PUSH1	rax
	jmp	compile_comma

	;; None of the above?
.3:	DROP1				; Discard result of number
	ERROR	11


;; Check stack, dictionary limits
	section .text
check:	;; Check user variables
	UFETCH	rax, UP			; Writing past end?
	cmp	rax, USER_SIZE
	ja	.1

	;; Check parameter stack
	cmp	SP, U			; Underflow?
	ja	.2
	SP0	rax
	sub	rax, 8
	cmp	rax, [rax]		; Sentinel?
	jne	.2
	sub	rax, DATA_SIZE		; Overflow?
	cmp	SP, rax
	jb	.3

	;; Check return stack
	cmp	RP, rax			; Underflow?
	ja	.4
	RP0	rax
	sub	rax, 8
	cmp	rax, [rax]		; Sentinel?
	jne	.4
	sub	rax, RETURN_SIZE	; Overflow?
	cmp	RP, rax
	jb	.5

	;; Check dictionary
	HERE	rax
	mov	rcx, [rel DICTIONARY]
	add	rcx, DICT_SIZE
	cmp	rax, rcx		; Writing past end of dictionary?
	ja	.6
	UFETCH	rax, FORTH
	cmp	rax, [U + DICT_START]
	jae	.7			; Overwriting dictionary entries?
	UFETCH	rax, MACRO
	cmp	rax, [U + FORTH_START]
	jae	.8
	ret

.1:	ERROR	1
.2:	ERROR	2
.3:	ERROR	3
.4:	ERROR	4
.5:	ERROR	5
.6:	ERROR	6
.7:	ERROR	7
.8:	ERROR	8


	section .data
	align 8, db 0
bdispatch:
	dq	parse_error		; BLACK is not used
	dq	bred
	dq	bgreen
	dq	byellow
	dq	parse_error		; BLUE should never get here
	dq	parse_error		; MAGENTA is not used
	dq	bcyan
	dq	parse_error		; WHITE should never get here
	dq	parse_error		; BLACK is not used
	dq	bred
	dq	bgreen_number
	dq	byellow_number
	dq	parse_error		; BLUE should never get here
	dq	parse_error		; MAGENTA is not used
	dq	bcyan_number
	dq	parse_error		; WHITE should never get here

;; Binary interpreter
	section .text
binterpret:
	call	bword			; Loop over each word of buffer
	test	T, T			; Got a word?
	jnz	.1
	mov	rdx, [SP]		; Special case for number 0
	cmp	rdx, -1
	jne	.2
.1:	UFETCH	eax, COLOR
	lea	rcx, [rel bdispatch]
	call	[rcx + 8*rax]		; Dispatch on current COLOR
	call	check
	jmp	binterpret
.2:	DROP2				; Discard result of bword
	ret


;; Handle current word according to its color
	section .data
	align	8, db 0
tdispatch:
	dq	parse_error		; BLACK is not used
	dq	red
	dq	green
	dq	yellow
	dq	parse_error		; BLUE should never get here
	dq	parse_error		; MAGENTA is not used
	dq	cyan
	dq	parse_error		; WHITE should never get here

;; ( --) Text interpreter
	section .text
tinterpret:
	call	_tword			; Loop over each word of buffer
	mov	eax, [T]		; End of input?
	test	eax, eax
	jz	.1
	UFETCH	eax, COLOR
	and	eax, 7			; Ignore ALT bit
	lea	rcx, [rel tdispatch]
	call	[rcx + 8*rax]		; Dispatch on current COLOR
	call	check
	jmp	tinterpret
.1:	DROP1				; Discard result of tword
	ret


;; Collect usage profile
	section .text
usage:	cmp	SP, [U + SP_MIN]	; Parameter stack usage
	jae	.1
	USTORE	SP_MIN, SP
.1:	cmp	RP, [U + RP_MIN]	; Return stack usage
	jae	.2
	USTORE	RP_MIN, RP
.2:	HERE	rax			; Dictionary usage
	cmp	rax, [U + DICT_MAX]
	jbe	.3
	USTORE	DICT_MAX, rax
.3:	ret


;; Include bootstrap code
	section .rodata
bootstrap_forth:
%defstr prelude PRELUDE
	incbin	prelude
	db	0x83			; (yellow)
	db	"main"
bootstrap_len	equ	($ - bootstrap_forth)
	db	0

	section .text
bootstrap:
	lea	rax, [rel bootstrap_forth]
	mov	qword [U + USE], rax
	mov	qword [U + BLK], 0	; Block 0
	mov	qword [U + IN], 0	; Byte 0
	mov	qword [U + SOURCE_ID], -1
	mov	qword [U + SPAN], bootstrap_len
	RP0	RP			; Setup return stack
	sub	RP, 8
	mov	[RP], RP		; Sentinel
	call	tinterpret		; Interpret bootstrap source
	jmp	abort			; Return to warm start

	section .text
encode_wordlist:
	push	r8
	mov	r8, rax
.loop:	movzx	eax, byte [r8]
	test	eax, eax		; More to do?
	jz	.done
	mov	rcx, r8
	inc	rcx
	PUSH2	rcx, rax		; c-addr, u
	call	encode
	POP2	rax, rdx
	mov	[r8 + 0], rax		; 12 bytes
	mov	[r8 + 8], edx
	sub	r8, 16
	jmp	.loop
.done:	pop	r8
	ret

	section .text
encode_wordlists:
	lea	rax, [rel builtin_macro]
	call	encode_wordlist
	lea	rax, [rel builtin_forth]
	call	encode_wordlist
	ret

;; quit ( --) Main loop of outer interpreter
	section .text
quit:	mov	qword [U + USE], 0	; No current source file
	mov	qword [U + SOURCE_ID], 0
	xor	edi, edi		; Read from terminal
	call	use_block
	mov	rax, [rel BOOTSTRAP]	; Once only...
	test	rax, rax
	jnz	.1
	mov	qword [rel BOOTSTRAP], -1
	call	encode_wordlists
	jmp	bootstrap		; Execute Forth bootstrap
.1:	RP0	RP			; Setup return stack
	sub	RP, 8
	mov	[RP], RP		; Sentinel
	call	query			; Loop over each line of input
	call	tinterpret
	call	usage
	jmp	.1


;; abort ( --)
;; Warm start sets up parameter stack and execution environment
	section .text
abort:	SP0	SP			; Setup parameter stack
	sub	SP, 8
	mov	[SP], SP		; Sentinel
	DROP1				; T can never be empty
	UADDR	rax, FORTH		; Initialize search order
	USTORE	CURRENT, rax
	jmp	quit			; Enter main loop


;; Cold start sets up system data structures
	section .text
cold:
	;; Allocate user area
	mov	qword [U + UP], UP + 8

	mov	RP, U			; Allocate return stack
	sub	RP, DATA_SIZE
	push	RP			; Sentinel

	mov	rdi, [rel DICTIONARY]	; Subdivide dictionary space
	USTORE	MACRO_START, rdi
	mov	rax, rdi
	add	rax, MACRO_SIZE
	USTORE	FORTH_START, rax
	mov	rax, rdi
	add	rax, WORDLIST_SIZE
	USTORE	DICT_START, rax
	USTORE	DP, rax			; Next available

	;; Link wordlists
	UFETCH	rdi, MACRO_START
	USTORE	MACRO, rdi		; Point to allocated wordlist
	xor	eax, eax		; Clear first 8 bytes
	mov	[rdi], rax
	lea	rdx, [rel builtin_macro]
	mov	[rdi + 8], rdx		; Link to builtin wordlist

	UFETCH	rdi, FORTH_START
	USTORE	FORTH, rdi		; Point to allocated wordlist
	xor	eax, eax
	mov	[rdi], rax
	lea	rdx, [rel builtin_forth]
	mov	[rdi + 8], rdx		; Link to builtin wordlist

	;; Initialize usage tracking
	SP0	rax			; Paramater stack usage
	USTORE	SP_MIN, rax
	RP0	rax			; Return stack usage
	USTORE	RP_MIN, rax
	HERE	rax			; Dictionary usage
	USTORE	DICT_MAX, rax

	jmp	abort			; Run the interpreter


	global	main
	section	.text
main:	mov	[rel ARGC], rdi		; Save process startup information
	mov	[rel ARGV], rsi
	mov	[rel ENVIRON], rdx

	mov	U, RP			; Save initial stack frame
	sub	U, (BUFFER_SIZE + USER_SIZE)
	and	U, -4096

	;; Allocate dictionary
	xor	r9d, r9d		; offset
	mov	r8, -1			; fd
	mov	r10, (MAP_ANON | MAP_PRIVATE)	; flags
	mov	edx, 7			; prot = PROT_EXEC | PROT_WRITE | PROT_READ
	mov	esi, DICT_SIZE		; len
	xor	edi, edi		; addr
	mov	eax, SYS_mmap
	syscall
	mov	[rel DICTIONARY], rax

	jmp	cold			; Relay into cold start


	section .text
	weak global start
start:	;; Save process startup information
%ifdef FreeBSD
	;; FreeBSD aligns stack before entry, passes (unaligned)
	;; address of arguments in RDI.
	mov	rax, rdi
%endif
%ifdef Linux
	mov	[rel ATEXIT], rdx	; To be called at exit
	mov	rax, RP
%endif
	mov	rdi, [rax + 0]		; argc
	lea	rsi, [rax + 8]		; argv
	lea	rdx, [rsi + 8*rdi + 8]	; environ
	jmp	main


;; Define dictionary headers for all builtin words
%macro	HEADER	2
%%start	db	%%len			; Length byte
%%name	db	%1			; Up to 11 bytes of name
%%len	equ	($ - %%name)
	db	(11 - %%len) dup (0)	; Padding
	dd	(%2 - %%start)		; Offset to code
%endmacro

;; End of a wordlist
%macro	TAIL 0
	db	16 dup (0)
%endmacro

	section .data
	align	16, db 0

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
	HEADER	"word",		_tword
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
	HEADER	"align",	_align
	HEADER	"abs",		_abs
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


;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated
;; documentation files (the "Software"), to deal in the Software
;; without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to
;; whom the Software is furnished to do so, subject to the
;; following conditions:
;;
;; The above copyright notice and this permission notice shall
;; be included in all copies or substantial portions of the
;; Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
;; KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;; WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
