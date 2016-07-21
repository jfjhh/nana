;
; Custom Bootloader.
; Copyright (C) 2016  Alex Striff
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

;
; TODO: load kernel and other resources from disk and run kernel
;

[bits 16]
[org 0x7e00]			; add to offsets

%include "macros.asm"

;;;;;;;; JUMP TO MAIN CODE ;;;;;;;;
jmp start
;;;;;;;; JUMP TO MAIN CODE ;;;;;;;;

;;;;;;;; START DATA ;;;;;;;;

; strings
dlstr general_ok,	0x02, " OK ", 0x02
dlstr general_err,	0x02, " ERROR ", 0x02
dlstr lowmem_err,	0x01, " Not enough low memory ", 0x01
dlstr vbe_err,		0x02, " Could not get VBE info ", 0x02
dlstr a20_err,		0x03, " Could not enable the A20 line ", 0x03
dlstr splash_err,	0x04, " Could not show the splash screen ", 0x04
dlstr mmap_err,		0x05, " Could not create system memory map ", 0x05

; text mode attributes
stdattr:	equ 0x0a	; text mode video attr for normal messages
errattr:	equ 0x0c	; text mode video attr for error messages

; LBA on disk of the splash image bitmap
splashinfo:	; memory used will be overwritable after calling nana_splash
.lba:		equ 0x400	; LBA of bitmap (8K in size)
.off:		equ 0x5000	; load high as possible, below bootloader
.seg:		equ .off >> 4	; as a segment value, for convenience
.lfbseg:	equ 0xa000	; the linear frame buffer segment (real mode)
.mode:		equ 0x13	; the video mode for int 0x10

; VBE information structure
vbeinfo:
.vbeoff:	equ 0x0700		; offset of VBE info block
.modeptr:	equ .vbeoff + 0x0e	; where in the block the modes are
.modeinfo:	equ .vbeoff + 0x200	; VBE 2.0 block is 512 bytes long

; system memory map information structure
mmap:		; leaves space for 0x47 VBE entries after block
.count:		db 0		; number of entries
.off:		equ vbeinfo.modeinfo + 0x4700

; GDT descriptor structure
gdtinfo:
.lastbyte:	dw gdt_end - gdt - 1	; last byte in table
.start:		dd gdt			; start of table
		dw 0			; high 2 bytes of location are 0

; temporary GDT before kernel creates its own
gdt:
.nulldesc:	dd 0		; bytes 0-3 of gdt location: bits 0-31
		dd 0		; bytes 4-5 of gdt location: bits 48-63
.dpl0code:	db 0xff, 0xff, 0, 0, 0, 10011010b, 11001111b, 0
.dpl0data:	db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
.dpl3code:	db 0xff, 0xff, 0, 0, 0, 11111010b, 11001111b, 0
.dpl3data:	db 0xff, 0xff, 0, 0, 0, 11110010b, 11001111b, 0
.extra:		times 4 dq 0
gdt_end:

; Disk Address Packet for reading from the boot disk
dap:				; disk address packet for int 0x13, ah=0x42
.size:		db 0x10		; size: 0x10
.reserved:	db 0x00		; reserved byte (0x00)
.blocks:	dw 0x0010	; blocks to transfer (8K for splash bitmap)
.transbuf:	dd 0x00000000	; segment 0x0000, offset 0x0000
.startblk:	dq 0		; starting absolute block number

; information on the boot disk number and partition
diskinfo:
disk:		db 0
part:		dw 0

; kernel information table (should be passed to kernel or known at compile time)
; entry: 4-byte key string and 16-bit offset (from 0)
kinfo:
.disk:		db "DISK"
		dw diskinfo
.mmap:		db "MMAP"
		dw mmap
.vbe:		dw "VBE#"
		dw vbeinfo.modeinfo
.end:		dq 0
		dw 0

;;;;;;;; END DATA ;;;;;;;;

;;;;;;;; START BOOTLOADER CODE ;;;;;;;;

;;;;;;;; START MAIN CODE ;;;;;;;;

start:
	mov [ds:disk], dl	; save disk number
	mov [ds:part], si	; save MBR partition offset

lowmem:	; lowmem detection
	clc			; clear carry flag
	int 0x12		; request low memory size
	jc .err			; the carry flag is set if it failed
	; ax is amount of contiguous memory in KB from 0 (max: 0x27f)
	cmp ax, 0x40		; need at least 64 KiB low mem. to load things
	jl .err			; cannot boot without enough memory
	jmp vbe

.err:	push ax			; print found memory, for user to see
	push word 1
	call print_hex
	add sp, 4
	center_msg_h errattr, lowmem_err

vbe:	; gather information on VBE video modes
	mov di, vbeinfo.vbeoff	; the offset of the info block
	mov word [es:di+0], 0x4256	; "VB" (little endian)
	mov word [es:di+2], 0x3245	; "E2" (little endian)
	mov ax, 0x4f00		; get VBE info, with "VBE2" set for VBE 2.0 info
	int 0x10
	jc .ns
	cmp ax, 0x004f		; check success
	je .load		; continue

.ns:	push ax			; if not supported, show err code
	push word 1
	call print_hex
	add sp, 4
.err:	center_msg_h errattr, vbe_err
	
	; load all the mode info
.load:	mov si, [vbeinfo.modeptr]
	mov di, vbeinfo.modeinfo

	cld			; make sure DF is forwards
.mode:	lodsw			; get the mode word [es:si]
	cmp ax, 0xffff		; check for end of list word
	je .end			; continue

	mov cx, 128		; zero the entry (256 bytes long for VBE 3.0)
.zero:	mov bx, cx
	dec bx
	shl bx, 1		; multiply by 2 because words
	mov word [es:di+bx], 0
	loop .zero

	mov cx, ax		; make it the word
	mov ax, 0x4f01		; get VBE mode info
	int 0x10		; entry stored at [es:di]
	jc .err
	cmp ax, 0x004f		; check success
	jne .next		; just continue on if failed

	; si already incremented by lodsw
.next:	add di, 256		; offset to next entry
	jmp .mode

.end:
	sub si, [vbeinfo.modeptr]
	shr si, 1		; number of entries processed
	dec si			; disregard lodsw increment

; enable the A20 line if it is not already enabled by the BIOS
a20:
	call detect_a20		; check if already enabled by the BIOS
	or ax, ax
	jnz memmap

	mov ax, 0x2401		; try to enable with the BIOS function
	int 0x15
	call detect_a20
	or ax, ax
	jnz memmap

	cli			; try with the keyboard contoller method
	call a20wait.a
	mov al, 0xad
	out 0x64, al

	call a20wait.a
	mov al, 0xd0
	out 0x64, al

	call a20wait.b
	in al,0x60
	push eax

	call a20wait.a
	mov al, 0xd1
	out 0x64, al

	call a20wait.a
	pop eax
	or al, 1<<1
	out 0x60, al

	call a20wait.a
	mov al, 0xae
	out 0x64, al

	call a20wait.a
	sti

	mov cx, 32		; check this many times
.kbdlp:	call detect_a20	
	or ax, ax
	jnz memmap
	mov dx, cx
	mov cx, 128
.kbdwt:	nop
	loop .kbdwt
	mov cx, dx
	loop .kbdlp

	in al, 0x92		; try the Fast A20 method (unstable)
	test al, 1<<1
	jnz .end
	or al, 1<<1
	and al, 0xfe
	out 0x92, al

.end:	mov cx, 32		; check this many times
.fastl:	call detect_a20	
	or ax, ax
	jnz memmap
	mov dx, cx
	mov cx, 128
.fastw:	nop
	loop .fastw
	mov cx, dx
	loop .fastl

	; could not enable A20: give up!
.err:	center_msg_h errattr, a20_err

; create a system memory map, in the format of an int 0x15, ax=0xe820 map,
; even if e820 is not available (uses other functions and reformats results)
memmap:	; all e820-style entries use 24-byte ACPI 3.X-compatible entries
.e820:	mov di, mmap.off	; [es:di] is offset of memory map
	mov [es:di+20], dword 1	; force a valid ACPI 3.X entry
	mov eax, 0xe820		; get e820 system memory map
	xor ebx, ebx		; start at beginning of map
	mov ecx, 24		; size of result buffer (24 for ACPI 3.X compat)
	mov edx, 0x0534d4150	; magic number: ASCII "SMAP"
	int 0x15
	jc .e801
	cmp eax, 0x0534d4150
	jne .e801
	test ebx, ebx		; a one-entry list is useless
	je .e801
	jmp .entry		; e820 is supported
.next:	mov eax, 0xe820		; eax and ecx get trashed on every int 0x15 call
	mov [es:di + 20], dword 1	; force a valid ACPI 3.X entry
	mov ecx, 24		; ask for 24 bytes again
	int 0x15
	jc short .end		; carry set: "end of list already reached"
	mov edx, 0x0534D4150	; fix potentially trashed register
.entry:	jcxz .empty		; skip empty entries
	cmp cl, 20		; 24-byte ACPI 3.X response?
	jbe short .noext
	test byte [es:di + 20], 1<<0	; ACPI 3.X "ignore this data" bit?
	je .empty
.noext:	mov ecx, [es:di + 8]	; get lower uint32_t of memory region length
	or ecx, [es:di + 12]	; OR with upper uint32_t to test for zero
	jz .empty		; if length uint64_t is 0, skip entry
	inc byte [mmap.count]	; got a good entry: go to next entry
	add di, 24		; offset
.empty:	test ebx, ebx		; if ebx resets to 0, at end of list
	jne short .next
	mov dword [es:di],    0	; null entry at end
	mov dword [es:di+4],  0
	mov dword [es:di+8],  0
	mov dword [es:di+12], 0
.end:	clc			; fix CF set as end of list
	jmp .ok

.e801:	mov ax, 0xe801		; get memory for >64 MiB configurations
	xor cx, cx		; zero to be able to check if changed
	xor dx, dx
	xor eax, eax		; for setting entry values later
	xor ebx, ebx
	xor edx, edx
	int 0x15
	jc .da88
	cmp ah, 0x86		; unsupported command
	je .da88
	cmp ah, 0x80		; invalid command
	je .da88
	jcxz .axbx		; if cx and dx are zero, ax and bx
	mov ax, cx
	mov bx, dx
.axbx:	test ax, bx		; check these are not zero, either
	jz .da88
	jmp .to16e820

.da88:	stc
	mov ax, 0xda88		; get extended memory size
	int 0x15
	jc .ah88
	mov ax, bx		; amount is in cl:bx
	xor edx, edx
	mov dl, cl		; amount is now in dx:ax
	jmp .toe820

.ah88:	mov ah, 0x88		; get extended memory size (286+)
	clc			; get around CF bug in some BIOSes
	int 0x15
	jc .ah8a
	xor edx, edx		; ax:dx is value
	jmp .toe820

.ah8a:	xor al, al
	xor dx, dx
	mov ah, 0x8a		; get big memory size
	mov bx, ax		; copy value
	test bx, dx		; not zero if any 1-bits in dx:ax
	jz .ahc7
	jmp .toe820

.ahc7:	mov ah, 0xc0		; get configuration: IBM ah=c7 memory map
	int 0x15		; (XT >1986/1/10,AT mdl 3x9,CONV,XT286,PS)
	jc .cmos
	test ah, ah		; check for success
	jnz .cmos
	test byte [es:bx+0x06], 1<<4	; test bit 4 of feature byte 2
	jz .cmos		; if not set, 0xc7 is not supported
	mov ah, 0xc7		; return memory-map information (IBM)
	mov si, mmap.off	; [ds:si] is offset of memory map
	int 0x15
	jc .cmos
	cmp word [ds:si], 66	; mimimum size of IBM c7 map
	jl .cmos

	; convert IBM ah=c7 mem. map to an e820 mem. map
	mov dword [ds:si+0x0a], eax	; 01 MiB - 16 MiB (1 KiB blocks)
	mov dword [ds:si+0x0e], ebx	; 16 MiB - 04 GiB (1 KiB blocks)

	mov dword [mmap.off+0x00], 0x00100000	; start of extended memory
	mov dword [mmap.off+0x04], 0
	shld dword [mmap.off+0x08], eax, 42	; all of eax, plus 10 for KiB=>B
	mov dword [mmap.off+0x10], 1	; usable memory type
	inc byte [mmap.count]

	mov dword [mmap.off+0x18], 0x01000000	; start of 16 MiB
	mov dword [mmap.off+0x1c], 0
	shld dword [mmap.off+0x20], ebx, 42	; all of eax, plus 10 for KiB=>B
	mov dword [mmap.off+0x28], 1	; usable memory type
	inc byte [mmap.count]

	mov dword [mmap.off+0x30], 0	; 24-byte null entry (map end)
	mov dword [mmap.off+0x38], 0
	mov word  [mmap.off+0x3c], 0
	jmp .ok

.cmos:	cli			; read memory information from CMOS
	xor eax, eax
	mov al, 0x30
	out 0x70, al		; read low byte of extended mem.
	in al, 0x71
	mov bl, al
	mov al, 0x31
	out 0x70, al		; read high byte of extended mem.
	in al, 0x71
	mov ah, al
	mov al, bl
	sti			; ax is now the amount of extended mem. in KiB
	xor dx, dx		; now dx:ax is the amount
	test ax, ax
	jz .err
	jmp .toe820

.to16e820: ; 2nd e820 entry (offsets are entry offset + 24)
	; turn amount above 16 MiB (bs=64KiB) an e820 entry (blocks in dx:bx)
	mov dword [mmap.off+0x18], 0x01000000	; start of 16 MiB
	mov dword [mmap.off+0x1c], 0
	shl ebx, 16		; move low word to be high word for shld
	shld dword [mmap.off+0x20], ebx, 32	; all of bx, plus 16 for 64K=>B
	shl edx, 16		; move low word to be high word for shld
	shld dword [mmap.off+0x24], edx, 32	; all of dx, plus 16 for 64K=>B
	mov dword [mmap.off+0x28], 1	; usable memory type
	mov dword [mmap.off+0x30], 0	; 24-byte null entry (map end)
	mov dword [mmap.off+0x38], 0
	mov word  [mmap.off+0x3c], 0
	inc byte [mmap.count]

.toe820: ; 1st e820 entry
	; turn extended KiB into an e820 entry (KiB in dx:ax)
	mov dword [mmap.off+0x00], 0x00100000	; start of extended memory
	mov dword [mmap.off+0x04], 0
	shl eax, 16		; move low word to be high word for shld
	shld dword [mmap.off+0x08], eax, 26	; all of ax, plus 10 for KiB=>B
	shl edx, 16		; move low word to be high word for shld
	shld dword [mmap.off+0x0c], edx, 26	; all of dx, plus 10 for KiB=>B
	cmp byte [mmap.count], 0	; check if an entry after this exists
	jnz .ok
	mov dword [mmap.off+0x10], 1	; usable memory type
	mov dword [mmap.off+0x18], 0	; 24-byte null entry (map end)
	mov dword [mmap.off+0x20], 0
	mov word  [mmap.off+0x24], 0
	inc byte [mmap.count]
	jmp .ok

.err:	center_msg_h errattr, mmap_err

.ok:

splash:
	call nana_splash	; show the splash screen while still possible

	; play some block chords
	; (about until monitor switches modes, making everything seem smoother)
	; mov bx, 165
	; call beep
	; mov bx, 196
	; call beep
	; mov bx, 247
	; call beep
	; mov bx, 147
	; call beep
	; mov bx, 196
	; call beep
	; mov bx, 247
	; call beep
	; mov bx, 131
	; call beep
	; mov bx, 165
	; call beep
	; mov bx, 196
	; call beep
	; mov bx, 147
	; call beep
	; mov bx, 185
	; call beep
	; mov bx, 220
	; call beep
	; mov bx, 392
	; call beep
	; mov bx, 494
	; call beep
	; mov bx, 587
	; call beep

; setup protected mode
pmode:
	jmp halt
	cli			; clear interrupts
	in al, 0x70		; disable NMIs
	or al, 1<<7
	out 0x70, al

	; set null descriptor to have the GDT's location
	mov ax, word [gdtinfo.start]
	mov word [gdt], ax
	mov ax, word [gdtinfo.start+2]
	mov word [gdt+2], ax
	mov ax, word [gdtinfo.start+2]
	mov word [gdt+6], ax

	lgdt [gdtinfo]		; load gdt
	mov eax, cr0		; switch to pmode by setting pmode bit
	or al, 1<<0
	mov cr0, eax
	jmp $ + 2		; tell 386/486 to not crash
	sti			; set interrupts

	; setup segments
	mov ax, 0x02<<3	; select descriptor 2 (data descriptor)
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax
	jmp 0x01<<3:.setcs	; update cs to be descriptor 1 (code descriptor)
.setcs:

	jmp halt

;;;;;;;; END MAIN CODE ;;;;;;;;

;;;;;;;; START SUBROUTINES ;;;;;;;;

;
; write a message on the screen (see BIOS int 0x10, ah=0x13)
;
; bl is the text mode attribute to write with
; cx is the string length
; dx is the row (high nybble) and col (low nybble)
; bp is the offset of the string
;
msg:
	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	int 0x10		; do the write
	jc halt
	ret

;
; wait loops for enabling the A20 line via the keyboard controller method
;
a20wait:
.a:
	in al, 0x64
	test al, 1<<1
	jnz .a
	ret

.b:
	in al, 0x64
	test al, 1<<0
	jz .b
	ret

;
; prints the first parameter number of words on stack in hex to the display
;
; example use:
;
; push 0xcafe
; push word 1
; call print_hex
; add sp, 4
;
; output text: 0xcafe
;
print_hex:
	push bp
	mov bp, sp
	pusha

	mov cx, [bp+4]		; the size word parameter

	mov ax, 0x0e30		; teletype output, ASCII '0'
	int 0x10
	mov al, 0x78		; ASCII 'x'
	int 0x10

.words:
	mov si, cx
	shl si, 1
	mov dx, [bp+si+4]	; a word
	mov di, cx
	mov cx, 4		; 4 hex digits to a word

.digit:
	rol dx, 4		; rotate so that low nybble of the byte is used
	mov ax, 0x0e0f		; ah is teletype output,
				; al is mask for low nybble
	and al, dl		; get low nybble of dl in al
	add al, 0x90		; convert al to ASCII hex char
	daa			; uses "packed BCD addition," idk how
	adc al, 0x40
	daa
	int 0x10

	loop .digit

	mov al, 0x20		; separate words with spaces
	int 0x10

	mov cx, di
	loop .words

	popa
	pop bp
	ret

;
; check the A20 line: compare a low byte with the same byte 1 MiB higher
; to check if memory wraps (A20 is disabled)
;
; ax is 0 if A20 is disabled (memory wraps at 1 MiB), otherwise 1 (enabled)
;
detect_a20:
	push ds
	push es

	xor ax, ax
	mov es, ax
	not ax
	mov ds, ax

	mov di, 0x0500
	mov si, 0x0510

	cli
	mov al, byte [es:di]
	push ax
	mov al, byte [ds:si]
	push ax

	mov byte [es:di], 0x00
	mov byte [ds:si], 0xff
	cmp byte [es:di], 0xff

	pop ax
	mov byte [ds:si], al
	pop ax
	mov byte [es:di], al
	sti

	mov ax, 0
	je .exit
	inc ax
.exit:
	pop es
	pop ds
	ret

;
; draw the splash screen bitmap
; 
; splashinfo (see data section above) must be set by the VBE detection code
;
nana_splash:
	push es
	push ds

	mov ax, splashinfo.mode	; set video mode
	int 0x10
	jc .err
	mov ax, 0x0500		; set active display page, page to display
	int 0x10
	jc .err

	mov ax, splashinfo.lfbseg	; video memory linear framebuffer
	mov es, ax		; set segment

	mov ah, 0x0b		; set background color
	xor bh, bh
	mov bl, 0x3e		; as close as VGA can get to *the* top pink
	int 0x10

	; clear and attribute screen with video memory manipulation
	mov bl, 0x3e		; as close as VGA can get to *the* top pink
	mov cx, (320 * 200)	; pixels 320x200 vga video display
.fill:	mov si, cx
	mov byte [es:0+si-1], bl	; write to video memory
	loop .fill

	; read in the splash screen bitmap
	mov ax, splashinfo.lba	; for now, from a hardcoded block
	mov [dap.startblk], ax
	xor ax, ax
	mov [dap.startblk+2], ax
	mov ax, splashinfo.off
	mov [dap.transbuf], ax
	xor ax, ax
	mov [dap.transbuf+2], ax
	mov ah, 0x42		; extended read
	mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov si, dap		; ds is 0, so ds:si is right disk address packet
	int 0x13
	jc .err

	; map each bit of the read bitmap to a byte in video memory
	; the bitmap goes from high to low, so bit 7 of the first byte is the
	; first pixel in the image, and bit 0 of the first byte is the 8th pixel
	; in the image
	xor si, si		; zero si
	mov ax, splashinfo.seg	; loaded bitmap memory location
	mov ds, ax		; set segment for lodsb

	xchg bx, bx		; Bochs magic breakpoint

.bmp:	lodsb			; load the byte at [ds:si] into al
	mov ah, al		; save byte

	xor bx, bx		; index of bit in the byte
.bit:
	mov dl, 0x00		; black
	mov al, ah		; restore byte
	and al, 10000000b	; check bit 7 (leftmost bit)
	jz .nobit		; 0 means do not set the color
	mov di, si		; copy si
	dec di			; si is incremented by lodsb, so "undo"
	shl di, 3		; multiply by 8
	mov [es:bx+di], dl	; write the byte
.nobit:	shl ah, 1		; next bit
	inc bx			; index of next bit
	cmp bx, 8
	jb .bit

	cmp si, (320 * 200) / 8	; size of bitmap (in bytes)
	jb .bmp
	pop ds
	pop es
	ret

.err:	center_msg errattr, splash_err
	ret

;
; "utilize" the PC speaker (piezotransducer) to produce a frequency (forever)
;
; bx is the frequency to beep at
;
; a note's frequency is (2^(n/12) * 440) Hz, where n is the number of
; half-steps the note is above A4 (negative for notes that are below it)
;
beep:	
	push ax
	push dx

	; do bounds check on frequency, and choose the closest possible if
	; it is out of the possible range of frequencies:
	; because mode 3 is used for the PIT, a divisor of 1 cannot be used
	mov ax, 0xffff		; ax == slowest possible frequency (65535)
	cmp bx, 18		; is the requested frequency too low?
	jbe .pit		; yes, use slowest possible frequency

	mov ax, 2		; ax == fastest possible frequency (2)
	cmp bx, 0xffff		; is the requested frequency too high?
	jae .pit		; yes, use fastest possible frequency

	; to get the PIT frequency, divide 14.31818 MHz by freq and by 3
	mov ax, 0x9e99		; the above frequency, divided by 4 (err free):
	mov dx, 0x0036		; dx:ax == 3579545
	div bx			; ax == 3579545 / frequency, dx == remainder
	cmp dx, 0xffff		; is the remainder more than half?
	jb .close		; no, round down
	inc ax			; yes, round up

	; use the closest possible frequency to the one requested
.close:	mov bx, 3
	xor dx, dx		; dx:ax == 3579545 * 256 / frequency
	div bx			; ax == (3579545 * 256 / 3 * 256) / frequency
	cmp dx, 3 / 2		; is the remainder more than half?
	jb .pit			; no, round down
	inc ax			; yes, round up

.pit:	cli			; disable interrupts
	mov bx, ax		; save the frequency temporarily
	mov al, 10110110b	; cmd: channel 2, lobyte/hibyte, mode 3, bin
	out 0x43, al		; send command to PIT
	mov ax, bx		; divided frequency from argument
	out 0x42, al		; set low byte of divisor
	shr ax, 8
	out 0x42, al		; set high byte of divisor
	in al, 0x61		; get the kbd controller byte
	mov bl, al
	or al, 0x03		; toggle bits 1 and 0 on
	cmp al, bl
	je .end			; don't set if already set
	out 0x61, al		; set the PC speaker on
.end:	; speaker is beeping and will stop on keypress:
	sti			; enable interrupts

	mov cx, 0x0800
.waita:	push cx			; do lots of memory access for a hacky sleep
	xor cx, cx
	not cx
.waitb:	nop
	loop .waitb
	pop cx
	loop .waita

	cli
	in al, 0x61		; get the kbd controller byte
	and al, 0xfc		; toggle bits 1 and 0 off
	out 0x61, al		; set the PC speaker off
	sti

	pop dx
	pop ax
	ret

;
; halt forever (so user can read error messages)
;
halt:	cli
.loop:	hlt
	jmp .loop

;;;;;;;; END SUBROUTINES ;;;;;;;;

; endpad:	times 0x1200 - ($ - $$) db 0	; page-align in mem. 0x(7e00+200==8000)
; end:

;;;;;;;; END BOOTLOADER CODE ;;;;;;;;

