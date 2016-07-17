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

; text mode attributes
stdattr:	equ 0x0a	; text mode video attr for normal messages
errattr:	equ 0x0c	; text mode video attr for error messages

; memory location of the loaded splash image bitmap
splashaddr:	equ 0x400

; VBE information structure
vbeinfo:
.vbeoff:	equ 0x0700		; offset of VBE info block
.modeptr:	equ .vbeoff + 0x0e	; where in the block the modes are
.modeinfo:	equ .vbeoff + 512	; VBE 2.0 block is 512 bytes long

; GDT descriptor structure
gdtinfo:
.lastbyte	dw gdt_end - gdt - 1	; last byte in table
.start		dd gdt			; start of table

; temporary GDT before kernel creates its own
gdt:		dd 0,0		; entry 0 is always unused
flatdesc:	db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
gdt_end:

; Disk Address Packet for reading from the boot disk
dap:				; disk address packet for int 0x13, ah=0x42
.size:		db 0x10		; size: 0x10
.reserved:	db 0x00		; reserved byte (0x00)
.blocks:	dw 0x0010	; blocks to transfer (8K)
.transbuf:	dd 0x00000000	; segment 0x0000, offset 0x0000
.startblk:	dq 0		; starting absolute block number

; boot disk and partition, to be preserved for the kernel
disk:		db 0
part:		dw 0

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
	; ax is amount of contiguous memory in KB from 0
	cmp ax, 0x027f		; need 640 KiB (0x27f)
	jl .err			; cannot boot without enough memory
	jmp vbe

.err:	center_msg_h errattr, lowmem_err

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
	jnz pmode

	mov ax, 0x2401		; try to enable with the BIOS function
	int 0x15
	call detect_a20
	or ax, ax
	jnz pmode

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
	jnz pmode
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
	jnz pmode
	mov dx, cx
	mov cx, 128
.fastw:	nop
	loop .fastw
	mov cx, dx
	loop .fastl

	; could not enable A20: give up!
.err:	center_msg_h errattr, a20_err

	;
	; TODO: go into protected mode
	; TODO: int 0x15, ax = 0xe820 advanced memory detection.
	; TODO: proper GDT and IDT
	; TODO: paging now (in bootloader), or in kernel?
	;

; setup protected mode
pmode:
	call splash             ; show the splash screen while still possible

	mov bx, 196		; frequency of G3, musical note
	call beep		; why not beep annoyingly?

	cli
	lgdt [gdtinfo]		; load gdt
	mov eax, cr0		; switch to pmode by setting pmode bit
	or al, 1<<0
	mov cr0, eax
	jmp $ + 2		; tell 386/486 to not crash

	; mov ax, 0x10		; select descriptor 1 (data descriptor)
	; mov ds, ax
	; mov es, ax
	; mov fs, ax
	; mov gs, ax
	; mov ss, ax

	; jmp 0x08:0x1000		; set code descriptor, jump to kernel

	; and al, 0xfe		; back to realmode...
	; mov cr0, eax		; by toggling bit again

	; pop ds			; get back old segment
	; sti			; interrupts ok

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
; splashaddr must be set
;
splash:
	mov ax, 0x0013		; set video mode, 320x200 256-color VGA mode
	int 0x10
	jc .err
	mov ax, 0x0500		; set active display page, page to display
	int 0x10
	jc .err

	; clear and attribute screen with video memory manipulation
	mov ax, 0xa000		; 0x000a0000 is video memory in VGA
	mov es, ax		; set segment
	mov bl, 0x3e		; as close as VGA can get to *the* top pink
	mov cx, (320 * 200)	; pixels 640x480 vga video display
.fill:	mov si, cx
	mov byte [es:0+si-1], bl	; write to video memory
	loop .fill

	; read in the splash screen bitmap
	mov ax, splashaddr		; for now, from a hardcoded block
	mov [dap.startblk], ax
	xor ax, ax
	mov [dap.startblk+2], ax
	mov ax, 0x2000
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
	push ds
	xor si, si		; zero si
	mov ax, 0xa000		; video memory segment
	mov es, ax		; set segment for video mem writes
	mov ax, 0x0200		; loaded bitmap memory location (segment)
	mov ds, ax		; set segment for lodsb

.bmp:	lodsb			; load the byte at [ds:si] into al
	mov ah, al		; save byte

	xor bx, bx		; index of bit in the byte
.bit:
	; mov dl, 0x54		; as close as VGA can get to *the* bottom pink
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
	ret

.err:	center_msg_h errattr, splash_err

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
	mov ax, 0x10000		; ax == slowest possible frequency (65536)
	cmp bx, 18		; is the requested frequency too low?
	jbe .pit		; yes, use slowest possible frequency

	mov ax, 2		; ax == fastest possible frequency (2)
	cmp bx, 1193181 - 1	; is the requested frequency too high?
	jae .pit		; yes, use fastest possible frequency

	; to get the PIT frequency, divide 14.31818 MHz by freq and by 3
	mov ax, 0x9e99		; the above frequency, divided by 4 (err free):
	mov dx, 0x0036		; dx:ax == 3579545
	div bx			; ax == 3579545 / frequency, dx == remainder
	cmp dx, 3579545 / 2	; is the remainder more than half?
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

	mov cx, 0x1000
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

;;;;;;;; END BOOTLOADER CODE ;;;;;;;;

