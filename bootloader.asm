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

;;;;;;;; START BOOTLOADER CODE ;;;;;;;;

start:
	mov [ds:disk], dl	; save disk number
	mov [ds:part], si	; save MBR partition offset

; lowmem:	; lowmem detection
; 	clc			; clear carry flag
; 	int 0x12		; request low memory size
; 	jc .err			; the carry flag is set if it failed

; 	; ax is amount of contiguous memory in KB from 0.
; 	push ax
; 	push word 1
; 	call print_hex
; 	add sp, 4		; size word + print word = 2 words = 4 bytes
; 	jmp read

; .err:	mov ah, 0x13		; write string
; 	mov al, 00000001b	; update cursor, text only
; 	xor bh, bh		; page 0
; 	mov bl, errattr		; attribute
; 	mov cx, lowmem_err.len	; length
; 	mov dx, 0x0210		; row 2, col 16
; 	mov bp, lowmem_err	; es:bp is string
; 	int 0x10
; 	jmp halt

	; TODO: Check available video modes and skip splash if not supported
	; set video mode
video:	mov ax, 0x0013		; set video mode, 320x200 256-color VGA mode
	int 0x10
	mov ax, 0x0500		; set active display page, page to display
	int 0x10

	; clear and attribute screen with video memory manipulation
	mov ax, 0xa000		; 0x000a0000 is video memory in VGA
	mov es, ax		; set segment
	mov bl, 0x3e		; as close as VGA can get to *the* top pink
	mov cx, (320 * 200)	; pixels 640x480 vga video display
.pink:	mov si, cx
	mov byte [es:0+si-1], bl	; write to video memory
	loop .pink

	; read in the splash screen bitmap
	mov ax, 1024		; for now, from a hardcoded block
	mov [dap.startblk], ax
	xor ax, ax
	mov [dap.startblk+2], ax
	mov ax, 0x2000		; for now, to a hardcoded memory location
	mov [dap.transbuf], ax
	xor ax, ax
	mov [dap.transbuf+2], ax
	mov ah, 0x42		; extended read
	mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov si, dap		; ds is 0, so ds:si is right disk address packet
	int 0x13

	; TODO: map each bit of the read bitmap to a byte in video memory
	; the bitmap goes from high to low, so bit 7 of the first byte is the
	; first pixel in the image, and bit 0 of the first byte is the 8th pixel
	; in the image
	; mov bl, 0x54		; as close as VGA can get to *the* bottom pink
	push ds
	xor si, si		; zero si
	mov ax, 0xa000		; video memory segment
	mov es, ax		; set segment for video mem writes
	mov ax, 0x0200		; loaded bitmap memory location (segment)
	mov ds, ax		; set segment for lodsb

.bmp:	lodsb			; load the byte at [ds:si] into al
	mov ah, al		; save byte

	xor bx, bx		; index of bit in the byte
.bit:	mov dl, 0x54		; default out byte is the bottom pink
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

beep:	
	; a note's frequency is (2^(n/12) * 440) Hz, where n is the number of
	; half-steps the note is above A4
	cli			; disable interrupts
	mov al, 10110110b	; cmd: channel 2, lobyte/hibyte, mode 3, bin
	out 0x43, al		; send command to PIT
	mov ax, 1193		; 1193180 / 1000 (freq) == PIT frequency divisor
	out 0x42, al		; set low byte of divisor
	shr ax, 8
	out 0x42, al		; set high byte of divisor
	
	in al, 0x61		; get the kbd controller byte
	mov bl, al
	or al, 0x03		; toggle bits 1 and 0 on
	cmp al, bl
	je .end			; don't set if already set
	out 0x61, al		; set the PC speaker on
.end:	; speaker is beeping
	sti			; enable interrupts
	xor ah, ah		; get keystroke (blocking)
	int 0x16
	in al, 0x61		; get the kbd controller byte
	and al, 0xfc		; toggle bits 1 and 0 off
	out 0x61, al		; set the PC speaker off

	cli
.halt:	hlt
	jmp .halt

	;
	; TODO: go into protected mode and turn on A20 line to load more, higher
	; TODO: int 0x15, ax = 0xe820 advanced memory detection.
	; TODO: proper GDT and IDT
	; TODO: FAT (12, 16, or 32?) boot partition
	; TODO: paging?
	;
a20:	call detect_a20
	push ax
	push word 0x01
	call print_hex
	add sp, 4

; setup protected mode
	; interrupts already disabled
	lgdt [gdtinfo]		; load gdt
	mov eax, cr0		; switch to pmode by setting pmode bit
	or al, 1
	mov cr0, eax
	jmp $ + 2		; tell 386/486 to not crash
	mov ax, 0x10		; select descriptor 1 (data descriptor)
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax

	jmp 0x08:0x1000		; set code descriptor, jump to kernel

	; and al, 0xfe		; back to realmode...
	; mov cr0, eax		; by toggling bit again

	; pop ds			; get back old segment
	; sti			; interrupts ok

;;;;;;;; START EXTENDED SUBROUTINES ;;;;;;;;

; prints the first parameter number of words on stack in hex to the display
print_hex:
	push bp
	mov bp, sp

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
	rol   dx, 4      	; rotate so that low nybble of the byte is used
	mov   ax, 0x0e0f	; ah is teletype output,
				; al is mask for low nybble
	and   al, dl		; get low nybble of dl in al
	add   al, 0x90		; convert al to ASCII hex char
	daa			; uses "packed BCD addition," idk how
	adc   al, 0x40
	daa
	int   0x10

	loop .digit

	mov al, 0x20		; separate words with spaces
	int   0x10

	mov cx, di
	loop .words

	pop bp
	ret

; enable the A20 line: compare a low byte with 1 MiB higher to check mem wrap
detect_a20:
	pushf			; save flags
	xor ax, ax
	mov es, ax		; low segment
	not ax
	mov ds, ax		; high segment
	mov di, 0x0500		; low offset
	mov si, 0x0510		; high offset

	mov al, byte [es:di]	; save old values
	push ax
	mov al, byte [ds:si]
	push ax

	mov byte [es:di], 0x00
	mov byte [ds:si], 0xFF

	cmp byte [es:di], 0xFF	; if same as [ds:si], then it wraps

	pop ax			; restore old values
	mov byte [ds:si], al
	pop ax
	mov byte [es:di], al

	xor ax, ax
	je .exit
	inc ax

.exit:	popf			; load old flags
	ret

;;;;;;;; END EXTENDED SUBROUTINES ;;;;;;;;

;;;;;;;; START EXTENDED DATA ;;;;;;;;

disk:	db 0
part:	dw 0

gdtinfo:
	dw gdt_end - gdt - 1	; last byte in table
	dd gdt			; start of table

gdt:		dd 0,0		; entry 0 is always unused
flatdesc:	db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
gdt_end:

dap:				; disk address packet for int 0x13, ah=0x42
.size:		db 0x10		; size: 0x10
.reserved:	db 0x00		; reserved byte (0x00)
.blocks:	dw 0x0010	; blocks to transfer (8K)
.transbuf:	dd 0x00000000	; segment 0x0000, offset 0x0000
.startblk:	dq 0		; starting absolute block number

;;;;;;;; END EXTENDED DATA ;;;;;;;;

	align 512		; fill remaining sector with nops

;;;;;;;; END BOOTLOADER CODE ;;;;;;;;


