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

;;;;;;;; START BOOTLOADER CODE ;;;;;;;;

;;;;;;;; START MAIN CODE ;;;;;;;;

start:
	mov [ds:disk], dl	; save disk number
	mov [ds:part], si	; save MBR partition offset

lowmem:	; lowmem detection
	clc			; clear carry flag
	int 0x12		; request low memory size
	jc error		; the carry flag is set if it failed
	; ax is amount of contiguous memory in KB from 0
	cmp ax, 0x027f		; need 640 KiB (0x27f)
	jl error		; cannot boot without enough memory

vbe:	; gather information on VBE video modes
	mov di, vbeinfo.vbeoff	; the offset of the info block
	mov word [es:di+0], 0x4256	; "VB" (little endian)
	mov word [es:di+2], 0x3245	; "E2" (little endian)
	mov ax, 0x4f00		; get VBE info, with "VBE2" set for VBE 2.0 info
	int 0x10
	jc error
	cmp ax, 0x004f		; check success
	je .load		; continue

	push ax			; if not supported, show err code
	push word 1
	call print_hex
	add sp, 1
	jmp halt
	
	; load all the mode info
.load:	mov si, [vbeinfo.modeptr]
	mov di, vbeinfo.modeinfo

	cld			; make sure DF is forwards
.mode:	lodsw			; get the mode word [es:si]
	cmp ax, 0xffff		; check for end of list word
	je .end		; continue

	mov cx, 128		; zero the entry (256 bytes long for VBE 3.0)
.zero:	mov bx, cx
	dec bx
	shl bx, 1		; multiply by 2 because words
	mov word [es:di+bx], 0
	loop .zero

	mov cx, ax		; make it the word
	mov ax, 0x4f01		; get VBE mode info
	int 0x10		; entry stored at [es:di]
	jc error
	cmp ax, 0x004f		; check success
	jne .next		; just continue on if failed

	; si already incremented by lodsw
.next:	add di, 256		; offset to next entry
	jmp .mode

.end:
	sub si, [vbeinfo.modeptr]
	shr si, 1		; number of entries processed
	dec si			; disregard lodsw increment

%include "fun.asm"

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

;;;;;;;; END MAIN CODE ;;;;;;;;

;;;;;;;; START ERROR CODE ;;;;;;;;

okmsg:	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, stdattr		; attribute
	mov cx, general_ok.len	; length
	mov dx, 0x0200 | (80-general_ok.len) / 2 ; row 2, centered col
	mov bp, general_ok	; es:bp is string
	int 0x10
	jc error
	jmp halt

error:	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, errattr		; attribute
	mov cx, lowmem_err.len	; length
	mov dx, 0x0300 | (80-lowmem_err.len) / 2 ; row 3, centered col
	mov bp, lowmem_err	; es:bp is string
	int 0x10
	jmp halt

halt:	cli
.loop:	hlt
	jmp .loop

;;;;;;;; END ERROR CODE ;;;;;;;;

;;;;;;;; START EXTENDED SUBROUTINES ;;;;;;;;

; prints the first parameter number of words on stack in hex to the display
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

dlstr general_ok, 0x02, " OK ", 0x02
dlstr lowmem_err, "Not enough low memory! ", 0x02

stdattr:	equ 0x0a	; text mode video attr for normal messages
errattr:	equ 0x0c	; text mode video attr for error messages

; lowmem_err:	db "Not enough low memory!"
; .len:		equ $ - lowmem_err

vbeinfo:
.vbeoff:	equ 0x0700		; offset of VBE info block
.modeptr:	equ .vbeoff + 0x0e	; where in the block the modes are
.modeinfo:	equ .vbeoff + 512	; VBE 2.0 block is 512 bytes long

disk:		db 0
part:		dw 0

gdtinfo:
.lastbyte	dw gdt_end - gdt - 1	; last byte in table
.start		dd gdt			; start of table

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

