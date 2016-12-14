;
; Custom PXE boot code for OSMomo.
; Copyright (C) 2016  Alex Striff
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

[bits 16]
[org 0x7c00]			; add to offsets

;;;;;;;; START NETWORK BOOTSTRAP PROGRAM ;;;;;;;;

;;;;;;;; JUMP TO MAIN CODE ;;;;;;;;
jmp start
;;;;;;;; JUMP TO MAIN CODE ;;;;;;;;

;;;;;;;; START DATA ;;;;;;;;

; PXENV+ Structure
pxenv_struct:
; .signature:	times 8 db 0	; contains "PXENV+"
; .version:	dw 0		; MSB: maj. ver.; LSB: min. ver.

; .length:	db 0		; length of structure
; .checksum:	db 0		; checksum of structure

; .rmentry:	dd 0		; far ptr to real mode PXE API entry point
; .pmoffset:	dd 0		; offset to pmode PXE entry point (prefer !PXE)

; .pmselector:	dw 0		; nobody cares about these fields;
; .stackseg:	dw 0		; refer to the spec.
; .stacksize:	dw 0
; .bc_codesec:	dw 0
; .bc_codesize:	dw 0
; .bc_dataseg:	dw 0
; .bc_datasize:	dw 0
; .undi_dataseg:	dw 0
; .undi_datasize:	dw 0
; .undi_codeseg:	dw 0
; .undi_codesize:	dw 0

; .pxeptr:	dd 0		; far ptr to the !PXE structure, iff it exists

.seg:		dw 0		; saved PXENV+ segment
.off:		dw 0		; saved PXENV+ offset

; !PXE structure or PXE Entry Structure
pxe_ent_struct:
; .signature:	times 4 db 0	; contains "!PXE"

; .length:	db 0		; length of structure
; .checksum:	db 0		; checksum of structure

; .revision:	db 0		; contains 0
; .reserved:	db 0		; contains 0

; .undi_romid:	dd 0		; far ptr to UNDI ROM ID structure
; .bc_romid:	dd 0		; far ptr to BC ROM ID structure

; .rmentry:	dd 0		; far ptr to real mode PXE API entry point
; .pmentry:	dd 0		; far ptr to pmode PXE API entry point

; nobody cares about the other fields;
; refer to the spec.

.ptr:		dd 0		; far ptr to the !PXE structure

; strings
msg:	db 0x03, " PXE Momo ", 0x03
.len:	equ $ - msg
apmmsg:	db " APM Error: "
.len:	equ $ - apmmsg

;;;;;;;; END DATA ;;;;;;;;

;;;;;;;; START CODE ;;;;;;;;

start:
	; save NBP initial machine state
	mov [pxenv_struct.seg], es
	mov [pxenv_struct.off], bx
	pop dx			; ss:sp + 4 points to the !PXE structure
	pop cx
	mov [pxe_ent_struct.ptr], cx
	mov [pxe_ent_struct.ptr + 2], dx

	; clear screen
	mov ax, 0x0600		; scroll up window; entire window
	mov bh, 0x0d		; pink on black bottom fill attribute
	xor cx, cx		; top left row and column (ch,cl)
	mov dx, 0x154f		; bottom right row and column (dh,dl)
	int 0x10

	; print message
	mov ax, 0x1301		; write string, update cursor after write
	mov bx, 0x000d		; first page, pink on black attr
	mov cx, msg.len		; length of string
	mov dh, 1		; second row (top margin)
	mov dl, (80 - msg.len) / 2	; centered string
	xor bp, bp		; es:bp is the string to write
	mov es, bp
	mov bp, msg
	int 0x10

oldpxe:	; uses PXENV+ structure

	; verify PXENV+ structure signature
	mov bx, word [pxenv_struct.seg]	; verify signature
	mov es, bx
	mov bx, word [pxenv_struct.off]
	cmp dword [es:bx], "PXEN"
	jne .err
	cmp word [es:bx + 4], "V+"
	jne .err
	
	; verify PXENV+ structure checksum
	xor ax, ax
	xor cx, cx
	mov cl, [es:bx + 8]	; get length for checksum
	xor si, si
.cksum:	add al, [es:bx + si]
	inc si
	cmp si, cx
	jb .cksum
	test al, al
	jnz .err

	; check PXE version
	cmp word [es:bx + 6], 0x0201
	jae newpxe

.err:
	push dx
	push ax
	push dword [es:bx]	; 24 words
	push dword [es:bx + 4]
	push dword [es:bx + 8]
	push dword [es:bx + 12]
	push dword [es:bx + 16]
	push dword [es:bx + 20]
	push dword [es:bx + 24]
	push dword [es:bx + 28]
	push dword [es:bx + 32]
	push dword [es:bx + 36]
	push dword [es:bx + 40]
	push dword [es:bx + 44]
	push word 26
	call print_hex
	add sp, 52
	jmp apm_off


newpxe:	; !PXE structure

	; TODO: verify !PXE structure signature
	; mov ax, word far [pxe_ent_struct.ptr]	; verify signature
	cmp ax, "!P"
	jne .err
	cmp bx, "XE"
	jne .err

	; TODO: verify !PXE structure checksum
	xor ax, ax
	xor cx, cx
	mov cl, [es:bx + 8]	; get length for checksum
	xor si, si
.cksum:	add al, [es:bx + si]
	inc si
	cmp si, cx
	jb .cksum
	test al, al
	jnz .err

	; get keystroke
	xor ax, ax		; get keystroke (blocking)
	int 0x16
	jmp apm_off

.err:

apm_off:
	; APM power off
	mov ax, 0x5300		; APM installation check
	xor bx, bx		; device id: APM BIOS
	int 0x15
	jc .err
	cmp ax, 0x0100		; APM 1.0 cannot set device 1 to state 3
	jle .err

	mov ax, 0x5301		; APM real-mode interface connect
	xor bx, bx		; device id: APM BIOS
	int 0x15
	jc .err

	mov ax, 0x5308		; APM change power management state
	mov bx, 0x0001		; all devices
	mov cx, 0x0001		; power management on
	int 0x15
	jc .err

	mov ax, 0x5307		; APM set power state
	mov bx, 0x0001		; all devices
	mov cx, 0x0003		; off
	int 0x15
	jc .err

.err:	push ax			; save error code

	; print message
	mov ax, 0x1301		; write string, update cursor after write
	mov bx, 0x0004		; first page, pink on black attr
	mov cx, apmmsg.len	; length of string
	mov dh, (25 / 2)	; centered row
	mov dl, (80 - apmmsg.len) / 2	; centered string
	xor bp, bp		; es:bp is the string to write
	mov es, bp
	mov bp, apmmsg
	int 0x10

	; print ax
	; ax already on stack
	push word 1
	call print_hex
	add sp, 4

	; get keystroke
	xor ax, ax		; get keystroke (blocking)
	int 0x16

reset_vec:
	mov word [0x0472], 0	; cold reboot
	jmp 0F000h:0FFF0h	; reset vector address

halt:	cli
.loop:	hlt
	jmp .loop

;;;;;;;; END CODE ;;;;;;;;

;;;;;;;; START SUBROUTINES ;;;;;;;;

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

;;;;;;;; END SUBROUTINES ;;;;;;;;

;;;;;;;; END NETWORK BOOTSTRAP PROGRAM ;;;;;;;;

