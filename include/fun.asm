;
; Fun in the bootloader? (Non-essential showing off).
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

; draw the splash screen bitmap
splash:	mov ax, 0x0013		; set video mode, 320x200 256-color VGA mode
	int 0x10
	jc error
	mov ax, 0x0500		; set active display page, page to display
	int 0x10
	jc error

	; clear and attribute screen with video memory manipulation
	mov ax, 0xa000		; 0x000a0000 is video memory in VGA
	mov es, ax		; set segment
	mov bl, 0x3e		; as close as VGA can get to *the* top pink
	mov cx, (320 * 200)	; pixels 640x480 vga video display
.fill:	mov si, cx
	mov byte [es:0+si-1], bl	; write to video memory
	loop .fill

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
	jc error

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

; "utilize" the PC speaker (piezotransducer)
beep:	
	; a note's frequency is (2^(n/12) * 440) Hz, where n is the number of
	; half-steps the note is above A4
	cli			; disable interrupts
	mov al, 10110110b	; cmd: channel 2, lobyte/hibyte, mode 3, bin
	out 0x43, al		; send command to PIT
	mov ax, 9108		; 1193180 / 1000 (freq) == PIT frequency divisor
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
	jc error
	in al, 0x61		; get the kbd controller byte
	and al, 0xfc		; toggle bits 1 and 0 off
	out 0x61, al		; set the PC speaker off

	jmp halt

