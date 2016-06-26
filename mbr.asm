;
; Custom, minimalistic MBR.
;
; Alex Striff.
;

[bits 16]
[org 0x0600]			; add to offsets

;;;;;;;; START BOOTSECTOR ;;;;;;;;

;;;;;;;; START CODE ;;;;;;;;

start:
	; setup segments and stack
	xor ax, ax		; make it zero
	mov ds, ax		; ds = 0
	mov es, ax		; es = 0
	mov ss, ax		; stack starts at segment 0
	mov sp, 0x7c00		; 0x7600 past code start, after relocation,
				; making the stack 29.5KiB bytes in size,
				; allowing more room to load code in low mem
	cli			; no interrupts
	mov cx, 256		; 256 words in 512 bytes
	mov si, 0x7c00		; src
	mov di, 0x0600		; dest
	rep movsw		; copy the memory

	jmp 0x0000:newmem	; enforce new cs:ip, goto new location

newmem:
	sti			; interrupts ok
	mov [ds:disk], dl	; save disk number from BIOS

	xor si, si
	mov si, 0x07be		; first partition entry boot flag byte address
	mov cx, 4		; try 4 partitions
.booti:	mov al, [si]		; Nth partition entry boot flag byte
	cmp al, 0x80
	mov [ds:part], si	; save the partition entry offset
	; je bootp
	je ext			; CHS sucks, so just do extended read anyway
	add si, 0x10
	loop .booti

	mov word [dap.startblk], 1	; no partitions, so try sector 1
	mov word [dap.startblk+2], 0
	jmp ext.read

; bootp:	; read the bootable partitions
; 	; check if it can be done by CHS alone
; 	mov ax, word [si+8]
; 	add ax, word [dap.blocks]
; 	cmp ax, 2812		; max sectors possible on USB emulating floppy
; 	ja ext

; 	; get drive info
; 	cmp byte [ds:disk], 0x80	; hard drive
; 	jb .calc		; use defaults for emulated floppy

; 	xor ax, ax		; es:di == 0x0000:0x0000
; 	mov es, ax		; to guard against BIOS bugs
; 	mov di, ax
; 	mov ah, 0x08		; get drive parameters
; 	mov dl, [ds:disk]
; 	int 0x13
; 	jc .calc		; just try with default values

; 	dec dh			; dh is heads - 1
; 	mov [ds:heads], dh

; 	and cl, 0x3f		; sectors_per_track, low bits 5-0
; 	mov [ds:sec_per_track], cl

; .calc:	; sector == log_sec % SECTORS_PER_TRACK
; 	; head   == (log_sec / SECTORS_PER_TRACK) % HEADS
; 	mov ax, [si+8]		; logical sector number
; 	xor dx, dx		; dx is high part of dividend (== 0)
; 	mov bx, [ds:sec_per_track]	; divisor
; 	div bx			; do the division
; 	mov [ds:sec], dx	; sector is the remainder

; 	xor bx, bx
; 	mov bl, [ds:heads]	; modulus by found heads
; 	xor dx, dx
; 	div bx
; 	mov [ds:head], dx

; 	; track = log_sec / (SECTORS_PER_TRACK*HEADS)
; 	mov ax, [si+8]		; logical sector number
; 	xor dx, dx		; dx is high part of dividend
; 	mov bx, [ds:sec_per_track]	; divisor
; 	shl bx, 1		; but divisor times 2
; 	div bx			; do the division
; 	mov [ds:track], ax	; track is quotient

; 	; do the BIOS interrupt
; .try:	mov ax, destseg
; 	mov es, ax		; dest segment goes in es
; 	mov ah, 0x02		; read sectors to memory
; 	mov al, [dap.blocks]	; num sectors
; 	mov bx, [ds:track]	; track number...
; 	mov ch, bl		; goes in ch
; 	mov bx, [ds:sec]	; sector number...
; 	mov cl, bl		; goes in cl...
; 	inc cl			; but it must be 1-based, not 0-based
; 	mov bx, [ds:head]	; head number...
; 	mov dh, bl		; goes in dh
; 	mov dl, [ds:disk]	; boot drive number
; 	mov bx, destoff		; offset (es:bx points to buffer)

; 	int 0x13
; 	jnc ok

; 	dec byte [ds:tries]
; 	jnz .try

ext:	; an extended read is required
	mov ax, [si+8]		; first word of LBA sector
	mov [ds:dap.startblk], ax
	mov ax, [si+10]		; second word of LBA sector
	mov [ds:dap.startblk+2], ax

.read:
	mov ah, 0x42		; extended read
	mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov si, dap		; ds is 0, so ds:si is right disk address packet
	int 0x13
	jnc ok

	; show error with extended read
	mov bx, 0x4f00		; red bg, white text, blanks
	mov dx, msg.errread	; error message
	call print
	jmp halt

ok:
	; clear and attribute screen with video memory manipulation
	push es
	mov ax, 0xb800		; 0x000b8000 is video memory in EBA
	mov es, ax		; set segment
	mov bx, 0x3f00		; cyan bg, white text, NUL chars
	mov cx, 80 * 25		; chars in a text video display
.loop:	mov si, cx
	shl si, 1
	mov word [es:0+si-2], bx	; write to video memory
	loop .loop
	pop es

	mov si, [ds:part]	; ds:si points to boot part entry in mbr
	mov dl, [ds:disk]	; ensure dl is the disk number, just in case

	jmp destseg:destoff	; jump to loaded VBR

;;;;;;;; END CODE ;;;;;;;;

;;;;;;;; START SUBROUTINES ;;;;;;;;

; clear and attribute screen with video memory manipulation
print:
	push bp
	mov bp, sp

	mov ax, 0xb800		; 0x000b8000 is video memory in EBA
	mov es, ax		; set segment
	; bx already has attr/char word (param)
	mov cx, 80 * 25		; chars in a text video display
.loop:	mov si, cx
	shl si, 1
	mov word [es:0+si-2], bx	; write to video memory
	loop .loop

	; show a message
	mov bp, dx		; src buffer address
	; si was zero'd by previous loop
	xor bx, bx
	xor cx, cx
.msg:	mov di, cx
	shl di, 1		; mult 2 for text, not attr
	mov si, cx
	mov al, [bp+si]		; src byte
	mov [es:bx+di], al	; to dest memory
	inc cx
	cmp al, 0
	jne .msg

	pop bp
	ret

;;;;;;;; END SUBROUTINES ;;;;;;;;

;;;;;;;; START DATA ;;;;;;;;

msg:
.errread:	db "Cannot read disk!", 0

; sec_per_track:	dw 18
; heads:		dw 2
; head:		dw 0
; track:		dw 0
; sec:		dw 0
; tries:		db 5
disk:		db 0
part:		dw 0

destseg:	equ 0x0000
destoff:	equ 0x7c00

dap:				; disk address packet for int 0x13, ah=0x42
.size:		db 0x10		; size: 0x10
.reserved:	db 0x00		; reserved byte (0x00)
.blocks:	dw 0x0010	; blocks to transfer (8K)
.transbuf:	dd 0x00007c00	; segment 0x0000, offset 0x7c00
.startblk:	dq 2048		; starting absolute block number

;;;;;;;; END DATA ;;;;;;;;

	times 432-($-$$) db 0	; fill sector with zeros

halt:	cli
.loop:	hlt
	jmp .loop

diskid:	db "OSMomoDisk"

part1:	times 16 db 0x00	; partition entry 1
part2:	times 16 db 0x00	; partition entry 2
part3:	times 16 db 0x00	; partition entry 3
part4:	times 16 db 0x00	; partition entry 4

	db 0x55			; valid boot sector magic word
	db 0xaa

;;;;;;;; END BOOTSECTOR ;;;;;;;;

