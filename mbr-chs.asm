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
	mov ss, ax		; stack starts at segment 0
	mov sp, 0x7c00		; 0x7600 past code start, after relocation,
				; making the stack 29.5KiB bytes in size,
				; allowing more room to load code in low mem

	; do relocation of MBR to lower memory, to allow for another stage
	mov cx, 256		; 256 words in 512 bytes
	mov si, 0x7c00		; src
	mov di, 0x0600		; dest
	rep movsw		; copy the memory

	cli			; no interrupts
	jmp 0x0000:newmem	; enforce new cs:ip, goto new location

newmem:	sti			; interrupts ok
	mov [ds:disk], dl	; save disk number from BIOS

	;==========================================;
	; commented out sections are for chainloading a VBA from a bootable
	; partition table entry, rather than the area immediately after the MBR.
	;==========================================;
	;
	; xor si, si
	; mov si, 0x07be		; first partition entry boot flag byte address
	; mov cx, 4		; try 4 partitions
; .booti:	mov al, [si]		; Nth partition entry boot flag byte
	; cmp al, 0x80
	; je bootp
	; add si, 0x10
	; loop .booti

	; ; show error: no bootable partitions found
	; mov bx, 0x4f20		; red bg, white text, spaces
	; mov dx, msg.booti	; error message
	; call print
	; jmp halt

bootp:	; calculate CHS, then read the bootable partitions
	cmp byte [ds:disk], 0x80	; hard drive
	jb .calc		; use defaults for emulated floppy

	xor ax, ax		; es:di == 0x0000:0x0000
	mov es, ax		; to guard against BIOS bugs
	mov di, ax
	mov ah, 0x08		; get drive parameters
	mov dl, [ds:disk]
	int 0x13
	jc .calc		; just try with default values

	dec dh			; dh is heads - 1
	mov [ds:heads], dh

	and cl, 0x3f		; sectors_per_track, low bits 5-0
	mov [ds:sec_per_track], cl

.calc:
	; mov ax, [si+0x08]	; LBA of VBA of the found bootable partition
	; mov [ds:logsec], ax
	;==========================================;

	; sector == log_sec % SECTORS_PER_TRACK
	; head   == (log_sec / SECTORS_PER_TRACK) % HEADS
	mov ax, [ds:logsec]	; logical sector number
	xor dx, dx		; dx is high part of dividend (== 0)
	mov bx, [ds:sec_per_track]	; divisor
	div bx			; do the division
	mov [ds:sec], dx	; sector is the remainder
	and ax, 1		; same as mod by HEADS==2 (slight hack)
	mov [ds:head], ax

	; track = log_sec / (SECTORS_PER_TRACK*HEADS)
	mov ax, [ds:logsec]	; logical sector number
	xor dx, dx		; dx is high part of dividend
	mov bx, [ds:sec_per_track]	; divisor
	shl bx, 1		; but divisor times 2
	div bx			; do the division
	mov [ds:track], ax	; track is quotient

	; do the BIOS interrupt
.try:	mov ax, [ds:destseg]
	mov es, ax		; dest segment goes in es
	mov ax, 0x02 << 8 | sectors	; read sectors to memory, num sectors
	mov bx, [ds:track]	; track number...
	mov ch, bl		; goes in ch
	mov bx, [ds:sec]	; sector number...
	mov cl, bl		; goes in cl...
	inc cl			; but it must be 1-based, not 0-based
	mov bx, [ds:head]	; head number...
	mov dh, bl		; goes in dh
	mov dl, [ds:disk]	; boot drive number
	mov bx, [ds:destoff]	; offset (es:bx points to buffer)

	int 0x13
	jnc .ok

	; error
	push ax
	mov bx, 0x4f20		; red bg, white text, spaces
	mov dx, msg.chsread	; CHS read error message
	call print
	pop ax

	dec byte [ds:tries]
	jnz .try
	jmp halt		; cannot read, give up

.ok:	mov bx, 0x2f20		; green bg, white text, spaces
	mov dx, msg.okread	; ok message
	; call print

	jmp 0x0000:0x7c00	; jump to loaded code

;;;;;;;; END CODE ;;;;;;;;

;;;;;;;; START SUBROUTINES ;;;;;;;;

; prints an error message from a table
print:	; clear and attribute screen with video memory manipulation
	; bp is not set then used across a call, so clobbering is ok
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
	xor bx, bx		; zero bx
	mov bx, msg.off		; offset from start of video memory
	mov cx, msg.len		; length of msg
.msg:	mov di, cx
	shl di, 1		; mult 2 for text, not attr
	mov si, cx
	mov al, [bp+si-1]	; src byte
	mov [es:bx+di], al	; to dest memory
	loop .msg

	ret

;;;;;;;; END SUBROUTINES ;;;;;;;;

;;;;;;;; START DATA ;;;;;;;;

msg:
.len:		equ 42
.off:		equ ((18*80 + 80/4 + (80 - .len) / 2) / 2) * 2	; centering
.booti:		db "   ERROR: No bootable partitions found!   "
.chsread:	db "ERROR: Failed normal (CHS) BIOS disk read!"
.okread:	db "BIOS (CHS) Read Sectors to Memory success!"


;==========================================;
; commented out sections are for chainloading a VBA from a bootable
; partition table entry, rather than the area immediately after the MBR.
; sec_per_track, heads, and cylinders are given the default floppy values, in
; case the int 0x13 Get Drive Parameters fails on a floppy.
;==========================================;
; logsec:		db 1
; sec_per_track:	db 18
; heads:		db 2
; head:		dw 0
; track:		dw 0
; sec:		dw 0
;==========================================;
; for static reads of predetermined sectors on disk.
logsec:		dw 1
sec_per_track:	dw 18
heads:		dw 2
head:		dw 0
track:		dw 0
sec:		dw 1
;==========================================;
sectors:	equ 8		; USB drive may emulate 1.44 floppy; only 4KiB
destseg:	dw 0x0000
destoff:	dw 0x7c00

disk:		db 0x80
tries:		db 5

;;;;;;;; END DATA ;;;;;;;;

	times 432-($-$$) db 0	; fill with zeroes (some BIOSes require it)

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

