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
	; mov sp, 0x9c00		; 0x2000 past code start,
	; 			; making the stack 8KiB in size
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
	je bootp
	add si, 0x10
	loop .booti

	; TODO: show error: no bootable partitions found
	mov bx, 0x4f03		; red bg, white text, hearts
	mov dx, err.booti	; error message
	call error
	jmp halt

	; read the bootable partitions
bootp:	; cross check CHS with LBA for small disks (>8GiB)
	; xor bx, bx		; zero bx
	; mov bl, [si+3]
	; shl bx, 2		; move cyl bits into bl
	; shr bl, 2		; fix sector bits
	; mov [ds:sec], al	; save sector number
	; mov bl, [si+2]		; low 8 bits of start cylinder number
	; mov [ds:track], bx	; save full cylinder number
	; mov bl, [si+1]		; move starting head to bl
	; mov [ds:head], bl	; save starting head

	; ; sector = log_sec % SECTORS_PER_TRACK
	; ; head   = (log_sec / SECTORS_PER_TRACK) % HEADS
	; mov ax, [si+10]		; logical sector number (low word)
	; xor dx, dx		; dx is high part of dividend (== 0)
	; mov bx, sec_per_track	; divisor
	; div bx			; do the division
	; cmp [ds:sec], dx	; sector is the remainder, compare with CHS
	; jne .ext		; no match
	; and ax, 1		; same as mod by HEADS==2 (slight hack)
	; cmp [ds:head], ax	; compare with CHS
	; jne .ext		; no match

	; ; track = log_sec / (SECTORS_PER_TRACK*HEADS)
	; mov ax, [si+10]		; logical sector number (low word)
	; xor dx, dx		; dx is high part of dividend
	; mov bx, sec_per_track*2	; divisor
	; div bx			; do the division
	; cmp [ds:track], ax	; track is quotient, compare with CHS
	; jne .ext		; no match

; ; .ext:	; DEBUGGING: ignore ext
	; ; read the VBR; this is a small drive, so use CHS: int 0x13, ah=0x02
	; mov di, 5		; try to read more than once
; .loop:	mov ax, 0x0201		; read sectors into memory, 1 sector
	; mov ch, [si+3]		; sector number and high 2 bits of cylinder
	; mov cl, [si+2]		; low 8 bits of start cylinder number
	; mov dh, [si+1]		; starting head
	; mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	; mov bx, 0x7c00		; es is 0, so just bx is destination address
	; int 0x13
	; jnc .ok

	; dec di			; try again
	; jnz .loop

	; ; TODO: show error with reading
	; mov bx, 0x4f01		; red bg, white text, hollow smileys
	; mov dx, err.chsread	; error message
	; jmp error

.ext:	; an extended read is required
	mov ax, [si+8]		; first word of LBA sector
	mov [dpkt.startblk], ax
	mov ax, [si+10]		; second word of LBA sector
	mov [dpkt.startblk+2], ax

	mov ah, 0x42		; extended read
	mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov si, dpkt		; ds is 0, so ds:si is right disk address packet
	int 0x13
	jnc .ok

	; show error with extended read
	mov bx, 0x4f02		; red bg, white text, smileys
	mov dx, err.extread	; error message
	call error
	jmp halt

.ok:	; ds:si points to boot part entry in mbr
	mov dl, [ds:disk]	; ensure dl is the disk number, just in case

	mov bx, 0x2f20		; green bg, white text, spaces
	mov dx, err.okread	; ok message
	call error

	jmp 0x0000:0x7c00	; jump to loaded VBR

error:	; clear and attribute screen with video memory manipulation
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

	; show an error message
	mov bp, dx		; src buffer address
	; si was zero'd by previous loop
	xor bx, bx		; zero bx
	mov bx, err.off		; offset from start of video memory
	mov cx, err.len		; length of msg
.msg:	mov di, cx
	shl di, 1		; mult 2 for text, not attr
	mov si, cx
	mov al, [bp+si-1]	; src byte
	mov [es:bx+di], al	; to dest memory
	loop .msg

	pop bp
	ret

;;;;;;;; END CODE ;;;;;;;;

;;;;;;;; START SUBROUTINES ;;;;;;;;

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

;;;;;;;; END SUBROUTINES ;;;;;;;;

;;;;;;;; START DATA ;;;;;;;;

err:
.len:		equ 42
.off:		equ ((18*80 + 80/4 + (80 - .len) / 2) / 2) * 2	; centered
.booti:		db "   ERROR: No bootable partitions found!   "
.chsread:	db "  ERROR: Failed normal (CHS) disk read!   "
.extread:	db "ERROR: Failed extended (>8 GiB) disk read!"
.okread:	db "OK: Extended read (>8 GiB) was successful!"

sec_per_track:	equ 18
heads:		equ 2
cylinders:	equ 80

head:		dw 0
track:		dw 0
sec:		dw 0

disk:		db 0

dpkt:				; disk packet for int 0x13, ah=0x42
.size:		db 0x10		; size: 0x10
.reserved:	db 0x00		; reserved byte (0x00)
.blocks:	dw 0x0001	; blocks to transfer
.transbuf:	dd 0x07c00000	; 0x7c00 offset, segment 0x0000
.startblk:	dq 0		; starting absolute block number

;;;;;;;; END DATA ;;;;;;;;

	times 442-($-$$) nop	; fill sector with nops

halt:	cli
.loop:	hlt
	jmp .loop

part1:	times 16 db 0x00	; partition entry 1
part2:	times 16 db 0x00	; partition entry 2
part3:	times 16 db 0x00	; partition entry 3
part4:	times 16 db 0x00	; partition entry 4

	db 0x55			; valid boot sector magic word
	db 0xaa

;;;;;;;; END BOOTSECTOR ;;;;;;;;

