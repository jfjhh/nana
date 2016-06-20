;
; Custom Bootloader.
;
; Alex Striff.
;

[bits 16]
[org 0x7c00]			; add to offsets

;;;;;;;; START MACROS ;;;;;;;;

%macro newline 0

	mov ax, 0x0e0d		; teletype output, ASCII CR
	int 0x10
	mov al, 0x0a		; ASCII LF
	int 0x10

%endmacro

;;;;;;;; END MACROS ;;;;;;;;

 ;;;;;;;; START BOOTSECTOR ;;;;;;;;

;;;;;;;; START CODE ;;;;;;;;

start:
	; setup segments and stack
	xor ax, ax		; make it zero
	mov ds, ax		; ds = 0
	mov es, ax		; es = 0
	mov ss, ax		; stack starts at segment 0
	mov sp, 0x9c00		; 0x2000 past code start,
				; making the stack 8KiB in size
	; mov sp, 0x7fff		; 0x03ff past code start,
	; 			; making the stack 1023 bytes in size

	; clear and attribute screen with video memory manipulation
clear:	push es
	mov ax, 0xb800		; 0x000b8000 is video memory in EBA
	mov es, ax		; set segment
	mov bx, 0x0a20		; blank space, light gray text
	mov cx, 80 * 25		; chars in a text video display
.loop:	mov si, cx
	shl si, 1
	mov word [es:0+si-2], bx	; write to video memory
	loop .loop
	pop es

	; setup display color, cursor, etc.
	mov ah, 0x0b	; BIOS interrupt set bg color
	mov bx, 0x0009	; blue bg
	int 0x10

	mov ah, 0x01		; setup cursor
	mov ch, 01000010b	; erratic blink, topmost scan line 2
	or cl, 00000011b	; bottom scan line 3
	int 0x10

	; os banner
	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, [ds:strattr]	; attribute
	mov cx, os_banner_len	; length
	mov dx, 0x0100 | (80 - os_banner_len) / 2	; row 0, centered
	mov bp, os_banner	; es:bp is string
	int 0x10

lowmem:	; lowmem detection
	; print message
	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, [ds:strattr]	; attribute
	mov cx, lowmem_msg_len	; length
	mov dx, 0x0200		; row 2, col 0
	mov bp, lowmem_msg	; es:bp is string
	int 0x10

	; do the BIOS interrupt
	clc			; clear carry flag
	int 0x12		; request low memory size
	jc .err			; the carry flag is set if it failed

	; ax is amount of contiguous memory in KB from 0.
	push ax
	push word 1
	call print_hex
	add sp, 4		; size word + print word = 2 words = 4 bytes
	jmp .end

.err:	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, [ds:strattr]	; attribute
	mov cx, lowmem_err_len	; length
	mov dx, 0x0210		; row 2, col 16
	mov bp, lowmem_err	; es:bp is string
	int 0x10
.end:

; read more bootloader code from disk into memory
	push word 0x8000	; offset
	push word 0x0000	; segment
	push word 1		; number of sectors to read
	push word 1		; logical sector
	call read_sectors
	add sp, 8

; print some of the copied sector
	newline

	; first dword
	push dword [0x8000]
	push word 2
	call print_hex
	add sp, 6

	; last dword
	push dword [0x07fb]
	push word 2
	call print_hex
	add sp, 6

	jmp 0x8000

; if kernel exits, do as much nothing as possible
halt:	cli			; no interrupts
.loop:	hlt			; halt the CPU until next interrupt
	jmp .loop		; even if hlt is interrupted, hlt again

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

; read sectors from disk into memory
; - first word param is the logical sector number
; - second word param is the number of sectors to read
; - third param is the segment to read to
; - fourth param is the offset to read to
read_sectors:
	push bp			; [bp+4] is log sec num, [bp+6] is num sec,
	mov bp, sp		; [bp+8] is segment, [bp+10] is offset

	newline
	push dword [bp+4]
	push dword [bp+8]
	push word 4
	call print_hex
	add sp, 10

	; sector = log_sec % SECTORS_PER_TRACK
	; head   = (log_sec / SECTORS_PER_TRACK) % HEADS
	mov ax, [bp+4]		; logical sector number
	xor dx, dx		; dx is high part of dividend (== 0)
	mov bx, sec_per_track	; divisor
	div bx			; do the division
	mov [ds:sec], dx	; sector is the remainder
	and ax, 1		; same as mod by HEADS==2 (slight hack)
	mov [ds:head], ax

	; track = log_sec / (SECTORS_PER_TRACK*HEADS)
	mov ax, [bp+4]		; logical sector number
	xor dx, dx		; dx is high part of dividend
	mov bx, sec_per_track*2	; divisor
	div bx			; do the division
	mov [ds:track], ax	; track is quotient

	; do the BIOS interrupt
.try:	mov ax, [bp+8]
	push es
	mov es, ax		; dest segment goes in es
	mov ah, 0x02		; read sectors to memory
	mov al, [bp+6]		; number of sectors
	mov bx, [ds:track]	; track number...
	mov ch, bl		; goes in ch
	mov bx, [ds:sec]	; sector number...
	mov cl, bl		; goes in cl...
	inc cl			; but it must be 1-based, not 0-based
	mov bx, [ds:head]	; head number...
	mov dh, bl		; goes in dh
	mov dl, 0x80		; hard code drive number to be 0x80 (hard drive)
	mov bx, [bp+10]		; offset (es:bx points to buffer)

	int 0x13
	pop es
	jnc .ok

	; error
	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, [ds:errattr]	; attribute
	mov cx, lowmem_err_len	; length
	mov dx, 0x0301		; row 3, col 0
	mov bp, lowmem_err	; es:bp is string
	int 0x10

	; print status
	shr ax, 8
	push ax
	push word 1
	call print_hex
	add sp, 4

	dec byte [cs:tries]
	jnz .try

	jmp halt		; cannot read, give up

.ok:
	pop bp
	ret

;;;;;;;; END SUBROUTINES ;;;;;;;;

;;;;;;;; START STRINGS ;;;;;;;;

strattr:	db 0x0f	; normal bg color, light green text
errattr:	db 0xf4	; pink bg color, red text

os_banner:	db " Unnamed ", 0x02, "S "
os_banner_len:	equ $ - os_banner

lowmem_msg:	db "Low Mem KiB: "
lowmem_msg_len:	equ $ - lowmem_msg

lowmem_err:	db "ERR!"
lowmem_err_len:	equ $ - lowmem_err

;;;;;;;; END STRINGS ;;;;;;;;

;;;;;;;; START TABLES ;;;;;;;;

sec_per_track:	equ 18
heads:		equ 2
cylinders:	equ 80

head:		dw 0
track:		dw 0
sec:		dw 0
tries:		db 3

gdtinfo:
	dw gdt_end - gdt - 1	; last byte in table
	dd gdt			; start of table

gdt:		dd 0,0		; entry 0 is always unused
flatdesc:	db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
gdt_end:

;;;;;;;; END TABLES ;;;;;;;;

	; times 510-($-$$) db 0	; fill sector w/ 0's
	; 			; required by some BIOSes
	times 510-($-$$) nop	; fill sector with nops
	db 0x55			; valid boot sector magic word
	db 0xaa

;;;;;;;; END BOOTSECTOR ;;;;;;;;

;;;;;;;; START AFTER BOOTSECTOR CODE ;;;;;;;;

after_bootsector:		; should start at 0x7e00
	mov ax, 0x0e02		; teletype output, smiley
	int 0x10

	cli
.loop:	hlt
	jmp .loop

	;
	; TODO: go into protected mode, turn on A20 line, to load more stuff
	; TODO: int 0x15, ax = 0xe820 advanced memory detection.
	; TODO: FAT
	;

; setup unreal mode (enable 32-bit offsets, but not beyond 1MiB mem)
	; cli			; no interrupts
	; push ds			; save real mode

	; lgdt [gdtinfo]		; load gdt register

	; mov eax, cr0		; switch to pmode by...
	; or al, 1		; setting pmode bit
	; mov cr0, eax

	; jmp $ + 2		; tell 386/486 to not crash

	; mov bx, 0x08		; select descriptor 1
	; mov ds, bx		; 8h = 1000b

	; and al, 0xfe		; back to realmode...
	; mov cr0, eax		; by toggling bit again

	; pop ds			; get back old segment
	; sti			; interrupts ok

	times 512-($-after_bootsector) nop	; fill remaining with nops

;;;;;;;; END AFTER BOOTSECTOR CODE ;;;;;;;;

