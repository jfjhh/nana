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
	; use values from MBR loading
	; xor ax, ax		; make it zero
	; mov ds, ax		; ds = 0
	; mov es, ax		; es = 0
	; mov ss, ax		; stack starts at segment 0
	; mov sp, 0x9c00		; 0x2000 past code start,
	; 			; making the stack 8KiB in size
	mov [ds:disk], dl	; save disk number
	mov [ds:part], si	; save partition offset

	; clear and attribute screen with video memory manipulation
clear:	push es
	mov ax, 0xb800		; 0x000b8000 is video memory in EBA
	mov es, ax		; set segment
	mov bx, 0x5f00		; pink bg, white text, NUL chars
	mov cx, 80 * 25		; chars in a text video display
.loop:	mov si, cx
	shl si, 1
	mov word [es:0+si-2], bx	; write to video memory
	loop .loop
	pop es

	; setup display color, cursor, etc.
	mov ah, 0x0b	; BIOS interrupt set bg color
	mov bx, 0x0005	; pink bg
	int 0x10

	mov ah, 0x01		; setup cursor
	mov cx, 0x401f		; fast blink, block cursor: 0100 0000 0001 1111b
	int 0x10

	; os banner
	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, strattr		; attribute
	mov cx, os_banner_len	; length
	mov dx, 0x0100 | (80 - os_banner_len) / 2	; row 0, centered
	mov bp, os_banner	; es:bp is string
	int 0x10

lowmem:	; lowmem detection
	; print message
	mov ah, 0x13		; write string
	mov al, 00000001b	; update cursor, text only
	xor bh, bh		; page 0
	mov bl, strattr		; attribute
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
	mov bl, strattr		; attribute
	mov cx, lowmem_err_len	; length
	mov dx, 0x0210		; row 2, col 16
	mov bp, lowmem_err	; es:bp is string
	int 0x10
.end:

	; read more code from the hidden sectors
	mov si, [ds:part]	; get partition entry offset
	mov ax, [ds:si+8]	; first word of VBR sector
	inc ax			; skip over the VBR (this sector)
	mov [dap.startblk], ax
	mov ax, [ds:si+10]	; second word of VBR sector
	mov [dap.startblk+2], ax
	
	mov ah, 0x42		; extended read
	mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov si, dap		; ds is 0, so ds:si is right disk address packet
	int 0x13
	jnc .ok

.ok:	jmp 0x0000:after_bootsector

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

;;;;;;;; START STRINGS ;;;;;;;;

strattr:	equ 0x5f	; pink bg, white text
errattr:	equ 0x5c	; pink bg, light red text

os_banner:	db " Unnamed ", 0x02, "S "
os_banner_len:	equ $ - os_banner

lowmem_msg:	db "Low Mem KiB: "
lowmem_msg_len:	equ $ - lowmem_msg

lowmem_err:	db "ERR!"
lowmem_err_len:	equ $ - lowmem_err

;;;;;;;; END STRINGS ;;;;;;;;

;;;;;;;; START TABLES ;;;;;;;;

; sec_per_track:	equ 18
; heads:		equ 2
; cylinders:	equ 80

; head:		dw 0
; track:		dw 0
; sec:		dw 0
; tries:		db 3
disk:           db 0
part:           dw 0

dap:				; disk address packet for int 0x13, ah=0x42
.size:		db 0x10		; size: 0x10
.reserved:	db 0x00		; reserved byte (0x00)
.blocks:	dw 0x0010	; blocks to transfer (8K)
.transbuf:	dd 0x00007e00	; segment 0x0000, offset 0x7e00
.startblk:	dq 2049		; starting absolute block number

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
	; TODO: paging?
	; TODO: proper GDT and IDT
	; TODO: FAT
	;

	call detect_a20
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

	; times 508-($-after_bootsector) nop	; fill remaining with nops
	align 512		; fill remaining sector with nops

;;;;;;;; END AFTER BOOTSECTOR CODE ;;;;;;;;

