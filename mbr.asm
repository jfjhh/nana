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
	je ext			; CHS sucks, so just do extended read anyway
	add si, 0x10
	loop .booti

	mov word [dap.startblk], 1	; no partitions, so try sector 1
	mov word [dap.startblk+2], 0
	jmp ext.read

ext:	; an extended read is required
	mov ax, [si+8]		; first word of LBA sector
	mov [ds:dap.startblk], ax
	mov ax, [si+10]		; second word of LBA sector
	mov [ds:dap.startblk+2], ax

.read:	mov ah, 0x42		; extended read
	mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov si, dap		; ds is 0, so ds:si is right disk address packet
	int 0x13
	jnc ok

	; show error with extended read
	mov bx, 0x4f00		; red bg, white text, blanks
	mov dx, diskmsg		; error message
	call print
	jmp halt

ok:	mov si, [ds:part]	; ds:si points to boot part entry in mbr
	mov dl, [ds:disk]	; ensure dl is the disk number, just in case

	jmp destseg:destoff	; jump to loaded VBR

halt:	cli
.loop:	hlt
	jmp .loop

;;;;;;;; END CODE ;;;;;;;;

;;;;;;;; START SUBROUTINES ;;;;;;;;

; print a message to the screen with video memory manipulation
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

diskmsg:	db "Cannot read disk!", 0

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

	times 436-($-$$) db 0	; fill sector with zeros

diskid:	db "OSMomoDisk"		; disk identifier
parts:	times 64 db 0x00	; partition table

	db 0x55			; valid boot sector magic word
	db 0xaa

;;;;;;;; END BOOTSECTOR ;;;;;;;;

