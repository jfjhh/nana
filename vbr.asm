;
; Custom FAT reading VBR.
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
[org 0x7c00]			; add to offsets

;;;;;;;; START VOLUME BOOT SECTOR ;;;;;;;;

vbr:

%include "fat/bpb.inc"
%include "fat/ebr.inc"

;;;;;;;; START CODE ;;;;;;;;

start:	; Bootloader preconditions:
	; [ds:si] is the partition entry for the VBR
	; dl is the drive it was loaded from

	; setup segments and stack
	xor ax, ax		; make it zero
	mov es, ax		; for string comparison in reading FAT dir ents
	mov ss, ax		; stack starts at segment 0
	mov sp, 0x7c00		; just below code

	mov [ds:disk], dl	; save disk number
	mov [ds:part], si

read:	; read more code from the hidden sectors

	; find multiplier for FAT sectors from disk blocks (512 bytes)
	mov ax, [ds:vbr+11]	; nlo(num) is number of leading 1's (1110 => 3)
	sub ax, 512		; nlo(2^j - 2^k) == j - k for j > k > 0, nat.#s.
	mov cx, 0xFFFF		; and cx <- 2^nlo(above) is the multiplier, so:
.bytes:	shl ax, 1		; int 0x13 blocks == FAT blocks << cx
	inc cx			; with the (reasonable) assumption that FAT
	jnz .bytes		; blocks are 2^j | j > 9 (because 2^9 is 512)
	; cx is now the power of two of the multiplier (mult <- (1 << cx))
	mov [ds:fatmult], cl	; save the multiplier power of two (fits in cl)

	mov bx, [ds:vbr+22]	; sectors per FAT
	shl bx, cl		; multiply to get disk blocks from FAT sectors
	mov [dap.blocks], bx	; load a single FAT

	; pop si		; get partition entry offset
	mov ax, [ds:si+8]	; first word of VBR sector (reserved region)
	mov bx, [ds:vbr+14]	; the number of reserved sectors (from BPB)
	shl bx, cl		; multiply to get disk blocks from FAT sectors
	add ax, bx		; ax should now be the LBA of the FAT
	mov [dap.startblk], ax
	mov cx, [ds:si+10]	; second word of VBR sector
	mov [dap.startblk+2], cx

	mov ah, 0x42		; extended read
	; mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov si, dap		; ds is 0, so ds:si is right disk address packet
	int 0x13		; read the (first) FAT into memory
	jc .err

	mov ax, [dap.startblk]	; restore the FAT LBA
	mov bx, [dap.blocks]	; restore disk sectors per FAT
	xor cx, cx		; bx * cx is the offset from FATs to root dir
	mov cl, [ds:vbr+16]	; number of FATs
	dec cl			; e.g. 2 FATs means add 1 time to multiply by 2
	mov dx, bx		; save original bx
.rtoff:	add bx, dx		; manually multiply bx by cx
	loop .rtoff
	add ax, bx		; ax is now the first root dir sector
	mov [dap.startblk], ax
	push ax			; save to stack
	mov ax, [dap.transbuf]	; where the fat is loaded
	sub ax, 512		; move a block back to load a root dir block
	mov [dap.transbuf], ax	; where the root directory blocks are loaded

	; root_dir_blocks = ceil((root_entries_count*32)/(bytes_per_sector/512))
	xor ax, ax
	mov bx, [ds:vbr+17]	; number of directory entries
	mov cl, [ds:fatmult]	; get the multiplier: it adds to the right shift
	add cl, 4		; log_2(512/32) == log_2(2^9-2^5) == 4
	shrd bx, ax, cl		; (n*32)/512==(n*2^5)/2^9==n*2^-4==n>>4 (+ mult)
	mov cx, bx		; put the result in cx (for loop instruction)
	or ax, ax		; set ZF if applicable on ax (1's shrd from bx)
	jz .rtchg
	inc cx			; round up if there was any "remainder" in ax
.rtchg:	push cx			; save number of root dir disk blocks
	; cx is now the number of disk blocks in the root directory
	mov word [dap.blocks], 1	; only load a single block
.rtblk:	mov dl, [ds:disk]	; some BIOSes trash dx, so read the num again
	mov ah, 0x42		; extended read
	int 0x13		; read the root dir fat sector into memory
	jc .err

	push cx			; save the blocks left
	mov cx, 16		; 16, 32-byte entries per 512-byte block
.rtent:
	mov di, bootloadername	; set [es:di] to the bootloader's file name
	mov si, 16
	sub si, cx
	shl si, 5		; the current entry, counting from 0 up (* 32)
	cmp byte [ds:0+si+(fat-512)], 0	; fat - 512 is the block of entries
	je .err			; did not find the entry

	add si, fat-512		; ds:si points to entry now
	mov dx, cx		; save entry-iterative cx
	mov cx, 11		; 8.3 filename is 11 bytes
	repe cmpsb		; compare the filename for this entry
	jz .rtfnd		; found it!
	mov cx, dx		; restore cx

	loop .rtent
	pop cx

	inc dword [dap.startblk]	; read in the next block of entries
	loop .rtblk

.err:	mov ax, 0x1301		; write string, update cursor, text only
	xor bx, bx		; page 0
	mov bl, errattr		; attribute
	mov cx, load_err.len	; length
	mov dx, 0x0200 + (80 - load_err.len) / 2 ; row 2, col 16
	mov bp, load_err	; es:bp is string
	int 0x10
	jmp halt

; a: [ds:si] is the root directory entry of the bootloader
; b: word [ss:sp+2] is garbage
; c: word [ss:sp+4] is the number of disk blocks in the root directory
; d: word [ss:sp+6] is the number of disk blocks to the root directory
; so c+d == data region.
.rtfnd:
	; change DAP again
	add sp, 2		; fix stack
	mov ax, 0x7e00		; offset of where the file goes
	mov [dap.transbuf], ax
	pop ax			; root dir blocks
	pop bx			; blocks to root dir region
	add ax, bx		; the LBA of the data region
	mov [dap.startblk], ax	; save it (stack should be clear)
	push ax			; now the data region is on the stack
	mov al, [ds:vbr+13]	; sectors per cluster
	xor cx, cx
	mov cl, [ds:fatmult]
	push cx			; save power of two for blocks per cluster
	mov dx, 1		; start with 1 block
	shl dx, cl		; dx is the disk blocks per cluster
	mov [dap.blocks], dx
	
	; get starting cluster info
	sub si, 11		; repe cmpsb added 11 to the entry offset
	mov bx, [ds:0+si+0x1a]	; the starting cluster of the file
	mov ax, bx

	; address the stack with bp
	push sp
	pop bp

; a: word [ss:sp+0] is log_2(disk blocks per cluster)
; b: word [ss:sp+2] is the LBA of the data region
; so the LBA of cluster n is (b + (n << a))
.entnx:
	mov si, dap		; [ds:si] is the DAP for int 0x13, ah=0x42
	mov dx, bx		; save the active cluster
	shr ax, 1		; multiply by 1.5 (ax == bx) (FAT 12)
	add bx, ax
	mov ax, [ds:bx+fat]	; get the next cluster from the FAT
	test bx, 1		; even or odd entry? (because 12 bits)
	jnz .entlw
	shr ax, 4		; high 12 bits of entry
	jmp .entrd		; read the cluster
.entlw:	and ax, 0x0fff		; low 12 bits of entry
.entrd:	; read this cluster's data into memory
	; first_sector_of_cluster
	; = ((cluster - 2) * fat_boot->sectors_per_cluster) + first_data_sector
	mov bx, dx		; load the active cluster
	mov dx, [bp+2]		; the startblk
	mov cx, [bp+0]		; the power of two for blocks per cluster
	sub bx, 2		; cluster - 2 (ignore first two dir entries)
	shl bx, cl		; change cluster number to block number
	add dx, bx		; do data_region LBA + blocks to cluster_number
	mov [dap.startblk], dx	; update the startblk
	mov bx, ax		; save next cluster
	mov ah, 0x42		; extended read
	mov dl, [ds:disk]	; get the disk number
	int 0x13		; do the read
	jc .err

	mov ax, bx		; restore next cluster
	cmp ax, 0xff7		; marked bad cluster
	je .err			; error
	ja .ok			; >0xff7 means >=0xff8: last cluster
	
	mov bx, ax		; setup cluster number for next load
	mov cx, [dap.transbuf]	; where the cluster was loaded to
	mov dx, [dap.blocks]	; disk blocks per cluster
	shl dx, 9		; multiply blocks by 512 to get byte offset
	add cx, dx		; next cluster
	mov [dap.transbuf], cx	; update transbuf
	jmp .entnx		; loop and load next cluster

	; this was the last cluster, so the file has been read!
.ok:	add sp, 4		; stack should now be empty
	mov dl, [ds:disk]
	jmp 0x0000:0x7e00	; go to the loaded bootloader

halt:	cli
.loop	hlt
	jmp .loop

;;;;;;;; END CODE ;;;;;;;;

;;;;;;;; START STRINGS ;;;;;;;;

errattr:	equ 0x0c	; black bg, light red text
load_err:	db "ERR: Cannot Read FAT! ", 0x02
.len:		equ $ - load_err
bootloadername:	db "BOOTMOMO", "BIN"

;;;;;;;; END STRINGS ;;;;;;;;

;;;;;;;; START DATA ;;;;;;;;

fat:		equ 0x1000	; offset into loaded FAT

disk:		db 0		; disk number for int 0x13
part:		dw 0		; offset into MBR partition table
fatmult:	db 0		; multiplier for FAT blocks to disk blocks

dap:				; disk address packet for int 0x13, ah=0x42
.size:		db 0x10		; size: 0x10
.reserved:	db 0x00		; reserved byte (0x00)
.blocks:	dw 0x0010	; blocks to transfer (8K)
.transbuf:	dd 0x0000 + fat	; segment 0x0000, offset (FAT)
.startblk:	dq 2049		; starting absolute block number

;;;;;;;; END DATA ;;;;;;;;

vbrend:	times 510-($-$$) db 0	; fill remaining VBR with zeros
%assign noppad $ - vbrend
%warning INFO: %[noppad] bytes available in VBR.

	db 0x55			; valid boot sector magic word
	db 0xaa

;;;;;;;; END VOLUME BOOT SECTOR ;;;;;;;;

