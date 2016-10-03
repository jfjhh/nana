;
; FAT BIOS Parameter Block (BPB).
;

%ifndef FAT_BPB
%define FAT_BPB

%include "fat_12_16.inc"

;
; set the jmp length over the BPB and EBR, depending on the type of FAT
; filesystem used for the boot partition.
;
; the jmp short (0xeb, <off>) instruction is two bytes, so <off> is the offset
; the code, minus two. E.g. The FAT 12 / FAT 16 boot code is at offset 62 =>
; 0x3e, so 0x3e - 2 == 0x3c, and that is the jump value for FAT 12 / FAT 16.
;
%ifdef FAT32_BOOT

code:	equ 0x58

%elifdef FAT_12_16_BOOT

code:	equ 0x3c

%else
%fatal	"Need to know the type of the boot FAT FS! (12, 16, or 32)"
%endif

;                                 .=========.
;                                 | FAT BPB |
;-------------------.-------------'========='----------------.--------.--------.
; data              | description                            | offset | length |
;-------------------+----------------------------------------+--------+--------+
db 0xeb, code, 0x90 ; jmp short <boot code addr> nop.        | 0      | 3      |
db "MSWIN4.1"       ; OEM identifier. <-val. for leg. compat.| 3      | 8      |
dw 512              ; bytes per sector.                      | 11     | 2      |
db 0                ; sectors per cluster.                   | 13     | 1      |
dw 0                ; number of reserved sectors.            | 14     | 2      |
db 2                ; number of FATs on the media.           | 16     | 1      |
dw 0                ; number of directory entries.           | 17     | 2      |
dw 0                ; total sectors in log. volume.          | 19     | 2      |
db 0xf8             ; media descriptor byte.                 | 21     | 1      |
dw 0                ; sectors per FAT (FAT 12/16 only).      | 22     | 2      |
dw 0                ; sectors per track.                     | 24     | 2      |
dw 0                ; number of heads on the media.          | 26     | 2      |
dd 0                ; number of hidden sectors.              | 28     | 4      |
dd 0                ; large num. sectors (iff word@off19=~0).| 32     | 4      |
;-------------------'----------------------------------------'--------'--------'

%endif

