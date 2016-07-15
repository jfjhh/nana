;
; FAT Extended Boot Record (EBR).
;

%ifndef FAT_EBR
%define FAT_EBR

;
; set the jmp length over the BPB and EBR, depending on the type of FAT
; filesystem used for the boot partition.
;
%ifdef FAT32_BOOT

;                                  .========.
;                                  | FAT 32 |
;-------------------.--------------'========'----------------.--------.--------+
; data              | description                            | offset | length |
;-------------------+----------------------------------------+--------+--------+
dd 0                ; sectors per FAT (size of FAT in sec.). | 36     | 4      |
dw 0                ; vexilology time (some flags).          | 40     | 2      |
dw 0                ; FAT version (high:major,low:minor)-ver.| 42     | 2      |
dd 2                ; cluster number of root directory.      | 44     | 4      |
dw 0                ; sector number of the FSInfo structure. | 48     | 2      |
dw 0                ; sector number of the backup boot sec.  | 50     | 2      |
times 12 db 0x00    ; reserved and should be zero'ed.        | 52     | 12     |
db 0x80             ; drive number, same as ret. by BIOS.    | 64     | 1      |
db 0                ; reserved flags for Windows NT.         | 65     | 1      |
db 0x28             ; signature (either 0x28 or 0x29).       | 66     | 1      |
dd 0                ; VolumeID "Serial" number (ignorable).  | 67     | 4      |
times 11 db 0x20    ; volume label string (pad. with spaces).| 71     | 11     |
db "FAT32   "       ; system ID string (<-always) (no trust).| 82     | 8      |
; times 420 nop     ; boot code.                             | 90     | 420    |
; dw 0xaa55         ; bootable partition magic bytes.        | 510    | 2      |
;-------------------'----------------------------------------'--------'--------'

%elifdef (FAT12_BOOT || FAT16_BOOT)

;                            .===================.
;                            | FAT 12 and FAT 16 |
;-------------------.--------'==================='-----------.--------.--------+
; data              | description                            | offset | length |
;-------------------+----------------------------------------+--------+--------+
db 0x80             ; drive number, same as ret. by BIOS.    | 36     | 1      |
db 0                ; reserved flags for Windows NT.         | 37     | 1      |
db 0x28             ; signature (either 0x28 or 0x29).       | 38     | 1      |
dd 0                ; VolumeID "Serial" number (ignorable).  | 39     | 4      |
times 11 db 0x20    ; volume label string (pad. with spaces).| 43     | 11     |
times 8  db 0x20    ; system ID string (FAT type) (no trust).| 54     | 8      |
; times 448 nop     ; boot code.                             | 62     | 448    |
; dw 0xaa55         ; bootable partition magic bytes.        | 510    | 2      |
;-------------------'----------------------------------------'--------'--------'

%else
%fatal	"Need to know the type of the boot FAT FS! (12, 16, or 32)"
%endif

%endif

