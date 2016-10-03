;
; Get around derpy pre-processor that can't handle (A || B).
;

%ifndef FAT_12_16_INC
%define FAT_12_16_INC

%ifdef FAT12_BOOT
%define FAT_12_16_BOOT
%endif

%ifdef FAT16_BOOT
%define FAT_12_16_BOOT
%endif

%endif

