;
; Macros for bootloader.
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

;;;;;;;; START MACROS ;;;;;;;;

; reserve a string, and add its length
; params: label_name "string"
; results: label_name: db "string" (with .len being its length)
%macro dlstr 2+
%1:	db %2
%%end:
.len:	equ %%end - %1
%endmacro

; print a message, but centered on the screen
%macro center_msg 2
	mov bl, %1		; the attribute
	mov cx, %2.len		; the string length of a dlstr'd string
	mov dx, 0x02 | (80 - %2.len) / 2	; row 2, center the string
	mov bp, %2
	call msg
%endmacro

; do the message, but halt after
%macro center_msg_h 2
	center_msg %1, %2
	jmp halt
%endmacro

;;;;;;;; END MACROS ;;;;;;;;

