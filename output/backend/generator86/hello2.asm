		org 100h
L10:
section .data
T0:	db	"Hello",0
section .text
		mov bx, T0
		call puts
		call putln
L20:
section .data
T1:	db	"World!",0
section .text
		mov bx, T1
		call puts
		call putln
		mov ax,0x4c00
		int 0x21

puts:	; put a string to stdout
		; string start address in BX
		; string must be terminated with null
		; AX, BX, DX will be modified
		mov dl,[bx]     ; load character
		cmp dl, 0
		jz  .end
		mov ah,2		; output char to stdout (ah: 02, dl: char)
		int 0x21		; DOS
		inc bx
		jmp puts
.end:	ret

putln:	; put CR LF to stdout
		mov bx, .line
		jmp puts
.line:	db 0x0A, 0x0D, 0
