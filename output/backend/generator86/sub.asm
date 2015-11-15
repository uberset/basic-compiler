		org 100h
section .data
T0:	db	"5-2=",0
section .text
		mov bx, T0
		call puts
		call putln
		mov ax, 5
		push ax
		mov ax, 2
		push ax
		pop bx
		pop ax
		sub ax, bx
		push ax
		pop ax
		call puti
		call putln
		mov ax,0x4c00
		int 0x21

puti:	; put a signed integer (16 bit) to stdout
		; int in AX
		; AX, BX, CX, DX will be modified
		call int2decimal	; returns pointer to string in bx
		call puts
		ret

int2decimal:
		; convert a signed integer (16 bit) to a buffer
		; int in AX
		; AX, BX, CX, DX will be modified
		; buffer: CX
		; divisor: BX
		mov dl, '+'	; sign
		cmp	ax,0
		jge .unsigned
		neg ax
		mov dl, '-'
.unsigned:
		mov bx, .buffer
		mov [bx], dl	; sign
		mov cx, .endbuf-2
.next:	mov dx, 0
		mov bx, 10
		div bx	; ax = (dx, ax) / bx
				; dx = remainder
		mov bx, cx
		add dl, '0'
		mov [bx], dl	; digit
		dec cx
		cmp ax, 0
		jne .next
		; move sign if necessary
		; BX points to the first digit now
		mov dl, [.buffer]	; sign '+' or '-'
		cmp dl, '-'
		jne .end	; no '-'
		dec bx
		mov [bx], dl	; copy sign
.end:	ret
section .data
.buffer	db		"-", "12345", 0
.endbuf:
section .text

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
