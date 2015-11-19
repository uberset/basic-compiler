		org 100h
section .data
T0:	db	"5/2=",0
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
		cwd
		idiv bx
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

getsbuff:
		; get string from stdin
		; max size len + 1 (CR)
		; out bx: startaddress
		; user can edit text
		; uses ax, bx, dx
		mov bx, .buff
		mov dx, bx
		mov ah, 0ah
		int 21h
		mov al, [bx+1]	; actual length
		add al, 2		; offset
		xor ah, ah		; 0
		add bx, ax
		mov [bx], byte 0; terminate string
		mov bx, .buff+2	; string start address
		ret
section	.data
.len 	equ 80
.buff:	db .len+1	; max size (including CR)
		db 0		; actual size
times .len db 0		; the string
		db 0		; CR (or 0)
section .text

string2int:
		; convert string to signed int (16-bit)
		; regexp: [+-]?[0..9]*
		; BX: string addr
        ; result in AX
		; CH: sign
		; CL: char
		; changes BX
		mov ax, 0	; accu
		mov ch, 0	; sign
		mov cl, [bx]
		inc bx
		cmp cl, '+'
		je .next
		cmp cl, '-'
		jne .digits
		mov ch, -1
.next:	mov cl, [bx]
		inc bx
.digits:
		cmp cl, '0'
		jl .sign
		cmp cl, '9'
		jg .sign
		mov dx, 10
		mul dx
		sub cl, '0'
		mov dl, cl
		xor dh, dh
		add ax, dx
		jmp .next
.sign:	cmp ch, 0	; sign
		jge .end
		neg ax
.end:	ret


            