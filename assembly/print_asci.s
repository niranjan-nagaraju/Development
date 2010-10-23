; procedure to convert hex chars in AL to printable ASCII chars

HEX2ASC PROC
	; AL CONTAINS CHARS e.g. 3D
	; DX CONTAINS ASCII e.g. DH='3', DL='D'
	
	PUSH AX
	MOV AH, 00H
	MOV BL, 10H ; to divide
	DIV BL		; Now AL=03(Q), AH=0D(R)

	MOV BH, 0
	MOV BL, AL
	MOV DH, HEXCODE[BX]	;DH = '3'
	MOV BL, AH
	MOV DL, HEXCODE[BX]	;DL = 'D'

	RET
HEX2ASC END

.Data
HEXCODE DB '0123456789ABCDEF'
