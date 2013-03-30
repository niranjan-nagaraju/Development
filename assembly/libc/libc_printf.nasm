; libc and nasm calling convention example
; Source: security-tube
;			http://www.securitytube.net/video/6980

; libc calling a function:
;	push d, c, b, a if function takes f(a, b, c, d)
;	when function returns SP+4 takes back SP to value before call

extern printf
extern exit

; Define main as the starting point; gcc uses main by default.
global main


section .text
main:
	push message
	call printf
	add esp, 0x4	; Adjust the stack to where it was before the call

	mov eax, 0xa
	call exit


section .data
	message: db "Hello World", 0xA, 0x00  ; "Hello World\n" (Null terminated)
	mlen	 equ  $-message

