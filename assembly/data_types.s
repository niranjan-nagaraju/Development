# Data types

.data
	hellostr:
		.ascii "Hello World!"
	byteloc:
		.byte 10
	int32:
		.int 2
	int16:
		.short 3
	floater:
		.float 10.23
	intArray:
		.int 1,2,3,4,5,6

.bss
	.comm largebuf 10000

.text
	.globl _start

	_start:
		nop

		# exit
		movl $1, %eax
		movl $0, %ebx
		int $0x80
