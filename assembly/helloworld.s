# Use write system call
# write - 4
# write (fd, buf, len)


.data
helloworldstring:
	.ascii "Hello World\n"

.text
.globl _start

_start:
	movl $4, %eax
	movl $1, %ebx # stdout fd
	movl $helloworldstring, %ecx
	movl $12, %edx # Length
	int $0x80

	# Exit the program, exit(0)
	movl $1, %eax
	movl $0, %ebx
	int $0x80
