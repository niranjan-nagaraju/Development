.text

.global _start

_start:
	movl $1, %eax # Exit system call
	movl $0, %ebx # exit (0)
	int $0x80     # System call
