/**
 * Implement printf
 */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>


int
printv(char *fmt, ...)
{
	/** Implement printf() using vprintf()
	 * collect variable list arguments into a va_list
	 * use vprintf() to parse the format string,
	 * and print the rest
	 */
	va_list vl;
	int ret;

	va_start(vl, fmt);
	ret = vprintf(fmt, vl);
	va_end(vl);

	return ret;
}


/** Convert an integer into a string-representation for a specified base */
char *
convert_to_int(unsigned int number, int base)
{
	static char representation[] = "0123456789ABCDEF";
	static char buffer[50] = {0};
	char *ptr;

	ptr = &buffer[49];
	*ptr = 0;
	while (number) {
		*--ptr = representation[number % base];
		number /= base;
	}

	return ptr;
}


/** Print characters in a char array 's' */
void
putStr(char *s)
{
	while (*s) {
		putchar(*s++);
	}
}



/**
 * Printf(): Supported formats: %d, %c, %s, %%
 * returns: number of characters output
 */
int
print(char *fmt, ...)
{
	int ret = 0;
	va_list vl;
	char c;

	va_start(vl, fmt);
	while ((c=*fmt++)) {
		if(c == '%') {
			switch(*fmt) {
				case '%':
					{
						putchar('%');
						ret++;
					}
					break;
				case 'c':
					{
						int vc = va_arg(vl, int);
						putchar(vc);
						ret++;
					}
					break;
				case 'd':
					{
						int i = va_arg(vl, int);
						char *s;

						if (i < 0) {
							ret++;
							putchar('-');
							i = -i;
						}
						s = convert_to_int(i, 10);
						putStr(s);
						ret += strlen(s);
					}
					break;
				case 's':
					{
						char *s = va_arg(vl, char *);
						putStr(s);
						ret += strlen(s);
					}
					break;
				default:
					{
						// Invalid format - just print it as-is
						putchar('%');
						putchar(c);
						ret += 2;
					}
					break;
			}
			fmt++;
		} else {
			ret++;
			putchar(c);
		}
	}
	va_end(vl);

	return ret;
}


int
main(void)
{
	int ret, ret2;

	/** printf() returns number of bytes printed */
	assert (printv("Hello world: %d %s %c\n", 1, "hey", 'p') == strlen("Hello world: 1 hey p\n"));

	ret = print("abc %% %s %d %c %d %s\n",  "Hello", 11, 'A', 23456, "World!");
	ret2 = printf("abc %% %s %d %c %d %s\n",  "Hello", 11, 'A', 23456, "World!");
	assert(ret == ret2); /* compare with std library printf() */

	return 0;
}
