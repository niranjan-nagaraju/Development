#include <stdio.h>
#include <string.h>
#include <stdint.h>

#pragma pack(1)
void
vulnerable(int index)
{
	uint32_t value = 0;
	uint32_t array[16];
	array[index] = 0xDEADBEEF;

	printf("%p %p %p\n", array, &value, &array[index]);
	printf("%X\n", value);
}

int
main(void)
{
	vulnerable(16);
	return 0;
}
