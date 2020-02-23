#include <assert.h>
#include "reverse_hex_digits.h"

int
main(void)
{
	uint8_t byte = 0x12;
	uint16_t dbyte = 0x1234;
	uint32_t word = 0x12345678;
	uint64_t dword = 0x1234567890ABCDEFUL;

	assert(reverse8(byte) == 0x21);
	assert(reverse16(dbyte) == 0x4321);
	assert(reverse32(word) == 0x87654321);
	assert(reverse64(dword) == 0xFEDCBA0987654321UL);
}
