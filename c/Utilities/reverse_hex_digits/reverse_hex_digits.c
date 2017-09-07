#include "reverse_hex_digits.h"

/** Reverse hex digits in an integer */

/** reverse hex digits in a byte, 0x12 => 0x21 */
uint8_t 
reverse8(uint8_t x8)
{
	return ((x8 << 4) | (x8 >> 4));
}


/** reverse hex digits in a 32-bit word, 0x1234 => 0x4321 */
uint16_t
reverse16(uint16_t x16) 
{
	return (reverse8(x16 & 0xFF) << 8) | reverse8(x16 >> 8);
}

/** reverse hex digits in a 32-bit word, 0x12345678 => 0x87654321 */
uint32_t
reverse32(uint32_t x32) 
{
	return (reverse16(x32 & 0xFFFF) << 16) | reverse16(x32 >> 16);
}


/** reverse hex digits in a 64-bit word, 0x1234567890ABCDEFUL => 0xFEDCBA0987654321UL */
uint64_t
reverse64(uint64_t x64) 
{
	return ((uint64_t)reverse32(x64 & 0xFFFFFFFF) << 32) | reverse32(x64 >> 32);
}
