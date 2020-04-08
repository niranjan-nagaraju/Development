'''
Count the number of bits set in an integer, n.
'''
# return the number of bits set in n
def bits_set(n):
	count = 0
	while n:
		count += 1
		n &= (n-1)

	return count


if __name__ == '__main__':
	assert bits_set(0) == 0
	assert bits_set(1) == 1
	assert bits_set(2) == 1
	assert bits_set(3) == 2
	assert bits_set(4) == 1
	assert bits_set(5) == 2
	assert bits_set(6) == 2
	assert bits_set(7) == 3
	assert bits_set(8) == 1
	assert bits_set(9) == 2
	assert bits_set(10) == 2
	assert bits_set(11) == 3
	assert bits_set(15) == 4
	assert bits_set(0b1001001) == 3
	assert bits_set(0xABCD) == 10


