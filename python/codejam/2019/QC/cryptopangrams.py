'''
Ciphertext is made by multiplying every two consecutive primes

so a plaintext of length n becomes a ciphertext of length n-1
ciphertext[0]: prime(plaintext[0] * paintext[1])
ciphertext[1]: prime(plaintext[1] * paintext[2])
. . .
ciphertext[n-2]: prime(plaintext[n-2] * paintext[n-1])

To decode,
solve factors(ciphertext[0]) -> a,b
Use a,b to decode the rest of the primes
sort the unique 26 primes and map them to A-Z
Use this map to decrypt the message.
'''

import math

# n only has 2 prime factors (by definition)
def prime_factorize(n):
	# if n is even, one of the prime factors is 2
	if n % 2 == 0:
		return (2, n/2)

	for i in range(3, int(math.sqrt(n))+1, 2):
		if n % i == 0:
			# prime factors are i and n/i
			return (i, n/i)



def decrypt(ciphertext):
	(first, second) = prime_factorize(ciphertext[0])

	# products could be repeating initially (product = p1*p2)
	# we wouldn't know if plaintext corresponds to p1,p2 or p2,p1
	# Furthermore, as long as the product repeats,
	# say, if the plaintext is ABABABC....  for e.g, 
	# we wont be able to tell by looking at products alone, until we hit a different product
	# as all AB, will have p1*p2 which could very well be BA
	i = 0
	while ciphertext[i] == ciphertext[i+1]:
		# keep going until we find a non-repeating product
		# we will eventually find one, so we don't have to check for
		# i < len(ciphertext) boundaries
		i += 1

	# at this point, we are i s.t. for all 0<=i, ciphertext[i] is the same
	# ciphertext[i+1] is the first non-repeating pattern
	# e.g.
	#   primes: 3,5,7, ... A=3, B=5, C=7
	#   ABABC -> 15 15 15 35
	#   15: 3 x 5
	#   Based on 35, we conclude, p1: 3, p2: 5, and therefore plaintext was AB...
	#   NOTE: odd or even repetitions matter
	'''
	| ciphertext[+1] % p1 == 0                 | repeats | sequence |                                                    |
	| Case 1: p1, p2: 3,5, ciphertext[+1] = 21 |         |          | Examples                                           |
	| Case 2: p1, p2: 3,5, ciphertext[+1] = 35 |         |          |                                                    |
	|------------------------------------------+---------+----------+----------------------------------------------------|
	| YES (case 1)                             | odd     | p2,p1    | BABAC => 15 15 15 21, BABABAC => 15 15 15 15 15 21 |
	| YES (case 1)                             | even    | p1,p2    | ABAC => 15 15 21, ABABAC => 15 15 15 15 21         |
	| NO  (case 2)                             | odd     | p1,p2    | ABC => 15 35, ABABC => 15 15 15 35                 |
	| NO  (case 2)                             | even    | p2,p1    | BABC => 15 15 35, BABABC => 15 15 15 15 35         |
	'''

	if ciphertext[i+1] % first == 0:
		# Case 1, flip when odd
		if (i+1)%2 == 1:
			first, second = second, first
	else:
		# Case 2, flip when even
		if (i+1)%2 == 0:
			first, second = second, first

	# add (first, second) to set, so there are no duplicate entries
	primes = {first, second}
	p = second
	for c in ciphertext[1:]:
		p =  c / p
		primes.add(p)

	primes_to_alpha_map = {}
	i = 0
	for p in sorted(primes):
		primes_to_alpha_map[p] = chr(ord('A')+i)
		i += 1

	plaintext = primes_to_alpha_map[first] + primes_to_alpha_map[second]
	p = second
	for c in ciphertext[1:]:
		p =  c / p
		plaintext += primes_to_alpha_map[p]

	return plaintext
	

if __name__ == "__main__":
	assert prime_factorize(217) == (7,31)
	assert prime_factorize(1819) == (17, 107)

	n,l, ciphertext = 103, 31, [217, 1891, 4819, 2291, 2987, 3811, 1739, 2491, 4717, 445, 65, 1079, 8383, 5353, 901, 187, 649, 1003, 697, 3239, 7663, 291, 123, 779, 1007, 3551, 1943, 2117, 1679, 989, 3053]
	assert decrypt(ciphertext) == 'CJQUIZKNOWBEVYOFDPFLUXALGORITHMS'


	n, l, ciphertext = 10000, 25, [3292937, 175597, 18779, 50429, 375469, 1651121, 2102, 3722, 2376497, 611683, 489059, 2328901, 3150061, 829981, 421301, 76409, 38477, 291931, 730241, 959821, 1664197, 3057407, 4267589, 4729181, 5335543]
	assert decrypt(ciphertext) == 'SUBDERMATOGLYPHICFJKNQVWXZ'

	n,l, ciphertext = 101, 25, [6, 15, 35, 77, 143, 221, 323, 437, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633, 9797]
	assert decrypt(ciphertext) == 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
	
	n, l, ciphertext = 101, 25, [202, 6, 15, 35, 77, 143, 221, 323, 437, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633]
	assert decrypt(ciphertext) == 'ZABCDEFGHIJKLMNOPQRSTUVWXY'

	# primes: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
	# plaintext: 'ABACDEFGHIIJKLMNOPQRSTUVWXYZ'
	n,l, ciphertext = 101, 27, [6, 6, 10, 35, 77, 143, 221, 323, 437, 529, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633, 9797]
	assert decrypt(ciphertext) == 'ABACDEFGHIIJKLMNOPQRSTUVWXYZ'

	# primes: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
	# plaintext: 'ABABCDEFGHIIJKLMNOPQRSTUVWXYZ'
	n,l, ciphertext = 101, 28, [6, 6, 6, 15, 35, 77, 143, 221, 323, 437, 529, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633, 9797]
	assert decrypt(ciphertext) == 'ABABCDEFGHIIJKLMNOPQRSTUVWXYZ'

	# primes: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
	# plaintext: 'BABCDEFGHIIJKLMNOPQRSTUVWXYZ'
	n,l, ciphertext = 101, 27, [6, 6, 15, 35, 77, 143, 221, 323, 437, 529, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633, 9797]
	assert decrypt(ciphertext) == 'BABCDEFGHIIJKLMNOPQRSTUVWXYZ'

	# primes: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
	# plaintext: 'BABACDEFGHIIJKLMNOPQRSTUVWXYZ'
	n,l, ciphertext = 101, 28, [6, 6, 6, 10, 35, 77, 143, 221, 323, 437, 529, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633, 9797]
	assert decrypt(ciphertext) == 'BABACDEFGHIIJKLMNOPQRSTUVWXYZ'

	# primes: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
	# plaintext: 'BACDEFGHIIJKLMNOPQRSTUVWXYZ'
	n,l, ciphertext = 101, 26, [6, 10, 35, 77, 143, 221, 323, 437, 529, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633, 9797]
	assert decrypt(ciphertext) == 'BACDEFGHIIJKLMNOPQRSTUVWXYZ'

	# primes: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
	# plaintext: 'BABABACDEFGHIIJKLMNOPQRSTUVWXYZ'
	n,l, ciphertext = 101, 30, [6, 6, 6, 6, 6, 10, 35, 77, 143, 221, 323, 437, 529, 667, 899, 1147, 1517, 1763, 2021, 2491, 3127, 3599, 4087, 4757, 5183, 5767, 6557, 7387, 8633, 9797]
	assert decrypt(ciphertext) == 'BABABACDEFGHIIJKLMNOPQRSTUVWXYZ'

	nTestcases = int(input())
	for i in xrange(nTestcases):
		n, l  = map(int, raw_input().strip().split())
		ciphertext = map(int, raw_input().strip().split())
		print 'Case #{0}: {1}'.format(i+1, decrypt(ciphertext))


