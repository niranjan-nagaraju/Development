'''
https://www.interviewbit.com/problems/sorted-permutation-rank/

Sorted Permutation Rank

Given a string, find the rank of the string amongst its permutations sorted lexicographically.
Assume that no characters are repeated.

Example :
	Input : 'acb'
	Output : 2

	The order permutations with letters 'a', 'c', and 'b' : 
	abc
	acb
	bac
	bca
	cab
	cba
The answer might not fit in an integer, so return your answer % 1000003
'''

'''
Solution Outline:
	Consider 'ab'
	1: ab
	2: ba

	Consider 'abc'
	1: abc
	2: acb
	3: bac
	4: bca
	5: cab
	6: cba

	Consider 'abcd' as the string
	1:  abcd
	2:  abdc
	3:  acbd
	4:  acdb
	5:  adbc
	6:  adcb
	7:  bacd
	8:  badc
	9:  bcad
    10:	bcda
	11: bdac
	12: bdca
	13: cabd
	14: cadb
	15: cbad
	16: cbda
	17: cdab
	18: cdba
	19: dabc
	20: dacb
	21: dbac
	22: dbca
	23: dcab
	24: dcba

	Let lower(x) be the number of elements lesser than x in sorted(A).
	in "abcd",
		lower(a) == 0
		lower(b) == 1
		lower(c) == 2
		lower(d) == 3

	We use this to calculate the rank of a permutation, p, (of say, 'abcd')
	  for e.g., p == 'cbad'

	  p[0] == c, lower[c] = 2 => there are atleast 2*(4-1)! == 2*6 == 12 permutations before cXXX
	  Now recalculate lower[] with 'c' removed
	  In 'abd'
	  lower(a) == 0
	  lower(b) == 1
	  lower(d) == 2
	  {Essentially only 'd' changed by -1}

	  p[1] == b, lower[b] == 1 => there are atleast 1*(4-2)! == 1*2! == 2 => 14 permutations before cbXX
	  Now recalculate lower[] with 'b' removed
	  In 'ad'
	  lower(a) == 0
	  lower(d) == 1

	  p[2] == 'a', lower[a] == 0 => there are 0 == 14 permutations before cbaX
	  Now recalculate lower[] with 'a' removed
	  In 'd'
	  lower(d) == 0

	  p[3] == 'd', lower[d] == 0 => 14 permutations before cbad
	  rank('cbad') == 15
'''
class Solution:
	def permutation_rank(self, A):
		def factorial(n):
			return reduce(lambda acc,x: acc*x, xrange(1, n+1), 1)

		if not A:
			return 0

		# Create a 256 byte lower[] table for all ascii characters
		MAX_CHARS = 256
		n = len(A)
		lower = [0]*(MAX_CHARS+1)
		for x in A:
			lower[ord(x)+1] = 1

		for i in xrange(1, MAX_CHARS+1):
			lower[i] += lower[i-1]

		i = 1
		offset = 0
		factor = factorial(n-1)
		for x in A:
			x_ = ord(x)
			offset += lower[x_] * factor

			# divide factor by n-1, n-2, .. so we get (n-2)!, (n-3)!, ... 
			factor /= (n-i) if n > i else 1 
			i += 1

			# remove x from lower[] calculations
			# removing x will only affect those characters that are already greater than x
			for j in xrange(x_+1, MAX_CHARS+1):
				lower[j] -= 1

		return (offset+1) % 1000003


if __name__ == '__main__':
	s = Solution()
	assert s.permutation_rank('bcda') == 10
	assert s.permutation_rank('CDAB') == 17
	assert s.permutation_rank('CBAD') == 15
	assert s.permutation_rank('string') == 598
	assert s.permutation_rank('bca') == 4
	assert s.permutation_rank('b') == 1
	assert s.permutation_rank('ab') == 1
	assert s.permutation_rank('abcd') == 1
	assert s.permutation_rank('DCBA') == 24

