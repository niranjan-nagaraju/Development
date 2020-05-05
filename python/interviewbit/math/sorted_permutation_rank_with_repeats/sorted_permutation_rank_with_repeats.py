'''
https://www.interviewbit.com/problems/sorted-permutation-rank-with-repeats/

Sorted Permutation Rank with Repeats

Given a string, find the rank of the string amongst its permutations sorted lexicographically.
Note that the characters might be repeated. If the characters are repeated, we need to look at the rank in unique permutations.
Look at the example for more details.

Example :

Input : 'aba'
Output : 2

The order permutations with letters 'a', 'a', and 'b' : 
aab
aba
baa
The answer might not fit in an integer, so return your answer % 1000003

NOTE: 1000003 is a prime number
NOTE: Assume the number of characters in string < 1000003 
'''

'''
Solution Outline:
	Consider "aabbc"
	Permutations (sorted lexicographically) are as below -
1  aabbc
2  aabcb
3  aacbb
4  ababc
5  abacb
6  abbac
7  abbca
8  abcab
9  abcba
10 acabb
11 acbab
12 acbba
13 baabc
14 baacb
15 babac
16 babca
17 bacab
18 bacba
19 bbaac
20 bbaca
21 bbcaa
22 bcaab
23 bcaba
24 bcbaa
25 caabb
26 cabab
27 cabba
28 cbaab
29 cbaba
30 cbbaa

Total number of permutations = 30
5!/(2!*2!) = 5*4*3*2*1/(2*2) = 120/4 = 30

Rank(bcaba):
	lower(a) = 0
	lower(b) = 2
	lower(c) = 4
	first character: 'b' => Number of permutations preceding bxxxx = 2*4! (without repeats)
	  There are two repeating characters to the right of b {including b itself, 2}
	  => Number of permutations preceding bxxxx = 2*4!/(2!*2!) = 2*4*3*2*1/(2*2) = 4*3*1 = 12
	rank = 12

	second character: 'c'
	Remove 'b' from lower table => {remaining characters: caba}
	lower(a) = 0
	lower(b) = 2
	lower(c) = 3 [only c gets changed]
	Number of permutations without repeats = 3*3!
	  Repeating characters to the right = 1 (repeats twice)
	3*3!/(2!) = 3*3*2*1/2 = 9
	rank += 9 = 21

	third character: 'a'
	Remove 'c' from lower table => {remaining characters: aba}
	lower(a): 0
	lower(b): 1
	Number of permutations without repeats = 0 * 2! = 0
	rank += 0 = 21

	fourth character: 'b'
	Remove 'a' from lower table => {remaining characters: ba}
	lower(a): 0
	lower(b): 1
	Number of permutations without repeats = 1 * 1! = 1
	rank += 1 = 22

	fifth character: 'a'
	Remove 'b' from lower table => {remaining characters: a}
	lower(a): 0
	Number of permutations without repeats = 0 * 0! = 1
	rank += 0 = 22

	Rank(bcaba) == 22+1
'''

from collections import defaultdict
class Solution:
	def permutation_rank_repeats(self, A):
		mod = 1000003
		def factorial(n):
			return reduce(lambda acc,x: acc*x, xrange(1, n+1), 1)

		if not A:
			return 0

		n = len(A)
		rank = 1

		factor = factorial(n-1)
		k = 1
		for i in xrange(n):
			# count number of elements to the right of A[i], that are lesser than it
			lower_than_i = 0

			# count number of elements that repeat
			# and the number of times they repeat
			repeats = defaultdict(lambda: 0)
			for j in xrange(i+1, n):
				if A[i] > A[j]:
					lower_than_i += 1
				repeats[A[j]] += 1

			# repeats count include current element as well
			repeats[A[i]] += 1

			denom = 1
			for v in repeats.values():
				denom *= factorial(v)

			rank += (factor * lower_than_i) / denom

			# divide factor by n-1, n-2, .. so we get (n-2)!, (n-3)!, ... 
			factor /= (n-k) if n>k else 1
			k += 1
		return rank % mod



if __name__ == '__main__':
	s = Solution()
	assert s.permutation_rank_repeats('bcaba') == 23
	assert s.permutation_rank_repeats('settLe') == 107
	assert s.permutation_rank_repeats('cbbaa') == 30
	assert s.permutation_rank_repeats('aabbc') == 1

	# should work even when there are no repeated elements
	assert s.permutation_rank_repeats('bcda') == 10
	assert s.permutation_rank_repeats('CDAB') == 17
	assert s.permutation_rank_repeats('CBAD') == 15
	assert s.permutation_rank_repeats('string') == 598
	assert s.permutation_rank_repeats('bca') == 4
	assert s.permutation_rank_repeats('b') == 1
	assert s.permutation_rank_repeats('ab') == 1
	assert s.permutation_rank_repeats('abcd') == 1
	assert s.permutation_rank_repeats('DCBA') == 24

