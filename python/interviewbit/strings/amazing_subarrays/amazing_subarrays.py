'''
https://www.interviewbit.com/problems/amazing-subarrays/

Amazing Subarrays
You are given a string S, and you have to find all the amazing substrings of S.

Amazing Substring is one that starts with a vowel (a, e, i, o, u, A, E, I, O, U).

Input
Only argument given is string S.

Output
Return a single integer X mod 10003, here X is number of Amazing Substrings in given string.

Constraints
1 <= length(S) <= 1e6
S can have special characters

Example

Input
    ABEC

Output
    6

Explanation
	Amazing substrings of given string are :
	1. A
	2. AB
	3. ABE
	4. ABEC
	5. E
	6. EC
	here number of substrings are 6 and 6 % 10003 = 6.
'''

'''
Solution Outline:
	Number of substrings start with A[i], is n-i (n: len(A), 0<=i<n)
	Count all such substrings for all A[i] in lower({a,e,i,o,u})
'''
class Solution:
	def count_amazing_substrings(self, A):
		mod = 10003
		n = len(A)
		count = 0
		for i in xrange(n):
			if A[i].lower() in ['a', 'e', 'i', 'o', 'u']:
				count = (count + n-i) % mod

		return count


if __name__ == '__main__':
	s = Solution()
	assert s.count_amazing_substrings("") == 0
	assert s.count_amazing_substrings("BCDGHJ") == 0
	assert s.count_amazing_substrings("ABEC") == 6


