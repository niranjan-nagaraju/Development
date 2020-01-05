'''
https://leetcode.com/problems/longest-palindromic-substring/

Given a string s, find the longest palindromic substring in s. You may assume that the maximum length of s is 1000.

Example 1:
	Input: "babad"
	Output: "bab"
	Note: "aba" is also a valid answer.

Example 2:
	Input: "cbbd"
	Output: "bb"
'''


'''
Solution Outline:
	NOTE: Two or more palindromes combined is not guaranteed to result in a palindrome.
	      So, solving the problem piecemeal and accumulating the results wont work.
	The trick is to stop as quickly as possible.

	1. Let S = {S0,S1, .. Sn}
	   Start with checking if {S0,..,Sn} is a palindrome, if not, 
	   check if {S0, .... Sn-1} is
	   if any of {S0,..,Si} is a palindrome, this is the maximum-length palindrome that begins with S0
	   (because of the 'greedy' way we are checking for palindromes)
	   Then, start checking for max-palindrome lengths beginning with S1, then S2 etc.

	2. If at any iteration i. we have calculated the max palindrome to be >= (n-i), then we need not proceed with {Si, Si+1, .. Sn}
	   because at Si, max palindrome length we can expect is (n-i), and we already have a result better than equal to (n-i).


Sample run 1:
============
S: "abcbd"
i: 0, max palindrome length begining with 'a'
   'abcbd' ? Not a palindrome
   'abcb' ? Not a palindrome
   'abc' ? Not a palindrome
   'ab' ? Not a palindrome
   'a' ? palindrome, len 1, max-len so far: 1

i: 1, max palindrome length begining with 'b'
   'bcbd'? Not a palindrome
   'bcb' ? palindrome, len 3, max-len so far: 3
   this is the best we can do at i:1

i: 2, max palindrome length beginning with 'c'
      at this point, string we are left with is 'cbd'
	  we already have a 3-character palindrome.
	  we dont have to look any further (as they all wont exceed 3)
	  return '3' as the answer.

'''

class Solution(object):
	# Check if string[startidx:] is a palindrome or not
	@staticmethod
	def isPalindrome(string, startIdx, endIdx):
		l,r = startIdx, endIdx
		while l < r:
			if string[l] != string[r]:
				return False
			l,r = l+1, r-1
		return True


	# Return length of max palindrome starting with string[startidx]
	@staticmethod
	def max_palindrome_len(string, startIdx, endIdx, lookup_table):
		for i in xrange(endIdx, -1, -1):
			lookup_startIdx = lookup_table.get(i)
			if lookup_startIdx is not None:
				if startIdx > lookup_startIdx:
					return 0,0,0
				return lookup_startIdx, i, (i-lookup_startIdx+1)

			if Solution.isPalindrome(string, startIdx, i):
				lookup_table[i] = startIdx
				return startIdx, i, (i-startIdx+1)

		# no palindromes found(?!)
		# this should never hit because a single character is a palindrome on its own
		# but for the sake of completeness, return 0
		return 0,0,0


	def longestPalindrome(self, s):
		"""
		:type s: str
		:rtype: str
		"""
		max_len = 0
		max_sIdx = 0
		max_eIdx = 0
		n = len(s)

		# keeps a tab of palindromes ending at string[endIdx]
		# so when we ask for a second time, what's the max palindrome length ending at 'endIdx'
		# we can use lookup table to answer
		# lookup table stores endIdx -> startIdx mapping indicating string[startIdx..endIdx] is a palindrome
		# if the query is for an endIdx, and our startIdx > lookup_table[startIdx], we are looking at a smaller subset of the palindrome
		# and we already know a bigger palindrome exists, so we can just return 0 in that case
		palindromes_lookup_table = {}
		for i in xrange(n):
			if max_len >= (n-i):
				# we have already found the maximum possible palindrome
				# its not gonna get any longer from here
				break

			sIdx, eIdx, curr_len = self.max_palindrome_len(s, i, n-1, palindromes_lookup_table)
			if curr_len > max_len:
				max_len = curr_len
				max_sIdx = sIdx
				max_eIdx = eIdx

		return s[max_sIdx : max_eIdx+1]




if __name__ == '__main__':
	s  = Solution()
	assert s.isPalindrome("ab", 0, 1) == False
	assert s.isPalindrome("ab", 0, 0) == True
	assert s.isPalindrome("ab", 1, 1) == True
	assert s.isPalindrome("abcbd", 1, 3) == True

	assert s.max_palindrome_len("abcd", 0, 3, {}) == (0, 0, 1)
	assert s.max_palindrome_len("abcbd", 1, 4, {}) == (1, 3, 3) # bcb
	assert s.max_palindrome_len("cbbd", 1, 3, {}) == (1, 2, 2) # bb
	assert s.max_palindrome_len("babad", 0, 4, {}) == (0, 2, 3) # bab
	assert s.max_palindrome_len("babad", 1, 4, {}) == (1, 3, 3) # aba

	assert s.longestPalindrome("abcbd") == "bcb"
	assert s.longestPalindrome("abcd") == "a"
	assert s.longestPalindrome("cbbd") == "bb"
	assert s.longestPalindrome("babad") == "bab"
	assert s.longestPalindrome("aaaabcaaa") == "aaaa"
	assert s.longestPalindrome("racecar") == "racecar"
	assert s.longestPalindrome("mississippi") == "ississi"

