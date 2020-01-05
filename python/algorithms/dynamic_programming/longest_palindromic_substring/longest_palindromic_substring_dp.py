#encoding: utf-8
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
DP Solution Outline:
	1. A single letter is a palindrome by itself (of length 1)
	2. Create a DP table set that stores a substring [x .. y] if a[x..y] is a palindrome
	   Add all two-letter palindromes (x+1, y if a[x+1] == a[y]) into the set.
	3. Of all 3-letter substrings, (x, y, z) if a[x] == a[z], then add [x..z] to the set
	4. 4-letter substrings work similarly, (w,x,y,z) if a[w] == a[z] and set contains [x..y], add [w..z] to set
	5. Repeat until all n-length subtrings are done
	   [i..j] is a palindrome if a[i] == a[j] and set contains [i+1 .. j-1]

	Number of substrings of length k in a string of length n: n-k+1

Time: O(n²), Space: O(n²)


Sample run 1:
============
S: "abcbd"
set: {}

k: 2
  "ab", "bc", "cb", "bd"
  None are palindromes
  set: {}

k: 3
  "abc", "bcb", "cbd"
  "bcb": 1-3 => add (1,3) to set
  set: {(1,3)}

k = 4
  "abcb", "bcbd"
  None are palindromes
  set: {(1,3)}

k = 5
  "abcbd"
  set: {(1,3)}

  return a[1..3] => "bcb"


Sample run 2:
============
S: "babad"
set: {}

k: 2
  "ba", "ab", "ba", , "ad"
  None are palindromes
  set: {}

k: 3
  "bab", "aba", "bad"
  "bab", 0-2, "aba": 1-3 are palindromes -> add to set
  set: {(0,2), (1,3)}

k: 4
  "baba", "abad"
  None are palindromes
  set: {(0,2), (1,3)}

k: 5
  "babad"
  None are palindromes
  set: {(0,2), (1,3)}

return either (0,2) --> "bab" or (1,3) --> "aba"
'''

# check if s[startIdx .. endIdx] is a palindrome or not
# if it is, add it to the palindrome set
def isPalindrome(s, startIdx, endIdx, palindrome_set):
	if s[startIdx] != s[endIdx]:
		return False
	else:
		# a[startIdx] == a[endIdx], check if substring between them is also a palindrome
		if (startIdx+1, endIdx-1) in palindrome_set:
			palindrome_set.add((startIdx, endIdx))
			return True
	return False


def longestPalindrome(s):
	"""
	:type s: str
	:rtype: str
	"""
	max_len = 1
	max_sIdx = 0
	n = len(s)
	palindrome_set = set()

	# Store all 1-letter and 2-letter palindromes in the set
	for i in xrange(n-1):
		palindrome_set.add((i,i))
		if s[i] == s[i+1]:
			palindrome_set.add((i, i+1))
			max_sIdx, max_len = i, 2
	palindrome_set.add((n-1, n-1))

	# Find if any of substrings of length 3 to n are palindromes
	for i in xrange(3, n+1):
		curr_len = i
		for j in xrange(0, n-i+1):
			if isPalindrome(s, j, j+i-1, palindrome_set) and \
			   max_len < curr_len:
					max_sIdx, max_len = j, curr_len
				
	return s[max_sIdx : max_sIdx+max_len]




if __name__ == '__main__':
	assert longestPalindrome("abcbd") == "bcb"
	assert longestPalindrome("abcd") == "a"
	assert longestPalindrome("cbbd") == "bb"
	assert longestPalindrome("babad") == "bab"
	assert longestPalindrome("aaaabcaaa") == "aaaa"
	assert longestPalindrome("racecar") == "racecar"
	assert longestPalindrome("mississippi") == "ississi"

