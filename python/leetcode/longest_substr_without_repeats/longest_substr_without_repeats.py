'''
https://leetcode.com/problems/longest-substring-without-repeating-characters/

Given a string, find the length of the longest substring without repeating characters.

Example 1:
Input: "abcabcbb"
Output: 3 
Explanation: The answer is "abc", with the length of 3.

Example 2:
Input: "bbbbb"
Output: 1
Explanation: The answer is "b", with the length of 1.

Example 3:
Input: "pwwkew"
Output: 3
Explanation: The answer is "wke", with the length of 3.

Note that the answer must be a substring, "pwke" is a subsequence and not a substring.
'''


'''
Solution Outline:
	1. Start accumulating characters from 'startposition=0' until one of the accumulated characters repeat.
	2. Use a hash-table to store characters->index mapping to quickly compare if the a character repeats.
	3. Upon encountering a repeat charater, c, use the hash-table to get c's previous index,
	   capture current substring length = (curr_idx - startposition),
	   start over from startposition+1 (so previous occurene of c isn't included in the new candidate substring)
	   at the end of the pass, maximum current substring length is the number we are looking for.


Sample run 1:
============
l: "abcab"
ht = {}

i:0 ht = {'a': 0}
i:1 ht = {'a': 0, 'b': 1}
i:2 ht = {'a': 0, 'b': 1, 'c': 2}
i:3 'a' already in ht
    curr streak: 3, max streak: 3
	startposition = ht['a']+1=1, clear previous 'a' index and update new position,  ht = {'b': 1, 'c': 2, 'a': 3}

i:4 'b' already in ht
    curr streak: (i-startposition) = 3, max streak: 3
	startposition = ht['b']+1 =2, clear previous 'b' index and update new position,  ht = {'c': 2, 'a': 3, 'b': 4}
   

END: i=5
	current streak: (i-startposition) = 3, max-streak: 3
	
max-streak: 3



Sample run 2:
============
l = "abcbc"
ht = {}

i:0 ht = {'a': 0}
i:1 ht = {'a': 0, 'b': 1}
i:2 ht = {'a': 0, 'b': 1, 'c': 2}
i:3 'b' already in ht, clear previous 'b' index and update new position,  ht = {'a': 0, 'b': 3, 'c': 2}
    curr streak: 3, max streak: 3
	startposition = ht['b']+1 = 2, 
i: 4 'c' already in ht, clear previous 'c' index and update new position,  ht = {'a': 0, 'b': 3, 'c': 4}
    curr streak: (i-startposition)=2, max streak: 3
	startposition = ht['c']+1 = 3

END: i=5
	current streak: (i-startposition) = 2, max-streak: 3
max-streak: 3


Sample run 3:
============
l = "daabc"
ht = {}

i:0 ht = {'d': 0}
i:1 ht = {'d': 0, 'a': 1}
i:2 'a' already in ht, update 'a' index, ht = {'d': 0, 'a': 2}
    curr streak: (i-startposition) = 2, max streak: 2
	startposition = old ht['a'] = 1+1 = 2
i:3 ht = {'d': 0, 'a': 2, 'b': 3}
i: 4 ht = {'d': 0, 'a': 2, 'b': 3, 'c': 4}

END: i=5
	current streak: (i-startposition) = 3, max-streak: 3
max-streak: 3
  

'''


class Solution(object):
	@staticmethod
	def lookup(s, c):
		return s[ord(c) - ord('a')]

	@staticmethod
	def set(s, c, val):
		s[ord(c) - ord('a')] = val

	def lengthOfLongestSubstring(self, s):
		"""
		:type s: str
		:rtype: int
		"""

		if not s:
			return 0

		# if there are only going to be lowercase characters, just use a table for 'a-z'
		current_substr_table = [None]*26
		current_substr_len = 0
		max_substr_len = 0
		startposition = 0
		for i in range(len(s)):
			# either lookup table doesnt have s[i]
			# or we have 'cleared' it, by marking it as None
			if self.lookup(current_substr_table, s[i]) is None:
				self.set(current_substr_table, s[i], i)
			else:
				# Get current streak length (substring without repeats)
				current_substr_len = (i-startposition)
				if current_substr_len > max_substr_len:
					max_substr_len = current_substr_len

				old_idx = self.lookup(current_substr_table, s[i])

				# Don't let startposition slide backwards
				# this is important because at 'daabcd'
				# we have alreay decided at 'daa'
				# that 'd' is not part of the substring
				# we shouldn't be going back to startposition=1
				# when we encounter 'abcd'
				if old_idx >= startposition:
					startposition = old_idx+1

				# update s[i]'s index in hash-table
				self.set(current_substr_table, s[i], i)

		current_substr_len = (i+1-startposition)
		if current_substr_len > max_substr_len:
			max_substr_len = current_substr_len
		return max_substr_len



if __name__ == '__main__':
	s = Solution()
	assert s.lengthOfLongestSubstring("abba") == 2
	assert s.lengthOfLongestSubstring("abcbc") == 3
	assert s.lengthOfLongestSubstring("abcab") == 3
	assert s.lengthOfLongestSubstring("abcabcbb") == 3
	assert s.lengthOfLongestSubstring("daabc") == 3
	assert s.lengthOfLongestSubstring("bbbbbbb") == 1
