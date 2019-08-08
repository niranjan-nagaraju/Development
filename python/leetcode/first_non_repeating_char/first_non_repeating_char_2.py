
'''
387. First Unique Character in a String

https://leetcode.com/problems/first-unique-character-in-a-string/

Given a string, find the first non-repeating character in it and return it's index. If it doesn't exist, return -1.

Examples:
	s = "leetcode"
	return 0.

	s = "loveleetcode",
	return 2.

Note: You may assume the string contain only lowercase letters.
'''

from collections import defaultdict
class Solution(object):
	def firstUniqChar(self, s):
		"""
		:type s: str
		:rtype: int
		"""
		counts = defaultdict(int)
		for i in xrange(len(s)):
			counts[s[i]] += 1

		for i in xrange(len(s)):
			if counts[s[i]] == 1:
				return i

		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.firstUniqChar("") == -1
	assert s.firstUniqChar("cccc") == -1
	assert s.firstUniqChar("ccbb") == -1
	assert s.firstUniqChar("leetcode") == 0
	assert s.firstUniqChar("loveleetcode") == 2
