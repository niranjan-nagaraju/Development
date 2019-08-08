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

class Solution(object):
	def firstUniqChar(self, s):
		"""
		:type s: str
		:rtype: int
		"""
		counts = [0] * 26
		indices = [None] * 26
		for i in xrange(len(s)):
			map_idx = ord(s[i]) - ord('a')
			counts[map_idx] += 1

			# First time we are seeing this character,
			# capture its first occurence, at index i
			if indices[map_idx] is None:
				indices[map_idx] = i

		for c in s:
			map_idx = ord(c) - ord('a')
			if counts[map_idx] == 1:
				return indices[map_idx]

		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.firstUniqChar("") == -1
	assert s.firstUniqChar("cccc") == -1
	assert s.firstUniqChar("ccbb") == -1
	assert s.firstUniqChar("leetcode") == 0
	assert s.firstUniqChar("loveleetcode") == 2
