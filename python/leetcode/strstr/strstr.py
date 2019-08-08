'''
https://leetcode.com/problems/implement-strstr/

Implement strStr().

Return the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.

Example 1:
	Input: haystack = "hello", needle = "ll"
	Output: 2

Example 2:
	Input: haystack = "aaaaa", needle = "bba"
	Output: -1

Clarification:
	What should we return when needle is an empty string? This is a great question to ask during an interview.
	For the purpose of this problem, we will return 0 when needle is an empty string. This is consistent to C's strstr() and Java's indexOf().
'''


'''
Solution Outline:
	Use a map/hash-table to create a reverse mapping of characters in 'haystack' to their indices.
	Align characters from 'needle' into 'haystack' and compare next characters until all characters match.
	If at any point, 'haystack' no longer matches 'needle', restart with next index at where first character of 'needle'
	begins in 'haystack'


Example:
	haystack: "abcaacad"
	needle: "aca"

Map:
|---+---------|
| a | 0 3 4 6 |
| b | 1       |
| c | 2 5     |
| d | 7       |

align "aca" with first occurence of "a" in haystack = 0
i: 0
  "a" = "a"
i: 1
  "b" != "c"

Next index of "a" in haystack = 3
i: 3
  "a" = "a"
i: 4
  "a" != "c"

Next index of "a" in haystack = 4
i: 4
  "a" = "a"
i: 5
  "c" == "c"
i: 6
  "a" == "a"
  fully matched, return 4

NOTE: comparisons after  aligning needle with haystack happens without the need for the map.
      Since the map is only needed for alignment. we need only store all indices where the first character of needle
	  appears in haystack.
'''

class Solution(object):
	def strStr(self, haystack, needle):
		"""
		:type haystack: str
		:type needle: str
		:rtype: int
		"""
		if not needle:
			return 0

		startIdxs = []
		for i in xrange(len(haystack)):
			if haystack[i] == needle[0]:
				startIdxs.append(i)

		for idx in startIdxs:
			i, j = idx, 0
			while j < len(needle):
				try:
					if needle[j] != haystack[i]:
						break
					j += 1
					i += 1
				except IndexError:
					return -1

			# matched completely
			if j == len(needle):
				return idx

		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.strStr("abcaacad", "aca") == 4

