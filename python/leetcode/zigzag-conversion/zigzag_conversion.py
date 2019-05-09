'''
https://leetcode.com/problems/zigzag-conversion/

The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)

P   A   H   N
A P L S I I G
Y   I   R
And then read line by line: "PAHNAPLSIIGYIR"

Example 1:

Input: s = "PAYPALISHIRING", numRows = 3
Output: "PAHNAPLSIIGYIR"

Example 2:

Input: s = "PAYPALISHIRING", numRows = 4
Output: "PINALSIGYAHRPI"
Explanation:

P     I    N
A   L S  I G
Y A   H R
P     I


'''

'''
Solution:

after compressing un-needed spaces:
P   I   N
A L S I G
Y A H R
P   I
'''



class Solution(object):
	def convert(self, s, numRows):
		"""
		:type s: str
		:type numRows: int
		:rtype: str
		"""

		rows = [[] for _ in xrange(numRows)]
		c = 0
		while True:
			try:
				for i in xrange(numRows):
					rows[i].append(s[c])
					c+=1

				for j in xrange(numRows-2, 0, -1):
					rows[j].append(s[c])
					c+=1
			except IndexError:
				break

		converted_str = reduce(lambda s,l: s+''.join(l), rows, '')
		return converted_str


if __name__ == '__main__':
	s = Solution()
	assert s.convert("PAYPALISHIRING", 3)  == "PAHNAPLSIIGYIR"
	assert s.convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI"


