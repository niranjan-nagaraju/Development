'''
https://www.interviewbit.com/problems/excel-column-title/

Excel Column Title

Given a positive integer, return its corresponding column title as appear in an Excel sheet.

For example:
    1 -> A
    2 -> B
    3 -> C
    ...
    26 -> Z
    27 -> AA
    28 -> AB 
'''


class Solution:
	def numberToTitle(self, A):
		title = ""
		while A:
			d = A % 26
			A /= 26

			if d == 0:
				title = 'Z' + title
			else:
				title = (chr(d + ord('A') - 1)) + title

			if d == 0:
				A -= 1

		return title


if __name__ == '__main__':
	s = Solution()
	assert s.numberToTitle(26) == 'Z'
	assert s.numberToTitle(27) == 'AA'
	assert s.numberToTitle(28) == 'AB'
	assert s.numberToTitle(52) == 'AZ'
	assert s.numberToTitle(53) == 'BA'
	assert s.numberToTitle(78) == 'BZ'
	assert s.numberToTitle(2054) == 'BZZ'

