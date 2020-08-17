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


'''
Solution Outline:
	Consider 53
	we know BA == 53 (because B: 1, A: 0, (1+1)*26+(0+1) == 53)
	To work it out backwards,
	(53-1)%26 == 0 => A
	(53-1)/26 = 2
	(2-1) % 26 == 1 => B

	Consider Z
	  26 == Z
	  Backwards,
	  (26-1)%26 == 25 = Z
'''


class Solution:
	def numberToTitle(self, A):
		title = ""
		while A:
			title = chr((A-1)%26 + ord('A')) + title
			A  = (A-1)/26

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

