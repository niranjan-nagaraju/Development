'''
https://www.interviewbit.com/problems/excel-column-number/

Excel Column Number

Given a column title as appears in an Excel sheet, return its corresponding column number.

Example:

    A -> 1
    B -> 2
    C -> 3
    ...
    Z -> 26
    AA -> 27
    AB -> 28 
'''
class Solution:
	# @param A : string
	# @return an integer
	def titleToNumber(self, A):
		en = 0
		for x in A:
			d = ord(x)-ord('A')+1
			en = en*26 + d

		return en



if __name__ == '__main__':
	s = Solution()
	assert s.titleToNumber('A') == 1
	assert s.titleToNumber('Z') == 26
	assert s.titleToNumber('AA') == 27
	assert s.titleToNumber('AZ') == 52
	assert s.titleToNumber('BA') == 53
	assert s.titleToNumber('BZ') == 78
	assert s.titleToNumber('BZZ') == 2054

