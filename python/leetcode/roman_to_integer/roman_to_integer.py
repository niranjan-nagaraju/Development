'''
https://leetcode.com/problems/roman-to-integer/

13. Roman to Integer

Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.

Symbol       Value
I             1
V             5
X             10
L             50
C             100
D             500
M             1000
For example, two is written as II in Roman numeral, just two one's added together. Twelve is written as, XII, which is simply X + II. The number twenty seven is written as XXVII, which is XX + V + II.

Roman numerals are usually written largest to smallest from left to right. However, the numeral for four is not IIII. Instead, the number four is written as IV. Because the one is before the five we subtract it making four. The same principle applies to the number nine, which is written as IX. There are six instances where subtraction is used:

I can be placed before V (5) and X (10) to make 4 and 9. 
X can be placed before L (50) and C (100) to make 40 and 90. 
C can be placed before D (500) and M (1000) to make 400 and 900.
Given a roman numeral, convert it to an integer. Input is guaranteed to be within the range from 1 to 3999.

Example 1:

Input: "III"
Output: 3
Example 2:

Input: "IV"
Output: 4
Example 3:

Input: "IX"
Output: 9
Example 4:

Input: "LVIII"
Output: 58
Explanation: L = 50, V= 5, III = 3.
Example 5:

Input: "MCMXCIV"
Output: 1994
Explanation: M = 1000, CM = 900, XC = 90 and IV = 4.
'''


'''
Solution Outline:
	From wiki,
	Symbol	I	V	X	L	C	 D	  M
	Value	1	5	10	50	100	 500  1000


	Individual decimal places
		Thousands	Hundreds	Tens	Units
	1	M			C			X		I
	2	MM			CC			XX		II
	3	MMM			CCC			XXX		III
	4				CD			XL		IV
	5				D			L		V
	6				DC			LX		VI
	7				DCC			LXX		VII
	8				DCCC		LXXX	VIII
	9				CM			XC		IX


	'I' preceding 'X' and 'V' contributes -1 to the result
	   IX: 9
	   IV: 4
	   XIV: 14
	'X' preceding 'L' and 'C' contributes -10
	   XL: 40
	   XC: 90
	   CXL: 140
	'C' preceding 'D' and 'M' contributes -100
	   CD: 400
	   CM: 900
	   MCD: 1400

	Everything else contributes a +ve value
'''

class Solution(object):
	def romanToInt(self, A):
		"""
		:type A: str
		:rtype: int
		"""

		values = {
					'I': 1,
					'V': 5,
					'X': 10,
					'L': 50,
					'C': 100,
					'D': 500,
					'M': 1000
					}

		result = 0
		for i in xrange(len(A)):
			result += values[A[i]]
			if i+1 < len(A):
				if A[i] == 'I' and (A[i+1] in ('V', 'X')):
					# Undo previous +1, so net result is is result-1
					result -= 2
				elif A[i] == 'X' and (A[i+1] in ('L', 'C')):
					# Undo previous +10, so net result is is result-10
					result -= 20
				elif A[i] == 'C' and (A[i+1] in ('D', 'M')):
					# Undo previous +100, so net result is is result-100
					result -= 200

		return result

if __name__ == '__main__':
	s = Solution()
	assert s.romanToInt('IV') == 4
	assert s.romanToInt('IX') == 9
	assert s.romanToInt('XIV') == 14
	assert s.romanToInt('XL') == 40
	assert s.romanToInt('XC') == 90
	assert s.romanToInt('CXL') == 140
	assert s.romanToInt('CD') == 400
	assert s.romanToInt('CM') == 900
	assert s.romanToInt('MCD') == 1400

	assert s.romanToInt('MMCDXXI') == 2421
	assert s.romanToInt('XXXIX') == 39
	assert s.romanToInt('CCXLVI') == 246
	assert s.romanToInt('DCCLXXXIX') == 789

	assert s.romanToInt('CLX') == 160
	assert s.romanToInt('CCVII') == 207
	assert s.romanToInt('MIX') == 1009
	assert s.romanToInt('MLXVI') == 1066

	assert s.romanToInt('MDCCLXXVI') == 1776
	assert s.romanToInt('MCMXVIII') == 1918
	assert s.romanToInt('MCMLIV') == 1954
	assert s.romanToInt('MMXIV') == 2014
	assert s.romanToInt('MMXX') == 2020



