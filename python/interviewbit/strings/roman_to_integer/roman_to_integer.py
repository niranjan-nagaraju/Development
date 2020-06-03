'''
https://www.interviewbit.com/problems/roman-to-integer/

Roman To Integer

Given a string A representing a roman numeral.
Convert A into integer.

A is guaranteed to be within the range from 1 to 3999.

NOTE: Read more
details about roman numerals at https://en.wikipedia.org/wiki/Roman_numerals#Roman_numeric_system

Input Format
The only argument given is string A.

Output Format
Return an integer which is the integer verison of roman numeral string.

For Example
Input 1:
    A = "XIV"
Output 1:
    14

Input 2:
    A = "XX"
Output 2:
    20
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
class Solution:
	def roman_to_integer(self, A):
		from collections import defaultdict

		# return 0 incase the string contains any other characters
		# 'XANKURM' : expected to return 1010
		values = defaultdict(
				lambda: 0, 
					{
					'I': 1,
					'V': 5,
					'X': 10,
					'L': 50,
					'C': 100,
					'D': 500,
					'M': 1000
					})

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
	assert s.roman_to_integer('IV') == 4
	assert s.roman_to_integer('IX') == 9
	assert s.roman_to_integer('XIV') == 14
	assert s.roman_to_integer('XL') == 40
	assert s.roman_to_integer('XC') == 90
	assert s.roman_to_integer('CXL') == 140
	assert s.roman_to_integer('CD') == 400
	assert s.roman_to_integer('CM') == 900
	assert s.roman_to_integer('MCD') == 1400

	assert s.roman_to_integer('MMCDXXI') == 2421
	assert s.roman_to_integer('XXXIX') == 39
	assert s.roman_to_integer('CCXLVI') == 246
	assert s.roman_to_integer('DCCLXXXIX') == 789

	assert s.roman_to_integer('CLX') == 160
	assert s.roman_to_integer('CCVII') == 207
	assert s.roman_to_integer('MIX') == 1009
	assert s.roman_to_integer('MLXVI') == 1066

	assert s.roman_to_integer('MDCCLXXVI') == 1776
	assert s.roman_to_integer('MCMXVIII') == 1918
	assert s.roman_to_integer('MCMLIV') == 1954
	assert s.roman_to_integer('MMXIV') == 2014
	assert s.roman_to_integer('MMXX') == 2020



