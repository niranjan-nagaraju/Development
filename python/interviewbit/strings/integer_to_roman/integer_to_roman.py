'''
https://www.interviewbit.com/problems/integer-to-roman/

Integer To Roman

Please Note:
	Another question which belongs to the category of questions which are intentionally stated vaguely.
	Expectation is that you will ask for correct clarification or you will state your assumptions before you start coding.

Input Format
The only argument given is integer A.

Output Format
Return a string denoting roman numeral version of A.

Constraints
1 <= A <= 3999

For Example
Input 1:
    A = 5
Output 1:
    "V"

Input 2:
    A = 14
Output 2:
    "XIV"
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
'''
class Solution:
	def integer_to_roman(self, A):
		units = ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX']
		tens= ['X', 'XX', 'XXX', 'XL', 'L', 'LX', 'LXX', 'LXXX', 'XC']
		hundreds = ['C', 'CC', 'CCC', 'CD', 'D', 'DC', 'DCC', 'DCCC', 'CM']
		thousands = ['M', 'MM', 'MMM'] # Max value of A is 3999

		conversion_tbl = [units, tens, hundreds, thousands]

		roman = ""
		for i in xrange(4):
			d = A%10
			A /= 10
			roman = (conversion_tbl[i][d-1] if d else '') + roman

			if A == 0:
				break

		return roman



if __name__ == '__main__':
	s = Solution()
	assert s.integer_to_roman(1234) == 'MCCXXXIV'
	assert s.integer_to_roman(4) == 'IV'
	assert s.integer_to_roman(9) == 'IX'
	assert s.integer_to_roman(14) == 'XIV'
	assert s.integer_to_roman(40) == 'XL'
	assert s.integer_to_roman(90) == 'XC'
	assert s.integer_to_roman(140) == 'CXL'
	assert s.integer_to_roman(400) == 'CD'
	assert s.integer_to_roman(900) == 'CM'
	assert s.integer_to_roman(1400) == 'MCD'

	assert s.integer_to_roman(2421) == 'MMCDXXI'
	assert s.integer_to_roman(39) == 'XXXIX'
	assert s.integer_to_roman(246) == 'CCXLVI'
	assert s.integer_to_roman(789) == 'DCCLXXXIX'

	assert s.integer_to_roman(160) == 'CLX'
	assert s.integer_to_roman(207) == 'CCVII'
	assert s.integer_to_roman(1009) == 'MIX'
	assert s.integer_to_roman(1066) == 'MLXVI'

	assert s.integer_to_roman(1776) == 'MDCCLXXVI'
	assert s.integer_to_roman(1918) == 'MCMXVIII'
	assert s.integer_to_roman(1954) == 'MCMLIV'
	assert s.integer_to_roman(2014) == 'MMXIV'
	assert s.integer_to_roman(2020) == 'MMXX'


