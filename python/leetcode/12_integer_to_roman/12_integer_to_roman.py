'''
Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.

Symbol       Value
I             1
V             5
X             10
L             50
C             100
D             500
M             1000
For example, 2 is written as II in Roman numeral, just two one's added together. 12 is written as XII, which is simply X + II. The number 27 is written as XXVII, which is XX + V + II.

Roman numerals are usually written largest to smallest from left to right. However, the numeral for four is not IIII. Instead, the number four is written as IV. Because the one is before the five we subtract it making four. The same principle applies to the number nine, which is written as IX. There are six instances where subtraction is used:

I can be placed before V (5) and X (10) to make 4 and 9. 
X can be placed before L (50) and C (100) to make 40 and 90. 
C can be placed before D (500) and M (1000) to make 400 and 900.
Given an integer, convert it to a roman numeral.

 

Example 1:

Input: num = 3
Output: "III"
Explanation: 3 is represented as 3 ones.
Example 2:

Input: num = 58
Output: "LVIII"
Explanation: L = 50, V = 5, III = 3.
Example 3:

Input: num = 1994
Output: "MCMXCIV"
Explanation: M = 1000, CM = 900, XC = 90 and IV = 4.
 

Constraints:
1 <= num <= 3999
'''

class Solution(object):
    def intToRoman(self, num):
        """
        :type num: int
        :rtype: str
        """
        units = ['', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX']
        tens = ['', 'X', 'XX', 'XXX', 'XL', 'L', 'LX', 'LXX', 'LXXX', 'XC']
        hundreds = ['', 'C', 'CC', 'CCC', 'CD', 'D', 'DC', 'DCC', 'DCCC', 'CM']
        thousands = ['', 'M', 'MM', 'MMM'] # Max value of A is 3999

        roman = thousands[num//1000]
        num %= 1000
        roman += hundreds[num//100]
        num %= 100
        roman += tens[num//10]
        num %= 10
        roman += units[num]

        return roman


if __name__ == '__main__':
	s = Solution()
	assert s.intToRoman(1234) == 'MCCXXXIV'
	assert s.intToRoman(4) == 'IV'
	assert s.intToRoman(9) == 'IX'
	assert s.intToRoman(14) == 'XIV'
	assert s.intToRoman(40) == 'XL'
	assert s.intToRoman(90) == 'XC'
	assert s.intToRoman(140) == 'CXL'
	assert s.intToRoman(400) == 'CD'
	assert s.intToRoman(900) == 'CM'
	assert s.intToRoman(1400) == 'MCD'

	assert s.intToRoman(2421) == 'MMCDXXI'
	assert s.intToRoman(39) == 'XXXIX'
	assert s.intToRoman(246) == 'CCXLVI'
	assert s.intToRoman(789) == 'DCCLXXXIX'

	assert s.intToRoman(160) == 'CLX'
	assert s.intToRoman(207) == 'CCVII'
	assert s.intToRoman(1009) == 'MIX'
	assert s.intToRoman(1066) == 'MLXVI'

	assert s.intToRoman(1776) == 'MDCCLXXVI'
	assert s.intToRoman(1918) == 'MCMXVIII'
	assert s.intToRoman(1954) == 'MCMLIV'
	assert s.intToRoman(2014) == 'MMXIV'
	assert s.intToRoman(2020) == 'MMXX'


        
