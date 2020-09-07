'''
https://www.interviewbit.com/problems/fraction/

Fraction

Given two integers representing the numerator and denominator of a fraction, return the fraction in string format.

If the fractional part is repeating, enclose the repeating part in parentheses.

Example :

Given numerator = 1, denominator = 2, return "0.5"
Given numerator = 2, denominator = 1, return "2"
Given numerator = 2, denominator = 3, return "0.(6)"
'''

'''
Solution Outline:
    Consider 50/22

        ------
    22)   50  (2.272
          44
        ------
           6 * 10
          60
          44
        ------
          16 * 10
          160
          154
        ------
            6 * 10
          60
          44
        ------
          16

    50/22 == 2.(27)
    when remainder repeats, the quotient repeats from the last time the same remainder was seen
    Use a hash-table to store the remainders to lookup repeats.
'''
class Solution:
	def get_decimal(self, numerator, denominator):
		if denominator == 0:
			raise ValueError('Divide by zero!')

		if numerator % denominator == 0:
			return str(numerator / denominator)

		sign = ''
		if (numerator < 0 and denominator > 0) or (denominator < 0 and numerator >= 0):
			sign = '-'

		numerator = abs(numerator)
		denominator = abs(denominator)
		quotients = [numerator // denominator]

		remainder = numerator % denominator
		remainder_tbl = {}
		while remainder != 0:
			if remainder_tbl.get(remainder) is not None:
				# remainder was previously seen when quotient was quotients[idx]
				idx = remainder_tbl[remainder]
				return sign + str(quotients[0]) + '.' + ''.join(map(str,quotients[1:idx])) + '(' + ''.join(map(str, quotients[idx:])) + ')'


			# Store the start of the quotient when we last saw this remainder
			remainder_tbl[remainder] = len(quotients)
			remainder *= 10
			numerator = remainder / denominator
			quotients.append(numerator)
			remainder = remainder % denominator

		# At this point, the remainder doesn't repeat
		# and so there are no repeating sequences in the quotients
		# Just join the integer quotient and the decimal parts
		return sign + str(quotients[0]) + '.' + ''.join(map(str, quotients[1:]))


if __name__ == '__main__':
	s = Solution()
	assert s.get_decimal(50, 8) == '6.25'
	assert s.get_decimal(50, 16) == '3.125'
	assert s.get_decimal(50, 22) == '2.(27)'
	assert s.get_decimal(50, 5) == '10'
	assert s.get_decimal(11, 6) == '1.8(3)'
	assert s.get_decimal(1, 2) == '0.5'
	assert s.get_decimal(2, 1) == '2'
	assert s.get_decimal(2, 3) == '0.(6)'
	assert s.get_decimal(22, 7) == '3.(142857)' # (22/7) is not pi
	assert s.get_decimal(-1, 2) == '-0.5'

