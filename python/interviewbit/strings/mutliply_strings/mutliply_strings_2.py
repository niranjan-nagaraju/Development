'''
https://www.interviewbit.com/problems/multiply-strings/

Multiply Strings

Given two numbers represented as strings, return multiplication of the numbers as a string.

Note: The numbers can be arbitrarily large and are non-negative.
Note2: Your answer should not have leading zeroes. For example, 00 is not a valid answer. 
For example,
given strings "12", "10", your answer should be "120".

NOTE : DO NOT USE BIG INTEGER LIBRARIES ( WHICH ARE AVAILABLE IN JAVA / PYTHON ).
We will retroactively disqualify such submissions and the submissions will incur penalties.
'''

'''
Solution Outline #2: (Optimize temporary variables from #1, i for Ai, j for Bj, are enough)
999 * 999 == 998001 => Product(a*b) where len(a): d1, len(b): d2, product(a*b) will have maximum of (d1+d2) digits.
	1 2 3 x 4 5 6
	-------------
	7 3 8
  6 1 5 +
4 9 2 +
---------
5 6 0 8 8

0. Let len(A) = dA, len(B) = dB
1. Initialize an accumulator of size (d1+d2) filled with 0s, k = 1
2. For each digit Ai i: [dA-1 .. 0], multiply with Bj, j: [dB-1 .. 0]
     for each Ai * Bj,
		Add the result in the accumulator at index [-k-l] {l: dA-1-i}
	So product by last digit of B gets filled right-left without any shifts
	product by last-but-one digit of B gets filled with 1 shift at the end, and so on.
3. Trim leading 0s and return the accumulator as a string.
   [If A and B both don't have leading zeroes, A*B won't either, perhaps trimming A and B before multiplication would help avoid this last step]

Sample run:
	A: "123"
	B: "45"

	Acc: [0, 0, 0, 0, 0]
	Bj: 5
	 x A:
	   5x3+0: 5, carry: 1
		Acc: [0, 0, 0, 0, 5]
	   5x2+1: 1, carry: 1
		Acc: [0, 0, 0, 1, 5]
	   5x1+1: 7, carry: 0
		Acc: [0, 0, 6, 1, 5]

	Bj: 4
		Acc: [0, 0, 6, 1, 5]
	 x A:
	   4x3+0: 2, carry: 1
	    Add to acc[-2]: 2+1 == 3, carry: 1
		Acc: [0, 0, 6, 3, 5]
	   4x2+1: 9, carry: 0
	    Add to acc[-3]: 6+9 == 5, carry: 1
		Acc: [0, 0, 5, 3, 5]
	   4x1+1: 5, carry: 0
	    Add to acc[-4]: 0+5 == 5, carry: 0
		Acc: [0, 5, 5, 3, 5]
	
	Return "5535"


Sample run 2:
	A: "999"
	B: "999"

	Acc: [0, 0, 0, 0, 0, 0]
	Bj: 9
	  x A:
	    9x9+0: 81=> 1, carry: 8
		 Acc: [0, 0, 0, 0, 0, 1]
	    9x9+8: 89=> 9, carry: 8
		 Acc: [0, 0, 0, 0, 9, 1]
	    9x9+8: 89=>9, carry: 8
		 Acc: [0, 0, 0, 9, 9, 1]
		Acc: [0, 0, 8, 9, 9, 1]

	Bj: 9
	  Acc: [0, 0, 8, 9, 9, 1]
	  x A:
	    9x9+0: 81
	     Add to acc[-2]: 9+81 == 90, => 0, carry: 9
		 Acc: [0, 0, 8, 9, 0, 1]
	    9x9+9: 90
	     Add to acc[-3]: 9+90 == 99, => 9, carry: 9
		 Acc: [0, 0, 8, 9, 0, 1]
	    9x9+9: 90
	     Add to acc[-4]: 8+90 == 98, => 8, carry: 9
		 Acc: [0, 0, 8, 9, 0, 1]
		Acc: [0, 9, 8, 9, 0, 1]

	Bj: 9
	  Acc: [0, 9, 8, 9, 0, 1]
	  x A:
	    9x9+0: 81
	     Add to acc[-3]: 9+81 == 90, => 0, carry: 9
		 Acc: [0, 9, 8, 0, 0, 1]
	    9x9+9: 90
	     Add to acc[-4]: 8+90 == 98, => 8, carry: 9
		 Acc: [0, 9, 8, 0, 0, 1]
	    9x9+9: 90
	     Add to acc[-5]: 9+90 == 99, => 9, carry: 9
		 Acc: [0, 9, 8, 0, 0, 1]
		Acc: [9, 9, 8, 0, 0, 1]

	return "998001"
'''
class Solution:
	def multiply(self, A, B):
		# Helper lambdas to convert between a single digit-string to integer
		# and vice-versa
		chrToInt = lambda x: ord(x)-ord('0')
		intToChr = lambda x: chr(ord('0')+x)
		m = len(A)
		n = len(B)

		product = [0]*(m+n)
		for j in xrange(1, n+1):
			carry = 0
			for i in xrange(1, m+1):
				p = chrToInt(B[-j]) * chrToInt(A[-i]) + carry + product[-j-i+1]
				carry = p/10
				product[-j-i+1] = p%10

			product[-j-i] = carry

		# product is all 0s
		# return a single 0
		if product == [0]*(m+n):
			return '0'

		# skip leading 0s
		i = 0
		while product[i] == 0:
			i += 1

		return reduce(lambda acc,x: acc+intToChr(x), product[i:], '')	


if __name__ == '__main__':
	s = Solution()
	assert s.multiply("123", "") == '0'
	assert s.multiply("123", "0") == '0'
	assert s.multiply("123", "1") == '123'
	assert s.multiply("12", "10") == '120'
	assert s.multiply("1", "123") == '123'
	assert s.multiply("123", "45") == '5535'
	assert s.multiply("45", "123") == '5535'
	assert s.multiply("999", "999") == '998001'
	assert s.multiply("101", "999") == '100899'
	assert s.multiply("999", "101") == '100899'

