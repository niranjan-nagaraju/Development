'''
https://www.interviewbit.com/problems/add-binary-strings/

Add Binary Strings

Given two binary strings, return their sum (also a binary string).

Example:
a = "100"
b = "11"
Return a + b = "111".
'''

class Solution:
	# Add two single bits and a specified carry
	# Return addition result, and a new carry
	def add_single_bit(self, a, b, carry=0):
		'''
		Truth Table for adding 2 bits

		A  B  carry  result
		-----------  ------
		0  0   0     (0, 0)
		0  0   1     (1, 0)
		0  1   0     (1, 0) 
		0  1   1     (0, 1)
		1  0   0     (1, 0)
		1  0   1     (0, 1)
		1  1   0     (0, 1)
		1  1   1     (1, 1)
		'''
		if   (a,b,carry) == ('0', '0', 0):
			return '0', 0
		elif (a,b,carry) == ('0', '0', 1):
			return '1', 0
		elif (a,b,carry) == ('0', '1', 0):
			return '1', 0
		elif (a,b,carry) == ('0', '1', 1):
			return '0', 1
		elif (a,b,carry) == ('1', '0', 0):
			return '1', 0
		elif (a,b,carry) == ('1', '0', 1):
			return '0', 1
		elif (a,b,carry) == ('1', '1', 0):
			return '0', 1
		elif (a,b,carry) == ('1', '1', 1):
			return '1', 1


	def add_binary_strings(self, A, B):
		nA = len(A)
		nB = len(B)
	
		carry = 0

		# result binary string will be of size (maximum of A or B + 1)
		result = [0]*(max(nA, nB)+1)
		k = 1
		while k <= len(result):
			if k > nA:
				a = '0'
			else:
				a = A[-k]

			if k > nB:
				b = '0'
			else:
				b = B[-k]

			result[-k], carry = self.add_single_bit(a, b, carry)
			k += 1

		return ''.join(result) if result[0] == '1' else ''.join(result[1:])


if __name__ == '__main__':
	s = Solution()
	assert s.add_binary_strings('100', '11') == '111'
	assert s.add_binary_strings('110', '11') == '1001'

