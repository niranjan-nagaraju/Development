'''
https://www.interviewbit.com/problems/largest-number/

Largest Number

Given a list of non negative integers, arrange them such that they form the largest number.
For example:
	Given [3, 30, 34, 5, 9], the largest formed number is 9534330.
	Note: The result may be very large, so you need to return a string instead of an integer.
'''

'''
Solution Outline:

Sort by descending order
key comparison: 
	a = AB vs b = WXYZ
	compare (a+b) vs (b+a)
	ie ABWXYZ vs WXYZAB

	for e.g, 94 vs 949 == 94949 vs 94994
	12 vs 121 == 12121 vs 12112
'''

class Solution:
	def largest_number(self, A):
		for i in xrange(len(A)):
			A[i] = str(A[i])

		number = ''.join(sorted(A, cmp=Solution.compare_keys, reverse=True))
		return str(int(number)) # so '0000' becomes '0'

	@staticmethod
	def compare_keys(a, b):
		return cmp(int(a+b), int(b+a))



if __name__ == '__main__':
	s = Solution()
	assert s.compare_keys('524', '61') == -1
	assert s.compare_keys('54', '548') == -1
	assert s.compare_keys('42', '4255') == -1
	assert s.compare_keys('42', '4231') == 1
	assert s.compare_keys('610', '60') == 1
	assert s.compare_keys('9', '98') == 1
	assert s.compare_keys('8', '89') == -1
	assert s.compare_keys('94', '949') == -1 # 94994 vs 94949

	assert s.largest_number([8, 89]) == '898'
	assert s.largest_number([54, 546, 548, 60]) == '6054854654'
	assert s.largest_number([1, 34, 3, 98, 9, 76, 45, 4]) == '998764543431'
	assert s.largest_number([3, 30, 34, 5, 9]) == '9534330'
	assert s.largest_number([12, 121]) == '12121'


