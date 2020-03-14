'''
https://www.interviewbit.com/problems/largest-number/

Largest Number

Given a list of non negative integers, arrange them such that they form the largest number.
For example:
	Given [3, 30, 34, 5, 9], the largest formed number is 9534330.
	Note: The result may be very large, so you need to return a string instead of an integer.
'''

'''
Solution Outline:   #FIXME: FAILS for many TCs

Sort by descending order
key comparison: 
   Shorter number (by length) -> append first digit to the end till == other number's length
   AB vs WXYZ == ABAA vs WXYZ
for e,g,

62 vs 6167, 62 -> 626167
62 vs 6258 -> 6258 62 vs *62 6258*
62 vs 6265 -> 6265 62 vs 62 6265
62 vs 6267 -> 6267 62 vs *62 6762*
62 vs 6278 -> *6278 62* vs 62 6278

8 vs 89 == 88 vs 89 ==> "89 8"

60 vs 610 == 606 vs 610 => "610 60"
60 vs 601 == 606 vs 601 => "60 601"
60 vs 620 == 606 vs 620 => "620 60"

42 vs 4255 == 4244 vs 4255 => "4255 42"
42 vs 4231 == 4244 vs 4231 => "42 4231"
42 vs 4245 == 4244 vs 4245 => "4245 42"
42 vs 4240 == 4244 vs 4240 => "42 4240"
NOTE:
	when their values equal, return the one with greater length as greater of the two values
	for e.g.
	94 vs 949
	== 949 == 949
	however, we cannot combine '94 949' because '949 94' is greater in value
'''

class Solution:
	def largest_number(self, A):
		for i in xrange(len(A)):
			A[i] = str(A[i])

		number = ''.join(sorted(A, cmp=Solution.compare_keys, reverse=True))
		return str(int(number)) # so '0000' becomes '0'

	@staticmethod
	def compare_keys(a, b):
		orig_a = a
		orig_b = b

		if len(a) > len(b):
			b += b[0]*(len(a)-len(b))
		else: # len(a) <= len(b)
			a += a[0]*(len(b)-len(a))

		result = cmp(int(a), int(b))
		if result == 0:
			return len(orig_a) - len(orig_b)

		return result



if __name__ == '__main__':
	s = Solution()
	assert s.compare_keys('524', '61') == -1
	assert s.compare_keys('54', '548') == -1
	assert s.compare_keys('42', '4255') == -1
	assert s.compare_keys('42', '4231') == 1
	assert s.compare_keys('610', '60') == 1
	assert s.compare_keys('9', '98') == 1
	assert s.compare_keys('8', '89') == -1
	assert s.compare_keys('94', '949') == -1 # even though 949 == 949, the final string should be longer string + smaller == 94994 vs 94949

	assert s.largest_number([8, 89]) == '898'
	assert s.largest_number([54, 546, 548, 60]) == '6054854654'
	assert s.largest_number([1, 34, 3, 98, 9, 76, 45, 4]) == '998764543431'
	assert s.largest_number([3, 30, 34, 5, 9]) == '9534330'
	assert s.largest_number([12, 121]) == '12121'  # FAIL



