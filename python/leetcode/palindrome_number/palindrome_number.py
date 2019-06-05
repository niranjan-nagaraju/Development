'''
https://leetcode.com/problems/palindrome-number/
Determine whether an integer is a palindrome
'''
class Solution(object):
	def isPalindrome(self, x):
		"""
		:type x: int
		:rtype: bool
		"""
		if x < 0:
			return False

		if x < 10:
			return True

		xStr = str(x)
		i, j = 0, len(xStr)-1
		while i < j:
			if xStr[i] != xStr[j]:
				return False
			i+= 1
			j-= 1

		return True


if __name__ == '__main__':
	s = Solution()
	assert s.isPalindrome(1) == True
	assert s.isPalindrome(-1) == False
	assert s.isPalindrome(-121) == False
	assert s.isPalindrome(121) == True
	assert s.isPalindrome(11) == True
	assert s.isPalindrome(10) == False
