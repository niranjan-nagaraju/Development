'''
https://www.interviewbit.com/problems/palindrome-string/

Given a string, determine if it is a palindrome, considering only alphanumeric characters and ignoring cases.

Example:
	"A man, a plan, a canal: Panama" is a palindrome.
	"race a car" is not a palindrome.

Return 0 / 1 ( 0 for false, 1 for true ) for this problem
'''
class Solution:
	# @param A : string
	# @return an integer
	def isPalindrome(self, A):
		i,j = 0, len(A)-1

		if not A:
			return 0

		while i < j:
			if not A[i].isalnum():
				i += 1
				continue

			if not A[j].isalnum():
				j -= 1
				continue

			if A[i].lower() != A[j].lower():
				return 0

			i += 1
			j -= 1

		return 1

if __name__ == '__main__':
	s = Solution()
	assert s.isPalindrome("A man, a plan, a canal: Panama") == 1
	assert s.isPalindrome("race a car") == 0

