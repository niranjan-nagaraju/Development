'''
https://www.interviewbit.com/problems/length-of-last-word/

Given a string s consists of upper/lower-case alphabets and empty space characters ' ', return the length of last word in the string.
If the last word does not exist, return 0.

Note: A word is defined as a character sequence consists of non-space characters only.

Example:
	Given s = "Hello World",
	return 5 as length("World") = 5.
Please make sure you try to solve this problem without using library functions. Make sure you only traverse the string once.
'''

class Solution:
	# @param A : string
	# @return an integer
	def lengthOfLastWord(self, A):
		if not A:
			return 0

		i = len(A)-1
		while i>=0 and A[i].isspace():
			i -= 1

		cnt = 0   
		while i>=0 and not A[i].isspace():
			cnt+=1
			i -= 1

		return cnt


if __name__ == '__main__':
	s = Solution()
	assert s.lengthOfLastWord(" ") == 0
	assert s.lengthOfLastWord("hello world! ") == 6
	assert s.lengthOfLastWord("hello world!") == 6
	assert s.lengthOfLastWord("		 World			") == 5

