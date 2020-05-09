'''
https://www.interviewbit.com/problems/reverse-the-string/

Reverse the String

Given a string A.

Return the string A after reversing the string word by word.

NOTE:
A sequence of non-space characters constitutes a word.
Your reversed string should not contain leading or trailing spaces, even if it is present in the input string.
If there are multiple spaces between words, reduce them to a single space in the reversed string.

Input Format
The only argument given is string A.

Output Format
Return the string A after reversing the string word by word.

For Example
Input 1:
    A = "the sky is blue"
Output 1:
    "blue is sky the"

Input 2:
    A = "this is ib"
Output 2:
    "ib is this"
'''
class Solution:
	# @param A : string
	# @return a strings
	def solve(self, A):
		# reverse A[l..r]
		def reverse(A, l, r):
			while l < r:
				A[l],A[r] = A[r],A[l]
				l+=1
				r-=1

		if not A:
			return A

		B = []

		# skip leading spaces
		left = 0
		while A[left].isspace():
			left += 1

		# skip trailing spaces
		n = len(A)    
		right = n-1
		while A[right].isspace():
			right -= 1

		x = left
		word_start = 0
		while x <= right:
			inspace = False
			while x<=right and A[x].isspace():
				# skip spaces
				x+=1
				inspace = True

			if inspace:
				# Reverse current word
				# ending of the current word is the last character in B
				reverse(B, word_start, len(B)-1)

				# if we had skipped atleast 1 'space'
				# restore a single 'space' in its place
				B.append(' ')

				# next word starts after the space
				word_start = len(B)

			B.append(A[x])
			x += 1

		# reverse last word
		reverse(B, word_start, len(B)-1)

		# reverse the whole string
		# so only the words order is reversed
		reverse(B, 0, len(B)-1)
		return ''.join(B)



if __name__ == '__main__':
	s = Solution()
	assert s.solve("   the  sky is  blue ") == "blue is sky the"
	assert s.solve("I am blue da ba deee") == "deee ba da blue am I"

