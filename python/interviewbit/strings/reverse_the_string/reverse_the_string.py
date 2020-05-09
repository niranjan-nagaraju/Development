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

