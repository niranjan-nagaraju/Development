class Solution:
	# @param A : integer
	# @param B : integer
	# @return an integer
	def gcd_r(self, A, B):
		if B == 0:
			return A
		return self.gcd_r(B, A%B)


	def gcd(self, A, B):
		while B:
			r = A % B
			A = B
			B = r

		return A



if __name__ == '__main__':
	s = Solution()
	assert (s.gcd(6, 6) == 6 == s.gcd_r(6,6))
	assert (s.gcd(0, 6) == 6 == s.gcd_r(0,6))
	assert (s.gcd(6, 0) == 6 == s.gcd_r(6,0))
	assert (s.gcd(6, 4) == 2 == s.gcd_r(6,4))
	assert (s.gcd(4, 6) == 2 == s.gcd_r(4,6))
	assert (s.gcd(36,24) == 12 == s.gcd_r(36,24))

