
# Recursive version
def gcd_r(m, n):
	if n == 0:
		return m
	else:
		return gcd(n,m%n)


# Iterative version
def gcd_i(A, B):
	while B:
		r = A % B
		A = B
		B = r

	return A

gcd = gcd_i # Use iterative version by default

if __name__ == '__main__':
	assert (gcd(6, 6) == 6 == gcd_r(6,6))
	assert (gcd(0, 6) == 6 == gcd_r(0,6))
	assert (gcd(6, 0) == 6 == gcd_r(6,0))
	assert (gcd(6, 4) == 2 == gcd_r(6,4))
	assert (gcd(4, 6) == 2 == gcd_r(4,6))
	assert (gcd(36,24) == 12 == gcd_r(36,24))

