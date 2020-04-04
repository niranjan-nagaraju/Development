#encoding: utf-8
'''
Calculate aᵇ in O(log b) time

Binary Exponentiation
	Use the binary representation of b to determine if we need to multiply while jumping base by powers of 2
	  a², a⁴, ...

	For e.g,
	7¹¹
	11 : 1011

	1  0  1  1
	7⁸ 7⁴ 7² 7¹

	7¹¹ = 7⁸ * 7² * 7¹
	7⁸ = 7⁴ * 7⁴
	7⁴ = 7² * 7²
	7² = 7¹ * 7¹

as to calculating aᵇ mod m,
  (ab mod m) = (a mod m) . (b mod m)
  because (𝑎+ℎ𝑚)(𝑏+𝑘𝑚)=𝑎𝑏+(ℎ𝑘𝑚+𝑎𝑘+𝑏ℎ)𝑚
  aᵇ mod m == (a mod m) . (a mod m) ... (a mod m) {b times}
'''

# Calculate aᵇ
def power_(a, b):
	res = 1

	while b:
		# multiply result if bit{i} in b is set
		if b & 1 == 1:
			res = res * a
		
		a = a * a
		b = b >> 1

	return res


# calculate aᵇ mod m
def power_m(a, b, m):
	res = 1

	while b:
		# multiply result if bit{i} in b is set
		if b & 1 == 1:
			res = (res * a) % m
		
		a = (a * a) % m
		b = b >> 1

	return res


# calculate aᵇ mod m if m is specified, else just aᵇ
def power(a, b, m=None):
	if m is None:
		return power_(a, b)

	return power_m(a, b, m)



if __name__ == '__main__':
	assert power_(2, 3) == 8
	assert power_(3, 11) == 177147
	assert power_m(3, 11, 10) == 7
	assert power_m(3, 11, 5) == 2
	assert power_m(3, 11, 7) == 5

	assert power(2, 3) == 8
	assert power(3, 11) == 177147
	assert power(3, 11, 10) == 7
	assert power(3, 11, 5) == 2
	assert power(3, 11, 7) == 5
	assert power(7, 11, 1000) == 743

