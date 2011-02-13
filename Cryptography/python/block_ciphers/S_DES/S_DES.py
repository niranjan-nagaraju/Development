
P10 = [3, 5, 2, 7, 4, 10, 1, 9, 8, 6]
P8 = [6, 3, 7, 4, 8, 5, 10, 9]

def apply_p10(key):
	permuted_key = []
	for i in range(0, 10):
		permuted_key.append(key[P10[i]-1])

	return permuted_key

def apply_p8(key):
	permuted_key = []
	for i in range(0, 8):
		permuted_key.append(key[P8[i]-1])

	return permuted_key

def left_shift(key, i):
	return key[i:] + key[:i]

def generate_key(key):
	p10_key = apply_p10(key)

	print 'P10 ->', p10_key

	ls1_l = left_shift(p10_key[:5], 1)
	ls1_r = left_shift(p10_key[5:], 1)

	print 'LS1 ->', ls1_l, ls1_r

	k1 = apply_p8(ls1_l + ls1_r)

	print 'K1 ->', k1

	ls2_l = left_shift(ls1_l, 2)
	ls2_r = left_shift(ls1_r, 2)

	print 'LS2 ->', ls2_l, ls2_r

	k2 = apply_p8(ls2_l + ls2_r)
	print 'K2 ->', k2


generate_key([1,0,1,0,0,0,0,0,1,0])
