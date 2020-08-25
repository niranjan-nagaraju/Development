
P10 = [3, 5, 2, 7, 4, 10, 1, 9, 8, 6]
P8 = [6, 3, 7, 4, 8, 5, 10, 9]

IP = [2, 6, 3, 1, 4, 8, 5, 7]
IP_inv = [4, 1, 3, 5, 7, 2, 8, 6]

E_P = [4, 1, 2, 3, 2, 3, 4, 1]

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


def apply_ip (plaintext):
	permuted_plaintext = []
	for i in range(0, 8):
		permuted_plaintext.append(plaintext[IP[i]-1])

	return permuted_plaintext


def apply_ip_inv (text):
	permuted_text = []
	for i in range(0, 8):
		permuted_text.append(text[IP_inv[i]-1])

	return permuted_text

def xor (n1, b1):
	if ( n1 == b1 ):
		return 0
	else:
		return 1

def xor_4 (n4, b4):
	xored = []
	for i in [0, 1, 2, 3]:
		xored.append(xor(n4[i], b4[i]))
	
	return xored


def fk (text, key):
	l = f[:4]
	r = f[4:]

	return ((xor_4 (l, e_p(r, key))) + r)
	

def e_p (n4, key):
	x8 = []
	for i in range(0, 8):
		x8.append(n4[e_p[i]-1])
	


	

generate_key([1,0,1,0,0,0,0,0,1,0])
