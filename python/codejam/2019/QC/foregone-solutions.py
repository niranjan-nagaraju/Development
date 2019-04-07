'''
4 key is broken

split N into A+B=N s.t. A and B don't contain 4
replace all 4s with 2s
'''

def split(n):
	orig_n = n
	d = 1
	fst = 0
	i = 1
	while n > 0:
		d = n%10
		n = n/10
		if d == 4:
			fst = fst + 2*i
		else:
			fst = fst + d*i
		i *= 10

	return (fst, orig_n-fst)


if __name__ == '__main__':
	nTestcases = int(input())
	for i in xrange(nTestcases):
		n = int(input())
		a,b = split(n)
		print 'Case #{0}: {1} {2}'.format(i+1,a,b)


