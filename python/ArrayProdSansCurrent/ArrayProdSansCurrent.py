#!/usr/bin/python

'''
There is an array A[N] of N numbers. You have to compose an array Output[N] such that Output[i] will be equal to multiplication of all the elements of A[N] except A[i]. For example Output[0] will be multiplication of A[1] to A[N-1] and Output[1] will be multiplication of A[0] and from A[2] to A[N-1].

Solve it without division operator and in O(n).
'''

def products (a):
	output = []

	cumulative_prod = 1
	sz = len(a)
	for i in range(0, sz):
		output.append(cumulative_prod)
		cumulative_prod *= a[i]

	cumulative_prod = 1
	for i in range(1, (sz+1)):
		output[-i] *= cumulative_prod
		cumulative_prod *= a[-i]

	return output

def main():
	a = range(1, 11)

	print a
	output = products (a)
	print output

if __name__ == "__main__":
	main()
