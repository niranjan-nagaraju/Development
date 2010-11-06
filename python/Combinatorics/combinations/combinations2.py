#!/usr/bin/python

def combinations_helper(a, prefix, startindex, n, r):
	if (startindex >= n):
		return 

	prefix.append(a[startindex])

	if len(prefix) == r:
		print prefix
		return

	combinations_helper(a, prefix[:], startindex+1, n, r)

	for i in range(startindex+2, n):
		combinations_helper(a, prefix[:], i, n, r)


def combinations(a, n, r):
	for i in range(n, r-1, -1):
		combinations_helper (a, [], 0, i, r) 
		a.pop(0)

def main():
	n = int(raw_input())
	r = int(raw_input())

	a = range(1, n+1)

	combinations(a, n, r)

if __name__ == "__main__":
	main()
