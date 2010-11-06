#!/usr/bin/python

def combinations_helper(lst, a, prefix, startindex, n, r):
	if (startindex >= n):
		return

	prefix.append(a[startindex])

	if len(prefix) == r:
		lst.append(prefix[:])
		return 

	combinations_helper(lst, a, prefix[:], startindex+1, n, r)

	for i in range(startindex+2, n):
		combinations_helper(lst, a, prefix[:], i, n, r)

def combinations(a, n, r):
	lst = []
	for i in range(n, r-1, -1):
		curr = combinations_helper (lst, a, [], 0, i, r) 
		a.pop(0)

	return lst

def main():
	n = int(raw_input())
	r = int(raw_input())
	a = range(1, n+1)

	comb_list = combinations(a, n, r)
	print len(comb_list)
	for i in comb_list:
		print i

if __name__ == "__main__":
	main()
