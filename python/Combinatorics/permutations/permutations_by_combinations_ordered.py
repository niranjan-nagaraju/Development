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

def right_rotations(comb, r):
	perms_for_comb = []
	for i in range(0, r):
		comb = comb[1:] + comb[:1]
		perms_for_comb.append(comb)

	return perms_for_comb[:]

def permutations(a, n, r):
	comb_list = combinations(a, n, r)
	perm_list = []

	for comb in comb_list:
		perm_list.extend(right_rotations(comb, r))

	return perm_list

def main():
	n = int(raw_input())
	r = int(raw_input())
	a = range(1, n+1)

	perm_list = permutations(a, n, r)
	perm_list.sort()
	print len(perm_list)
	for i in perm_list:
		print i

if __name__ == "__main__":
	main()
