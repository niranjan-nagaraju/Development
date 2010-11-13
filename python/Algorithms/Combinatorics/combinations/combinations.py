#!/usr/bin/python

def generate_combinations_helper(lst, a, prefix, startindex, n, r):
	if (startindex >= n):
		return

	prefix.append(a[startindex])

	if len(prefix) == r:
		lst.append(prefix[:])
		return 

	generate_combinations_helper(lst, a, prefix[:], startindex+1, n, r)

	for i in range(startindex+2, n):
		generate_combinations_helper(lst, a, prefix[:], i, n, r)

def generate_combinations(a, n, r):
	combinations = []
	for i in range(n, r-1, -1):
		curr = generate_combinations_helper (combinations, a, [], 0, i, r) 
		a.pop(0)

	return combinations

def main():
	n = int(raw_input())
	r = int(raw_input())
	a = range(1, n+1)

	combinations = generate_combinations(a, n, r)
	print len(combinations)
	for i in combinations:
		print i

if __name__ == "__main__":
	main()
