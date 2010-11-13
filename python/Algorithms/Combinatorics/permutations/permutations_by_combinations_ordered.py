#!/usr/bin/python

from permutations_by_combinations import *

def main():
	n = int(raw_input())
	r = int(raw_input())
	a = range(1, n+1)

	permutations = generate_permutations(a, n, r)
	permutations.sort()
	print len(permutations)
	for i in permutations:
		print i

if __name__ == "__main__":
	main()
