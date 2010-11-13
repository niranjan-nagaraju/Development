#!/usr/bin/python

from combinations import *
from all_permutations import *

def generate_permutations(a, n, r):
	# Generate unordered arrangements first
	combinations = generate_combinations(a, n, r)

	# for each unordered arrangement of length r in nCr, generate rPr ordered arrangements (permutations)
	permutations = []
	for combination in combinations:
		permutations_rr = permute_all(combination, r)
		for permutation in permutations_rr:
			permutations.append(permutation)

	return permutations

def main():
	n = int(raw_input())
	r = int(raw_input())
	a = range(1, n+1)

	permutations = generate_permutations(a, n, r)
	print len(permutations)
	for i in permutations:
		print i

if __name__ == "__main__":
	main()
