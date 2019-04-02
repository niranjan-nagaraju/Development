'''
Generate all subsets of a set, a powerset

Use set bits in the binary representation of a number to create a powerset.
e.g., 5
Use 0 -> 5, 2**5 == 32
Use bits set in 0, 1, 2, ... 31 as anchors to picking items from the set into a subset.

set: {a,b,c}
n: 3
bits-table of 0 to 7 (2**3-1)

i  | bits  | subset
---|-------|-------
1. | 0 0 0 |  {}
2. | 0 0 1 |  {c}
3. | 0 1 0 |  {b}
4. | 0 1 1 |  {b,c}
5. | 1 0 0 |  {a}
6. | 1 0 1 |  {a.c}
7. | 1 1 0 |  {a,b}
8. | 1 1 1 |  {a,b,c}
'''


# Add 1 to a binary set containing bits
# Adding 1 in binary => flip all 1s from the right until a 0 is found
# flip the 1 to 0, and the resulting bit-sequence is the result of adding 1
# to the binary sequence
# e.g.
# 111011 (+1) -> 111 011, flip last two 1s and the zero
#   = 111 100
def add_binary(bin_set, n):
	for i in range(n-1, -1, -1):
		bin_set[i] = 0 if bin_set[i] else 1

		if bin_set[i] == 1:
			return bin_set

	return bin_set



# Generate a list containing binary representations of 1..2**n
# to be used as a mask for filling in subsets
def generate_powerset_masks(n):
	binary_masks = []
	bin_mask = [0]*n
	for i in range(1<<n):
		binary_masks.append(bin_mask[:])
		bin_mask = add_binary(bin_mask, n)

	return binary_masks



def generate_powerset(a, n):
	powerset = []
	for bin_mask in generate_powerset_masks(n):
		subset = []
		for i in range(n):
			if bin_mask[i]:
				subset.append(a[i])

		# if the original set was a string, stringify the subset too
		if isinstance(a, str):
			subset = ''.join(subset)
		powerset.append(subset)

	return powerset


def basic_testcases():
	a = [1,3,5,7]
	assert generate_powerset(a, 4) == [[], [7], [5], [5, 7], [3], [3, 7], [3, 5], [3, 5, 7], [1], [1, 7], [1, 5], [1, 5, 7], [1, 3], [1, 3, 7], [1, 3, 5], [1, 3, 5, 7]]

	assert generate_powerset("abc", 3) == ['', 'c', 'b', 'bc', 'a', 'ac', 'ab', 'abc']
	assert generate_powerset("abcde", 5) == ['', 'e', 'd', 'de', 'c', 'ce', 'cd', 'cde', 'b', 'be', 'bd', 'bde', 'bc', 'bce', 'bcd', 'bcde', 'a', 'ae', 'ad', 'ade', 'ac', 'ace', 'acd', 'acde', 'ab', 'abe', 'abd', 'abde', 'abc', 'abce', 'abcd', 'abcde']
	

if __name__ == '__main__':
	basic_testcases()

