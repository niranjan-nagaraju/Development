'''
https://www.interviewbit.com/problems/xor-between-two-arrays/

Xor Between Two Arrays!

Problem Description
Given two integer array A and B, you have to pick one element from each array such that their xor is maximum.
Return this maximum xor value.


Problem Constraints
1 <= |A|, |B| <= 105
1 <= A[i], B[i] <= 109


Input Format
First argument is an integer array A.
Second argument is an integer array B.

Output Format
Return an integer denoting the maximum xor value as described in the question.

Example Input
Input 1:
 A = [1, 2, 3]
 B = [4, 1, 2]

Example Output
 Output 1:
   7

Example Explanation
Explanation 1:
    Pick A[2] and B[0] because their xor value is maximum. 3 ^ 4 = 7
'''



'''
Solution Outline:
    0. Use a binary-trie to build a prefix trie for A
    1. For each item, B[i], in B
       1.1 Traverse bits, MSB to LSB and for every bit b in B[i], match the opposite bit, b' in the trie
           to maximize b xor b' == 1 if b != b'
       1.2 If no such b' is found in the trie, traverse b in the path, and onto the next bit.
       1.3 Set or reset the bit b position in current xor if b' is found or not respectively.
    2. Return max of the current xors found for each B[i]
  
Sample run: 
    A: [1, 2, 3] -> [001, 010, 011]
    B: [1, 2, 4] -> [001, 010, 100]
    A as a prefix tree
            *
          /
         0
      /    \
     0      1
      \    / \
       1  0   1	

    Initially max xor: 0b000

    B[0] = 1 = 0b001
    B[0][0] = 0
    Locate '1' in trie level 1, so we get B[0][0] ^ y == 1 (path: *1)
    '1' not found at level1, Pick 0 instead, current xor: 0, max xor: 0
    B[0][1] = 0
    Locate '1' in (level 1, 0) children, found '1' (path: *01)
        => set 2nd bit in current_xor: 0b010
    B[0][2] = 1
    Locate '0' in (level 2, 1) children, found '0' (path: *010)
        => Set 3rd bit in current_xor: 0b011 > max_xor
        max_xor = 0b011 using B[0] and any of A[]
        1 ^ 1 = 0b000
        1 ^ 2 = 0b011
        1 ^ 3 = 0b010, max: 0b011

    B[1] = 2 = 0b010
      B[1][0] = 0, locate *1 in trie. Not found, => pick '0' instead,
      current xor: 0
      B[1][1] = 1, locate *00 in trie, => found => set 2nd bit in current xor
      current xor: 0b010
      B[1][2] = 0, locate *001 in trie => found => set 3rd bit in current xor
      current xor: 0b011
      == max xor
      => using only B[1] = 2, and any of A[]
      max_xor = 0b011
      2 ^ 1 = 0b011
      2 ^ 2 = 0b000
      2 ^ 3 = 0b001, max: 0b011

    B[2] = 4 = 0b100
      B[2][0] = 1, locate *0 in trie, found => set 1st bit in current xor
      current xor: 0b100
      B[2][1] = 0, locate *01 in trie, found => set 2nd bit in current xor
      current xor: 0b110
      B[2][2] = 0, locate *011 in trie, found => set 3rd bit in current xor
      current xor: 0b111
      > max_xor = 0b111
       => using only B[2] = 4, and any of A[]
       max_xor = 0b111
       4 ^ 1 = 0b101
       4 ^ 2 = 0b110
       4 ^ 3 = 0b111
    => max xor: 0b111 = 7
'''

from math import floor, log

# Provide bit-wise access to a stored
# number
# b = BitArray(5) == 0b101
# b[0], b[1], b[2] == 1, 0, 1
class BitArray(object):
	def __init__(self, number, max_width):
		self.number = number
		self.max_width = max_width

	# ensure idx is between 0..max_width-1
	def check_idx(func):
		def f(self, *args, **kwargs):
			idx = args[0]
			if not (0 <= idx < self.max_width):
				raise IndexError("Invalid index: %d" % idx)
			rv = func(self, *args, **kwargs)
			return rv
		return f

	# Get 'idx'th bit
	# idx is 0-indexed MSB to LSB
	@check_idx
	def __getitem__(self, idx):
		mask = 1 << (self.max_width - idx - 1)
		return 1 if (self.number & mask) else 0

	# Set 'idx'th bit to 'value'
	# value is either 1 or 0
	# idx is 0-indexed MSB to LSB
	@check_idx
	def __setitem__(self, idx, val):
		mask = 1 << (self.max_width - idx - 1)
		if val:
			self.number |= mask
		else:
			self.number &= ~mask


	def __str__(self):
		return str(self.number) + ': ' +\
				str([self.__getitem__(x) for x in xrange(self.max_width)])




class BinaryTrie(object):
	class Node(object):
		def __init__(self):
			self.children = [None, None]

	def __init__(self, max_width):
		self.root = BinaryTrie.Node()
		self.max_width = max_width


	def add(self, number):
		bA = BitArray(number, self.max_width)
		node = self.root
		for i in xrange(self.max_width):
			b = bA[i]
			if node.children[b] is None:
				node.children[b] = BinaryTrie.Node()
			node = node.children[b]


	# return max xor possible with the current trie
	# and the given number
	def calculate_max_xor(self, number):
		# return bit-wise opposite of x
		flip = lambda x: 0 if x == 1 else 1

		current_xor = BitArray(0, self.max_width)
		bA = BitArray(number, self.max_width)
		node = self.root
		for i in xrange(self.max_width):
			b = bA[i]
			b_ = flip(b)
			if node.children[b_] is not None:
				current_xor[i] = 1
				node = node.children[b_]
			else:
				current_xor[i] = 0 # this is redundant, but for the sake of consistency
				node = node.children[b]

		return current_xor.number



class Solution:
	def max_xor_two_arrays(self, A, B):
		if not A or not B:
			return 0

		max_number = max(max(A), max(B))
		max_bit_width = int(floor(log(max_number, 2))) + 1

		# Build a binary (prefix) trie out of A
		trieA = BinaryTrie(max_bit_width)
		map(lambda x: trieA.add(x), A)

		return max([trieA.calculate_max_xor(b) for b in B])


if __name__ == '__main__':
	b = BitArray(5, 5)
	assert (b[0], b[1], b[2], b[3], b[4]) == (0, 0, 1, 0, 1)
	assert str(b) == '5: [0, 0, 1, 0, 1]'
	b[0] = 1
	assert b[0] == 1
	assert b.number == 0b10101
	b[2] = 0
	assert b.number == 0b10001

	trie = BinaryTrie(3)
	trie.add(3)

	root = trie.root
	node0 = trie.root.children[0] 
	assert node0 is not None
	assert trie.root.children[1] is None
	node01 = trie.root.children[0].children[1]
	assert node01 is not None
	node011 = trie.root.children[0].children[1].children[1]
	assert node011 is not None
	assert node011.children == [None, None]

	trie.add(2)
	assert root == trie.root
	assert trie.root.children[0] == node0
	assert trie.root.children[0].children[1] == node01
	assert trie.root.children[0].children[1].children[1] == node011
	assert trie.root.children[0].children[1].children[0] is not None

	A = [1,2,3]
	b = 1
	bt = BinaryTrie(3)
	map(lambda x: bt.add(x), A)

	assert bt.calculate_max_xor(1) == 3
	assert bt.calculate_max_xor(2) == 3
	assert bt.calculate_max_xor(3) == 2
	assert bt.calculate_max_xor(4) == 7

	bt2 = BinaryTrie(7)
	A = [14,70,53,83,49,91,36,80,92,51,66,70]
	map(lambda x: bt2.add(x), A)
	assert max([bt2.calculate_max_xor(x) for x in A]) == 127

	s = Solution()
	assert s.max_xor_two_arrays([1,2,3], [1,2,4]) == 0b111
	assert s.max_xor_two_arrays([2,1,3], [4,2,1]) == 0b111
	assert s.max_xor_two_arrays([4,2,1], [2,1,3]) == 0b111
	assert s.max_xor_two_arrays(A, A) == 127  # max xor of any two numbers within A

