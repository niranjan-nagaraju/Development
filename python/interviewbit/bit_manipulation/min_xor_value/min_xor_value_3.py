#encoding: utf-8
'''
https://www.interviewbit.com/problems/min-xor-value/

Min XOR value

Given an integer array A of N integers, find the pair of integers in the array which have minimum XOR value. Report the minimum XOR value.

Input Format:
    First and only argument of input contains an integer array A

Output Format:
    return a single integer denoting minimum xor value

Constraints:
2 <= N <= 100 000  
0 <= A[i] <= 1 000 000 000

For Examples :
Example Input 1:
    A = [0, 2, 5, 7]
Example Output 1:
    2
Explanation:
    0 xor 2 = 2
Example Input 2:
    A = [0, 4, 7, 9]
Example Output 2:
    3
'''

'''
Solution Outline:
	The more the leading zeroes in a number, the smaller its value is.
	So we essentially need a pair (x,y), s.t. x^y has the most leading zeroes amongst all possible pairs.
	Considering that a^b == 0 if a==b
	We are looking for a pair (x,y) such that their matching prefix-length is the longest.
	e.g,
	  A: [0, 2, 5, 7]
	  000
	  010
	  101
	  111
	    Amongst these, 000 and 010 have the longest prefix and also the lowest xor value.
		NOTE: 101 and 111 also have the same prefix length as the longest, however prefixes beginning with 0s win
		in the event of a tie.


	 e.g,
	   A: [0, 4, 7, 9]
	   0000
	   0100
	   0111
	   1001
	     0100 and 0111 have the longest prefix length (01), => 0100 ^ 0111 = 0011 = 3 is the minimum xor value


Algorithm:
	0. Use a binary trie to store prefixes.
	1. Initialize a binary trie with bits corresponding to A[0] {MSB to LSB}
	2. For each number, A[i],
	     Check how much of A[i]'s prefix matches with the trie.
		 If the prefix >= max prefix length so far,
		    Calculate xor (A[i][prefix:], trie[prefix:]) # skip the matching prefix xor, as they'll be zeroes
			If this xor < minimum so far,
			   Update minimum xor
			Add number A[i] to the trie (We only need to add bits starting from the prefix)
		 Else
		   Just add number A[i] to the trie, this number doesn't have a minimum xor with the numbers seen so far.
		   NOTE: (We only need to add bits starting from the prefix)


Sample run:
	A: [5, 10, 25, 20, 15]

	25 needs 5-bits to store
	Min xor = 2⁵-1 == 31

	5:  00101
	10: 01010
	25: 11001
	20: 10100
	15: 01111

	Trie:
	   Add(5)
 	      *
 	     /
 	    0
 	   /
 	  0
       \
        1
       /
      0
       \ 
        1

  A[i]: 10 (01010)
  prefix match so far: 0* -> length 1 (matches 00101)
  find xor(1010, 0101) = 1111 < minimum xor so far = 15
  Add(10)
  Trie:
 	      *
 	     /
 	    0
 	   /   \
 	  0     1
       \   /
        1 0
       /   \
      0     1
       \    /
        1  0


  A[i]: 25 (11001)
  prefix match so far: * -> length 0
  xor would be 1---- which is > 15
  Add(25)
  Trie:
             * 
 	     /      \
 	    0        1
 	   /   \      \
 	  0     1      1 
       \   /      /
        1 0      0
       /   \    /
      0     1  0
       \    /   \
        1  0     1


  A[i]: 20 (10100)
  prefix match so far: 1* -> length 1 == max prefix length (matches 11001)
  find xor(0010, 1001) = 1011 < minimum xor so far = 11
  Add(20)
  Trie:
             * 
 	     /      \
 	    0         1
 	   /   \    /   \
 	  0     1  0      1 
       \   /    \     /
        1 0      1    0
       /   \    /    /
      0     1  0    0
       \    /  /    \
        1  0  0      1

  A[i]: 15 (01111)
  prefix match so far: 01* -> length 2 > max prefix length (matches 01010)
  find xor(111, 010) = 101 < minimum xor so far = 5
  Add(15)? No need to add last number

  return 5 as minimum xor value of any pair
'''
class BinaryTrie:
	class Node:
		def __init__(self):
			self.children = [None, None]


	def __init__(self, max_bits):
		self.root = BinaryTrie.Node()
		self.max_bits = max_bits


	# Start adding number[level:] under prefix root-node
	# root is assumed to be at 'level' nodes deep from the trie node
	def addAfterLevel(self, number, level=0, root=None):
		if root is None:
			root = self.root

		node = root
		for i in xrange(self.max_bits-level-1, -1, -1):
			b = 1 if ((number >> i) & 1) else 0
			if node.children[b] is None:
				node.children[b] = BinaryTrie.Node()

			node = node.children[b]


	# Match number against the trie
	# and return the max prefix length it matches against the numbers in the trie
	# also return the node at which they diverge
	def matchPrefix(self, number):
		node = self.root
		prefix_len = 0
		for i in xrange(self.max_bits-1, -1, -1):
			b = 1 if ((number >> i) & 1) else 0
			if node.children[b] is None:
				break

			node = node.children[b]
			prefix_len += 1

		return (prefix_len, node)


	# xor number[level:] with matching number starting at prefix root-node
	# root is assumed to be at 'level' nodes deep from the trie node
	def xorAfterLevel(self, number, level, root):
		node = root

		# Accumulate bit-wise xor after skipping 'level' bits in number
		xor = 0
		for i in xrange(self.max_bits-level-1, -1, -1):
			b = 1 if ((number >> i) & 1) else 0
			if node.children[b] is not None:
				# matched same ith bit in number and the trie
				# xor for the bit would be 0
				xor = (xor << 1) | 0
				node = node.children[b]
			elif node.children[1-b] is not None:
				# Couldn't match ith bit in number and the trie
				# xor for the bit would be 1
				xor = (xor << 1) | 1
				node = node.children[1-b]

		return xor


class Solution:
	def find_min_xor_pair(self, A):
		import math

		# max depth of the trie = max number of bits needed to represent the maximum number in A
		max_bits = int(math.log(max(A),2)+1)
		minimum_xor = (1<<max_bits)
		max_prefix_len = 0

		btrie = BinaryTrie(max_bits)
		btrie.addAfterLevel(A[0])
		
		# Process 2nd element till the penultimate number in the array
		# We don't have to add the last number into the trie
		for i in xrange(1, len(A)-1):
			(prefix_len, node) = btrie.matchPrefix(A[i])

			if prefix_len >= max_prefix_len:
				max_prefix_len = prefix_len
				minimum_xor = min(minimum_xor, btrie.xorAfterLevel(A[i], prefix_len, node))
			
			btrie.addAfterLevel(A[i], prefix_len, node)
			

		# Process last element in the array
		(prefix_len, node) = btrie.matchPrefix(A[-1])
		if prefix_len >= max_prefix_len:
			minimum_xor = min(minimum_xor, btrie.xorAfterLevel(A[-1], prefix_len, node))

		return minimum_xor



if __name__ == '__main__':
	s = Solution()
	assert s.find_min_xor_pair([5,10,25,20,15]) == 5
	assert s.find_min_xor_pair([5,10,25,20,15,26]) == 3
	assert s.find_min_xor_pair([0,2,5,7]) == 2
	assert s.find_min_xor_pair([0,4,7,9]) == 3
	assert s.find_min_xor_pair([ 3, 2, 13, 1, 5, 13, 0, 13, 13 ]) == 0
