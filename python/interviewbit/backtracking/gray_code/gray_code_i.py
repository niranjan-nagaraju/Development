'''
https://www.interviewbit.com/problems/gray-code/

Gray Code

The gray code is a binary numeral system where two successive values differ in only one bit.

Given a non-negative integer n representing the total number of bits in the code, print the sequence of gray code. A gray code sequence must begin with 0.

For example, given n = 2, return [0,1,3,2]. Its gray code sequence is:

00 - 0
01 - 1
11 - 3
10 - 2
There might be multiple gray code sequences possible for a given n.
Return any such sequence.
'''


'''
Solution Outline:
    1. Gray code sequences g(n) for n-bit can be generated using g(n-1) by prepending 0s to g(n-1)
        and prepending 1s to g'(n-1) {g'(n-1) is the reverse/reflection of g(n-1)
        e.g.,
          g(1): {0,1}
          g(2): 0+{0,1}, 1+{1,0}
              : {00, 01, 11, 10}
          g(3): 0+g(2), 1+g'(2)
              : 0+{00, 01, 11, 10}, 1+{10, 11, 01, 00}
              : {000, 001, 011, 010, 110, 111, 101, 100}
    2. For a backtracking solution, Use a prefix tree, with g(level) containing gray code sequences of level-bit.
                                    {}
                            /                 \    
                           0                   1
                        /    \               /    \  
                     00        01         11        10
                    /  \      /  \       /  \      /  \
                   000 001   011 010    110 111   101 100

    3. In essence, if a node `B` at level x, is a left child, then create B0 and B1 as left and right children of B respectively.
        If `B` is a right child, B1 and B0 as left and right children of B.

    Aside: To get a gray-code equivalent for a binary sequence, B,
            Scan B from MSB to LSB
                For each bit b, follow its right child if it is 1
                    follow the left child if it is 0
            *Alternately*, Pick the decimal(B)th item at level `len(B)`

            e.g,
            B: 4 (100)
                1 -> 11 -> 110
                bin2gray(b100) = b110
            B: 7(111)
                1 -> 10 -> 100
                bin2gray(b111) = b100
'''

# Iterative version
class Solution:
	def generate_gray_code(self, n):
		# bitstream[pos] = bit
		# `pos` is counted from MSB to LSB
		# assign_bit(0b1100, 1, 3) == 0b1110
		# assign_bit(0b1100, 0, 2)  == 0b1000
		def assign_bit(bitstream, bit, pos):
			if bit:
				# set `pos` bit
				return bitstream | 1<<(n-pos)
			else:
				# clear `pos` bit
				return bitstream & ~(1<<(n-pos))


		LEFT, RIGHT = 0, 1
		seq = 0

		# Push right followed by left into the stack
		# so when popped in LIFO, the order reverts to L-R 
		# NOTE:
		#   Each entry in the stack contains a triplet(bit, level, direction)
		#     bit: bit value 0/1 of a node in the prefix tree
		#     level: level of the node in the tree (1 <= level <= n)
		#     direction: If node is a left/right child of its parent
		stack = [(1,1,RIGHT), (0,1,LEFT)]
		while stack:
			bit, level, direction = stack.pop()
			seq = assign_bit(seq, bit, level)
			if level == n:
				yield seq
			else:
				if direction == LEFT:
					# left child: 0, right child: 1
					stack.append((1, level+1, RIGHT))
					stack.append((0, level+1, LEFT))
				else: # direction == RIGHT
					# left child: 1, right child: 0
					stack.append((0, level+1, RIGHT))
					stack.append((1, level+1, LEFT))



if __name__ == '__main__':
	s = Solution()

	assert [_ for _ in s.generate_gray_code(1)] == [0,1]
	assert [_ for _ in s.generate_gray_code(2)] == [0,1,3,2]
	assert [_ for _ in s.generate_gray_code(2)] == [0b00, 0b01, 0b11, 0b10]
	assert [_ for _ in s.generate_gray_code(3)] == [0b000, 0b001, 0b011, 0b010, 0b110, 0b111, 0b101, 0b100]
	assert [_ for _ in s.generate_gray_code(4)] == [
			0b0000, 0b0001, 0b0011, 0b0010, 0b0110, 0b0111, 0b0101, 0b0100,
			0b1100, 0b1101, 0b1111, 0b1110, 0b1010, 0b1011, 0b1001, 0b1000
			]


