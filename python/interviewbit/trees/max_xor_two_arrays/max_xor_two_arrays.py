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
      B[1][2] = 0, locate *011 in trie => found => set 3rd bit in current xor
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




if __name__ == '__main__':
    b = BitArray(5, 5)
    assert (b[0], b[1], b[2], b[3], b[4]) == (0, 0, 1, 0, 1)
    assert str(b) == '5: [0, 0, 1, 0, 1]'
    b[0] = 1
    assert b[0] == 1
    assert b.number == 0b10101
    b[2] = 0
    assert b.number == 0b10001
   

