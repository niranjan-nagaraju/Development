'''
https://www.interviewbit.com/problems/zigzag-string/

Zigzag String

The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)

P.......A........H.......N
..A..P....L....S....I...I....G
....Y.........I........R
And then read line by line: PAHNAPLSIIGYIR
Write the code that will take a string and make this conversion given a number of rows:

string convert(string text, int nRows);

convert("PAYPALISHIRING", 3) should return "PAHNAPLSIIGYIR"

**Example 2 : **
ABCD, 2 can be written as

A....C
...B....D
and hence the answer would be ACBD.
'''

'''
Solution Outline:
	1. Use a temporary list of rows
    2. Store the characters into their respective rows
    3. Copy back the characters into the output list row by row

sample run:
  A: "PAYPALISHIRING", nRows = 3
  
  rows = [
	      []
		  []
		  []
		  ]
  
  rows = [
	      ['P']
		  []
		  []
		  ]
  rows = [
	      ['P']
		  ['A']
		  []
		  ]
  rows = [
	      ['P']
		  ['A']
		  ['y']
		  ]
  rows = [
	      ['P']
		  ['A', 'P']
		  ['y']
		  ]
  rows = [
	      ['P', 'A']
		  ['A', 'P']
		  ['y']
		  ]
  rows = [
	      ['P', 'A']
		  ['A', 'P', 'L']
		  ['y']
		  ]
  rows = [
	      ['P', 'A']
		  ['A', 'P', 'L']
		  ['y', 'I']
		  ]
  rows = [
	      ['P', 'A']
		  ['A', 'P', 'L', 'S']
		  ['y', 'I']
		  ]
  rows = [
	      ['P', 'A', 'H']
		  ['A', 'P', 'L', 'S']
		  ['y', 'I']
		  ]
  rows = [
	      ['P', 'A', 'H']
		  ['A', 'P', 'L', 'S', 'I']
		  ['y', 'I']
		  ]
  rows = [
	      ['P', 'A', 'H']
		  ['A', 'P', 'L', 'S', 'I']
		  ['y', 'I', 'R']
		  ]
  rows = [
	      ['P', 'A', 'H']
		  ['A', 'P', 'L', 'S', 'I', 'I']
		  ['y', 'I', 'R']
		  ]
  rows = [
	      ['P', 'A', 'H', 'N']
		  ['A', 'P', 'L', 'S', 'I', 'I']
		  ['y', 'I', 'R']
		  ]
  rows = [
	      ['P', 'A', 'H', 'N']
		  ['A', 'P', 'L', 'S', 'I', 'I', 'G']
		  ['y', 'I', 'R']
		  ]

  return 'PAHNAPLSIIGYIR'
'''
class Solution:
	def convert_zigzag(self, A, nRows):
		if not A:
			return A

		rows = [[] for _ in xrange(nRows)]
		i = 0
		while i < len(A):
			try:
				for r in xrange(nRows):
					rows[r].append(A[i])
					i += 1

				for r in xrange(nRows-2, 0, -1):
					rows[r].append(A[i])
					i += 1
			except IndexError:
				break

		return reduce(lambda acc,x: acc+''.join(x), rows, '')



if __name__ == '__main__':
	s = Solution()
	assert s.convert_zigzag('', 4) == ''
	assert s.convert_zigzag('PAYPALISHIRING', 3) == 'PAHNAPLSIIGYIR'
	assert s.convert_zigzag('PAYPALISHIRING', 4) == 'PINALSIGYAHRPI'
	assert s.convert_zigzag('A', 3) == 'A'
	assert s.convert_zigzag('ABCD', 2) == 'ACBD'

