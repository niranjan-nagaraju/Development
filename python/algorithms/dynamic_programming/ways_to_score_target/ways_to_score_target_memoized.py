'''
Given a game where a player can score a,b or c points at once, Calculate the number of ways they can reach a target score, S.

e.g.
S = 13, a, b, c = 3, 5, 10
Number of ways the player can score 13 is 5
Sequences: {3,10}, {3,5,5}, {5,3,5}, {5,5,3}, {10, 3}
'''

'''
Solution Outline: Top-down, recursive implementation
	1. Fix 'a' as the starting score, and count how many ways the target score of S-a can be made using a,b,c
	2. Fix 'b' as the starting score, and count how many ways the target score of S-b can be made using a,b,c
	3. Fix 'c' as the starting score, and count how many ways the target score of S-c can be made using a,b,c
	4. Solve for S-x (x in [a,b,c]) recursively until S==0 at which point we'd have counted one valid sequence.
	   S < 0 => the sequence currently under consideration cannot form the required target sum.

    Recurrence relation:
	   f(0) = 1
	   f(x) = 0, if x < 0
	   f(S) = f(S-a) + f(S-b) + f(S-c)

	Sample run:
	S = 13, a, b, c = 3, 5, 10
	f(13) = f(10) + f(8) + f(3)
	  f(10) = f(7) + f(5) + f(0)
	    f(7) = f(4) + f(2) + f(-3)
		  f(4) = f(1) + f(-1) + f(-6)
		    f(1) = f(-2) + f(-4) + f(-16) == 0 + 0 + 0 == 0
			f(-1) = 0
			f(-6) = 0
		  f(4) = 0
		  f(2) = f(-1) + f(-3) + f(-8) = 0
		  f(-3) = 0
	    f(7) = 0
	    f(5) = f(2) + f(0) + f(-5)
		  f(2) = f(-1) + f(-3) + f(-8) == 0
		  f(0) = 1
		  f(-5) = 0
	    f(5) = 1  {corresponds to 3, 5, 5}
		f(0) = 1 {corresponds to 3,10}
	  f(10) = 0 + 1 + 1 = 2
	  f(8) = f(5) + f(3) + f(-2)
	    f(5) = f(2) + f(0) + f(-12)
		  f(2) = 0
		  f(0) = 1
		f(5) = 1 {corresponds to 5, 3, 5}
		f(3) = f(0) + f(-2) + f(-7) = 1 {corresponds to 5, 5, 3}
	  f(8) = 1 + 1 + 0 = 2
	  f(3) = f(0) + f(-2) + f(-7) = 1 {corresponds to 10, 3}

	f(13) = f(10) + f(8) + f(3) == 2 + 2 + 1 = 5
'''

class NumWaysToScore(object):
	def __init__(self, a, b, c):
		self.a = a
		self.b = b
		self.c = c

	# NOTE: We can no longer get sequences
	# because memoization cuts short the counting repeatedly
	# To extract all the sequences would need to actually exhaustively count like in the recursive version.
	def countWays(self, score):
		def _count(score):
			if memoized.get(score):
				return memoized[score]

			if score == 0:
				return 1
			if score < 0:
				return 0
			
			memoized[score] = _count(score-a) + _count(score-b) + _count(score-c)
			return memoized[score]


		memoized = {}
		a, b, c = self.a, self.b, self.c
		return _count(score)





if __name__ == '__main__':
	n = NumWaysToScore(3,5,10)
	assert n.countWays(13) == 5

	# order of a,b,c should return the same results
	n = NumWaysToScore(5,10,3)
	assert n.countWays(13) == 5

	n = NumWaysToScore(5,3,10)
	assert n.countWays(13) == 5

	n2 = NumWaysToScore(3,4,8)
	assert n2.countWays(12) == 4
	assert n2.countWays(10) == 3

