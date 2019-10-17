'''
Given a game where a player can score a,b or c points at once, Calculate the number of ways they can reach a target score, S.
The score sequences are counted un-ordered.
i.e. {3,5,5} is the same as {5,5,3} as is {5,3,5}

e.g.
S = 13, a, b, c = 3, 5, 10
Number of ways the player can score 13 is 2
Sequences: {3,10}, {3,5,5}
'''

'''
Solution Outline: Top-down, recursive implementation
	0. Sort a,b,c. new a,b,c s.t. a < b < c
	1. Fix 'a' as the starting score, and count how many ways the target score of S-a can be made using a,b,c
	2. Fix 'b' as the starting score, and count how many ways the target score of S-b can be made using b,c
	   This ensures we always an increasing sequence
	3. Fix 'c' as the starting score, and count how many ways the target score of S-c can be made using c
	4. Solve for S-x (x in [a,b,c]) recursively until S==0 at which point we'd have counted one valid sequence.
	   S < 0 => the sequence currently under consideration cannot form the required target sum.

    Recurrence relation:
	   f(0) = 1
	   f(x) = 0, if x < 0
	   f(S) = g(S-a) + h(S-b) + i(S-c)

	   g(S) = g(S-a) + h(S-b) + i(S-c), g(0) = 1, g(x) = 0, if x < 0
	   h(S) = h(S-b) + i(S-c), h(0) = 1, h(x) = 0, if x < 0
	   i(S) = i(S-c), i(0) = 1, i(x) = 0, if x < 0
	     or just check if S | c (S is divisible by c)

	Sample run:
	S = 13, a, b, c = 3, 5, 10
	f(13) = g(10) + h(8) + i(3)
		g(10) = g(7) + h(5) + i(0)
			g(7) = g(4) + h(2) + i(-3)
				g(4) = g(1) + h(-1) + i(-20) = 0
				h(2) = h(-5) + hi-8) = 0
					i(-3) = 0
			g(7) = g(4) + h(2) + i(-3) = 0
			h(5) = h(0) + i(-5) = 1 {corresponds to 3, 5, 5}
			i(0) = 1 {corresponds to 3, 10}
		g(10) = g(7) + h(5) + i(0) = 0 + 1 + 1 = 2
		h(8) = h(3) + i(-2)
			h(3) = h(-2) + i(-12) = 0
		h(8) = h(3) + i(-2) = 0
		i(3) = i(-7) = 0
	f(13) = g(10) + h(8) + i(3) = 2 + 0 + 0 = 2

'''

class NumWaysToScore3(object):
	def __init__(self, a, b, c):
		self.a, self.b, self.c  = sorted([a, b, c])

	def countWays(self, score):
		def f(prefix, n):
			if n == 0:
				sequence.append(prefix[:])
				return 1
			if n < 0:
				return 0
			return f(prefix+[a], n-a) + g(prefix+[b], n-b) + h(prefix+[c], n-c)

		def g(prefix, n):
			if n == 0:
				sequence.append(prefix[:])
				return 1
			if n < 0:
				return 0
			return g(prefix+[b], n-b) + h(prefix+[c], n-c)

		def h(prefix, n):
			if n < 0:
				return 0
			if n % c == 0:
				sequence.append(prefix[:] + [c]*(n/c))
				return 1
			return 0

		sequence = []
		a, b, c = self.a, self.b, self.c
		return f([], score), sequence


	'''
	Consolidate f,g and h()
	The only difference between f, g and h are that f operates on all a,b,c
	g on b,c and h operates on only c
	'''
	def countWays_2(self, score):
		def _count(prefix, points, n):
			if n == 0:
				sequence.append(prefix[:])
				return 1
			if n < 0:
				return 0

			num_ways = 0
			for i in xrange(len(points)):
				x = points[i]
				num_ways += _count(prefix + [x], points[i:], n - x)
			return num_ways


		points = [self.a, self.b, self.c]
		sequence = []
		return _count([], points, score), sequence


if __name__ == '__main__':
	n = NumWaysToScore3(3,5,10)
	assert n.countWays(13) == (2, [[3,5,5], [3,10]])
	assert n.countWays_2(13) == (2, [[3,5,5], [3,10]])
	assert n.countWays(15) == (3, [[3, 3, 3, 3, 3], [5, 5, 5], [5, 10]])
	assert n.countWays_2(15) == (3, [[3, 3, 3, 3, 3], [5, 5, 5], [5, 10]])
	assert n.countWays(10) == (2, [[5, 5], [10]])
	assert n.countWays_2(10) == (2, [[5, 5], [10]])

	# order of a,b,c should return the same results
	n = NumWaysToScore3(5,10,3)
	assert n.countWays(13) == (2, [[3,5,5], [3,10]])
	assert n.countWays_2(13) == (2, [[3,5,5], [3,10]])
	assert n.countWays(15) == (3, [[3, 3, 3, 3, 3], [5, 5, 5], [5, 10]])
	assert n.countWays_2(15) == (3, [[3, 3, 3, 3, 3], [5, 5, 5], [5, 10]])

	n = NumWaysToScore3(5,3,10)
	assert n.countWays(13) == (2, [[3,5,5], [3,10]])
	assert n.countWays_2(13) == (2, [[3,5,5], [3,10]])
	assert n.countWays(15) == (3, [[3, 3, 3, 3, 3], [5, 5, 5], [5, 10]])
	assert n.countWays_2(15) == (3, [[3, 3, 3, 3, 3], [5, 5, 5], [5, 10]])

	n2 = NumWaysToScore3(3,4,8)
	assert n2.countWays(12) == (3, [[3, 3, 3, 3], [4, 4, 4], [4, 8]])
	assert n2.countWays_2(12) == (3, [[3, 3, 3, 3], [4, 4, 4], [4, 8]])
	assert n2.countWays(10) == (1, [[3, 3, 4]])
	assert n2.countWays_2(10) == (1, [[3, 3, 4]])
	assert  n2.countWays(32) == (10, [[3, 3, 3, 3, 3, 3, 3, 3, 4, 4], [3, 3, 3, 3, 3, 3, 3, 3, 8], [3, 3, 3, 3, 4, 4, 4, 4, 4], [3, 3, 3, 3, 4, 4, 4, 8], [3, 3, 3, 3, 4, 8, 8], [4, 4, 4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4, 8], [4, 4, 4, 4, 8, 8], [4, 4, 8, 8, 8], [8, 8, 8, 8]])
	assert  n2.countWays_2(32) == (10, [[3, 3, 3, 3, 3, 3, 3, 3, 4, 4], [3, 3, 3, 3, 3, 3, 3, 3, 8], [3, 3, 3, 3, 4, 4, 4, 4, 4], [3, 3, 3, 3, 4, 4, 4, 8], [3, 3, 3, 3, 4, 8, 8], [4, 4, 4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4, 8], [4, 4, 4, 4, 8, 8], [4, 4, 8, 8, 8], [8, 8, 8, 8]])

