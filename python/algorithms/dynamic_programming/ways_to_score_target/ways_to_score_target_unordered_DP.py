'''
Given a game where a player can score a,b or c points at once, Calculate the number of ways they can reach a target score, S.
The score sequences are counted un-ordered.
i.e. {3,5,5} is the same as {5,5,3} as is {5,3,5}

e.g.
S = 13, a, b, c = 3, 5, 10
Number of ways the player can score 13 is 2
Sequences: {3,10}, {3,5,5}

NOTE: This is essentially the same as the coin-change problem at this point (albeit with 3 coin denominations)
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


	W[x] = 0, x < 0
	W[x] = 1 if x % c == 0

	U[0] = 1
	U[x] = 0, x< 0
	U[x] = U[x-b] + W[x-c], x > 0
	=> U[x] = U[x-b] + W[x]

	T[0] = 1
	T[x] = 0, x < 0
	T[x] = T[x-a] + U[x-b] + W[x-c], x > 0, x in 1 .. S
	=> T[x] = T[x-a] + U[x]
	return T[S]

	Sample run:
	S = 13, a, b, c = 3, 5, 10
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]
	W[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  0,  0]

	1:
	i-3: < 0
	i-5: < 0
	i-10: < 0

	W[1] = W[-9] = 0
	U[1] = U[-4] + W[1] = 0
	T[1] = T[-2] + U[1] = 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]
	W[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  0,  0]

	2:
	i-3: < 0
	i-5: < 0
	i-10: < 0
	W[2] = W[-8] = 0
	U[2] = U[-3] + W[2] = 0
	T[2] = T[-1] + U[2] = 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	3:
	i-3: = 0
	i-5: < 0
	i-10: < 0
	W[3] = W[-7] = 0
	U[3] = U[-2] + W[3] = 0
	T[3] = T[0] + U[3] = 1
	T[3] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	4:
	i-3: = 1
	i-5: < 0
	i-10: < 0
	W[4] = W[-6] = 0
	U[4] = U[-1] + W[4] = 0
	T[4] = T[1] + U[4] = 0
	T[4] = 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	5:
	i-3: = 2
	i-5: = 0
	i-10: < 0
	W[5] = W[-5] = 0
	U[5] = U[0] + W[5] = 1
	T[5] = T[2] + U[5] = 1
	T[5] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 0, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  0,  0,  0,  0]

	6:
	i-3: = 3
	i-5: = 1
	i-10: < 0
	W[6] = W[-4] = 0
	U[6] = U[1] + W[6] = 0
	T[6] = T[3] + U[6] = 1
	T[6] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  0,  0,  0,  0]

	7:
	i-3: = 4
	i-5: = 2
	i-10: < 0
	W[7] = W[-3] = 0
	U[7] = U[2] + W[7] = 0
	T[7] = T[4] + U[7] = 0
	T[7] = 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 0, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  0,  0,  0,  0]

	8:
	i-3: = 5
	i-5: = 3
	i-10: < 0
	W[8] = W[-2] = 0
	U[8] = U[3] + W[8] = 0
	T[8] = T[5] + U[8] = 1
	T[8] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 1, 0,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  0,  0,  0,  0]

	9:
	i-3: = 6
	i-5: = 4
	i-10: < 0
	W[9] = W[-1] = 0
	U[9] = U[4] + W[9] = 0
	T[9] = T[6] + U[9] = 1
	T[9] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 1, 1,  0,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	10:
	i-3: = 7
	i-5: = 5
	i-10: = 0
	W[10] = W[0] = 1
	U[10] = U[5] + W[10] = 2
	T[10] = T[7] + U[10] = 2
	T[10] = 2
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 1, 1,  2,  0,  0,  0]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  2,  0,  0,  0]
	W[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  0,  0]

	11:
	i-3: = 8
	i-5: = 6
	i-10: = 1
	W[11] = W[1] = 0
	U[11] = U[6] + W[11] = 0
	T[11] = T[8] + U[11] = 1
	T[11] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 1, 1,  2,  1,  0,  0]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  2,  0,  0,  0]
	W[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  0,  0]

	12:
	i-3: = 9
	i-5: = 7
	i-10: = 2
	W[12] = W[2] = 0
	U[12] = U[7] + W[12] = 0
	T[12] = T[9] + U[12] = 1
	T[12] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 1, 1,  2,  1,  1,  0]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  2,  0,  0,  0]
	W[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  0,  0]

	13:
	i-3: = 10
	i-5: = 8
	i-10: = 3
	W[13] = W[3] = 0
	U[13] = U[8] + W[13] = 0
	T[13] = T[10] + U[13] = 2
	T[13] = 2
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 1, 1,  2,  1,  1,  2]
	U[] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  2,  0,  0,  0]
	W[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  0,  0]

	return T[13] = 2
'''

class NumWaysToScore3(object):
	def __init__(self, a, b, c):
		self.a, self.b, self.c  = [a, b, c]
		self.T, self.U, self.W = None, None, None

	def countWays(self, score):
		T = [1] + [0] * score
		U = [1] + [0] * score
		W = [1] + [0] * score

		a, b, c = self.a, self.b, self.c
		self.T, self.U, self.W = T, U, W

		for i in xrange(0, score+1, c):
			W[i] = 1

		lookup = lambda table,i: 0 if i < 0 else table[i]
		for i in xrange(1, score+1):
			U[i] = lookup(U, i-b) + lookup(W, i)
			T[i] = lookup(T, i-a) + U[i]
	
		return T[score]


	'''
	Consolidate T, U and W tables
	'''
	def countWays_2(self, score):
		points = [self.a, self.b, self.c]
		T = [([1] + [0] * score) for x in xrange(len(points))]  # T[0], T[1], T[2]: T, U, W

		for i in xrange(0, score+1, points[-1]):
			T[-1][i] = 1

		a, b, c = self.a, self.b, self.c
		lookup = lambda table,i: 0 if i < 0 else table[i]
		for i in xrange(1, score+1):
			for j in xrange(len(points)-2, -1, -1):
				T[j][i] = lookup(T[j], i-points[j]) + lookup(T[j+1], i)

		return T[0][score]



if __name__ == '__main__':
	n = NumWaysToScore3(3,5,10)
	assert n.countWays(13) == 2
	assert n.countWays_2(13) == 2
	assert n.countWays(15) == 3
	assert n.countWays_2(15) == 3
	assert n.countWays(10) == 2
	assert n.countWays_2(10) == 2

	# order of a,b,c should return the same results
	n = NumWaysToScore3(5,10,3)
	assert n.countWays(13) == 2
	assert n.countWays_2(13) == 2
	assert n.countWays(15) == 3
	assert n.countWays_2(15) == 3

	n = NumWaysToScore3(5,3,10)
	assert n.countWays(13) == 2
	assert n.countWays_2(13) == 2
	assert n.countWays(15) == 3
	assert n.countWays_2(15) == 3

	n2 = NumWaysToScore3(3,4,8)
	assert n2.countWays(12) == 3
	assert n2.countWays_2(12) == 3
	assert n2.countWays(10) == 1
	assert n2.countWays_2(10) == 1
	assert n2.countWays(32) == 10
	assert n2.countWays_2(32) == 10

