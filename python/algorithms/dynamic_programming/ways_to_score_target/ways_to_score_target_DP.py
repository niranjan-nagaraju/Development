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

	Build a DP table bottom-up
	T[0] = 1
	T[x] = T[x-a] + T[x-b] + T[x-c], x > 0, x in 1 .. S
	return T[S]

	Sample run:
	S = 13, a, b, c = 3, 5, 10
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	1:
	i-3: < 0
	i-5: < 0
	i-10: < 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	2:
	i-3: < 0
	i-5: < 0
	i-10: < 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	3:
	i-3: == 0
	i-5: < 0
	i-10: < 0
	T[3] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	4:
	i-3: == 1
	i-5: < 0
	i-10: < 0
	T[4] = T[1] = 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0]

	5:
	i-3: == 2
	i-5: = 0
	i-10: < 0
	T[5] = T[2] = 0
	T[5] += T[0] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 0, 0, 0, 0,  0,  0,  0,  0]

	6:
	i-3: == 3
	i-5: = 1
	i-10: < 0
	T[6] = T[3] = 1
	T[6] += T[1] = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 0, 0,  0,  0,  0,  0]

	7:
	i-3: == 4
	i-5: = 2
	i-10: < 0
	T[7] = T[4] = 0
	T[7] += T[2] = 0
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 0, 0,  0,  0,  0,  0]

	8:
	i-3: == 5
	i-5: = 3
	i-10: < 0
	T[8] = T[5] = 1
	T[8] += T[3] = 1+1 = 2
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 2, 0,  0,  0,  0,  0]

	9:
	i-3: == 6
	i-5: = 4
	i-10: < 0
	T[9] = T[6] = 1
	T[9] += T[4] = 1+0 = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 2, 1,  0,  0,  0,  0]

	10:
	i-3: == 7
	i-5: = 5
	i-10: = 0
	T[10] = T[7] = 0
	T[10] += T[5] = 0+1 = 1
	T[10] += T[0] = 0+1+1 = 2
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 2, 1,  2,  0,  0,  0]

	11:
	i-3: == 8
	i-5: = 6
	i-10: = 1
	T[11] = T[8] = 2
	T[11] += T[6] = 2+1 = 3
	T[11] += T[1] = 2+1+0 = 3
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 2, 1,  2,  3,  0,  0]

	12:
	i-3: == 9
	i-5: = 7
	i-10: = 2
	T[12] = T[9] = 1
	T[12] += T[7] = 1+0 = 1
	T[12] += T[2] = 1+0+0 = 1
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 2, 1,  2,  3,  1,  0]

	13:
	i-3: == 10
	i-5: = 8
	i-10: = 3
	T[12] = T[10] = 2
	T[12] += T[8] = 2+2 = 4
	T[12] += T[3] = 2+2+1 = 5
	       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
	T[] = [1, 0, 0, 1, 0, 1, 1, 0, 2, 1,  2,  3,  1,  5]



'''

class NumWaysToScore3(object):
	def __init__(self, a, b, c):
		self.a = a
		self.b = b
		self.c = c
		self.dp_table = [1]

	def countWays(self, score):
		a, b, c = self.a, self.b, self.c
		T = self.dp_table

		# if the previous DP table upto a score S, wasnt big enough for current score (ie. current score > S)
		# we need to extend the DP table, but only from previous score to current score
		# from T[len(T)+1 : score+1]
		start = len(T)
		T += [0] * (score - len(T) + 1)
		lookup = lambda x: 0 if x < 0 else T[x]
		for i in xrange(start, score+1):
			T[i] = lookup(i-a) + lookup(i-b) + lookup(i-c)

		return T[score]



	'''
	Consolidate the 3 calls into a loop so it can eventually work any number of inputs
	'''
	def countWays_2(self, score):
		points = [self.a, self.b, self.c]
		T = self.dp_table

		# if the previous DP table upto a score S, wasnt big enough for current score (ie. current score > S)
		# we need to extend the DP table, but only from previous score to current score
		# from T[len(T)+1 : score+1]
		start = len(T)
		T += [0] * (score - len(T) + 1)
		for i in xrange(start, score+1):
			for j in xrange(len(points)):
				x = points[j]
				if i-x >= 0:
					T[i] += T[i-a]

		return T[score]




if __name__ == '__main__':
	n = NumWaysToScore3(3,5,10)
	assert n.countWays(13) == 5
	assert n.countWays_2(13) == 5
	# extends current DP table to fit in 2 more entries, 14, and 15
	assert n.countWays(15) == 4
	assert n.countWays_2(15) == 4
	# DP table already has solution for 11
	assert n.countWays(11) == 3
	assert n.countWays_2(11) == 3

	# order of a,b,c should return the same results
	n = NumWaysToScore3(5,10,3)
	assert n.countWays(13) == 5
	assert n.countWays_2(13) == 5

	n = NumWaysToScore3(5,3,10)
	assert n.countWays(13) == 5
	assert n.countWays_2(13) == 5

	n2 = NumWaysToScore3(3,4,8)
	assert n2.countWays(12) == 4
	assert n2.countWays_2(12) == 4
	assert n2.countWays(10) == 3
	assert n2.countWays_2(10) == 3

