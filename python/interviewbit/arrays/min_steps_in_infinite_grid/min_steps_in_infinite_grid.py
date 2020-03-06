'''
https://www.interviewbit.com/problems/min-steps-in-infinite-grid/

You are in an infinite 2D grid where you can move in any of the 8 directions :

 (x,y) to 
    (x+1, y), 
    (x - 1, y), 
    (x, y+1), 
    (x, y-1), 
    (x-1, y-1), 
    (x+1,y+1), 
    (x-1,y+1), 
    (x+1,y-1) 
You are given a sequence of points and the order in which you need to cover the points. Give the minimum number of steps in which you can achieve it. You start from the first point.

Input :
Given two integer arrays A and B, where A[i] is x coordinate and B[i] is y coordinate of ith point respectively.

Output :
Return an Integer, i.e minimum number of steps.

Example :
Input : [(0, 0), (1, 1), (1, 2)]
Output : 2

It takes 1 step to move from (0, 0) to (1, 1). It takes one more step to move from (1, 1) to (1, 2).
'''

'''
Solution Outline:
	Given two points p1: (x1, y1) and p2: (x2, y2) [x2 > x1, y2 > y1]

               x2,y2
	x------------x 
	|            |
	|            |
	|            |
	|            |
	x------------x
   x1,y1	
   One can reach p2 from p1 by first traversing horizontal (x2-x1) and then the vertical distance (y2-y1)
   (Total steps being (x2-x1) + (y2-y1))
   This won't be the minimum steps as we can move diagonally to cover (+1,+1) in a single step.
   So, We'd want to cover as much steps as diagonally as possible.
   How many steps can we take diagonally depends on
     i. h = (x2-x1), v = (y2-y1), h = 1
	    p1, p2 form a square
		and the entire distance can be covered diagonally
		so minimum steps = h == v == (x2-x1) == (y2-y1)
	ii. h > v
	    after covering d steps, we'd reach (d,d) and can no longer move vertically up
		  remainder = (h - d) steps
		  remainder + d = h steps
		  Therefore, min steps = h = (x2-x1)
		e.g., p1: (4, 7), p2: (9, 9)
		x2-x1 = 5 > y2-y1 = 2
		diagonal distance that can be covered = 2
		after (2,2) from (4, 7) -> we'd be at (6, 9)
		remainder of (9-6) == 3 has to be covered horizontally
		=> min steps = (9-4) = 5 steps
	iii. h < v
		similar to ii, we'd reach a conclusion that
		min steps = v = (y2-y1)

	Minimum number of steps to reach (x2,y2) from (x1, y1) would be max((x2-x1), (y2-y1))

	for any generic points p1 and p2,
	  minimum number of steps = max(abs(x2-x1), abs(y2-y1))
'''

class Solution:
	# @param X : list of integers
	# @param Y : list of integers
	# Points are represented by (X[i], Y[i])
	# @return an integer
	def coverPoints(self, X, Y):
		moves = 0
		for i in range(1, len(X)):
			moves += max( abs(X[i]-X[i-1]), abs(Y[i]-Y[i-1]) )

		return moves


if __name__ == '__main__':
	s = Solution()
	s.coverPoints([0,1,1], [0,1,2]) == 2
	points = [[4, 6], [1, 2], [4, 5], [10, 12]]
	assert s.coverPoints([x for x,_ in points], [y for _,y in points]) == 14

