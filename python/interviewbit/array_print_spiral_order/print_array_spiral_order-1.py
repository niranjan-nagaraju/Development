'''
	https://www.interviewbit.com/problems/spiral-order-matrix-i/
   Print 2D array of m x n, in a spiral order
'''


'''
Spiral moves Right, Down, Left, Up
'''

def spiral_print(arr, m, n):
	t = 0
	b = m-1 
	l = 0 
	r = n-1
	direction = 'R'

	while (t <= b) and (l<=r):
		#print 'Direction: ', direction
		if direction == 'R':
			for i in range(l, r+1):
				print arr[t][i],
			t += 1
			direction = 'D'
		elif direction == 'D':
			for i in range(t, b+1):
				print arr[i][r],
			r -= 1
			direction = 'L'
		elif direction == 'L':
			for i in range(r, l-1, -1):
				print arr[b][i],
			b -= 1
			direction = 'U'
		elif direction == 'U':
			for i in range(b, t-1, -1):
				print arr[i][l],
			l += 1
			direction = 'R'

a = [
		[1, 2, 3, 4],
		[5, 6, 7, 8],
		[9, 10, 11, 12],
		[13, 14, 15, 16],
	]

spiral_print(a, len(a), len(a[0]))
