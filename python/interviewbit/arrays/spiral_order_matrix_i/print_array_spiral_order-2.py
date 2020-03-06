'''
	https://www.interviewbit.com/problems/spiral-order-matrix-i/
   Print 2D array of m x n, in a spiral order
'''


'''
In an mxn matrix,

Spiral moves 
   Right: n steps,  (starting at 0,0)
   Down: m-1 steps, (starting at 1, n-1)
   Left: n-1 steps, (starting at m-1,n-2)
   Up: m-2 steps,   (starting at 0,m-1)
   
   then repeat with inner rect/square, 
   Right: (n-2) steps starting at (1,1), etc
   when any direction move fails due to '0' steps, return

'''

class Direction(object):
	RIGHT = 1
	DOWN = 2
	LEFT = 3
	UP = 4

def move(arr, r, c, steps, direction, numbersList):
	if steps == 0:
		return False

	if direction == Direction.RIGHT:
		for i in range(steps):
			#print arr[r][c+i],
			numbersList.append(arr[r][c+i])
	elif direction == Direction.DOWN:
		for i in range(steps):
			#print arr[r+i][c],
			numbersList.append(arr[r+i][c])
	elif direction == Direction.LEFT:
		for i in range(steps):
			#print arr[r][c-i],
			numbersList.append(arr[r][c-i])
	elif direction == Direction.UP:
		for i in range(steps):
			#print arr[r-i][c],
			numbersList.append(arr[r-i][c])

	return True


def moveRight(a, r, c, steps, numbersList):
	#print 'Right',
	return move(a, r, c, steps, Direction.RIGHT, numbersList)

def moveDown(a, r, c, steps, numbersList):
	#print 'Down',
	return move(a, r, c, steps, Direction.DOWN, numbersList)

def moveLeft(a, r, c, steps, numbersList):
	#print 'Left',
	return move(a, r, c, steps, Direction.LEFT, numbersList)

def moveUp(a, r, c, steps, numbersList):
	#print 'Up',
	return move(a, r, c, steps, Direction.UP, numbersList)


def spiral_print(a, r, c):
	return spiral_print_helper(a, r, c, [])

# level indicates how deep we are in the spiral from the outside
# Goes rect starting at (0,0) -> (1,1), ...
def spiral_print_helper(a, m, n, numbersList, level=0):
	if not  moveRight(a, level, level, n-(2*level), numbersList):
		return 

	if not moveDown(a, level+1, n-1-level, m-1-(2*level), numbersList):
		return

	if not moveLeft(a, m-1-level, n-2-level, n-1-(2*level), numbersList):
		return

	if not moveUp(a, m-2-level, 0+level, m-2-(2*level), numbersList):
		return

	# Go one level inner
	spiral_print_helper(a, m, n, numbersList, level+1)
	return numbersList


a = [
		[1, 2, 3],
		[5, 6, 7],
		[9, 10, 11],
		[13, 14, 15],
	]
assert(spiral_print(a, len(a), len(a[0]))  == [1, 2, 3, 7, 11, 15, 14, 13, 9, 5, 6, 10])

a = [
		[1, 2, 3],
		[4, 5, 6],
		[7, 8, 9],
	]
assert(spiral_print(a, 3, 3) == [1,2,3,6,9,8,7,4,5])

assert(spiral_print(
	    [
			"abcde",
			"fghij",
			"klmno",
			"pqrst",
			"uvwxy",
		], 
		5, 5) == ['a', 'b', 'c', 'd', 'e', 'j', 'o', 't', 'y', 'x', 'w', 'v', 'u', 'p', 'k', 'f', 'g', 'h', 'i', 'n', 's', 'r', 'q', 'l', 'm'])

