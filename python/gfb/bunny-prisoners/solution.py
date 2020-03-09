def identity(x, y):
	sigma = lambda n: n * (n+1)/2
	return 1 + sigma(x+y-1) - y


def solution(x,y):
	return str(identity(x,y))


def get_triangle(rows):
	triangle = [[] for _ in xrange(rows)]
	columns = 0
	for i in xrange(rows, 0, -1):
		columns += 1
		for j in xrange(1, columns+1):
			triangle[rows-i].append(identity(j, i))

	return triangle
	

if __name__ == '__main__':
	assert solution(3,2) == '9'
	assert solution(5,10) == '96'
	assert get_triangle(4) == [
			[7],
			[4, 8],
			[2, 5, 9],
			[1, 3, 6, 10]]

	assert get_triangle(6) == [
			[16],
			[11, 17],
			[7, 12, 18],
			[4, 8, 13, 19],
			[2, 5, 9, 14, 20],
			[1, 3, 6, 10, 15, 21]]


