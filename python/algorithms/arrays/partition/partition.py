# Partition positive integers to the left,
# non-positive ones to the right
def partition(a):
	i, j = 0, len(a)-1
	while i <= j:
		if a[i] > 0:
			i += 1
		elif a[j] <= 0:
			j -= 1
		else:
			a[i], a[j] = a[j], a[i]
			i += 1
			j -= 1

	# i -> number of +ve numbers
	return  a, i


if __name__ == '__main__':
	assert partition([1,2,0]) == ([1,2,0], 2)
	assert partition([3,4,-1,1])  == ([3,4,1,-1], 3)
	assert partition([-8, -7, -6]) == ([-8, -7, -6], 0)
	assert partition([1,2,3,4,5]) == ([1,2,3,4,5], 5)
	assert partition([1,3,2,5,0,4]) == ([1,3,2,5,4,0], 5)
	assert partition([1,5,2,4]) == ([1,5,2,4], 4)
	assert partition([6,0,1,4,3,2]) == ([6,2,1,4,3,0], 5)
	assert partition([-4, 7, 1, 2, 3, 5, 4, 2, -3]) == ([2, 7, 1, 2, 3, 5, 4, -4, -3], 7)
	assert partition([-4, 7, -1, 2, 3, -5, 4, 6, -3]) == ([6, 7, 4, 2, 3, -5, -1, -4, -3], 5)


