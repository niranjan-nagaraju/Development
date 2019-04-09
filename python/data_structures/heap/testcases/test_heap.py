from data_structures.heap.heap import Heap

def basic_testcases():
	l = [5,4,3,2,1,6,7,8,9,0]
	intermediate_heaps = [
			[5],
			[4, 5],
			[3, 5, 4],
			[2, 3, 4, 5],
			[1, 2, 4, 5, 3],
			[1, 2, 4, 5, 3, 6],
			[1, 2, 4, 5, 3, 6, 7],
			[1, 2, 4, 5, 3, 6, 7, 8],
			[1, 2, 4, 5, 3, 6, 7, 8, 9],
			[0, 1, 4, 5, 2, 6, 7, 8, 9, 3]
			]

	h = Heap()
	for i in xrange(len(l)):
		h.add(l[i])
		assert h.items == intermediate_heaps[i]



if __name__ == '__main__':
	basic_testcases()
