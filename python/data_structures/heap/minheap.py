from heap import Heap as Minheap

if __name__ == '__main__':
	h = Minheap()
	l = [5,4,3,2,1,6,7,8,9,0]

	for i in xrange(len(l)):
		h.add(l[i])

	heap_order = []
	while h:
		heap_order.append(h.remove())

	assert heap_order == range(10)
