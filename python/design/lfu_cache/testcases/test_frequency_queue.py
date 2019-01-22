import sys
sys.path.append("../../../")
from design.lfu_cache.lfu_cache import FrequencyQueue


if __name__ == '__main__':
	fqItem = FrequencyQueue(5)

	assert ((not fqItem) == True)
	fqItem.enqueue(('a', 1))
	fqItem.enqueue(('b', 2))
	fqItem.enqueue(('c', 3))

	assert(fqItem.frequency == 5)

	assert(str(fqItem) == "5, [3]: ('a', 1) ('b', 2) ('c', 3) ")

	assert(fqItem.front() == ('a', 1))
	assert(fqItem.back() == ('c', 3))

	i = 3
	while fqItem:
		print fqItem
		assert (len(fqItem) == i)
		i -= 1
		fqItem.dequeue()

	assert(fqItem.frequency == 5)
	assert ((not fqItem) == True)
