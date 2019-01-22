import sys
sys.path.append("../../../")
from design.lfu_cache.lfu_cache import FrequencyQueuesList, FrequencyQueue


def test_basic_queue_functions():
	fqlist = FrequencyQueuesList()
	q1 = FrequencyQueue(0)
	q2 = FrequencyQueue(1)
	q3 = FrequencyQueue(3)

	assert(q1.frequency == 0)
	assert(q2.frequency == 1)
	assert(q3.frequency == 3)

	q1.enqueue(('a', 1))
	q1.enqueue(('b', 2))
	q1.enqueue(('c', 3))

	fqlist.push_back(q1)
	assert(str(fqlist) == "[1:\n0, [3]: ('a', 1) ('b', 2) ('c', 3) \n]")
	assert(len(fqlist) == 1)
	assert(str(fqlist.front()) == "0, [3]: ('a', 1) ('b', 2) ('c', 3) ")
	assert(str(fqlist.back()) == "0, [3]: ('a', 1) ('b', 2) ('c', 3) ")

	q2.enqueue(('d', 4))
	fqlist.push_back(q2)
	assert(str(fqlist) == "[2:\n0, [3]: ('a', 1) ('b', 2) ('c', 3) \n1, [1]: ('d', 4) \n]")
	assert(len(fqlist) == 2)
	assert(str(fqlist.front()) == "0, [3]: ('a', 1) ('b', 2) ('c', 3) ")
	assert(str(fqlist.back()) == "1, [1]: ('d', 4) ")

	q3.enqueue(('e', 5))
	q3.enqueue(('f', 6))
	fqlist.push_back(q3)
	assert(str(fqlist) == "[3:\n0, [3]: ('a', 1) ('b', 2) ('c', 3) \n1, [1]: ('d', 4) \n3, [2]: ('e', 5) ('f', 6) \n]")
	assert(len(fqlist) == 3)
	print fqlist

	q = fqlist.pop_front()
	assert(str(q) == "0, [3]: ('a', 1) ('b', 2) ('c', 3) ")
	assert(len(fqlist) == 2)

	assert(str(fqlist.front()) == "1, [1]: ('d', 4) ")
	assert(str(fqlist.back()) == "3, [2]: ('e', 5) ('f', 6) ")



def test_add():
	fqlist = FrequencyQueuesList()
	fq = FrequencyQueue(1)
	fq.enqueue(('a', 1))
	fqlist.push_front(fq)
	assert(fqlist.front().frequency == 1)

	# freq 0 queue doesnt exists, should create a new queue at the front with freq 0
	fqlist.add('abc', 'def')
	assert(fqlist.front().frequency == 0)
	assert(fqlist.front().front() == ('abc', 'def'))
	assert(fqlist.front().back() == ('abc', 'def'))
	assert(len(fqlist.front()) == 1) # 1 item at freq q #0
	assert(fqlist.back().frequency == 1)
	assert(len(fqlist) == 2)

	fqlist.add('new', 'entry')
	assert(fqlist.front().frequency == 0)
	assert(fqlist.back().frequency == 1)
	assert(fqlist.front().front() == ('abc', 'def'))
	assert(fqlist.front().back() == ('new', 'entry'))
	assert(len(fqlist.front()) == 2)
	assert(len(fqlist) == 2) # fqlist's size doesnt change, there are just two frequencies right now



def test_promote():
	fqlist = FrequencyQueuesList()
	fq = FrequencyQueue(0)
	fq.enqueue(('a', 1))
	fqlist.push_front(fq)
	assert(fqlist.front().frequency == 0)
	assert(str(fqlist) == "[1:\n0, [1]: ('a', 1) \n]")


	fqlist.promote(fqlist.head, fqlist.head.value.head)
	assert(fqlist.front().frequency == 1)
	assert(str(fqlist) == "[1:\n1, [1]: ('a', 1) \n]")


if __name__ == '__main__':
	test_basic_queue_functions()
	test_add()
	test_promote()
