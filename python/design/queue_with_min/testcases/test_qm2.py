from design.queue_with_min.queue_with_min3 import QueueMin
from data_structures.sll.sll import UnderFlowError

def TC():
	qm = QueueMin()
	assert(len(qm) == 0)

	qm.enqueue(4)
	assert(len(qm) == 1)
	assert(qm.front() == 4)
	assert(qm.back() == 4)
	assert(qm.minimum() == 4)

	qm.enqueue(1)
	assert(len(qm) == 2)
	assert(qm.front() == 4)
	assert(qm.back() == 1)
	assert(qm.minimum() == 1)

	qm.enqueue(5)
	assert(len(qm) == 3)
	assert(qm.front() == 4)
	assert(qm.back() == 5)
	assert(qm.minimum() == 1)

	qm.enqueue(3)
	assert(len(qm) == 4)
	assert(qm.front() == 4)
	assert(qm.back() == 3)
	assert(qm.minimum() == 1)

	qm.enqueue(2)
	assert(len(qm) == 5)
	assert(qm.front() == 4)
	assert(qm.back() == 2)
	assert(qm.minimum() == 1)


	assert(qm.dequeue() == 4)
	assert(len(qm) == 4)
	assert(qm.front() == 1)
	assert(qm.back() == 2)
	assert(qm.minimum() == 1)

	assert(qm.dequeue() == 1)
	assert(len(qm) == 3)
	assert(qm.front() == 5)
	assert(qm.back() == 2)
	assert(qm.minimum() == 2)

	assert(qm.dequeue() == 5)
	assert(len(qm) == 2)
	assert(qm.front() == 3)
	assert(qm.back() == 2)
	assert(qm.minimum() == 2)

	assert(qm.dequeue() == 3)
	assert(len(qm) == 1)
	assert(qm.front() == 2)
	assert(qm.back() == 2)
	assert(qm.minimum() == 2)

	qm.enqueue(6)
	assert(len(qm) == 2)
	assert(qm.front() == 2)
	assert(qm.back() == 6)
	assert(qm.minimum() == 2)

	assert(qm.dequeue() == 2)
	assert(len(qm) == 1)
	assert(qm.front() == 6)
	assert(qm.back() == 6)
	assert(qm.minimum() == 6)

	assert(qm.dequeue() == 6)
	assert(len(qm) == 0)

	# triggers underflow
	try:
		assert(qm.front() == None)
	except UnderFlowError as e:
		assert(True)

	# triggers underflow
	try:
		assert(qm.back() == None)
	except UnderFlowError as e:
		assert(True)
		
	assert(qm.minimum() == None)



if __name__ == '__main__':
	TC()
	print 'Queue with Min 2 (2 queues version): tests passed!'
