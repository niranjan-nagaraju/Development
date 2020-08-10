
from data_structures.array.sll import SLL

def basic_tests():
	sll = SLL(10)
	assert sll.allocated_list == -1 # Empty SLL
	assert len(sll) == 0

	sll.push_back(78)
	sll.push_back(10)
	sll.push_back(41)
	sll.push_back(36)
	sll.push_back(21)

	assert str(sll) == "[5]: 78 -> 10 -> 41 -> 36 -> 21 -> "
	assert len(sll) == 5

	sll.push_front(1)
	sll.push_front(3)

	assert str(sll) == "[7]: 3 -> 1 -> 78 -> 10 -> 41 -> 36 -> 21 -> "

	sll.push_back(8)
	sll.push_back(9)
	sll.push_back(10)
	assert str(sll) == "[10]: 3 -> 1 -> 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> 10 -> "

	assert sll.available_list == -1 # at capacity

	failed_push = False
	try: 
		sll.push_back(10)
	except MemoryError:
		failed_push = True
	assert failed_push == True

	failed_push = False
	try: 
		sll.push_front(11)
	except MemoryError:
		failed_push = True
	assert failed_push == True

	assert sll.pop_front() == 3
	assert str(sll) == "[9]: 1 -> 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> 10 -> "

	assert sll.pop_front() == 1
	assert str(sll) == "[8]: 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> 10 -> "

	assert sll.pop_back() == 10
	assert str(sll) == "[7]: 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> "

	assert sll.pop_back() == 9
	assert str(sll) == "[6]: 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> "

	assert sll.pop_back() == 8
	assert str(sll) == "[5]: 78 -> 10 -> 41 -> 36 -> 21 -> "

	l = [78, 10, 41, 36, 21]
	for i in xrange(5):
		assert len(sll) == 5-i
		assert sll.pop_front() == l[i]

	assert len(sll) == 0
	assert sll.allocated_list == -1
	assert sll.allocated_tail == -1


if __name__ == '__main__':
	basic_tests()

