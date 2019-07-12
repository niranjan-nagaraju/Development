from data_structures.sll.stack import Stack

def basic_tests():
	s = Stack()
	s.push(0)
	assert s.pop() == 0

	s.push(1)
	s.push(2)
	assert s.pop() == 2

	s.push(4)
	assert s.pop() == 4

	s.push(3)
	assert s.pop() == 3

	assert s.pop() == 1

	assert not s == True


if __name__ == '__main__':
	basic_tests()
