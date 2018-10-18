from dll import DLL


# Deque using a DLL
# A Deque is a double-ended queue which allows insert and remove from both ends
class Deque(DLL):
	def length(self):
		return self.size

	# return the item at the front of the Queue
	def front(self):
		return self.head.value

	# return the item at the end of the Queue
	def back(self):
		return self.tail.value


# basic tests
if __name__ == "__main__":
	deque = Deque()

	for i in range(1, 6):
		deque.push_front(i)
		assert(deque.length() == i)
		assert(deque.front() == i)

	for i in range(6, 11):
		deque.push_back(i)
		assert(deque.length() == i)
		assert(deque.back() == i)

	for i in range(1, 6):
		assert(deque.pop_front() == 5-i+1)
		assert(deque.length() == 10-i)

	for i in range(1, 6):
		assert(deque.pop_back() == 10-i+1)
		assert(deque.length() == 5-i)

