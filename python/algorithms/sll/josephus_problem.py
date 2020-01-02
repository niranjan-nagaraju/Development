'''
Josephus problem:
	N people are standing in a circle waiting to be executed.
	Counting begins at 1, and every second person is executed until one survivor remains.
	Return the survivor (survivor #)
'''

from data_structures.sll.circular_sll import CircularSLL

# Execute prev.next and remove them from the list
# Return new neighbor of prev from where the cycle repeats
def execute(c, prev):
	c.size -= 1
	toDelete = prev.next

	prev.next = toDelete.next
	toDelete.next = None # remove toDelete references so it can be GC'd

	# We just removed head of the Circular SLL,
	# Adjust SLL's head reference to new head
	if toDelete == c.head:
		c.head = prev.next

	# return next survivor node
	# from where execution starts again
	return prev.next


def josephus(n):
	c = CircularSLL.fromList(range(1, n+1))

	tmp = c.head

	# Start with '1'/head and execute every second person
	# until only one remains
	while c.size != 1:
		tmp = execute(c, tmp)

	return c.head.value


if __name__ == '__main__':
	assert josephus(2) == 1
	assert josephus(3) == 3
	assert josephus(4) == 1
	assert josephus(5) == 3
	assert josephus(6) == 5
	assert josephus(7) == 7
	assert josephus(14) == 13

