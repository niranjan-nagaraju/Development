'''
Implement a queue using stacks

Approach:
	An enqueue-efficient implementation using two stacks
	Enqueue is O(1), dequeue is O(n)

	Enqueue(x): 
		Push to S1
	Dequeue():
		if S2 has elements (they are already in queue-order)
		   return S2.pop()
		Otherwise (S2 is empty)
			Push all items from S1 to S2, S2 now has queue order
			return S2.pop()
		

	Test runs:
	Enqueue(1):
		S1: 1
		S2:
	 
	Enqueue(2):
		S1: 2 1
		S2: 

	Enqueue(3):
		S1: 3 2 1
		S2: 

	Dequeue(): 1
	    S1: 3 2 1
	    S2: 
		<--
		S1:
		S2: 1 2 3
		<-- x = S2.pop() == 1
		S1: 
		S2: 2 3

	Enqueue(4)
		S1: 4
		S2: 2 3

	Enqueue(5)
		S1: 5 4
		S2: 2 3

	Dequeue() : 2
		S1: 5 4
		S2: 2 3
		<-- S2.pop() == 2
		S1: 5 4
		S2: 3

	Dequeue(): 3
		S1: 5 4
		S2: 3
		<-- S2.pop() == 3
		S1: 5 4
		S2: 

	Dequeue(): 4
		S1: 5 4
		S2: 
		<-- 
		S1: 
		S2: 4 5

	Dequeue(): 5
'''	

class QueueUsingStack(object):

