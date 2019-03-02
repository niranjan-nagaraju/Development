'''
https://leetcode.com/problems/reverse-nodes-in-k-group/

Given a linked list, reverse the nodes of a linked list k at a time and return its modified list.
k is a positive integer and is less than or equal to the length of the linked list. If the number of nodes is not a multiple of k then left-out nodes in the end should remain as it is.

Example:
Given this linked list: 1->2->3->4->5
For k = 2, you should return: 2->1->4->3->5
For k = 3, you should return: 3->2->1->4->5
'''

import sys
from data_structures.sll.node import Node
from data_structures.sll.sll import SLL
sys.path.append("../../")


'''
Reverse k nodes starting from 'start'
and return the kth node from 'start', so it can be 
linked back to the chain
'''
def reverse_k_group(start, k):
	# Initially assume we have k-nodes to work with
	if not start:
		return

	curr = start
	succ = start.next

	# there's only one node,
	# just return 'start' so a redundant link happens
	# from previously left-off node to 'start'
	if not succ:
		return start

	# we need to adjust (k-1) links
	for i in xrange(1, k):
		# reverse curr->succ link one link at a time
		if not curr or not succ:
			# we have reversed 'i-1' links / 'i' nodes
			# but we don't have 'k' nodes to proceed
			# Undo reversal of 'i' nodes starting from 'curr'
			# e.g. k=6, list: a->b->c->d
			# after reversing 4 nodes, we find
			# a<-b<-c<-d == d->c->b->a, and succ:None, curr:d
			# reversing 4 nodes from 'd'

			# at this point
			# start and start.next are in a loop
			# start.next -> start, and start->start.next
			# fix this loop, and undo last i reverses
			start.next = None
			reverse_k_group(curr, i)
			return None

		tmp = succ.next
		succ.next = curr # reverse link curr->succ: curr<-succ

		# move forward to next link
		curr = succ
		succ = tmp


	# at the end of the loop, we'll have k-nodes properly reversed
	# except start node's next is still pointing to previous start.next
	# Change this to point to the (k+1)st node, pointed to by 'succ' after the loop
	# so if k=4
	# initial list was: a->b->c->d->e
	# new list should be: d->c->b->a->e
	start.next = succ

	# curr is the kth node 
	return curr




'''
Reverse nodes in the SLL, k-nodes at a time
'''
def reverse(sll, k):
	# if k == 1, no reversal needed
	if k < 2:
		return

	tmp = reverse_k_group(sll.head, k)
	if (tmp == None):
		return

	start = sll.head
	sll.head = tmp # first 'k' nodes done, update sll.head

	# Now do the other nodes 'k' at a time
	while True:
		# last saves where we left-off from last time
		# this will be used to link to the reversed block of next 'k' nodes 
		last = start

		# previous start's next is where next reversing begins
		# e.g. a->b->c->d->e->f->g->h->i,j    k=3
		# initially, start = 'a'
		# but after reversing once, c->b->a->d->e->f->g->h->i,j
		# next k-blocks starts at start.next (a->d), start = 'd'
		# and then c->b->a->f->e->d->g->h->i,j, 
		# then again, next k-blocks begin at start.next (d->g), start = 'g'
		# then later, start becomes 'i', and next k-block begins at start.next = 'j'
		start = start.next
		tmp = reverse_k_group(start, k)
		if not tmp:
			return
	
		last.next = tmp




if __name__ == '__main__':
	sll = SLL.fromList("abcde")
	tmp = reverse_k_group(sll.head, 4)
	sll.head = tmp
	assert(SLL.toList(sll) ==  map (lambda x: x, "dcbae"))

	sll2 = SLL.fromList("abcde")
	tmp = reverse_k_group(sll2.head, 6)
	assert(tmp == None)
	assert(SLL.toList(sll2) ==  map (lambda x: x, "abcde"))

	sll = SLL.fromList("abcde")
	reverse(sll, 2)
	assert(SLL.toList(sll) ==  map (lambda x: x, "badce"))

	sll = SLL.fromList("abcde")
	reverse(sll, 3)
	assert(SLL.toList(sll) ==  map (lambda x: x, "cbade"))

	sll = SLL.fromList("abcde")
	reverse(sll, 5)
	assert(SLL.toList(sll) ==  map (lambda x: x, "edcba"))

	sll = SLL.fromList("abcdefghijklmnopq")
	reverse(sll, 4)
	assert(SLL.toList(sll) ==  map (lambda x: x, "dcbahgfelkjiponmq"))

	sll = SLL.fromList("abcdefghijklmnopq")
	reverse(sll, 2)
	assert(SLL.toList(sll) ==  map (lambda x: x, "badcfehgjilknmpoq"))

	sll = SLL.fromList("abcdefghijklmnopq")
	reverse(sll, 3)
	assert(SLL.toList(sll) ==  map (lambda x: x, "cbafedihglkjonmpq"))


	head = Node('1')
	head.next = Node('2')
	head.next.next = Node('3')
	head.next.next.next = Node('4')
	head.next.next.next.next = Node('5')
	sll = SLL()
	sll.head = head
	sll.size = 5
	reverse(sll, 2)
	newhead = sll.head
	assert(newhead.value == '2')
	assert(newhead.next.value == '1')
	assert(newhead.next.next.value == '4')
	assert(newhead.next.next.next.value == '3')
	assert(newhead.next.next.next.next.value == '5')

