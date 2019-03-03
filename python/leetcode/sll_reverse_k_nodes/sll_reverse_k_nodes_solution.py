'''
https://leetcode.com/problems/reverse-nodes-in-k-group/

Given a linked list, reverse the nodes of a linked list k at a time and return its modified list.
k is a positive integer and is less than or equal to the length of the linked list. If the number of nodes is not a multiple of k then left-out nodes in the end should remain as it is.

Example:
Given this linked list: 1->2->3->4->5
For k = 2, you should return: 2->1->4->3->5
For k = 3, you should return: 3->2->1->4->5
'''

# Definition for singly-linked list.
class ListNode(object):
     def __init__(self, x):
         self.val = x
         self.next = None


class Solution(object):
	'''
	Reverse nodes in the SLL, k-nodes at a time
	'''
	def reverseKGroup(self, head, k):
		"""
		:type head: ListNode
		:type k: int
		:rtype: ListNode
		"""
		# if k == 1, no reversal needed
		if k < 2:
			return head

		tmp = self.reverse_helper(head, k)
		if not tmp:
			return head

		start = head
		head = tmp # first 'k' nodes done, update sll.head

		# Now do the other nodes 'k' at a time
		while tmp:
			# last saves where we left-off from last time
			# this will be used to link to the reversed block of next 'k' nodes 
			last = start

			# In a block of k nodes [start .. last],
			# after reversing, it becomes [last .. start]
			# Therefore, traversing start.next repeatedly will get us to next k nodes that needs reversing.
			# e.g. a->b->c->d->e->f->g->h->i,j    k=3
			# initially, start = 'a'
			# but after reversing once, c->b->a->d->e->f->g->h->i,j
			# next k-blocks starts at start.next (a->d), start = 'd'
			# and then c->b->a->f->e->d->g->h->i,j, 
			# then again, next k-blocks begin at start.next (d->g), start = 'g'
			# then later, start becomes 'i', and next k-block begins at start.next = 'j'
			start = start.next
			tmp = self.reverse_helper(start, k)

			# If we had k-nodes to begin with, link last block to this reversed block
			# if reverse_k_group() returned None, we are left with fewer than k nodes
			# at the end, return immediately
			if tmp:
				last.next = tmp

		return head



	'''
	Lookahead from 'start' node to see if we have k nodes (start node included)
	'''
	@staticmethod
	def lookahead(start, k):
		trav = start
		i = 0
		while trav and i<k:
			trav = trav.next
			i = i + 1

		if i == k:
			return True

		return False



	'''
	Reverse k nodes starting from 'start'
	and return the kth node from 'start', so it can be 
	linked back to the chain
	'''
	def reverse_helper(self, start, k):
		if not start:
			return None

		# We don't have k-nodes to work with
		if not self.lookahead(start, k):
			return None

		curr = start
		succ = start.next

		# we have k nodes to work with,
		# adjust (k-1) links and return the kth node
		# to be linked with previous k-blocks end
		for i in xrange(1, k):
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


if __name__ == '__main__':
	sol = Solution()

	# TC1
	head = ListNode('1')
	head.next = ListNode('2')
	head.next.next = ListNode('3')
	head.next.next.next = ListNode('4')
	head.next.next.next.next = ListNode('5')

	newhead = sol.reverseKGroup(head, 2)
	assert(newhead.val == '2')
	assert(newhead.next.val == '1')
	assert(newhead.next.next.val == '4')
	assert(newhead.next.next.next.val == '3')
	assert(newhead.next.next.next.next.val == '5')

	# TC2
	head = ListNode('1')
	head.next = ListNode('2')
	head.next.next = ListNode('3')
	head.next.next.next = ListNode('4')
	head.next.next.next.next = ListNode('5')

	newhead = sol.reverseKGroup(head, 3)
	assert(newhead.val == '3')
	assert(newhead.next.val == '2')
	assert(newhead.next.next.val == '1')
	assert(newhead.next.next.next.val == '4')
	assert(newhead.next.next.next.next.val == '5')

	# TC3
	head = ListNode('1')
	head.next = ListNode('2')
	head.next.next = ListNode('3')
	head.next.next.next = ListNode('4')
	head.next.next.next.next = ListNode('5')

	newhead = sol.reverseKGroup(head, 5)
	assert(newhead.val == '5')
	assert(newhead.next.val == '4')
	assert(newhead.next.next.val == '3')
	assert(newhead.next.next.next.val == '2')
	assert(newhead.next.next.next.next.val == '1')

	# TC4
	head = ListNode('1')
	head.next = ListNode('2')
	head.next.next = ListNode('3')
	head.next.next.next = ListNode('4')
	head.next.next.next.next = ListNode('5')

	newhead = sol.reverseKGroup(head, 1)
	assert(newhead.val == '1')
	assert(newhead.next.val == '2')
	assert(newhead.next.next.val == '3')
	assert(newhead.next.next.next.val == '4')
	assert(newhead.next.next.next.next.val == '5')

	


