https://leetcode.com/problems/copy-list-with-random-pointer/description/

Problem Description:
===================
	A linked list is given such that each node contains an additional random pointer 
	which could point to any node in the list or null.
	Return a deep copy of the list. 

Solution 1:
===========
Complexity:
	Time: O(n)
	Space: O(n)  {not counting the space needed for copies of the nodes}
Approach:
	For every node, a, in the SLL, Create a copy of the node a' but also store this association
	from a -> a' so anytime we are linking any two random nodes, a-> random = b, we can quickly link
	a'->random = b' using the association.



Solution 2:
==========
Complexity:
	Time: O(n)
	Space: O(1)  {not counting the space needed for copies of the nodes}
Approach:
	A hashtable is needed in solution 1 to store the association from a -> a'
	but what if the association is predictable? That would obviate the need for the extra storage.

	what if a' is a->next?
	Start with duplicating each node and adding the duplicate next to the original node, so the list becomes
	a->a'->b->b'-> ...
	With such an arrangement, if a->random = b, a'->random == a->next->random = b->next




 
