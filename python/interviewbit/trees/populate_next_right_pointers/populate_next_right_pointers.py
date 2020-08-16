'''
https://www.interviewbit.com/problems/populate-next-right-pointers-tree/

Populate Next Right Pointers Tree

Given a binary tree

    struct TreeLinkNode {
      TreeLinkNode *left;
      TreeLinkNode *right;
      TreeLinkNode *next;
    }
Populate each next pointer to point to its next right node. If there is no next right node, the next pointer should be set to NULL.

Initially, all next pointers are set to NULL.

Note:
You may only use constant extra space.
Example :

Given the following binary tree,

         1
       /  \
      2    3
     / \  / \
    4  5  6  7
After calling your function, the tree should look like:

         1 -> NULL
       /  \
      2 -> 3 -> NULL
     / \  / \
    4->5->6->7 -> NULL

Note 1: that using recursion has memory overhead and does not qualify for constant space.
Note 2: The tree need not be a perfect binary tree. 
'''



'''
Solution Outline:
 1. Use two linked-lists(with head and tail references), one for the current level and the other for the next level
 2. Use the current level linked list to traverse level-order, while linking the next-level nodes.
 	At the end of the current level, advance current level to next level, and start over building the next-level links
'''

# Definition for a  binary tree node
class TreeLinkNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None
		self.next = None


	def toList(self):
		l = []
		tmp = self
		while tmp:
			l.append(tmp.val)
			tmp = tmp.next
		return l


class LinkedList:
	def __init__(self, head=None, tail=None):
		self.head = head
		self.tail = tail

	# Append node to the end of the current linked list
	def append(self, node):
		if not self.head:
			self.head = self.tail = node
			return

		self.tail.next = node
		self.tail = node

	# Reset current list to empty list
	def reset(self):
		self.head = self.tail = None


class Solution:
	# @param root, a tree node
	# @return nothing
	def populate_next_right_pointers(self, root):
		if not root:
			return

		current_level = LinkedList(root, root)
		next_level = LinkedList()
		tmp = root
		while tmp:
			next_level.append(tmp.left) if tmp.left else None
			next_level.append(tmp.right) if tmp.right else None
			
			tmp = tmp.next
			if not tmp:
				# Done with current level
				# Advance current level to next level
				current_level.head = next_level.head
				current_level.tail = next_level.tail
				next_level.reset()
				tmp = current_level.head




if __name__ == '__main__':
	s = Solution()
	root = TreeLinkNode(1)
	root.left = TreeLinkNode(2)
	root.right = TreeLinkNode(3)
	root.left.left = TreeLinkNode(4)
	root.left.right = TreeLinkNode(5)
	root.right.left = TreeLinkNode(6)
	root.right.right = TreeLinkNode(7)
	
	s.populate_next_right_pointers(root)
	assert root.toList() == [1]
	assert root.left.toList() == [2,3]
	assert root.left.left.toList() == [4,5,6,7]


			

