from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.tree_maker import TreeMaker
import unittest

class Test_TreeMaker(unittest.TestCase):
	def test_make_TreeMakerree_from_list(self):
		'''
      1
    /   \
   2     3
 /   \
4     5
		'''
		l = [1,2,3,4,5]
		t = TreeMaker()
		bt = t.fromList(l)
		assert bt is not None
		assert bt.root is not None

		assert bt.root.value == 1
		assert bt.root.left.value == 2
		assert bt.root.right.value == 3

		assert bt.root.left.left.value == 4
		assert bt.root.left.right.value == 5
		assert bt.root.right.right == None

		assert bt.root.left.left.left == None
		assert bt.root.left.left.right == None
		assert bt.root.left.right.left == None
		assert bt.root.left.right.right == None

		'''
      1
    /   
   2
 /   \
4     5
		'''

		bt2 = TreeMaker.fromList([1, 2, None, 4, 5])
		assert bt2 is not None
		assert bt2.root is not None

		assert bt2.root.value == 1
		assert bt2.root.left.value == 2
		assert bt2.root.right == None

		assert bt2.root.left.left.value == 4
		assert bt2.root.left.right.value == 5

		assert bt2.root.left.left.left == None
		assert bt2.root.left.left.right == None

		'''
        1
      /   \
     2     3
   /         \
  4           5
 /             \
6               7
		'''
		bt3 = TreeMaker.fromList([1, 2, 3, 4, None, None, 5, 6, None, None, None, None, None, None, 7])
		assert bt3 is not None
		assert bt3.root is not None

		assert bt3.root.value == 1
		assert bt3.root.left.value == 2
		assert bt3.root.right.value == 3

		assert bt3.root.left.left.value == 4
		assert bt3.root.left.right == None
		assert bt3.root.right.right.value == 5
		assert bt3.root.right.left == None

		assert bt3.root.left.left.left.value == 6
		assert bt3.root.left.left.right == None
		assert bt3.root.right.right.right.value == 7
		assert bt3.root.right.right.left == None



	# from inorder and preorder traversals
	def test_from_traversal_in_pre(self):
		'''
                                1
                              /   \	
                             2     3
                           /  \  /  \
                          4   5 6    7

		In-order traversal:  [4,2,5,1,6,3,7]
		Pre-order traversal: [1,2,4,5,3,6,7]
		'''
		btree = TreeMaker.from_traversal_in_pre([4,2,5,1,6,3,7], [1,2,4,5,3,6,7])
		assert btree is not None
		assert btree.root is not None

		# Compare inorder and preorder traversals of the resulting tree
		inorder, preorder = [], []
		aggregate_list = lambda kwargs, data : kwargs['lst'].append(data)
		btree.preorder_traversal(aggregate_list, lst=preorder)
		btree.inorder_traversal(aggregate_list, lst=inorder)
		assert inorder, preorder == ([4,2,5,1,6,3,7], [1,2,4,5,3,6,7])

		'''
                                1
                              /   \	
                             2     3
                                 /  \
                                6    7

		In-order traversal:  [2,1,6,3,7]
		Pre-order traversal: [1,2,3,6,7]
		'''
		btree = TreeMaker.from_traversal_in_pre([2,1,6,3,7], [1,2,3,6,7])
		assert btree is not None
		assert btree.root is not None

		# Compare inorder and preorder traversals of the resulting tree
		inorder, preorder = [], []
		aggregate_list = lambda kwargs, data : kwargs['lst'].append(data)
		btree.preorder_traversal(aggregate_list, lst=preorder)
		btree.inorder_traversal(aggregate_list, lst=inorder)
		assert inorder, preorder == ([2,1,6,3,7], [1,2,3,6,7])

		'''
                                1
                              /    	
                             2      
                           /  \      
                          4   5       

		In-order traversal:  [4,2,5,1]
		Pre-order traversal: [1,2,4,5]
		'''
		btree = TreeMaker.from_traversal_in_pre([4,2,5,1], [1,2,4,5])
		assert btree is not None
		assert btree.root is not None

		# Compare inorder and preorder traversals of the resulting tree
		inorder, preorder = [], []
		aggregate_list = lambda kwargs, data : kwargs['lst'].append(data)
		btree.preorder_traversal(aggregate_list, lst=preorder)
		btree.inorder_traversal(aggregate_list, lst=inorder)
		assert inorder, preorder == ([4,2,5,1], [1,2,4,5])



if __name__ == '__main__':
	unittest.main()

