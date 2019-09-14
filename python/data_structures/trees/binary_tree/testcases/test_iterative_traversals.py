from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.iterative_traversals import IterativeTraversals
from data_structures.trees.binary_tree.tree_maker import TreeMaker
import unittest


class Test_IterativeTraversals(unittest.TestCase):
	def test_inorder_traversal(self):
		aggregate_list = lambda kwargs, data : kwargs['lst'].append(data)
		'''
      1
    /   \
   2     3
 /  \   /  \
4    5 6    7
		'''
		l = [1,2,3,4,5,6,7]
		bt = TreeMaker.fromList(l)

		inorder = []
		IterativeTraversals.inorder_traversal(bt, aggregate_list, lst=inorder)
		inorder_r = []
		bt.inorder_traversal(aggregate_list, lst=inorder_r)
		assert inorder == inorder_r == [4,2,5,1,6,3,7]

		'''
      1
    /   
   2
 /   \
4     5
		'''
		bt = TreeMaker.fromList([1, 2, None, 4, 5])
		inorder = []
		IterativeTraversals.inorder_traversal(bt, aggregate_list, lst=inorder)
		inorder_r = []
		bt.inorder_traversal(aggregate_list, lst=inorder_r)
		assert inorder == inorder_r == [4,2,5,1]


		'''
        1
      /   \
     2     3
   /         \
  4           5
 /             \
6               7
		'''
		bt = TreeMaker.fromList([1, 2, 3, 4, None, None, 5, 6, None, None, None, None, None, None, 7])
		inorder = []
		IterativeTraversals.inorder_traversal(bt, aggregate_list, lst=inorder)
		inorder_r = []
		bt.inorder_traversal(aggregate_list, lst=inorder_r)
		assert inorder == inorder_r == [6,4,2,1,3,5,7]



if __name__ == '__main__':
	unittest.main()
