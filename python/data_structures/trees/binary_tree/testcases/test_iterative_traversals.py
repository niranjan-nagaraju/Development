from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.iterative_traversals import IterativeTraversals
from data_structures.trees.binary_tree.tree_maker import TreeMaker
import unittest


class Test_IterativeTraversals(unittest.TestCase):
	def setUp(self):
		self.aggregate_list = lambda kwargs, data : kwargs['lst'].append(data)
		'''
      1
    /   \
   2     3
 /  \   /  \
4    5 6    7
		'''
		l = [1,2,3,4,5,6,7]
		self.bt = TreeMaker.fromList(l)

		'''
      1
    /   
   2
 /   \
4     5
		'''
		self.bt2 = TreeMaker.fromList([1, 2, None, 4, 5])

		'''
        1
      /   \
     2     3
   /         \
  4           5
 /             \
6               7
		'''
		self.bt3 = TreeMaker.fromList([1, 2, 3, 4, None, None, 5, 6, None, None, None, None, None, None, 7])


	def test_inorder_traversal(self):
		bt = self.bt
		inorder = []
		IterativeTraversals.inorder_traversal(bt, self.aggregate_list, lst=inorder)
		inorder_r = []
		bt.inorder_traversal(self.aggregate_list, lst=inorder_r)
		assert inorder == inorder_r == [4,2,5,1,6,3,7]

		bt = self.bt2
		inorder = []
		IterativeTraversals.inorder_traversal(bt, self.aggregate_list, lst=inorder)
		inorder_r = []
		bt.inorder_traversal(self.aggregate_list, lst=inorder_r)
		assert inorder == inorder_r == [4,2,5,1]

		bt = self.bt3
		inorder = []
		IterativeTraversals.inorder_traversal(bt, self.aggregate_list, lst=inorder)
		inorder_r = []
		bt.inorder_traversal(self.aggregate_list, lst=inorder_r)
		assert inorder == inorder_r == [6,4,2,1,3,5,7]


	def test_postorder_traversal(self):
		bt = self.bt
		postorder = []
		IterativeTraversals.postorder_traversal(bt, self.aggregate_list, lst=postorder)
		postorder_r = []
		bt.postorder_traversal(self.aggregate_list, lst=postorder_r)
		assert postorder == postorder_r == [4,5,2,6,7,3,1]

		bt = self.bt2
		postorder = []
		IterativeTraversals.postorder_traversal(bt, self.aggregate_list, lst=postorder)
		postorder_r = []
		bt.postorder_traversal(self.aggregate_list, lst=postorder_r)
		assert postorder == postorder_r == [4,5,2,1]

		bt = self.bt3
		postorder = []
		IterativeTraversals.postorder_traversal(bt, self.aggregate_list, lst=postorder)
		postorder_r = []
		bt.postorder_traversal(self.aggregate_list, lst=postorder_r)
		assert postorder == postorder_r == [6,4,2,7,5,3,1]


	def test_preorder_traversal(self):
		bt = self.bt
		preorder = []
		IterativeTraversals.preorder_traversal(bt, self.aggregate_list, lst=preorder)
		preorder_r = []
		bt.preorder_traversal(self.aggregate_list, lst=preorder_r)
		assert preorder == preorder_r == [1,2,4,5,3,6,7]

		bt = self.bt2
		preorder = []
		IterativeTraversals.preorder_traversal(bt, self.aggregate_list, lst=preorder)
		preorder_r = []
		bt.preorder_traversal(self.aggregate_list, lst=preorder_r)
		assert preorder == preorder_r == [1,2,4,5]

		bt = self.bt3
		preorder = []
		IterativeTraversals.preorder_traversal(bt, self.aggregate_list, lst=preorder)
		preorder_r = []
		bt.preorder_traversal(self.aggregate_list, lst=preorder_r)
		assert preorder == preorder_r == [1,2,4,6,3,5,7]


if __name__ == '__main__':
	unittest.main()
