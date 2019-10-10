
from data_structures.trees.binary_tree.tree_maker import TreeMaker
from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.clone_tree import CloneTree
import unittest

class Test_CloneTree(unittest.TestCase):
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


	def test_clone_tree(self):
		bt = self.bt
		inorder = []
		bt.inorder_traversal(self.aggregate_list, lst=inorder)

		bt_copy = CloneTree.clone_r(self.bt)
		inorder_copy = []
		bt_copy.inorder_traversal(self.aggregate_list, lst=inorder_copy)
		assert inorder == inorder_copy == [4,2,5,1,6,3,7]


if __name__ == '__main__':
	unittest.main()
