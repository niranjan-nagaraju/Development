
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




	# Test recursive version of the clone method
	def test_clone_tree_recursive(self):
		bt = self.bt
		bt_copy = CloneTree.clone_r(bt)
		inorder, preorder, postorder = [], [], []
		inorder_c, preorder_c, postorder_c = [], [], []
		bt.inorder_traversal(self.aggregate_list, lst=inorder)
		bt_copy.inorder_traversal(self.aggregate_list, lst=inorder_c)
		bt.postorder_traversal(self.aggregate_list, lst=postorder)
		bt_copy.postorder_traversal(self.aggregate_list, lst=postorder_c)
		bt.preorder_traversal(self.aggregate_list, lst=preorder)
		bt_copy.preorder_traversal(self.aggregate_list, lst=preorder_c)
		assert inorder == inorder_c == [4,2,5,1,6,3,7]
		assert postorder == postorder_c == [4,5,2,6,7,3,1]
		assert preorder == preorder_c == [1,2,4,5,3,6,7]

		bt = self.bt2
		bt_copy = CloneTree.clone_r(bt)
		inorder, preorder, postorder = [], [], []
		inorder_c, preorder_c, postorder_c = [], [], []
		bt.inorder_traversal(self.aggregate_list, lst=inorder)
		bt_copy.inorder_traversal(self.aggregate_list, lst=inorder_c)
		bt.postorder_traversal(self.aggregate_list, lst=postorder)
		bt_copy.postorder_traversal(self.aggregate_list, lst=postorder_c)
		bt.preorder_traversal(self.aggregate_list, lst=preorder)
		bt_copy.preorder_traversal(self.aggregate_list, lst=preorder_c)
		assert inorder == inorder_c == [4,2,5,1]
		assert postorder == postorder_c == [4,5,2,1]
		assert preorder == preorder_c == [1,2,4,5]

		bt = self.bt3
		bt_copy = CloneTree.clone_r(bt)
		inorder, preorder, postorder = [], [], []
		inorder_c, preorder_c, postorder_c = [], [], []
		bt.inorder_traversal(self.aggregate_list, lst=inorder)
		bt_copy.inorder_traversal(self.aggregate_list, lst=inorder_c)
		bt.postorder_traversal(self.aggregate_list, lst=postorder)
		bt_copy.postorder_traversal(self.aggregate_list, lst=postorder_c)
		bt.preorder_traversal(self.aggregate_list, lst=preorder)
		bt_copy.preorder_traversal(self.aggregate_list, lst=preorder_c)
		assert inorder == inorder_c == [6,4,2,1,3,5,7]
		assert postorder == postorder_c == [6,4,2,7,5,3,1]
		assert preorder == preorder_c == [1,2,4,6,3,5,7]



	# Test iterative version of the clone method
	def test_clone_tree_iterative(self):
		bt = self.bt
		bt_copy = CloneTree.clone_i(bt)
		inorder, preorder, postorder = [], [], []
		inorder_c, preorder_c, postorder_c = [], [], []
		bt.inorder_traversal(self.aggregate_list, lst=inorder)
		bt_copy.inorder_traversal(self.aggregate_list, lst=inorder_c)
		bt.postorder_traversal(self.aggregate_list, lst=postorder)
		bt_copy.postorder_traversal(self.aggregate_list, lst=postorder_c)
		bt.preorder_traversal(self.aggregate_list, lst=preorder)
		bt_copy.preorder_traversal(self.aggregate_list, lst=preorder_c)
		assert inorder == inorder_c == [4,2,5,1,6,3,7]
		assert postorder == postorder_c == [4,5,2,6,7,3,1]
		assert preorder == preorder_c == [1,2,4,5,3,6,7]

		bt = self.bt2
		bt_copy = CloneTree.clone_i(bt)
		inorder, preorder, postorder = [], [], []
		inorder_c, preorder_c, postorder_c = [], [], []
		bt.inorder_traversal(self.aggregate_list, lst=inorder)
		bt_copy.inorder_traversal(self.aggregate_list, lst=inorder_c)
		bt.postorder_traversal(self.aggregate_list, lst=postorder)
		bt_copy.postorder_traversal(self.aggregate_list, lst=postorder_c)
		bt.preorder_traversal(self.aggregate_list, lst=preorder)
		bt_copy.preorder_traversal(self.aggregate_list, lst=preorder_c)
		assert inorder == inorder_c == [4,2,5,1]
		assert postorder == postorder_c == [4,5,2,1]
		assert preorder == preorder_c == [1,2,4,5]

		bt = self.bt3
		bt_copy = CloneTree.clone_i(bt)
		inorder, preorder, postorder = [], [], []
		inorder_c, preorder_c, postorder_c = [], [], []
		bt.inorder_traversal(self.aggregate_list, lst=inorder)
		bt_copy.inorder_traversal(self.aggregate_list, lst=inorder_c)
		bt.postorder_traversal(self.aggregate_list, lst=postorder)
		bt_copy.postorder_traversal(self.aggregate_list, lst=postorder_c)
		bt.preorder_traversal(self.aggregate_list, lst=preorder)
		bt_copy.preorder_traversal(self.aggregate_list, lst=preorder_c)
		assert inorder == inorder_c == [6,4,2,1,3,5,7]
		assert postorder == postorder_c == [6,4,2,7,5,3,1]
		assert preorder == preorder_c == [1,2,4,6,3,5,7]



if __name__ == '__main__':
	unittest.main()

