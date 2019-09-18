from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.flatten import FlattenBinaryTree
from data_structures.trees.binary_tree.tree_maker import TreeMaker
import unittest


class Test_Flatten(unittest.TestCase):
	# called before every testcase
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

		'''
    1
   / \
  2   5
 / \   \
3   4   6
		'''
		self.bt4 = TreeMaker.fromList([1,2,5,3,4,None,6])

	def test_inorder_flatten_to_dll(self):
		def _test(bt, inorder_sequence):
			(head, tail) = FlattenBinaryTree.binTreeToDLL_in(bt)
			assert head.value == inorder_sequence[0]
			assert tail.value == inorder_sequence[-1]

			forward = []
			tmp = head
			while tmp:
				forward.append(tmp.value)
				tmp = tmp.right

			reverse = []
			tmp = tail
			while tmp:
				reverse.append(tmp.value)
				tmp = tmp.left

			assert forward == inorder_sequence
			assert reverse == forward[::-1]

		_test(self.bt, [4,2,5,1,6,3,7])
		_test(self.bt2, [4,2,5,1])
		_test(self.bt3, [6,4,2,1,3,5,7])
		_test(self.bt4, [3,2,4,1,5,6])


	def test_preorder_flatten_to_dll(self):
		def _test(bt, preorder_sequence):
			(head, tail) = FlattenBinaryTree.binTreeToDLL_pre(bt)
			assert head.value == preorder_sequence[0]
			assert tail.value == preorder_sequence[-1]

			forward = []
			tmp = head
			while tmp:
				forward.append(tmp.value)
				tmp = tmp.right

			reverse = []
			tmp = tail
			while tmp:
				reverse.append(tmp.value)
				tmp = tmp.left

			assert forward == preorder_sequence
			assert reverse == forward[::-1]

		_test(self.bt, [1,2,4,5,3,6,7])
		_test(self.bt2, [1,2,4,5])
		_test(self.bt3, [1,2,4,6,3,5,7])
		_test(self.bt4, [1,2,3,4,5,6])


if __name__ == '__main__':
	unittest.main()

