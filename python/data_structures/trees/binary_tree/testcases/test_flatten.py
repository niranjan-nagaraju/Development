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


	def test_inorder_flatten_to_dll(self):
		bt = self.bt
		(head, tail) = FlattenBinaryTree.binTreeToDLL_in(bt)
		assert head.value == 4
		assert tail.value == 7

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

		assert forward == [4,2,5,1,6,3,7]
		assert reverse == forward[::-1]


		bt = self.bt2
		(head, tail) = FlattenBinaryTree.binTreeToDLL_in(bt)
		assert head.value == 4
		assert tail.value == 1

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

		assert forward == [4,2,5,1]
		assert reverse == forward[::-1]


		bt = self.bt3
		(head, tail) = FlattenBinaryTree.binTreeToDLL_in(bt)
		assert head.value == 6
		assert tail.value == 7

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

		assert forward == [6,4,2,1,3,5,7]
		assert reverse == forward[::-1]


	def test_postorder_flatten_to_dll(self):
		bt = self.bt
		(head, tail) = FlattenBinaryTree.binTreeToDLL_pre(bt)
		assert head.value == 1
		assert tail.value == 7

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

		assert forward == [1,2,4,5,3,6,7]
		assert reverse == forward[::-1]

		bt = self.bt2
		(head, tail) = FlattenBinaryTree.binTreeToDLL_pre(bt)
		assert head.value == 1
		assert tail.value == 5

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

		assert forward == [1,2,4,5]
		assert reverse == forward[::-1]


		bt = self.bt3
		(head, tail) = FlattenBinaryTree.binTreeToDLL_pre(bt)
		assert head.value == 1
		assert tail.value == 7

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

		assert forward == [1,2,4,6,3,5,7]
		assert reverse == forward[::-1]


if __name__ == '__main__':
	unittest.main()

