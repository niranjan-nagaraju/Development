from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.node import Node


def basic_tests():
	# prefix equation tree : "+a*bc"
	'''
	     +
	   /   \	
      a     *
	      /   \
	     b     c
	'''		 

	root = Node("+")
	lnode = Node("a")
	rnode = Node("*")
	root.setChildren(lnode, rnode)

	rlnode = Node("b")
	rrnode = Node("c")
	rnode.setChildren(rlnode, rrnode)

	btree = BinaryTree(root)

	assert(btree.height() == 3)
	assert btree.span() == 3
	assert btree.width() == 2

	'''
           1
         /   \
        3     2
       / \     \  
      5   3     9 
	'''
	root = Node("1")
	root.setChildren(Node("3"), Node("2"))
	root.left.setChildren(Node("5"), Node("3"))
	root.right.right = Node("9")
	btree2 = BinaryTree(root)
	assert(btree2.height() == 3)
	assert btree2.span() == 4
	assert btree2.width() == 3

	'''
           1
         /   \
        3     2
       / 
      5
	'''
	root = Node("1")
	root.setChildren(Node("3"), Node("2"))
	root.left.left = Node("5")
	btree3 = BinaryTree(root)
	assert(btree3.height() == 3)
	assert btree3.span() == 3
	assert btree3.width() == 2


	btree4 = BinaryTree(Node('blah'))
	assert(btree4.height() == 1)
	assert btree4.span() == 0
	assert btree4.width() == 1



def traversals():
	# prefix equation tree : "+a*bc"
	'''
	     +
	   /   \	
      a     *
	      /   \
	     b     c
	'''		 

	root = Node("+")
	lnode = Node("a")
	rnode = Node("*")
	root.setChildren(lnode, rnode)

	rlnode = Node("b")
	rrnode = Node("c")
	rnode.setChildren(rlnode, rrnode)

	btree = BinaryTree(root)


	print 'Preorder: ',
	btree.preorder_traversal()
	print

	l = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data)
	btree.preorder_traversal(collate_fn, lst=l)
	assert (l == ['+', 'a', '*', 'b', 'c'])

	print 'Postorder: ',
	btree.postorder_traversal()
	print

	l = []
	btree.postorder_traversal(collate_fn, lst=l)
	assert (l == ['a',  'b', 'c', '*', '+'])

	print 'Inorder: ',
	btree.inorder_traversal()
	print

	l = []
	btree.inorder_traversal(collate_fn, lst=l)
	assert (l == ['a', '+', 'b', '*', 'c'])

	print 'Level order: ',
	btree.levelorder_traversal()
	print

	l = []
	btree.levelorder_traversal(collate_fn, lst=l)
	assert (l == ['+', 'a', '*', 'b', 'c'])


	print 'Left View: ',
	btree.left_view()
	print

	l = []
	btree.left_view(collate_fn, lst=l)
	assert (l == ['+', 'a',  'b'])

	print 'Right View: ',
	btree.right_view()
	print

	l = []
	btree.right_view(collate_fn, lst=l)
	assert (l == ['+', '*',  'c'])

	print 'Top View: ',
	btree.top_view()
	print

	l = []
	btree.top_view(collate_fn, lst=l)
	assert (l == ['+', 'a',  '*', 'c'])

	print 'Top View L-R: ',
	btree.top_view_LR()
	print

	l = []
	btree.top_view_LR(collate_fn, lst=l)
	assert (l == ['a', '+',  '*', 'c'])


	print 'Bottom View: ',
	btree.bottom_view()
	print

	l = []
	btree.bottom_view(collate_fn, lst=l)
	assert (l == ['a', 'b',  '*', 'c'])

	print 'Testcase TC1 passed!'



def test_path_and_lca():
	'''
		 1
	   /   \    
	  2     3
	 /  \  /  \
	4    5 6   7
	'''      

	root = Node(1)
	lnode = Node(2)
	rnode = Node(3)
	root.setChildren(lnode, rnode)

	lnode.setChildren(Node(4), Node(5))
	rnode.setChildren(Node(6), Node(7))
	btree = BinaryTree(root)

	assert(btree.height() == 3)

	nodes =  btree.path_n(7)
	path = [1,3,7]
	i=0
	assert(len(nodes) == 3)
	for n in nodes:
		assert n.value == path[i]
		i += 1

	# find path using a node reference
	nodes =  btree.path_n(rnode)
	path = [1,3]
	i=0
	for n in nodes:
		assert n.value == path[i]
		i += 1

	assert(btree.path_n(8) == [])

	assert btree.path_1(7) == [1,3,7]
	assert btree.path_1(5) == [1,2,5]
	assert btree.path_1(6) == [1,3,6]
	assert btree.path_1(1) == [1]
	assert btree.path_1(2) == [1, 2]
	assert btree.path_1(9) == []

	assert btree.path_2(7) == [1,3,7]
	assert btree.path_2(5) == [1,2,5]
	assert btree.path_2(6) == [1,3,6]
	assert btree.path_2(1) == [1]
	assert btree.path_2(2) == [1, 2]
	assert btree.path_2(9) == []

	assert btree.path(2) == [1, 2]
	assert btree.path(9) == []

	n1 = rnode.left
	n2 = lnode.right
	assert n1.value == 6
	assert n2.value == 5
	assert btree.lca(n1, n2) == root

	assert btree.lca(rnode, lnode) == root
	assert btree.lca(rnode.right, rnode.left) == rnode
	assert btree.lca(lnode.right, lnode.left) == lnode
	assert btree.lca(lnode.left, rnode.right) == root

	assert btree.lca(rnode, 2) == root
	assert btree.lca(7, rnode.left) == rnode
	assert btree.lca(5, 4) == lnode
	assert btree.lca(4, 7) == root



def test_tree_maker():
	'''
        1
      /   \	
     2     3
   /        \
  4          5	
	'''
	btree = BinaryTree.fromList([1, 2, 3, 4, None, None, 5])
	assert btree is not None
	assert btree.root is not None

	assert btree.root.value == 1
	assert btree.root.left.value == 2
	assert btree.root.right.value == 3

	assert btree.root.left.left.value == 4
	assert btree.root.left.right == None
	assert btree.root.right.left == None
	assert btree.root.right.right.value == 5

	assert btree.root.left.left.left == None
	assert btree.root.left.left.right == None
	assert btree.root.right.right.left == None
	assert btree.root.right.right.right == None

	'''
							1
						  /   \	
						 2     3
					   /  \  /  \
					  4   5 6    7

	In-order traversal:  [4,2,5,1,6,3,7]
	Pre-order traversal: [1,2,4,5,3,6,7]
	'''
	btree = BinaryTree.from_traversal_in_pre([4,2,5,1,6,3,7], [1,2,4,5,3,6,7])
	assert btree is not None
	assert btree.root is not None

	# Compare inorder and preorder traversals of the resulting tree
	inorder, preorder = [], []
	aggregate_list = lambda kwargs, data : kwargs['lst'].append(data)
	btree.preorder_traversal(aggregate_list, lst=preorder)
	btree.inorder_traversal(aggregate_list, lst=inorder)
	assert inorder, preorder == ([4,2,5,1,6,3,7], [1,2,4,5,3,6,7])




if __name__ == "__main__":
	basic_tests()
	traversals()
	test_path_and_lca()
	test_tree_maker()
