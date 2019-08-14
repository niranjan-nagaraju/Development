
import sys
from data_structures.trees.bst.bst import BST
from data_structures.trees.binary_tree.node import *


def test_traversals():
	'''
		Manually construct the below tree:
			4
		  /	  \
		1      6
		      / \
		     5   7

	'''
	root = Node(4)
	lnode = Node(1)
	rnode = Node(6)
	root.setChildren(lnode, rnode)

	rlnode = Node(5)
	rrnode = Node(7)
	rnode.setChildren(rlnode, rrnode)

	print_fn = lambda x,y : sys.stdout.write(str(y) + ' ')

	bst = BST(root)
	print 'Preorder: ',
	bst.preorder_traversal(print_fn)
	print

	l = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data)
	bst.preorder_traversal(collate_fn, lst=l)
	assert (l == [4, 1, 6, 5, 7])

	print 'Postorder: ',
	bst.postorder_traversal(print_fn)
	print

	l = []
	bst.postorder_traversal(collate_fn, lst=l)
	assert (l == [1, 5, 7, 6, 4])

	print 'Inorder: ',
	bst.inorder_traversal(print_fn)
	print

	l = []
	bst.inorder_traversal(collate_fn, lst=l)
	assert (l == [1, 4, 5, 6, 7])



def test_insert():
	bst = BST()
	l = [2,1,4,5,3,8]
	'''
	  Resulting tree:
	          2
			/   \
		   1	  4
		        /  \
			   3    5
			         \
					  8	
	
	   Inorder: 1 2 3 4 5 8
	'''
	for x in l:
		bst.insert(x)

	assert(bst.size == len(l))
	assert(bst.height() == 3)

	assert(bst.root.value == 2)

	l2 = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data)
	bst.inorder_traversal(collate_fn, lst=l2)

	# NOTE: sorted(list): returns a new list with sorted order without modifying the input list unlike list.sort()
	assert (l2 == sorted(l))




def test_path_and_lca():
	'''
		 4
	   /   \    
	  2     6
	 /  \  /  \
	1    3 5   7
	'''      

	root = Node(4)
	lnode = Node(2)
	rnode = Node(6)
	root.setChildren(lnode, rnode)

	lnode.setChildren(Node(1), Node(3))
	rnode.setChildren(Node(5), Node(7))
	btree = BST(root)

	assert(btree.height() == 2)

	nodes =  btree.path_n(7)
	path = [4,6,7]
	i=0
	assert(len(nodes) == 3)
	for n in nodes:
		assert n.value == path[i]
		i += 1

	# find path using a node reference
	nodes =  btree.path_n(rnode)
	path = [4,6]
	i=0
	for n in nodes:
		assert n.value == path[i]
		i += 1

	assert(btree.path_n(8) == [])

	assert btree.path_1(7) == [4,6,7]
	assert btree.path_1(5) == [4,6,5]
	assert btree.path_1(6) == [4,6]
	assert btree.path_1(1) == [4,2,1]
	assert btree.path_1(2) == [4, 2]
	assert btree.path_1(9) == []

	assert btree.path_2(7) == [4,6,7]
	assert btree.path_2(5) == [4,6,5]
	assert btree.path_2(6) == [4,6]
	assert btree.path_2(1) == [4,2,1]
	assert btree.path_2(2) == [4, 2]
	assert btree.path_2(9) == []


	assert btree.path(2) == [4, 2]
	assert btree.path(9) == []

	n1 = rnode.left
	n2 = lnode.right
	assert n1.value == 5
	assert n2.value == 3
	assert btree.lca(n1, n2) == root

	assert btree.lca(rnode, lnode) == root
	assert btree.lca(rnode.right, rnode.left) == rnode
	assert btree.lca(lnode.right, lnode.left) == lnode
	assert btree.lca(lnode.left, rnode.right) == root

	assert btree.lca(rnode, 2) == root
	assert btree.lca(7, rnode.left) == rnode
	assert btree.lca(5, 4) == root
	assert btree.lca(1, 3) == lnode




if __name__ == "__main__":
	test_traversals()
	test_insert()
	test_path_and_lca()

