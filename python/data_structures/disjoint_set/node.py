'''
Node in a disjoint-set data structure
Contains
  key: data/value of the set item
  rank: number of children under this Node
        (relevant if the node is a representative of the set)
  representative: representative for the set this node belongs to,
        None if this node is the representative for the set.
'''

class Node(object):
	def __init__(self, key):
		self.key = key
		self.rank = 0
		self.representative = None

	
	def __str__(self):
		return str(self.key)


	def __repr__(self):
		return '{key: ' + str(self.key) + ', rank: ' + str(self.rank) + ', rep: ' + str(self.representative) + '}'




if __name__ == '__main__':
	n = Node('word')
	print n, repr(n)
