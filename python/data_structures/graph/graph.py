
class Graph(object):
	def __init__(self, rows=1, columns=1):
		self.rows = rows
		self.columns = columns
		self._matrix = [[]]
		self._adj_list = []

	
	@property
	def matrix(self):
		return self._matrix 


	@property
	def adj_list(self):
		return self._adj_list 



	def bfs(self):
		pass


	def dfs(self):
		pass
