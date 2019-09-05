'''
https://leetcode.com/problems/evaluate-division/

Equations are given in the format A / B = k, where A and B are variables represented as strings, and k is a real number (floating point number). Given some queries, return the answers. If the answer does not exist, return -1.0.

Example:
Given a / b = 2.0, b / c = 3.0.
queries are: a / c = ?, b / a = ?, a / e = ?, a / a = ?, x / x = ? .
return [6.0, 0.5, -1.0, 1.0, -1.0 ].

The input is: vector<pair<string, string>> equations, vector<double>& values, vector<pair<string, string>> queries , where equations.size() == values.size(), and the values are positive. This represents the equations. Return vector<double>.

According to the example above:

equations = [ ["a", "b"], ["b", "c"] ],
values = [2.0, 3.0],
queries = [ ["a", "c"], ["b", "a"], ["a", "e"], ["a", "a"], ["x", "x"] ]. 
 

The input is always valid. You may assume that evaluating the queries will result in no division by zero and there is no contradiction.
'''


'''
Algorithm outline (DFS-based):
	Use a directed graph to store each equation,
	if the equation is a/b = k, Add an edge from a->b with a weight k, and an edge from b->a with a weight 1/k.
	To answer each query p/q, find a path from p in the graph that ends with q.
	NOTE: There can be multiple paths to get p/q but per problem desciption, the answer would be unique.
'''

from collections import defaultdict
class Solution(object):
	def calcEquation(self, equations, values, queries):
		"""
		:type equations: List[List[str]]
		:type values: List[float]
		:type queries: List[List[str]]
		:rtype: List[float]
		"""
		graph = defaultdict(list)
		for i in xrange(len(equations)):
			a,b = equations[i]
			v = values[i]

			# add an edge from a->b
			graph[a].append((b, v))
			graph[b].append((a, 1.0/v))


		# DFS-based path utility that accumulates products in the current path
		# a/b=k, b/c=l, a/c = a/b*b/c = k*l
		def dfs_path_util(curr, end, value=1.0):
			if curr == end:
				answers.append(round(value,5))
				return

			visited[curr] = True
			for b,v in graph[curr]:
				if not visited[b]:
					dfs_path_util(b, end, value*v)

			visited[curr] = False
			return



		# Use DFS to find a path that calculates a/b
		def calculate_query_and_add_to_answer(a, b):
			# numerator, or denominator or both are not found
			# in the graph (ie the input equations)
			if not graph[a] or not graph[b]:
				answers.append(-1.0)
				return

			num_answers = len(answers)
			dfs_path_util(a, b)

			# DFS couldn't find a path from a->b
			# and therefore a/b cannot be calculated
			# return -1.0
			if num_answers == len(answers):
				answers.append(-1.0)



		# Start answering queries
		visited = defaultdict(bool)
		answers = []
		for q in queries:
			a, b = q
			calculate_query_and_add_to_answer(a, b)

		return answers



if __name__ == '__main__':
	s = Solution()
	assert s.calcEquation([["a","b"],["b","c"]],
			[2.0,3.0],
			[["a","c"],["b","a"],["a","e"],["a","a"],["x","x"]]) == [6.0,0.5,-1.0,1.0,-1.0]


	assert s.calcEquation([["x1","x2"],["x2","x3"],["x3","x4"],["x4","x5"]],
			[3.0,4.0,5.0,6.0],
			[["x1","x5"],["x5","x2"],["x2","x4"],["x2","x2"],["x2","x9"],["x9","x9"]]) == [360.0,0.00833,20.0,   1.0,-1.0,-1.0]


	assert s.calcEquation([["a","b"],["c","d"]],
			[1.0,1.0],
			[["a","c"],["b","d"],["b","a"],["d","c"]]) == [-1.0,-1.0,1.0,1.0]

