# -*- coding: iso-8859-15 -*-
'''
https://code.google.com/codejam/contest/32013/dashboard


* Problem
  -------
The urban legend goes that if you go to the Google homepage and search for "Google", the universe will implode. We have a secret to share... It is true! Please don't try it, or tell anyone. All right, maybe not. We are just kidding.

The same is not true for a universe far far away. In that universe, if you search on any search engine for that search engine's name, the universe does implode!

To combat this, people came up with an interesting solution. All queries are pooled together. They are passed to a central system that decides which query goes to which search engine. The central system sends a series of queries to one search engine, and can switch to another at any time. Queries must be processed in the order they're received. The central system must never send a query to a search engine whose name matches the query. In order to reduce costs, the number of switches should be minimized.

Your task is to tell us how many times the central system will have to switch between search engines, assuming that we program it optimally.

Input
------
The first line of the input file contains the number of cases, N. N test cases follow.

Each case starts with the number S -- the number of search engines. The next S lines each contain the name of a search engine. Each search engine name is no more than one hundred characters long and contains only uppercase letters, lowercase letters, spaces, and numbers. There will not be two search engines with the same name.

The following line contains a number Q -- the number of incoming queries. The next Q lines will each contain a query. Each query will be the name of a search engine in the case.

Output
------
For each input case, you should output:

Case #X: Y
where X is the number of the test case and Y is the number of search engine switches. Do not count the initial choice of a search engine as a switch.
Limits

0 < N ≤ 20

Small dataset

2 ≤ S ≤ 10

0 ≤ Q ≤ 100

Large dataset

2 ≤ S ≤ 100

0 ≤ Q ≤ 1000


Sample
=======

Input 
2
5
Yeehaw
NSM
Dont Ask
B9
Googol
10
Yeehaw
Yeehaw
Googol
B9
Googol
NSM
B9
NSM
Dont Ask
Googol
5
Yeehaw
NSM
Dont Ask
B9
Googol
7
Googol
Dont Ask
NSM
NSM
Yeehaw
Yeehaw
Googol

Output
Case #1: 1
Case #2: 0

In the first case, one possible solution is to start by using Dont Ask, and switch to NSM after query number 8.
For the second case, you can use B9, and not need to make any switches.

===================================

* TC 1
3 Search engines,
A B C

7 queries,
B C C A C C C

Create a table mapping search engines to queries, the entries count how far the search engine can get you starting
from a Query 'q' before needing a switch.

|   | B | C | C | A | C | C | C |
|---+---+---+---+---+---+---+---|
| A | 3 | 2 | 1 | 0 | 3 | 2 | 1 |
| B | 0 | 6 | 5 | 4 | 3 | 2 | 1 |
| C | 1 | 0 | 0 | 1 | 0 | 0 | 0 |

Start with Q1, A takes us farther to 3 queries from Q1
From Q4, B does better = 4 (3+4 == 7) single switch needed.


* TC 2
5 search engines
Yeehaw
NSM
Dont Ask
B9
Googol

10 queries
10
Yeehaw
Yeehaw
Googol
B9
Googol
NSM
B9
NSM
Dont Ask
Googol

|          | Yeehaw | Yeehaw | Googol | B9 | Googol | NSM | B9 | NSM | Dont Ask | Googol |
|----------+--------+--------+--------+----+--------+-----+----+-----+----------+--------|
| Yeehaw   | 0      | 0      | 8      | 7  | 6      | 5   | 4  | 3   | 2        | 1      |
| NSM      | 5      | 4      | 3      | 2  | 1      | 0   | 1  | 0   | 2        | 1      |
| Dont Ask | 8      | 7      | 6      | 5  | 4      | 3   | 2  | 1   | 0        | 1      |
| B9       | 3      | 2      | 1      | 0  | 2      | 1   | 0  | 3   | 2        | 1      |
| Googol   | 2      | 1      | 0      | 1  | 0      | 4   | 3  | 2   | 1        | 0      |

Yeehaw: start with 'Dont Ask', queries answered = 8
Q9: 'Dont Ask', pick any of Yeehaw, NSM or DontAsk to answer the last 2.
switches needed = 1


* TC3
5 search engines
Yeehaw
NSM
Dont Ask
B9
Googol

7 queries
Googol
Dont Ask
NSM
NSM
Yeehaw
Yeehaw
Googol


|          | Googol | Dont Ask | NSM | NSM | Yeehaw | Yeehaw | Googol |
|----------+--------+----------+-----+-----+--------+--------+--------|
| Yeehaw   | 4      | 3        | 2   | 1   | 0      | 0      | 1      |
| NSM      | 2      | 1        | 0   | 0   | 3      | 2      | 1      |
| Dont Ask | 1      | 0        | 5   | 4   | 3      | 2      | 1      |
| B9       | 7      |          |     |     |        |        |        |
| Googol   |        |          |     |     |        |        |        |

B9 can solve 7 queries.


* Is the solution optimal?
  -----------------------
  S1   x          y              z                      
+ Q1 ------- S2 -------- S1 ............. S2
  S2 S2 .... S2 ........ S1 ............. S2


S1 covers x+y
S2 covers y+z
(y+z) > (x+y)

but x+y+z still needs just one switch even if we assume S3 or some combination of Sx (not S1) can handle x
best case scenario, only one Sx handles x, which is optimistic - even if it does, its only as optimal as starting with S1

'''


# Fill in coverage numbers for a given search engine, and a list of queries
# coverage numbers: number of queries a search engine can cover starting from a query, i
# before needing a switch
# Queries: 7 queries, ['B', 'C', 'C', 'A', 'C', 'C', 'C']
# search_engine: 'B' => coverage = [0, 6, 5, 4, 3, 2, 1]
# search_engine: 'A' => coverage = [3, 2, 1, 0, 3, 2, 1]
# search_engine: 'C' => coverage = [1, 0, 0, 1, 0, 0, 0]
def fill_coverage(search_engine, nQueries, queries, coverage):
	current_coverage_count = 0
	for i in xrange(nQueries-1, -1, -1):
		# query matches search engine's name
		# a switch is needed
		if queries[i] == search_engine:
			current_coverage_count = 0
			coverage[i] = 0
		else:
			current_coverage_count += 1
			coverage[i] = current_coverage_count



# Create a sxq matrix (s: num of search engines, q: number of queries)
# where each entry indicates how long search engine S[i] can continue processing
# queries starting from queries[j] before needing a switch
# Then use the coverage matrix to pick the search engine, S[i] that can get us the farthest
# from query #0, to say, query #x, then find S[j] that can get us the farthest from query #x,
# and so on, until all queries have been answered
#
# returns the number of search-engine switches needed to answer all queries
def optimize_search_queries(nSearchEngines, search_engines, nQueries, queries):
	if nQueries == 0:
		return 0

	coverage_matrix = [[0 for j in xrange(nQueries)] for i in xrange(nSearchEngines)]
	for s in xrange(nSearchEngines):
		fill_coverage(search_engines[s], nQueries, queries, coverage_matrix[s])
		# Search engine, s, can cover all queries
		# 0 switches needed, return immediately
		# without needing to calculate the remainder of the matrix
		if coverage_matrix[s][0] == nQueries:
			return 0

	starting_query = 0
	num_switches = 0
	while starting_query < nQueries:
		# Lookup the matrix column-wise at 'starting_query' column
		# and figure out the search engine, s, that can get answer the most queries
		# from query[starting_query]
		max_queries = 0
		for s in xrange(nSearchEngines):
			if coverage_matrix[s][starting_query] > max_queries:
				max_queries = coverage_matrix[s][starting_query]

		# Found best search engine, s,  that handles max # of queries
		# Delegate those queries to s,
		# and restart again next query in line
		starting_query += max_queries
		num_switches += 1

	# Undo counting answering first query as a switch
	# number of effective switches will be one less than we counted
	return num_switches-1






def test_fill_coverage():
	# TC 1:
	queries = ['B', 'C', 'C', 'A', 'C', 'C', 'C']
	nQueries = len(queries)
	search_engines = ['A', 'B', 'C']
	nSearchEngines = len(search_engines)
	coverage_matrix = [[0 for j in xrange(nQueries)] for i in xrange(nSearchEngines)]
	fill_coverage(search_engines[0], nQueries, queries, coverage_matrix[0])
	assert coverage_matrix[0] == [3, 2, 1, 0, 3, 2, 1]

	fill_coverage(search_engines[1], nQueries, queries, coverage_matrix[1])
	assert coverage_matrix[1] == [0, 6, 5, 4, 3, 2, 1]

	fill_coverage(search_engines[2], nQueries, queries, coverage_matrix[2])
	assert coverage_matrix[2] == [1, 0, 0, 1, 0, 0, 0]
	assert optimize_search_queries(nSearchEngines, search_engines, nQueries, queries) == 1

	# TC 2:
	queries = ['B', 'C', 'C', 'A', 'C', 'C', 'C', 'B', 'A']
	nQueries = len(queries)
	search_engines = ['A', 'B', 'C']
	nSearchEngines = len(search_engines)
	coverage_matrix = [[0 for j in xrange(nQueries)] for i in xrange(nSearchEngines)]
	for i in xrange(nSearchEngines):
		fill_coverage(search_engines[i], nQueries, queries, coverage_matrix[i])
	assert coverage_matrix == [
			[3, 2, 1, 0, 4, 3, 2, 1, 0],
			[0, 6, 5, 4, 3, 2, 1, 0, 1],
			[1, 0, 0, 1, 0, 0, 0, 2, 1]
			]
	assert optimize_search_queries(nSearchEngines, search_engines, nQueries, queries) == 2

	# TC 3
	search_engines = ['Yeehaw', 'NSM', 'Dont Ask', 'B9', 'Googol']
	nSearchEngines = len(search_engines)
	queries = ['Yeehaw', 'Yeehaw', 'Googol', 'B9', 'Googol', 'NSM', 'B9', 'NSM', 'Dont Ask', 'Googol']
	nQueries = len(queries)

	coverage_matrix = [[0 for j in xrange(nQueries)] for i in xrange(nSearchEngines)]
	for i in xrange(nSearchEngines):
		fill_coverage(search_engines[i], nQueries, queries, coverage_matrix[i])
	assert coverage_matrix == [
			[0, 0, 8, 7, 6, 5, 4, 3, 2, 1],
			[5, 4, 3, 2, 1, 0, 1, 0, 2, 1],
			[8, 7, 6, 5, 4, 3, 2, 1, 0, 1],
			[3, 2, 1, 0, 2, 1, 0, 3, 2, 1],
			[2, 1, 0, 1, 0, 4, 3, 2, 1, 0]
		]
	assert optimize_search_queries(nSearchEngines, search_engines, nQueries, queries) == 1

	# TC 4
	search_engines = ['Yeehaw', 'NSM', 'Dont Ask', 'B9', 'Googol']
	nSearchEngines = len(search_engines)
	queries = ['Googol', 'Dont Ask', 'NSM', 'NSM', 'Yeehaw', 'Yeehaw', 'Googol']
	nQueries = len(queries)
	coverage_matrix = [[0 for j in xrange(nQueries)] for i in xrange(nSearchEngines)]
	for i in xrange(nSearchEngines):
		fill_coverage(search_engines[i], nQueries, queries, coverage_matrix[i])
	assert coverage_matrix == [
			[4, 3, 2, 1, 0, 0, 1],
			[2, 1, 0, 0, 3, 2, 1],
			[1, 0, 5, 4, 3, 2, 1],
			[7, 6, 5, 4, 3, 2, 1],
			[0, 5, 4, 3, 2, 1, 0]
		]

	assert optimize_search_queries(nSearchEngines, search_engines, nQueries, queries) == 0



def basic_tests():
	test_fill_coverage()


if __name__ == '__main__':
	run_tests = True
	if run_tests:
		basic_tests()

	nTestcases = int(input())
	for i in xrange(nTestcases):
		nSearchEngines = int(input())
		search_engines = map (lambda x: raw_input().strip(), range(nSearchEngines))
		nQueries = int(input())
		queries = map (lambda x: raw_input().strip(), range(nQueries))
		print 'Case #{0}: {1}'.format(i+1, optimize_search_queries(nSearchEngines, search_engines, nQueries, queries))
	



