/**
https://www.interviewbit.com/problems/permutations/

Permutations

Given a collection of numbers, return all possible permutations.

Example:

	[1,2,3] will have the following permutations:

	[1,2,3]
	[1,3,2]
	[2,1,3] 
	[2,3,1] 
	[3,1,2] 
	[3,2,1]

NOTE
No two entries in the permutation sequence should be the same.
For the purpose of this problem, assume that all the numbers in the collection are unique.
*/


/**
Solution Outline:
	A simple permutation-generator starts with only the first element,
	  Then at level 2, Makes 2 copies, Inserts second element at indices [0,1]
	  At level 3, Makes 3 copies of the previous level permutations, Inserts third element at indices [0,1,2] for each copy

	e.g.,
	A: [1, 2, 3]
	l0: []
	l1: [1]
	l2: [1] [1] -> [1,2], [2,1]
	l3: [1,2], [2,1] * 3 -> [1,2], [1,2], [1,2], [2,1], [2,1], [2,1]
		-> [1,2,3], [1,3,2], [3,1,2], [2,1,3], [2,3,1], [3,2,1]

	For a backtracking algorithm, Do a DFS traversal, at each (level, i), Add A[level] at index i and backtrack.
	At level == length(A), add current permutation to results list.

  A: [x, y, z]

                                         f([], x, 0):
                    /                                               \
                 f([x], y, 0)                                     f([x], y, 1)
            /          |         \                           /          |        \
f([y,x], z, 0)  f([y,x], z, 1)  f([y,x], z, 2)   f([x,y], z, 0)  f([x,y], z, 1)  f([x,y], z, 2)
  \               \               \                \               \               \    
 [z,y,x]          [y,z,x]         [y,x,z]          [z,x,y]         [x,z,y]         [x,y,z]

*/
class Solution {
  def permutations(A: Array[Int]): Array[Array[Int]] = {
	var results = Array[Array[Int]]()

	// recursive permutations generator
	def permutations_(prefix: Array[Int], level: Int): Unit = {
	  if(prefix.length == A.length) {
		results :+= prefix
		return
	  }

	  for(i <- 0 to prefix.length) {
		permutations_(
		  prefix.slice(0,i) ++ Array(A(level)) ++ prefix.slice(i, prefix.length),
		  level+1
		)
		// backtrack to previous levels
      }
	}

	/** Compare two permutation arrays */
	def compare_permutations(one: Array[Int], two: Array[Int]): Boolean = {
	  for(i <- 0 until one.length) {
		if(one(i) != two(i))
		  return one(i) < two(i)
	  }
	  return false
	}

	permutations_(Array[Int](), 0)
	return results.sortWith(compare_permutations)
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
	val s = new Solution

    {
	  val expected = Array(Array(1,2), Array(2,1))
	  val perms = s.permutations(Array(1,2))
	  for(i <- 0 until expected.length) {
		assert( perms(i) sameElements expected(i) )
	  }
	}

    {
	  val expected = Array(Array(1))
	  val perms = s.permutations(Array(1))
	  for(i <- 0 until expected.length) {
		assert( perms(i) sameElements expected(i) )
	  }
	}

    {
	  val expected = Array(Array(1,2,3), Array(1,3,2), Array(2,1,3), Array(2,3,1), Array(3,1,2), Array(3,2,1))
	  val perms = s.permutations(Array(1,2,3))
	  for(i <- 0 until expected.length) {
		assert( perms(i) sameElements expected(i) )
	  }
	}

  }
}

