/**
https://www.interviewbit.com/problems/intersection-of-sorted-arrays/

Intersection Of Sorted Arrays

Find the intersection of two sorted arrays.
OR in other words,
Given 2 sorted arrays, find all the elements which occur in both the arrays.

Example :

Input : 
    A : [1 2 3 3 4 5 6]
    B : [3 3 5]

Output : [3 3 5]

Input : 
    A : [1 2 3 3 4 5 6]
    B : [3 5]

Output : [3 5]
*/

/**
Solution Outline:
  1. Proceed as with merging two sorted arrays
     1.1 Initialize two pointers, i and j to begin at A and B respectively
     1.2 If A[i] < B[j]:
           Move i ahead
         If A[i] > B[j]:
           Move j ahead
         If A[i] == B[j]:
           Copy A[i] to output
           Move ahead both i,j
  2. When either of A or B runs out, we are done finding the intersection of A and B, return the collected output.


Sample run:
  A: [1, 3, 3, 4, 5, 6]
  B: [2, 3, 5]

  i, j = 0
  A[i] = 1
  B[j] = 2
  A[i] < B[j] => i++
    i = 1

  A[i] = A[1] = 3
  B[j] = 2
  A[i] > B[j] => j++
    j = 1
  
  A[i] = A[1] = 3
  B[j] = B[1] = 3
  A[i] = B[j]
    Output: [3]
    i++, j++
    i = 2, j = 2

  A[i] = A[2] = 3
  B[j] = B[2] = 5
  A[i] < B[j] => i++
    i = 3

  A[i] = A[3] = 4
  B[j] = B[2] = 5
  A[i] < B[j] => i++
    i = 4

  A[i] = A[4] = 5
  B[j] = B[2] = 5
  A[i] = B[j]
    Output: [3, 5]
    i++, j++
    i = 5, j = 3

  A[i] = A[5] = 5
  B[j] = (j = len(B)): exit

  Output: [3,5]
*/
#include <assert.h>
#include <vector>
using namespace std;

class Solution
{
	public:
		vector<int> find_intersection(const vector<int> &A, const vector<int> &B);
};

vector<int>
Solution::find_intersection(const vector<int> &A, const vector<int> &B)
{
	int i, j;
	vector<int> intersects;

	i = j = 0;
	while (i<A.size() && j<B.size()) {
		if (A[i] < B[j])
			i++;
		else if (A[i] > B[j])
			j++;
		else { /* A[i] == B[j] */
			intersects.push_back(A[i]);
			i++; j++;
		}
	}

	return intersects;
}


int
main(void)
{
	Solution s;
	{
		vector<int> A = {1,3,3,4,5,6};
		vector<int> B = {2,3,5};
		vector<int> expected_AB = {3,5};

		vector<int> AB = s.find_intersection(A, B);
		assert(AB == expected_AB);
	}
	{
		// reversing A and B should yield the same output
		vector<int> A = {2,3,5};
		vector<int> B = {1,3,3,4,5,6};
		vector<int> expected_AB = {3,5};

		vector<int> AB = s.find_intersection(A, B);
		assert(AB == expected_AB);
	}
	{
		vector<int> A = {1,2,3,3,4,5,6};
		vector<int> B = {3,3,5};
		vector<int> expected_AB = {3,3,5};

		vector<int> AB = s.find_intersection(A, B);
		assert(AB == expected_AB);
	}
	{
		vector<int> A = {1,2,3,3,4,5,6};
		vector<int> B = {3,5};
		vector<int> expected_AB = {3,5};

		vector<int> AB = s.find_intersection(A, B);
		assert(AB == expected_AB);
	}

	return 0;
}
