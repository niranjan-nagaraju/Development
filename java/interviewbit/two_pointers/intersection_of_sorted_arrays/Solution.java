
/*
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

/*
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Solution {
	// DO NOT MODIFY THE LIST. IT IS READ ONLY
	public static ArrayList<Integer> intersect(final List<Integer> A, final List<Integer> B) {
		int i, j;
		ArrayList<Integer> intersects = new ArrayList<>();

		i = j = 0;
		while (i<A.size() && j<B.size()) {
			if (A.get(i) < B.get(j))
				i++;
			else if (A.get(i) > B.get(j))
				j++;
			else { /* A[i] == B[j] */
				intersects.add(A.get(i));
				i++; j++;
			}
		}

		return intersects;
	}

	public static void main(String[] args) {
		List<Integer> A = Arrays.asList(1,3,3,4,5,6);
		List<Integer> B = Arrays.asList(2,3,5);
		assert Solution.intersect(A,B).equals(Arrays.asList(3,5));
		
		/* reversing A and B should yield the same output */
		assert Solution.intersect(
			Arrays.asList(2,3,5),
			Arrays.asList(1,3,3,4,5,6)).equals(Arrays.asList(3,5));

		assert Solution.intersect(
			Arrays.asList(1,2,3,3,4,5,6),
			Arrays.asList(3,3,5)).equals(Arrays.asList(3,3,5));

		assert Solution.intersect(
			Arrays.asList(1,2,3,3,4,5,6),
			Arrays.asList(3,5)).equals(Arrays.asList(3,5));
	}
}
