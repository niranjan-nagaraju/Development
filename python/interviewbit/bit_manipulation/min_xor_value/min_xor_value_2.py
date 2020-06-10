#encoding: utf-8
'''
https://www.interviewbit.com/problems/min-xor-value/

Min XOR value

Given an integer array A of N integers, find the pair of integers in the array which have minimum XOR value. Report the minimum XOR value.

Input Format:
    First and only argument of input contains an integer array A

Output Format:
    return a single integer denoting minimum xor value

Constraints:
2 <= N <= 100 000  
0 <= A[i] <= 1 000 000 000

For Examples :
Example Input 1:
    A = [0, 2, 5, 7]
Example Output 1:
    2
Explanation:
    0 xor 2 = 2
Example Input 2:
    A = [0, 4, 7, 9]
Example Output 2:
    3
'''

'''
Solution Outline: (O(nlogn) time):
	Sort the array, and return the minimum xor value of any consecutive pairs.
	
	Consider 3 numbers, a, b and c
	a <= b <= c
	a^c >= a^b, or b^c

	Proof:
	  Let i be the bit where a,b and c differ from each other.
	   i.e. the first (i-1) bits of a,b, and c are all the same.

	  Case 1:
	    a[i] == b[i]
		  => a[i] != c[i] and b[i] != c[i]
		  therefore, a[i]^c[i] == 1, and b[i]^c[i] == 1, and a[i]^b[i] == 0
		     => a^b has a leading zero
		  so,
		  a^b < b^c and a^c

	  Case 2:
		b[i] == c[i]
		 => a[i] != c[i] and a[i] != b[i]
		   therefore, b[i]^c[i] == 0, 
		     and a[i]^c[i] == 1, a[i]^b[i] == 1
			 => b^c has a leading zero
			so,
			  b^c < a^b and a^c

	  Case 3:
		a[i] == c[i]
		 => b[i] != c[i] and a[i] != b[i]
		 (this is impossible to achieve with a <= b <= c)
		 and therefore contradicts the base condition a<=b<=c.
		 for e.g,
		   prefix: 10
		     i = 2
		     a[i] = c[i] = 0
			 => b[i] = 1
			 but this would mean b[i] > c[i]
			   e.g, 
				 a: 8   (1000)
				 b: 10  (1010)
				 c: 9   (1001)
			OR
			 a[i] = c[i] = 1
			 then b[i] = 0
			 but this would mean a[i] > b[i]
			   e.g,
				 a: 10  (1010)
				 b: 8   (1000)
				 c: 13  (1011)
			  
	Therefore, a^c >= a^b, or b^c will always hold
	
	e.g.,
	 a = 7, b = 10, c=11
	 a^b = 7 ^ 10
	 0111
	 1010
	 ----
	 1101
	 ----
	 = 13

	 b^c = 10^11
	 1010
	 1011
	 ----
	 0001
	 ----
	 = 1

	 a^c = 7^11
	 0111
	 1011
	 ----
	 1100
	 ----
	 = 10

	 b^c > a^c in this case
'''
class Solution:
	def find_min_xor_pair(self, A):
		A.sort()
		min_xor_value = A[0]^A[1]
		for i in xrange(1, len(A)-1):
			if min_xor_value > (A[i]^A[i+1]):
				min_xor_value = A[i]^A[i+1]

		return min_xor_value



if __name__ == '__main__':
	s = Solution()
	assert s.find_min_xor_pair([0,2,5,7]) == 2
	assert s.find_min_xor_pair([0,4,7,9]) == 3
	assert s.find_min_xor_pair([5,10,25,20,15,26]) == 3
	assert s.find_min_xor_pair([5,10,25,20,15]) == 5
	assert s.find_min_xor_pair([ 3, 2, 13, 1, 5, 13, 0, 13, 13 ]) == 0

