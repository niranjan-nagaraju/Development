#encoding: utf-8
'''
https://www.interviewbit.com/problems/numbers-of-length-n-and-value-less-than-k/

Numbers of length N and value less than K

Given a set of digits (A) in sorted order, find how many numbers of length B are possible whose value is less than number C.
 NOTE: All numbers can only have digits from the given set. 

Examples:
	Input:
	  0 1 5  
	  1  
	  2  
	Output:  
	  2 (0 and 1 are possible)  

	Input:
	  0 1 2 5  
	  2  
	  21  
	Output:
	  5 (10, 11, 12, 15, 20 are possible)

Constraints:
    1 <= B <= 9, 0 <= C <= 1e9 & 0 <= A[i] <= 9 
'''

'''
Solution Outline: Dynamic Programming
  Let C: k₁k₂k₃..kₓ be the target
      and length(A) == d
  If B == 0: Then there are no solutions
  If B == 1 => single digit, return count of numbers in A, < C
  If B < x: There are no solutions
  If B > x: There are dˣ solutions if A doesn't contain a 0, or (d-1)*(dˣ⁻¹) solutions if A does contain a 0
     (There wont be leading zeroes in the solution, so 0354 is the same as 354, and needn't be counted separately)
   
  If B == x,
    Then Solve for number of solutions < C using DP 
	C: k₁k₂k₃..kₓ
	Assuming (k₁k₂k₃..kₓ₋₁, B-1) is solved and has 's' solutions,
	  Then (k₁k₂k₃..kₓ, B) can be solved as below -
	     if kₓ is present in A, then the number of solutions 
		    = (s-1) * d + 1 * (number of elements < kₓ)
			otherwise,
			 = s * d

		Solution(k₁, 1) == Number of elements in A < k₁

		At any C[0..i], if C[i] doesnt exist in A, then,
		  from then on, all solutions below can have
		  DP[i+1] = DP[i]*d numbers

For e.g,
   A: [0, 1, 2, 3, 4, 6]
   B: 4
   C: 3542

   d = len(A) = 6
   DP[0] = 0

   Length 1:
     C0 = 3, 3 is in A
	 DP[1] = DP[0]*3 + lower[3]-1 (0 cannot be the first digit)
	 = 0 + 3-1 = 2

   Length 2:
     C[0,1] = 35
	 Number of solutions:
		5 is not in A, => set flag (from hereon, all DP[i] = DP[i-1]*d)
		  => DP[1]*d + lower[5]
		  == 2*6 + 5 = 12+5 = 17
		DP[2] = 17

	Length 3:
	  C[0:2] = 354
	  DP[3] =
	     flag == 1
		  => DP[2] * d
		   = 17*6 = 102}
		DP[3] = 102

	Length 4:
	  C[0:3] = 3542
	  DP[4] =
		  flag == 1
		  => DP[3]*d
		  = 102*6 +
		  = 612
'''
class Solution:
	def count_numbers(self, A, B, C):
		# Split a number n into its digits
		# 123 -> [1,2,3]
		def toList(n):
			if n == 0:
				return [0]

			nlist = []
			while n:
				nlist.insert(0, n%10)
				n /= 10

			return nlist


		if not A:
			return 0

		if B == 0:
			return 0

		if B == 1:
			ret = reduce(lambda counter,x: counter+1 if x < C else counter, A, 0)
			return ret

		C = toList(C)
		if B > len(C):
			return 0

		d = len(A)
		if B < len(C):
			if A[0] == 0:
				return (d-1) * (d**(B-1))
			else:
				return d ** B


		# B == len(C)
		DP = [0]* (B+1)

		lookup_tbl = set()
		for x in A:
			lookup_tbl.add(x)


		# Calculate lower[i] for each i: 0-9
		# lower[i]: number of elements lower than i in A
		# A: [1,2,4]
		# lower[0] = 0, lower[1]=0, lower[2]=1, lower[3]=2, lower[4]=2, lower[5]=3
		lower = [0] * 11
		for i in xrange(d):
			lower[A[i]+1] = 1

		for i in xrange(1, 11):
			lower[i] += lower[i-1]

		# Do not include 0 as first number
		DP[1] = lower[C[0]] if A[0] > 0 else lower[C[0]]-1
		element_present = C[0] in lookup_tbl
		for i in xrange(2, B+1):
			DP[i] += DP[i-1]*d

			# All items in C[0:i] are in A
			if element_present:
				DP[i] += lower[C[i-1]]

			element_present = element_present and C[i-1] in lookup_tbl

		return DP[B]



if __name__ == '__main__':
	s = Solution()
	assert s.count_numbers([2,9], 5, 17015) == 0
	assert s.count_numbers([0,1,2,3,5], 1, 2) == 2
	assert s.count_numbers([0,1,2,3,5], 1, 253) == 5
	assert s.count_numbers([0,1,2,3,5], 3, 253) == 48
	assert s.count_numbers([0,1,2,5], 2, 21) == 5
	assert s.count_numbers([0,1,2,5,6,7], 2, 253) == 30
	assert s.count_numbers([1,2,5,6,7,8], 2, 253) == 36
	assert s.count_numbers([0,1,2,5,6,7], 3, 253) == 57
	assert s.count_numbers([0,1,2,3,4,5,6], 4, 3524) == 949
	assert s.count_numbers([0,1,2,3,4,6], 4, 3524) == 612


