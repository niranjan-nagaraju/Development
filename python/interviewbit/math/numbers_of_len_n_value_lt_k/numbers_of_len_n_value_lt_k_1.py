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
Solution Outline: Brute force O(N^B) {N: number of digits, B: length of digits needed}
  Enumerate all numbers with len(digits) : 1, 2, ..., n
    At each length, s, {1<=s<=n}, Exclude A[i], .. A[k-1] if A[i] > C[s]
    Given all valid numbers of length s, digits of length (s+1) can be obtained by appending A[i] {0<=i<len(A)}
	 as long as {digits(s)+A[i]} <= C[0:s+1]

   At length, s = B, include A[i] to the end only if {digits(s-1)+A[i]} < C[0:B-1]


if B == 0 => no solution possible
B = 1 => single digit, return count of numbers in A, < C
B > len(C) => no solution possible = 0

B < len(C)
  N: len(A)
    If A has 0 => 1st digit cannot be 0 => (N-1) ** B
    A doesnt have 0 => N ** B 

B = len(C):
	Enumerate all numbers of length 1, 2, ..., B
	Start with length 1: Filter all A[i] <= C[0]
	length 2: Append all A[i] to each of previous levels as long as p+A[i] <= C[0,1] into a set (so duplicates are removed)
	length 3: Append all A[i] to each of previous levels as long as p+A[i] <= C[0,1,2]
	. . .
	length == B, Append all A[i] to each of previous levels as long as p+A[i] < C (into a set)
	  return size of the set at length = B

  For e.g.,
    A: 0 1 2 3 5
    B: 3
    C: 253

    At s = 1,
      Candidates: {0, 1, 2} [can't have the starting digit greater than C[0]}  
    At s = 2,
      Candidates: {00, 10, 20, 01, 11, 21, 20, 21, 22, 03, 13, 23, 05, 15, 25}  {Include A[i] if prev_level+A[i] <= C[0,1]}
                : {10, 11, 13, 15, 20, 21, 22, 23, 25}
	At s = 3, (s == B-1, include A[i] as long as prev_level + A[i] < C[0:B-1]), for e,g [2,5]+[3] == C[0:2] == [2,5,3] => dont include
      Candidates: {100, 110, 130, 150, 200, 210, 220, 230, 250,
                   101, 111, 131, 151, 201, 211, 221, 231, 251,
                   102, 112, 132, 152, 202, 212, 222, 232, 252,
                   103, 113, 133, 153, 203, 213, 223, 233,
                   105, 115, 135, 155, 205, 215, 225, 235}

'''
class Solution:
	def enumerate_numbers(self, A, B, C):
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


		# Combine a list of digits into a number
		# [1,2,3] => 123
		def fromList(nlist):
			n = 0
			for d in nlist:
				n = n*10+d 
			return n

		# Recursively enumerate digits at each length s, s in {1, 2, .. B}
		# and count them
		def enumerate_digits(s, prev_level_digits):
			if s == B:
				return len(prev_level_digits)

			curr_level = set()
			for num in prev_level_digits:
				for x in A:
					if s == B-1:
						if fromList(num+(x,)) >= fromList(C[:s+1]):
							break
					else:
						if fromList(num+(x,)) > fromList(C[:s+1]):
							break

					curr_level.add(num+(x,))

			return enumerate_digits(s+1, curr_level)


		if B == 0:
			return 0

		if B == 1:
			count = 0
			for x in A:
				if x >= C:
					break
				count += 1
			return count

		C = toList(C)
		if B > len(C):
			return 0
		elif B < len(C):
			if A[0] == 0:
				return (len(A)-1) * (len(A))**(B-1)
			else:
				return len(A) ** B

		return enumerate_digits(1, set([(i,) for i in A if i <= C[0] and i>0]))


if __name__ == '__main__':
	s = Solution()
	assert s.enumerate_numbers([0,1,2,3,5], 1, 2) == 2
	assert s.enumerate_numbers([0,1,2,3,5], 1, 253) == 5
	assert s.enumerate_numbers([0,1,2,3,5], 3, 253) == 48
	assert s.enumerate_numbers([0,1,2,5], 2, 21) == 5
	assert s.enumerate_numbers([0,1,2,5,6,7], 2, 253) == 30
	assert s.enumerate_numbers([1,2,5,6,7,8], 2, 253) == 36
	assert s.enumerate_numbers([0,1,2,5,6,7], 3, 253) == 57
	assert s.enumerate_numbers([0,1,2,3,4,5,6], 4, 3524) == 949

