#encoding: utf-8
'''
https://www.interviewbit.com/problems/kth-row-of-pascals-triangle/

Kth Row of Pascal's Triangle

Given an index k, return the kth row of the Pascal’s triangle.

Pascal’s triangle : To generate A[C] in row R, sum up A’[C] and A’[C-1] from previous row R - 1.

Example:
	Input : k = 3
	Return : [1,3,3,1]
	
NOTE: k is 0 based. k = 0, corresponds to the row [1]. 
Note: Could you optimize your algorithm to use only O(k) extra space?
'''

'''
Solution Outline:
    pascal's triangle row n = nc0, nc1, nc2, ..., ncn
    row 3 = 3C0, 3C1, 3C2, 3C3 = 1, 3, 3, 1

(n,k) = n!/k! * (n-k)!
(n, k+1) = n!/(k+1)! * (n-k-1)!

n,k+1/ n,k  = 1/(k+1)! * (n-k)! / (1/k! * (n-k)!)
        = k! * (n-k)! / ((k+1)! * (n-k-1))!
        = k! / (k+1)!  * (n-k)! / (n-k-1)!
	= (n-k)/(k+1)

(n, k+1) = (n, k) * (n-k)/(k+1)

n = 6
k = 1 to n/2
1, 6, . . , 6, 1
a[1] and a[n-2] are
  nc2 = n-1/2 * n == 5*6/2 = 15
1, 6, 15, . . ., 15, 6, 1
  nc3 = 15 * n-2/3 = 15 * 4/3 = 20
1, 6, 15, 20, 15, 6, 1


n = 8
row = 1, 8, _ _ _ _ _, 8, 1

row[2] and row[6] are
(8-1)/2 * 8 = 7/2*8 = 7*4 = 28
row = 1, 8, 28, _ _ _, 28, 8, 1

row[3] and row[n-3] == row[5] are
(8-2)/3 * 28 == 6/3*28 = 2*28 == 56
row = 1, 8, 28, 56, _ 56, 28, 8, 1

row[4] and row[n-4] == row[4] is
(8-3)/4 * 56 == 5/4* 56 == 5*14 == 70
row = 1, 8, 28, 56, 70, 56, 28, 8, 1
'''
class Solution:
	# @param k : integer
	# @return a list of integers
	def getRow(self, k):
		# row 'k' has k+1 numbers
		row = [None]*(k+1)
		row[0] = row[-1] = 1

		# Every row's left and right sides are symmetric
		next_value = 1
		for i in xrange(k/2):
			next_value = next_value * (k-i)/(i+1)
			# next_value = row[i] * (k-i)/(i+1) -- use next_value instead
			row[i+1] = next_value
			row[k-i-1] = next_value

		return row


if __name__ == '__main__':
	s = Solution()
	assert s.getRow(0) == [1]
	assert s.getRow(1) == [1,1]
	assert s.getRow(2) == [1,2,1]
	assert s.getRow(3) == [1,3,3,1]
	assert s.getRow(4) == [1,4,6,4,1]
	assert s.getRow(5) == [1,5,10,10,5,1]
	assert s.getRow(6) == [1,6,15,20,15,6,1]
	assert s.getRow(7) == [1,7,21,35,35,21,7,1]
	assert s.getRow(8) == [1,8,28,56,70,56,28,8,1]



