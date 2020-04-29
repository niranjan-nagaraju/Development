#encoding: utf-8
'''
https://www.interviewbit.com/problems/allocate-books/

Allocate Books

Given an array of integers A of size N and an integer B.

College library has N bags,the ith book has A[i] number of pages.

You have to allocate books to B number of students so that maximum number of pages alloted to a student is minimum.

A book will be allocated to exactly one student.
Each student has to be allocated at least one book.
Allotment should be in contiguous order, for example: A student cannot be allocated book 1 and book 3, skipping book 2.
Calculate and return that minimum possible number.

NOTE: Return -1 if a valid assignment is not possible.



Input Format
The first argument given is the integer array A.
The second argument given is the integer B.

Output Format
Return that minimum possible number

Constraints
1 <= N <= 10^5
1 <= A[i] <= 10^5

For Example

Input 1:
    A = [12, 34, 67, 90]
    B = 2
Output 1:
    113
Explanation 1:
    There are 2 number of students. Books can be distributed in following fashion : 
        1) [12] and [34, 67, 90]
        Max number of pages is allocated to student 2 with 34 + 67 + 90 = 191 pages
        2) [12, 34] and [67, 90]
        Max number of pages is allocated to student 2 with 67 + 90 = 157 pages 
        3) [12, 34, 67] and [90]
        Max number of pages is allocated to student 1 with 12 + 34 + 67 = 113 pages

        Of the 3 cases, Option 3 has the minimum pages = 113.

Input 2:
    A = [5, 17, 100, 11]
    B = 4
Output 2:
    100
'''

'''
Solution Outline:
	Reference: https://www.topcoder.com/community/competitive-programming/tutorials/binary-search/

	If max is the maximum number of pages allocated to a student
	  lowest max: max(A), [Since each student has to be allocated a book]
	  Highest max: sum(A) (ΣA[i])

	With the lowest max, the allocation might not be optimal -> might need more students than are available (especially if B < len(A))
	With the highest max, all books are allocated to a single student

	The solution lies somewhere in between {lowest max, highest max}
	The first allocation, b, between {lowest_max, highest_max} s.t, each student is allocated atleast a book, and all books are allocated is the
	optimal allocation.
	Move to the right for lesser students (num students for books > B)
	Move to the left for increasing students count (num students for books <= B)

	NOTE:
	if B > len(A), while it might be possible to get an optimal allocation,
		every student can not be allocated a book and therefore there wont be a solution.

Sample run 1:
	A: [12, 34, 67, 90]
	B: 2

	l = 90, h = sum(A) = 203

	mid = (90+203)/2 = 146
	Num students needed for 146 (with max allocation 146 to each student) is
	  2 == B
	  12+34+67 == 113 for student 1
	  student == 90
	  Look to the left for B == 2
	  h = mid = 146
	
	l = 90, h = 146
	mid = (90+146)/2 = 118
	Num students needed for 118 (with max allocation 118 to each student) is
	  2 == B
	  student 1: 12+34+67 == 113
	  student 2: 90
	  Look to the left for a better allocation with B == 2
	  h = mid == 118

	l = 90, h = 118
	mid = 104
	Num students needed for 104 (with max allocation 104 to each student) is
	 Student 1: 12+34 == 46
	 Student 2: 67
	 Student 3: 90
	 == 3 > B
	  Look to the right
	  l = mid+1 == 105

	l = 105, h == 118
	mid = 111
	Num students needed for 111 (with max allocation 111 to each student) is
	 Student 1: 12+34 == 46
	 Student 2: 67
	 Student 3: 90
	 == 3 > B
	  Look to the right
	  l = mid+1 == 112

	l = 112, h == 118
	mid = 115
	Num students needed for 115 (with max allocation 115 to each student) is
	 Student 1: 12+34+67 == 113
	 Student 2: 90
	 == 2 == B
	  Look to the left for a better allocation with B == 2
	  h = mid = 115

	l = 112, h == 115
	mid = 113
	Num students needed for 113 (with max allocation 113 to each student) is
	 Student 1: 12+34+67 == 113
	 Student 2: 90
	 == 2 == B
	  Look to the left for a better allocation with B == 2
	  h = mid = 113

	l = 112, h == 113
	mid = 112
	Num students needed for 112 (with max allocation 112 to each student) is
	 Student 1: 12+34 == 46
	 Student 2: 67
	 Student 3: 90
	 == 3 > B
	  Look to the left for a better allocation with B == 2
	  l = mid+1 == 113

	l = h = 113
	return 113

Sample run 2:
	A: [5, 17, 100, 11]
	B: 4
	NOTE: Without the need for every student being allocated a book, the optimal allocation here would be B=3 [5,17], [100], [11], with minimum max being 100, but can also work with B=4
	So unless B <= len(A), there's a solution
	if B > len(A), while it might be possible to get an optimal allocation, every student will not be allocated a book and therefore there wont be a solution.

	l = 100, h=133
	mid = 116
	Num students needed for 116 (with max allocation 116 to each student) is
	  Student 1: 5+17 = 22
	  Student 2: 100
	  Student 3: 11
	  = 3 < B
	Move to the left to increase students
	h = mid-1 == 115

	l = 100, h = 115
	mid = 107
	Num students needed for 107 (with max allocation 107 to each student) is
	  Student 1: 5+17 = 22
	  Student 2: 100
	  Student 3: 11
	  = 3 < B
	Move to the left to increase students
	h = mid == 107

	l = 100, h = 107
	mid = 103
	Num students needed for 103 (with max allocation 103 to each student) is
	  Student 1: 5+17 = 22
	  Student 2: 100
	  Student 3: 11
	  = 3 < B
	Move to the left to increase students
	h = mid == 103

	l = 100, h = 103
	mid = 101
	Num students needed for 101 (with max allocation 101 to each student) is
	  Student 1: 5+17 = 22
	  Student 2: 100
	  Student 3: 11
	  = 3 < B
	Move to the left to increase students
	h = mid == 101

	l = 100, h = 101
	mid = 100
	Num students needed for 100 (with max allocation 100 to each student) is
	  Student 1: 5+17 = 22
	  Student 2: 100
	  Student 3: 11
	  = 3 < B
	Move to the left to increase students
	h = mid == 100

	l == h
	return 100
	# NOTE: Even though the final allocation was for for 3 (< B)
	# there are enough books to be passed around for B=4, and the maximum doesn't change (!!!!)


Sample run 3:
	A: [5,7,17,18,11]
	B: 3

	18, 58
	mid: (18+58)/2 == 38
	 allocation: {5+7+17==29, 18+11==29} [ == 2]
	 Look to left
	 h = 38

	18, 38
	mid: (18+38)/2 == 28
	 allocation: {5+7==12, 17, 18, 11} [==4>B]
	 Look to the right for fewer students
	 l = mid+1 == 29

	29, 38
	mid: (29+38)/2 == 33
	 allocation: {5+7+17==29, 18+11==29} [==2<B]
	 look to left
	 h = 33

	29, 33
	mid: (29+33)/2 == 31
	 allocation: {5+7+17==29, 18+11==29} [==2<B]
	 look to left
	 h = 31

	29, 31
	mid: 30
	 allocation: {5+7+17==29, 18+11==29} [==2<B]
	 look to left
	 h = 30

	29, 30
	mid: 29
	 allocation: {5+7+17==29, 18+11==29} [==2<B]
	 look to left
	 h = 29

	l == h == 29
	return 29
'''
class Solution:
	def allocate_books(self, A, B):
		# Find number of students
		# needed to allocate 'max_pages'
		# across books
		def find_allocation(max_pages):
			students = 1
			current_allocation=0
			for p in A:
				if p+current_allocation <= max_pages:
					current_allocation += p
				else:
					students += 1
					current_allocation = p

			return students


		if B > len(A):
			return -1

		l = max(A)

		# If number of students == number of books
		# Allocate one book to each student
		# Minimum maximum pages == max(A)
		if B == len(A):
			return l

		h = sum(A)
		while l < h:
			mid = (l+h)/2
			num_students = find_allocation(mid)
			if num_students <= B:
				# Look to the left for a better allocation
				# if == B
				# if < B, look to the left to increase
				# number of students who are allocated books
				h = mid
			else:
				# num_students > B
				# Look to the right to reduce
				# number of students who are allocated books
				l = mid+1

		return l


if __name__ == '__main__':
	s = Solution()
	assert s.allocate_books([12, 34, 67, 90], 2) == 113
	assert s.allocate_books([5, 17, 100, 11], 4) == 100
	assert s.allocate_books([5, 7, 17, 18, 11], 3) == 29

