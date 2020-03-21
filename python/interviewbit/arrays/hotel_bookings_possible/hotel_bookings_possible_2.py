'''
https://www.interviewbit.com/problems/hotel-bookings-possible/

Hotel Bookings Possible

A hotel manager has to process N advance bookings of rooms for the next season. His hotel has K rooms. Bookings contain an arrival date and a departure date. He wants to find out whether there are enough rooms in the hotel to satisfy the demand. Write a program that solves this problem in time O(N log N) .

Input:


First list for arrival time of booking.
Second list for departure time of booking.
Third is K which denotes count of rooms.

Output:

A boolean which tells whether its possible to make a booking. 
Return 0/1 for C programs.
O -> No there are not enough rooms for N booking.
1 -> Yes there are enough rooms for N booking.
Example :

Input : 
        Arrivals :   [1 3 5]
        Departures : [2 6 8]
        K : 1

Return : False / 0 

At day = 5, there are 2 guests in the hotel. But I have only one room. 
'''

'''
Solution Outline: O(nlogn), memory: O(1)
	1. Sort the arrivals and departures dates separately
	2. 'Merge' the two sorted arrivals and departures while incrementing rooms required for each arrival picked
	    and decrementing for each departure picked during the merge.


Sample run:
	A: [1, 3, 5]
	D: [2, 8, 6]

	A': [1,3,5]
	D': [2,6,8]

	rooms = 0

	i: 0, j: 0

	A'[i] = 1 < D'[j] = 2
	i += 1 = 1
	rooms = 1

	A'[i] = 3 > D'[j] = 2
	j += 1 = 1
	rooms = 0

	A'[i] = 3 < D'[j] = 6
	i += 1 = 2
	rooms = 1

	A'[i] = 5 < D'[j] = 6
	i += 1 = 3
	rooms = 2


	i == 3
	j : 1

	End: 
	pick j
	rooms = 1
	j += 1 = 2

	pick j
	rooms = 0
	j += 1 = 3
'''

class Solution:
	def hotel_bookings_possible(self, A, D, k):
		A.sort()
		D.sort()

		rooms_needed = 0
		i, j = 0,0
		while i < len(A) and j < len(D):
			if A[i] < D[j]:
				i += 1
				rooms_needed += 1
			else:
				j += 1
				rooms_needed -= 1

			if rooms_needed > k:
				return False

		
		# Run a check if arrivals[] is not fully exhausted
		while i < len(A):
			rooms_needed += 1
			i += 1
			if rooms_needed > k:
				return False

		# NOTE: If only the departures remain un-processed
		# rooms required will only go down
		# and can be skipped

		return True



if __name__ == '__main__':
	s = Solution()
	assert s.hotel_bookings_possible([1,3,5], [2,6,8], 1) == False
	assert s.hotel_bookings_possible([1,3,5], [2,8,6], 2) == True
	assert s.hotel_bookings_possible([1,2,5], [3,8,6], 2) == True
	assert s.hotel_bookings_possible([1,2,5], [6,8,6], 2) == False
	assert s.hotel_bookings_possible([1,2,3], [2,3,4], 1) == True

