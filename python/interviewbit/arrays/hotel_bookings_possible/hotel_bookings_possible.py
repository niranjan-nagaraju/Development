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
Solution Outline: Time: O(nlogn), memory: O(n)
	1. Sort the arrivals and departures dates while marking arrivals as +1, departures by -1
	2. For each date, increment rooms by +1 if the date is an arrival, decrement by 1 if the date is a depature.


Sample run:
	A: [1, 3, 5]
	D: [2, 6, 8]

	ad: [(1,A), (2,D), (3,A), (5,A). (6,D), (8,D)]

	rooms = 0

	i: 0
	A => rooms = 1

	i: 1
	D => rooms = 0

	i: 2
	A => rooms = 1

	i: 3
	A => rooms = 2 (> k return False)

	i: 4
	D => rooms = 1

	i: 5
	D => rooms = 0
'''

class Solution:
	def hotel_bookings_possible(self, A, D, k):
		ad = [(a, 1) for a in A] + [(d,-1) for d in D]

		# sort by arrival/departure dates
		# if arrival[i] == dearture[j], i != j
		# place departure[j] before arrival
		ad = sorted(ad, cmp=lambda (w,x),(y,z): cmp(w,y) if w != y else cmp(x,z))

		rooms_needed = 0
		for e in ad:
			# NOTE: e[0] (arrival/departure dates) itself is no longer needed
			# sorted 'ad' could only contain +1/-1 for arrivals/departures

			# Increment rooms for each arrival/Decrement for each departure
			rooms_needed += e[1]

			if rooms_needed > k:
				return False

		return True



if __name__ == '__main__':
	s = Solution()
	assert s.hotel_bookings_possible([1,3,5], [2,6,8], 1) == False
	assert s.hotel_bookings_possible([1,3,5], [2,8,6], 2) == True
	assert s.hotel_bookings_possible([1,2,5], [3,8,6], 2) == True
	assert s.hotel_bookings_possible([1,2,5], [6,8,6], 2) == False
	assert s.hotel_bookings_possible([1,2,3], [2,3,4], 1) == True

