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
Solution Outline:
	1. Sort the arrivals and departures by 'Arrival' dates
	2. Use 2 pointers i, j at arrivals and departures
	   Start at i = arrivals[0], (departures[0] correponds to departure date for arrivals[0])
	   j = departures[0]
	   Number of rooms needed initially = 1 (for arrivals[0])
	   Check arrivals[i] vs departures[j]
	    Pick whichever is lesser
		  if arrivals is picked, increment rooms += 1 --> check if rooms > k to see if this is viable or not
		  if departures is picked, decrement rooms


Sample run:
	A: [1, 3, 5]
	D: [2, 6, 8]

	i = 1, j = 0, rooms = 1
	arrivals[i] = 3 > departures[0] => rooms -- == 0
	j = 1

	arrivals[i] = 3 < departures[j] => rooms ++ == 1
	i =2

	arrivals[i] = 5 < departures[j] => rooms ++ == 2 (> k)
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
