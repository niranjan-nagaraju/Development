'''
There are N+1 parking spots, numbered from 0 to N. 
There are N cars numbered from 1 to N parked in various parking spots with one left empty. 
Reorder the cars so that car #1 is in spot #1, car #2 is in spot #2 and so on. 
Spot #0 will remain empty. The only allowed operation is to take a car and move it to the free spot.
'''


'''
Linear solution: (Needs atmost 2*n car moves)
	Have empty slots move from right to left until slot 0
	Start at rightmost slot, move the car{i} there to the current empty slot.
	This leaves slot{n} as the empty slot. Move car{n} to slot{n}.
	Repeat with {n-1}, ... 0
	At slot 1, we are done
'''

import parking_slots_common	as c


def move_cars(slots, cars, empty_slot):
	global total_moves

	i = n
	while i != 0:
		if slots[i] == i:
			# Nothing to do, the car is already in its rightful slot
			print i, 'Already ordered!'
			i = i - 1
			continue

		# Move i'th rightmost car to empty slot, 
		# unless the empty slot is rightmost i'th slot itself
		if empty_slot != i:
			slots[empty_slot] = slots[i]
			slots[i] = 0
			total_moves += 1
			c.update_cars_at_slots(cars, slots, empty_slot)

		print i, slots, # First move to ensure slot{i} is empty

		# Now find where car{i} is and move it the empty slot, {i}
		car_at_slot = cars[i]
		slots[i] = i # Move it to the right slot
		slots[car_at_slot] = 0 # Remove from slot where car{i} is right now
		empty_slot = car_at_slot # slot where car{i} was found is now empty
		total_moves += 1

		# This is strictly not needed, as when a car has been moved to its
		# rightful position, we wont ever need to query it again
		# However, for the sake of completeness /AND/ the fact that the reverse lookup
		# looks confusing while debugging (plus it's an O(1) operation anyway), 
		# Update that car{i} has been moved to slot{i}
		c.update_cars_at_slots(cars, slots, i) 
		print '->', slots  # second move where car{i} is moved to slot{i}

		i = i - 1



n, slots, empty_slot =  c.get_input_slots()
cars = c.create_reverse_lookup(slots, n)
print slots

total_moves = 0
move_cars(slots, cars, empty_slot)
print 'Total moves:', total_moves


assert(slots == range(n+1))

'''
TC: 1
[17:35:38 parking-slot-problem]$ python parking-slots-linear.py 
5
5 1 2 3 x 4
[5, 1, 2, 3, 0, 4]
5 [5, 1, 2, 3, 4, 0] -> [0, 1, 2, 3, 4, 5]
4 Already ordered!
3 Already ordered!
2 Already ordered!
1 Already ordered!
Total moves: 2

TC: 2
[17:39:37 parking-slot-problem]$ python parking-slots-linear.py 
5
2 x 3 5 1 4
[2, 0, 3, 5, 1, 4]
5 [2, 4, 3, 5, 1, 0] -> [2, 4, 3, 0, 1, 5]
4 [2, 4, 3, 1, 0, 5] -> [2, 0, 3, 1, 4, 5]
3 [2, 1, 3, 0, 4, 5] -> [2, 1, 0, 3, 4, 5]
2 [2, 1, 0, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
1 Already ordered!
Total moves: 7

TC: 3
[17:40:40 parking-slot-problem]$ python parking-slots-linear.py 
5
5 4 x 3 2 1
[5, 4, 0, 3, 2, 1]
5 [5, 4, 1, 3, 2, 0] -> [0, 4, 1, 3, 2, 5]
4 [2, 4, 1, 3, 0, 5] -> [2, 0, 1, 3, 4, 5]
3 Already ordered!
2 [2, 1, 0, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
1 Already ordered!
Total moves: 6

TC: 4
[17:41:06 parking-slot-problem]$ python parking-slots-linear.py 
5
3 2 x 1 5 4
[3, 2, 0, 1, 5, 4]
5 [3, 2, 4, 1, 5, 0] -> [3, 2, 4, 1, 0, 5]
4 [3, 2, 4, 1, 0, 5] -> [3, 2, 0, 1, 4, 5]
3 [3, 2, 1, 0, 4, 5] -> [0, 2, 1, 3, 4, 5]
2 [1, 2, 0, 3, 4, 5] -> [1, 0, 2, 3, 4, 5]
1 [1, 0, 2, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
Total moves: 8

TC: 5
[17:45:51 parking-slot-problem]$ python parking-slots-linear.py 
5
x 1 2 3 4 5
[0, 1, 2, 3, 4, 5]
5 Already ordered!
4 Already ordered!
3 Already ordered!
2 Already ordered!
1 Already ordered!
Total moves: 0


TC: 6
[17:46:21 parking-slot-problem]$ python parking-slots-linear.py 
5
x 5 4 3 2 1
[0, 5, 4, 3, 2, 1]
5 [1, 5, 4, 3, 2, 0] -> [1, 0, 4, 3, 2, 5]
4 [1, 2, 4, 3, 0, 5] -> [1, 2, 0, 3, 4, 5]
3 Already ordered!
2 [1, 2, 0, 3, 4, 5] -> [1, 0, 2, 3, 4, 5]
1 [1, 0, 2, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
Total moves: 6

TC: 7
[17:47:03 parking-slot-problem]$ python parking-slots-linear.py 
5
3 2 x 4 5 1
[3, 2, 0, 4, 5, 1]
5 [3, 2, 1, 4, 5, 0] -> [3, 2, 1, 4, 0, 5]
4 [3, 2, 1, 4, 0, 5] -> [3, 2, 1, 0, 4, 5]
3 [3, 2, 1, 0, 4, 5] -> [0, 2, 1, 3, 4, 5]
2 [1, 2, 0, 3, 4, 5] -> [1, 0, 2, 3, 4, 5]
1 [1, 0, 2, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
Total moves: 7


TC: 8
[18:04:06 parking-slot-problem]$ python parking-slots-linear.py 
5
5 4 x 1 2 3
[5, 4, 0, 1, 2, 3]
5 [5, 4, 3, 1, 2, 0] -> [0, 4, 3, 1, 2, 5]
4 [2, 4, 3, 1, 0, 5] -> [2, 0, 3, 1, 4, 5]
3 [2, 1, 3, 0, 4, 5] -> [2, 1, 0, 3, 4, 5]
2 [2, 1, 0, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
1 Already ordered!
Total moves: 7
'''
