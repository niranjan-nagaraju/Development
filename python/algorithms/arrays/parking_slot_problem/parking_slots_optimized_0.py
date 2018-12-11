'''
There are N+1 parking spots, numbered from 0 to N. 
There are N cars numbered from 1 to N parked in various parking spots with one left empty. 
Reorder the cars so that car #1 is in spot #1, car #2 is in spot #2 and so on. 
Spot #0 will remain empty => There is no car #0. 
The only allowed operation is to take a car and move it to the free spot.
'''

'''
Solution:
	0. A reverse lookup table, to easily 'find' which slot a car is at.
	1. Identify car slot, 'e'.
	2. Find where car{e} is and move it to slot{e}, car{e} is now in its rightful position
	3. slot{x}, where car{e} originally was is now empty.
	4. slot{x} is now the new empty slot 'e', repeat [1] until all n cars are moved.

	5. Problems arise when there's a loop (e.g. cars[1,2] are at slots[2,1] etc and/or
  	     (i) when '0' is the empty slot to begin with or 
	     (ii) becomes the 'new' empty slot while we are still not done moving 'n'cars 
		     [This happens when there exists 'x' | car{x} is at slots{0}, so when we move car{x} to slot{x}, new empty slot is '0']

	6. Handle problems at [5] by going L-R, finding the first car{i}, slot{j} that's not in its rightful position and moving it to slot '0'. New empty slot is 'j' [Needs reverse lookup table to be updated for the move from j->0]
'''

import parking_slots_common	as c

# Recursively move cars from one empty slot to another
# Returns when all cars have been moved to their rightful position.
def move_car(slots, cars, empty_slot, num_cars_done, n):
	global total_moves
	# Moved all cars, return
	if num_cars_done == n:
		return

	# We have reached full circle back to empty-slot at slot 0
	# There are perhaps more cars to be sorted
	# At this point, if we have moved all 'n' cars, it means there's nothing else to do
	# Otherwise, we have prematurely hit slot 0 due to a 'loop'
	if empty_slot == 0:
		# Scan L->R for the first slot{i} with incorrectly parked car{j}
		# Move car{j} at slot{i} to 'empty slot' (0 in this case)
		# update reverse lookup - O(1) operation just update that car{j} is now at slot{0}
		# Repeat move_car
		for i in range(1,n+1):
			if slots[i] != i:
				break

		# All cars are already ordered
		if (i == n):
			print 'All cars are in order!'
			return

		slots[0] = cars[i] # Move car at slot{i} to 0
		slots[i] = 0 # slot{i} is now empty
		c.update_cars_at_slots(cars, slots, empty_slot)
		empty_slot = i # new empty slot

		total_moves += 1
		print ' ->', slots
		return move_car(slots, cars, empty_slot, num_cars_done, n)


	car_at_slot = cars[empty_slot]

	slots[car_at_slot] = 0 # Remove car from slot
	slots[empty_slot] = empty_slot # Move right-numbered car into the empty-slot

	# This is strictly not needed, as when a car has been moved to its
	# rightful position, we wont ever need to query it again
	# However, for the sake of completeness /AND/ the fact that the reverse lookup
	# looks confusing while debugging (plus it's an O(1) operation anyway), 
	# Update that car{i} has been moved to slot{i}
	c.update_cars_at_slots(cars, slots, empty_slot) 

	empty_slot = car_at_slot # new empty slot is where we moved from

	total_moves += 1
	print slots
	return move_car(slots, cars, empty_slot, (num_cars_done+1), n)


n, slots, empty_slot =  c.get_input_slots()
cars = c.create_reverse_lookup(slots, n)

print slots

total_moves = 0	  
move_car(slots, cars, empty_slot, 0, n)
print 'Total moves:', total_moves

assert(slots == range(n+1))

'''
TC: 1
[17:20:54]$ python parking_slots_optimized_0.py 
5
5 1 2 3 x 4
[5, 1, 2, 3, 0, 4]
[5, 1, 2, 3, 4, 0]
[0, 1, 2, 3, 4, 5]
All cars are in order!
Total moves: 2

TC: 2
[17:21:51]$ python parking_slots_optimized_0.py 
5
2 x 3 5 1 4
[2, 0, 3, 5, 1, 4]
[2, 1, 3, 5, 0, 4]
[2, 1, 3, 5, 4, 0]
[2, 1, 3, 0, 4, 5]
[2, 1, 0, 3, 4, 5]
[0, 1, 2, 3, 4, 5]
Total moves: 5

TC: 3
[17:25:00]$ python parking_slots_optimized_0.py 
5
5 4 x 3 2 1
[5, 4, 0, 3, 2, 1]
[5, 4, 2, 3, 0, 1]
[5, 0, 2, 3, 4, 1]
[5, 1, 2, 3, 4, 0]
[0, 1, 2, 3, 4, 5]
All cars are in order!
Total moves: 4

TC: 4
[17:26:13]$ python parking_slots_optimized_0.py 
5
3 2 x 1 5 4
[3, 2, 0, 1, 5, 4]
[3, 0, 2, 1, 5, 4]
[3, 1, 2, 0, 5, 4]
[0, 1, 2, 3, 5, 4]
 -> [5, 1, 2, 3, 0, 4]
[5, 1, 2, 3, 4, 0]
[0, 1, 2, 3, 4, 5]
Total moves: 6

TC: 5
[17:26:51]$ python parking_slots_optimized_0.py 
5          
x 1 2 3 4 5
[0, 1, 2, 3, 4, 5]
All cars are in order!
Total moves: 0

TC: 6
[17:27:20]$ python parking_slots_optimized_0.py 
5     
x 5 4 3 2 1 
[0, 5, 4, 3, 2, 1]
 -> [5, 0, 4, 3, 2, 1]
[5, 1, 4, 3, 2, 0]
[0, 1, 4, 3, 2, 5]
 -> [4, 1, 0, 3, 2, 5]
[4, 1, 2, 3, 0, 5]
[0, 1, 2, 3, 4, 5]
All cars are in order!
Total moves: 6


TC: 7
[17:34:14]$ python parking_slots_optimized_0.py 
5
3 2 x 4 5 1
[3, 2, 0, 4, 5, 1]
[3, 0, 2, 4, 5, 1]
[3, 1, 2, 4, 5, 0]
[3, 1, 2, 4, 0, 5]
[3, 1, 2, 0, 4, 5]
[0, 1, 2, 3, 4, 5]
Total moves: 5

TC: 8
[18:04:11]$ python parking_slots_optimized_0.py 
5
5 4 x 1 2 3
[5, 4, 0, 1, 2, 3]
[5, 4, 2, 1, 0, 3]
[5, 0, 2, 1, 4, 3]
[5, 1, 2, 0, 4, 3]
[5, 1, 2, 3, 4, 0]
[0, 1, 2, 3, 4, 5]
Total moves: 5
'''
