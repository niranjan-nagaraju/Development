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
def move_cars(slots, cars, empty_slot):
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
			update_cars_at_slots(cars, slots, empty_slot)

		print i, slots, # First move to ensure slot{i} is empty

		# Now find where car{i} is and move it the empty slot, {i}
		car_at_slot = cars[i]
		slots[i] = i # Move it to the right slot
		slots[car_at_slot] = 0 # Remove from slot where car{i} is right now
		empty_slot = car_at_slot # slot where car{i} was found is now empty

		# This is strictly not needed, as when a car has been moved to its
		# rightful position, we wont ever need to query it again
		# However, for the sake of completeness /AND/ the fact that the reverse lookup
		# looks confusing while debugging (plus it's an O(1) operation anyway), 
		# Update that car{i} has been moved to slot{i}
		update_cars_at_slots(cars, slots, i) 
		print '->', slots  # second move where car{i} is moved to slot{i}

		i = i - 1


# Update the reverse lookup table on which slot has which car
# when a car has been moved to slot{i} from slot{j}
# NOTE: slot{j} is empty now and reverse lookup doesn't need this info
def update_cars_at_slots(cars, slots, i):
	cars[slots[i]] = i


# Create a reverse lookup of cars -> slots
# so we can lookup in O(1) which slot[i], car[j] currently is
# Return reverse lookup table 
def create_reverse_lookup(slots, n):
	cars_at_slots = [0] * (n+1)
	i = 0 
	while i<=n:
		cars_at_slots[slots[i]] = i
		i = i + 1

	cars_at_slots[0] = 0 # There's no car numbered 0

	return cars_at_slots


def get_input_slots(n):
	i = 0
	current_slots_str = raw_input().split()
	slots = [0] * (n+1)
	while i <= n:
		try:
			slots[i] = int(current_slots_str[i])
		except ValueError:
			slots[i] = 0
			empty_slot = i
			pass
		i = i + 1

	return slots, empty_slot

n = int(raw_input()) # 'n+1' slots
slots, empty_slot =  get_input_slots(n)
cars = create_reverse_lookup(slots, n)
#print slots, empty_slot
#print cars
move_cars(slots, cars, empty_slot)


assert(slots == range(n+1))

'''
TC 1:
[00:11:47 parking-slot-problem]$ python parking-slots-linear.py 
5
x 1 2 3 4 5
5 Already ordered!
4 Already ordered!
3 Already ordered!
2 Already ordered!
1 Already ordered!

TC 2:
[00:12:41 parking-slot-problem]$ python parking-slots-linear.py 
5
5 4 x 3 2 1
5 [5, 4, 1, 3, 2, 0] -> [0, 4, 1, 3, 2, 5]
4 [2, 4, 1, 3, 0, 5] -> [2, 0, 1, 3, 4, 5]
3 Already ordered!
2 [2, 1, 0, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
1 Already ordered!

TC 3:
[00:15:10 parking-slot-problem]$ python parking-slots-linear.py 
5
3 2 x 4 5 1
5 [3, 2, 1, 4, 5, 0] -> [3, 2, 1, 4, 0, 5]
4 [3, 2, 1, 4, 0, 5] -> [3, 2, 1, 0, 4, 5]
3 [3, 2, 1, 0, 4, 5] -> [0, 2, 1, 3, 4, 5]
2 [1, 2, 0, 3, 4, 5] -> [1, 0, 2, 3, 4, 5]
1 [1, 0, 2, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]

TC 4:
[00:16:33 nnagaraj parking-slot-problem]$ python parking-slots-linear.py 
5
3 2 x 1 5 4
5 [3, 2, 4, 1, 5, 0] -> [3, 2, 4, 1, 0, 5]
4 [3, 2, 4, 1, 0, 5] -> [3, 2, 0, 1, 4, 5]
3 [3, 2, 1, 0, 4, 5] -> [0, 2, 1, 3, 4, 5]
2 [1, 2, 0, 3, 4, 5] -> [1, 0, 2, 3, 4, 5]
1 [1, 0, 2, 3, 4, 5] -> [0, 1, 2, 3, 4, 5]
'''
