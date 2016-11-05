'''
There are N+1 parking spots, numbered from 0 to N. 
There are N cars numbered from 1 to N parked in various parking spots with one left empty. 
Reorder the cars so that car #1 is in spot #1, car #2 is in spot #2 and so on. 
Spot #0 will remain empty. The only allowed operation is to take a car and move it to the free spot.
'''

def move_car(slots, cars, empty_slot):
	car_at_slot = cars[empty_slot]

	print car_at_slot
	slots[car_at_slot] = 0 # Remove car from slot
	slots[empty_slot] = empty_slot # Move right-numbered car into the empty-slot
	empty_slot = car_at_slot # new empty slot is where we moved from

	return slots, empty_slot

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

for i in range(n):
	slots, empty_slot = move_car(slots, cars, empty_slot)
	print slots, empty_slot

assert(slots == range(n+1))


'''
TC: 1
[20:30:17 nnagaraj Algorithms]$ python parking-slots.py 
5
2 x 3 5 1 4
4
[2, 1, 3, 5, 0, 4] 4
5
[2, 1, 3, 5, 4, 0] 5
3
[2, 1, 3, 0, 4, 5] 3
2
[2, 1, 0, 3, 4, 5] 2
0
[0, 1, 2, 3, 4, 5] 0


TC: 2
[20:31:01 nnagaraj Algorithms]$ python parking-slots.py 
5
5 4 x 3 2 1
4
[5, 4, 2, 3, 0, 1] 4
1
[5, 0, 2, 3, 4, 1] 1
5
[5, 1, 2, 3, 4, 0] 5
0
[0, 1, 2, 3, 4, 5] 0
0
[0, 1, 2, 3, 4, 5] 0


FAILED TC: 3 TODO: Take care of loops, FTR, even [X, 2, 1] will fail
[20:27:03 nnagaraj Algorithms]$ python parking-slots.py 
5          
3 2 x 1 5 4
1
[3, 0, 2, 1, 5, 4] 1
3
[3, 1, 2, 0, 5, 4] 3
0
[0, 1, 2, 3, 5, 4] 0
0
[0, 1, 2, 3, 5, 4] 0
0
[0, 1, 2, 3, 5, 4] 0
Traceback (most recent call last):
	  File "parking-slots.py", line 59, in <module>
	      assert(slots == range(n+1))
		  AssertionError
'''
