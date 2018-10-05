'''
Common code used between the two solutions for the parking slot problem
'''


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


# Update the reverse lookup table on which slot has which car
# when a car has been moved to slot{i} from slot{j}
# NOTE: slot{j} is empty now and reverse lookup doesn't need this info
def update_cars_at_slots(cars, slots, i):
	cars[slots[i]] = i


# Parse input and return current slot, empty slot to begin with.
# Format:
# Line1: n
# Line2: [current arrangement of cars in slots, x indicates empty-slot]
# Sample input
# 5
# 5 4 x 1 2 3
def get_input_slots():
	n = int(raw_input()) # 'n+1' slots
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

	return n, slots, empty_slot


