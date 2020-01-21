'''
Forward Binary search on a sorted list

Usually performed when the length of a list is unknonwn before hand / a  semi-infinite stream of sorted items,
A forward binary search is essentially a binary search but in reverse, it starts at items[0:1],
then expands search horizon to [2:4], [5:8], [9:16], [17:32], etc until it finds a proper sub-array to match.
If it finds a plausible range, say [x:y], it'll then perform traditional binary search on items [x:y]

If one of the plausible range [l,h] exceeds stream length, then backtrack until a valid upperbound, h, is found
  and run binary search over [l,h]
'''


from binary_search import binary_search_r



# Find a safe upper-bound for stream, lst,
# within [l..h]
# if l itself is out of the stream's range, return -1
# Else, backtrack until stream's length is found
def find_safe_ub(lst, l, h):
	last_safe = -1
	try:
		if lst[l]:
			last_safe = l
	except IndexError:
		return -1

	mid = (l+h)/2
	while l<=h:
		mid = (l+h)/2
		try:
			lst[mid]
			last_safe = mid
			l = mid+1
		except IndexError:
			h = mid-1
		
	return last_safe
			
			

		

def forward_binary_search_r(lst, key):
	# Recursive helper to do forward binary search
	def forward_binary_search_helper(l=0, h=1):
		try:
			if key <= lst[h]: # key is in current window
				# Start a traditional binary search within the current search window, lst[l:h]
				return binary_search_r(lst, key, l, h)
			elif key > lst[h]: # key > current window, double window for next search
				return forward_binary_search_helper(h+1, h*2)
		except IndexError:
			# We have hit an IndexError at some index h==2^y
			# backtrack by half until we find an index that's actually valid
			# NOTE: It could very well be that [l, h] both are out of stream-length
			h = find_safe_ub(lst, l, h)

			# The entire [l,h] range exceeds stream length
			# and we haven't found the key yet => key doesn't exist in the stream
			if h == -1:
				return -1

			return binary_search_r(lst, key, l, h)

		# Couldn't find 'key'
		return -1

	# search key is less than the first item
	# in the stream.
	# since the stream is sorted in non-decreasing order,
	# it'll not be found later
	if key < lst[0]:
		return -1

	return forward_binary_search_helper()


if __name__ == "__main__":
	a = range(32)
	assert forward_binary_search_r(a, 16) == 16
	assert forward_binary_search_r(a, 31) == 31
	assert forward_binary_search_r(a, 32) == -1
	
	a = range(0, 32, 2)
	assert forward_binary_search_r(a, 11) == -1
	assert forward_binary_search_r(a, 2) == 1
	assert forward_binary_search_r(a, 8) == 4
	assert forward_binary_search_r(a, 10) == 5
	assert forward_binary_search_r(a, 9) == -1
	assert forward_binary_search_r(a, 64) == -1
	assert forward_binary_search_r(a, -1) == -1

	a = range(5, 64, 2)
	assert forward_binary_search_r(a, 4) == -1
	assert forward_binary_search_r(a, 64) == -1
	assert forward_binary_search_r(a, 63) == 29
	assert forward_binary_search_r(a, 5) == 0
	assert forward_binary_search_r(a, 11) == 3
	assert forward_binary_search_r(a, 19) == 7
	assert forward_binary_search_r(a, 27) == 11
	assert forward_binary_search_r(a, 37) == 16
	assert forward_binary_search_r(a, 53) == 24
	assert forward_binary_search_r(a, 55) == 25

